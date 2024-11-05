  -- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-------------------------------------------------------------------------------
---- TRANSLATION
-------------------------------------------------------------------------------

module Purescript.ModelTranslation where

import Prelude

import Control.Monad.Reader (Reader, runReader, ask)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Set (toUnfoldable) as SET
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (Error)
import Foreign.Object (Object, empty, filterKeys, fromFoldable, isEmpty, lookup, mapWithKey, singleton, toUnfoldable)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Data.EncodableMap (keys, lookup) as EM
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (typeUri2LocalName_, typeUri2typeNameSpace_)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (StateSpec, stateSpec2StateIdentifier)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), PropertyType(..), RoleType, roletype2string)
import Purescript.YAML (load)

-- The keys are the languages; the values are the translations of the local type name.
-- They can be translations of any type.
newtype Translations = Translations (Object String)

instance Semigroup Translations where
  append (Translations t1) (Translations t2) = Translations (t1 <> t2)

-- The translations of many properties.
-- The keys are the property names; the values are their translations.
newtype PropertiesTranslation = PropertiesTranslation (Object Translations)

-- The keys are the Action names. Even though Action names are not qualified in the 
-- DomeinFile, we have qualified them in the Translation.
newtype ActionsTranslation = ActionsTranslation (Object Translations)

-- In the translation table we will generate a pseudo-qualified action name from the stateSpec and the local action name.
newtype ActionsPerStateTranslation = ActionsPerStateTranslation (Object ActionsTranslation)

newtype RoleTranslation = RoleTranslation
  { translations :: Translations
  , properties :: PropertiesTranslation
  , actions :: ActionsPerStateTranslation
  }

-- The keys are the local role names.
newtype RolesTranslation = RolesTranslation (Object RoleTranslation)

newtype ContextTranslation = ContextTranslation
  { translations :: Translations
  , users :: RolesTranslation
  , things :: RolesTranslation
  , contextroles :: RolesTranslation
  -- The keys are the local context names
  , contexts :: ContextsTranslation}

newtype ContextsTranslation = ContextsTranslation (Object ContextTranslation)

-- A singleton object. The key is the model name.
type ModelTranslation = 
  { namespace :: String
  , contexts :: Maybe ContextsTranslation
  , roles :: Maybe RolesTranslation
  }

-------------------------------------------------------------------------------
---- GENERATE FIRST TRANSLATION FROM DOMEINFILE
---- Generate a structure that can be declared to be JSON.
---- This is the canonical form that we generate from the YAML.
---- From this, generate the first YAML file;
---- From this, generate the key-value translation table.
-------------------------------------------------------------------------------
generateFirstTranslation :: DomeinFile -> ModelTranslation
generateFirstTranslation (DomeinFile dr) = flip runReader dr do 
  toplevelContexts <- pure $ filterKeys isDefinedAtTopLevel dr.contexts
  contexts <- if isEmpty toplevelContexts
    then pure Nothing 
    else Just <<< ContextsTranslation <$> (for dr.contexts translateContext)
  eroles <- for (filterKeys isDefinedAtTopLevel dr.enumeratedRoles)
    (translateRole <<< E)
  croles <- for (filterKeys isDefinedAtTopLevel dr.calculatedRoles)
   (translateRole <<< C)
  allRoles <- pure $ fromFoldable ((toUnfoldable eroles <> toUnfoldable croles) :: Array (Tuple String RoleTranslation))
  roles <- if isEmpty allRoles then pure Nothing else pure $ Just $ RolesTranslation allRoles
  pure { namespace: dr.namespace, contexts, roles}
  where
  isDefinedAtTopLevel :: String -> Boolean
  isDefinedAtTopLevel s = dr.namespace == typeUri2typeNameSpace_ s

translateContext :: Context -> Reader DomeinFileRecord ContextTranslation
translateContext (Context {id, gebruikerRol, contextRol, rolInContext, nestedContexts}) = do 
  users <- translateRoles gebruikerRol
  things <- translateRoles rolInContext
  contextroles <- translateRoles contextRol
  contexts <- ContextsTranslation <<< fromFoldable <$> for nestedContexts \ct -> (lookupContextType ct >>= translateContext >>= pure <<< Tuple (unwrap ct))
  pure $ ContextTranslation
    { translations: Translations empty
    , users
    , things
    , contextroles
    , contexts}
  where
  translateRoles :: Array RoleType -> Reader DomeinFileRecord RolesTranslation
  translateRoles roles = RolesTranslation <$> fromFoldable <$> for roles 
    (\rt -> (lookupRoleType rt >>= translateRole >>= pure <<< Tuple (roletype2string rt)))


lookupRoleType :: RoleType -> Reader DomeinFileRecord Role
lookupRoleType rt = do 
  {enumeratedRoles, calculatedRoles} <- ask
  case lookup (roletype2string rt) enumeratedRoles of 
    Nothing -> pure $ C $ unsafePartial fromJust $ lookup (roletype2string rt) calculatedRoles
    Just r -> pure $ E r

lookupContextType :: ContextType -> Reader DomeinFileRecord Context
lookupContextType (ContextType ct) = do 
  {contexts} <- ask
  unsafePartial case lookup ct contexts of 
    Just c -> pure c

translateRole :: Role -> Reader DomeinFileRecord RoleTranslation
translateRole (E (EnumeratedRole rec)) = do
  -- The translations of all properties.
  (properties :: PropertiesTranslation) <- do
    (ps :: Array (Tuple String Translations)) <- (for rec.properties translatePropertyType)
    pure $ PropertiesTranslation $ fromFoldable ps
  actions <- translateActions rec.actions
  pure $ RoleTranslation {translations: Translations empty, properties, actions}
translateRole (C (CalculatedRole rec)) = do 
  actions <- translateActions rec.actions
  pure $ RoleTranslation {translations: Translations empty, properties: PropertiesTranslation empty, actions}

translateActions :: EncodableMap StateSpec (Object Action) -> Reader DomeinFileRecord ActionsPerStateTranslation
translateActions actions = ActionsPerStateTranslation <<< fromFoldable <$> 
  (for (SET.toUnfoldable $ EM.keys actions :: Array StateSpec) \state -> pure $ Tuple 
    (unwrap $ stateSpec2StateIdentifier state) 
    (ActionsTranslation 
      (const (Translations empty) <$> (unsafePartial fromJust $ EM.lookup state actions))))

-- The translations of a single property.
translatePropertyType :: PropertyType -> Reader DomeinFileRecord (Tuple String Translations)
translatePropertyType (CP (CalculatedPropertyType p)) =  pure $ Tuple p (Translations empty)
translatePropertyType (ENP (EnumeratedPropertyType p)) = pure $ Tuple p (Translations empty)

-------------------------------------------------------------------------------
---- THE TRANSLATION CLASS
-------------------------------------------------------------------------------
class Translation a where 
  qualify :: String -> a -> a
  writeKeys :: a -> Writer (Object Translations) Unit
  writeYaml :: String -> a -> Writer String Unit
  addTranslations :: TranslationTable -> a -> a

instance Translation Translations where
  qualify _ a = a
  writeKeys _ = pure unit
  writeYaml indent (Translations translations) = do
    tell (i indent "translations" colonNl)
    void $ for (object2array translations)
      \(Tuple lang translation) -> do
        tell (i indent tab lang colonSpace translation nl)
  addTranslations _ t = t 

instance Translation PropertiesTranslation where
  qualify namespace (PropertiesTranslation propsAndTranslations) = PropertiesTranslation $ qualifyObjectKeys namespace propsAndTranslations
  writeKeys (PropertiesTranslation translations) = tell translations
  writeYaml indent (PropertiesTranslation properties) = void $ forWithIndex properties
    \propName translations -> do
      tell (i indent (typeUri2LocalName_ propName) colonNl)
      writeYaml (indent <> tab) translations
  addTranslations (TranslationTable table) (PropertiesTranslation obj) = PropertiesTranslation $
    flip mapWithKey obj \propName translations -> case lookup propName table of 
      Nothing -> translations
      Just ts -> ts

instance Translation ActionsTranslation where
  qualify namespace (ActionsTranslation obj) = ActionsTranslation $ qualifyObjectKeys namespace obj
  writeKeys (ActionsTranslation translations) = tell translations
  writeYaml indent (ActionsTranslation actions) = void $ forWithIndex actions
    \actionName translations -> do
      tell (i indent actionName colonNl)
      writeYaml (indent <> tab) translations
  addTranslations (TranslationTable table) (ActionsTranslation a) = ActionsTranslation $ 
    flip mapWithKey a \actionName translations -> case lookup actionName table of
      Nothing -> translations
      Just ts -> ts

instance Translation ActionsPerStateTranslation where
  qualify namespace (ActionsPerStateTranslation obj) = ActionsPerStateTranslation $ qualifyObjectKeys namespace obj
  writeKeys (ActionsPerStateTranslation actiontranslations) = void $ for actiontranslations writeKeys
  writeYaml indent (ActionsPerStateTranslation actions) = case actions of
    none | isEmpty none -> pure unit
    actions' -> do
      tell (i indent "actions" colonNl)
      void $ forWithIndex actions'
        \stateName acts -> do 
          tell (i (indent <> tab) (typeUri2LocalName_ stateName) colonNl)
          void $ for_ actions (writeYaml (indent <> tab <> tab))
  addTranslations table (ActionsPerStateTranslation a) = ActionsPerStateTranslation $ addTranslations table <$> a

instance Translation RolesTranslation where
  qualify namespace (RolesTranslation obj) = RolesTranslation ((qualify namespace) <$> obj)
  writeKeys (RolesTranslation roleTranslations) = forWithIndex_ roleTranslations 
    \roleName (RoleTranslation {translations, properties, actions}) -> do 
      tell (singleton roleName translations)
      writeKeys properties
      writeKeys actions
  writeYaml indent (RolesTranslation userRoles) = void $ for (object2array userRoles)
    \(Tuple roleName roleTranslation) -> do
      tell (i indent (typeUri2LocalName_ roleName) colonSpace)
      writeYaml (indent <> tab) roleTranslation
  addTranslations tbl@(TranslationTable table) (RolesTranslation a) = RolesTranslation $ flip mapWithIndex a 
    \roleName r@(RoleTranslation rec) -> case lookup roleName table of 
      Nothing -> addTranslations tbl r
      Just ts -> addTranslations tbl (RoleTranslation rec {translations = ts})

instance Translation RoleTranslation where
  qualify namespace (RoleTranslation {translations, properties, actions}) = let 
    properties' = qualify namespace properties
    actions' = qualify namespace actions
    in
    RoleTranslation {translations, properties: properties', actions: actions'}
  writeKeys _ = pure unit
  writeYaml indent (RoleTranslation {translations, properties, actions}) = do
    writeYaml indent translations
    writeYaml indent properties
    writeYaml indent actions
  addTranslations table (RoleTranslation {translations, properties, actions}) = RoleTranslation $ 
    { translations
    , properties: addTranslations table properties
    , actions: addTranslations table actions}

instance Translation ContextTranslation where
  qualify namespace (ContextTranslation {translations, users, things, contextroles, contexts}) = let 
    users' = qualify namespace users
    things' = qualify namespace things
    contextroles' = qualify namespace contextroles
    contexts' = qualify namespace contexts
    in
    ContextTranslation {translations, users: users', things: things', contextroles: contextroles', contexts: contexts'}
  writeKeys _ = pure unit
  writeYaml indent (ContextTranslation {translations, users, things, contextroles, contexts:ctxts}) = do 
    writeYaml indent translations
    tell (i indent "users" colonNl)
    writeYaml (indent <> tab) users
    tell (i indent "things" colonNl)
    writeYaml (indent <> tab) things
    tell (i indent "contextroles" colonNl)
    writeYaml (indent <> tab) contextroles
    tell (i indent "contexts" colonNl)
    writeYaml (indent <> tab) ctxts
  addTranslations table (ContextTranslation {translations, users, things, contextroles, contexts:ctxts}) = ContextTranslation 
    { translations
    , users: addTranslations table users
    , things: addTranslations table things
    , contextroles: addTranslations table contextroles
    , contexts: addTranslations table ctxts
    }

instance Translation ContextsTranslation where
  qualify namespace (ContextsTranslation obj) = ContextsTranslation ((qualify namespace) <$> obj)
  writeKeys (ContextsTranslation contextTranslations) = forWithIndex_ contextTranslations
    \contextName (ContextTranslation {translations, users, things, contextroles, contexts}) -> do
      tell (singleton contextName translations)
      writeKeys users
      writeKeys things
      writeKeys contextroles
      writeKeys contexts
  writeYaml indent (ContextsTranslation ctxts) = void $ for (object2array ctxts)
    \(Tuple contextName contextTranslation) -> do
      tell (i indent (typeUri2LocalName_ contextName) colonSpace)
      writeYaml (indent <> tab) contextTranslation
  addTranslations tbl@(TranslationTable table) (ContextsTranslation a) = ContextsTranslation $ flip mapWithIndex a 
    \contextName r@(ContextTranslation rec) -> case lookup contextName table of 
      Nothing -> addTranslations tbl r
      Just ts -> addTranslations tbl (ContextTranslation rec {translations = ts})

qualifyObjectKeys :: forall a. Translation a => String -> Object a -> Object a
qualifyObjectKeys namespace obj = fromFoldable 
  (((toUnfoldable obj) :: Array (Tuple String a)) <#> 
    \(Tuple key val) -> Tuple (i namespace "$" key) (qualify (i namespace "$" key) val))

object2array :: forall a. Object a -> Array (Tuple String a)
object2array = toUnfoldable

-------------------------------------------------------------------------------
---- GENERATE YAML FROM TRANSLATION
-------------------------------------------------------------------------------
tab :: String
tab = "  "

colonSpace :: String
colonSpace = ":"

colonNl :: String
colonNl = ":"

nl :: String
nl = "\n"

writeTranslationYaml :: ModelTranslation -> String
writeTranslationYaml {namespace, contexts, roles} = execWriter 
  do
    tell (namespace <> colonNl)
    for_ contexts \contexts' -> do
      tell (i tab "contexts" colonNl)
      writeYaml tab contexts'
    for_ roles \roles' -> do
      tell (i tab "roles" colonNl)
      writeYaml tab roles'

-------------------------------------------------------------------------------
---- QUALIFY NAMES IN TRANSLATION
-------------------------------------------------------------------------------
qualifyModelTranslation :: ModelTranslation -> ModelTranslation
qualifyModelTranslation {namespace, contexts, roles} = let
  contexts' = (qualify namespace) <$> contexts 
  roles' = (qualify namespace) <$> roles
  in
  {namespace, contexts, roles}

-------------------------------------------------------------------------------
---- CONSTRUCT TRANSLATIONTABLE FROM TRANSLATION
-------------------------------------------------------------------------------
-- Each key is the string representation of a type (ContextType, EnumeratedRoleType,
-- CalculatedRoleType, EnumeratedPropertyType, CalculatedPropertyType) or of the combination 
-- of a StateIdentifier and an Action name (separated by '$').
newtype TranslationTable = TranslationTable (Object Translations)

generateTranslationTable :: ModelTranslation -> TranslationTable
generateTranslationTable {contexts, roles} = TranslationTable $ execWriter do
  for_ contexts writeKeys
  for_ roles writeKeys

-------------------------------------------------------------------------------
---- PARSE YAML TO TRANSLATION
-------------------------------------------------------------------------------
parseTranslation :: String -> Effect (Either Error ModelTranslation)
parseTranslation source = map qualifyModelTranslation <$> load source

-------------------------------------------------------------------------------
---- ADD TRANSLATIONS
-- Add existing translations from a TranslationTable to a new Translations derived from a DomeinFile
-------------------------------------------------------------------------------
augmentModelTranslation :: TranslationTable -> ModelTranslation -> ModelTranslation
augmentModelTranslation table {namespace, contexts, roles} = 
  { namespace
  , contexts: addTranslations table <$> contexts
  , roles: addTranslations table <$> roles}