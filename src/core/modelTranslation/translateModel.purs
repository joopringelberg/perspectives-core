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
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Set (toUnfoldable) as SET
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, filterKeys, fromFoldable, isEmpty, lookup, toUnfoldable)
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

-- The keys are the languages; the values are the translations of the local type name.
-- They can be translations of any type.
newtype Translations = Translations (Object String)

-- The translations of many properties.
-- The keys are the local property names; the values are their translations.
newtype PropertiesTranslation = PropertiesTranslation (Object Translations)

newtype ActionsTranslation = ActionsTranslation (Object Translations)

-- In the translation table we will generate a pseudo-qualified action name from the stateSpec and the local action name.
newtype ActionsPerStateTranslation = ActionsPerStateTranslation (Object ActionsTranslation)

type RoleTranslation = 
  { translations :: Translations
  , properties :: PropertiesTranslation
  , actions :: ActionsPerStateTranslation
  }

-- The keys are the local role names.
type RolesTranslation = Object RoleTranslation

newtype ContextTranslation = ContextTranslation
  { translations :: Translations
  , users :: RolesTranslation
  , things :: RolesTranslation
  , contextroles :: RolesTranslation
  -- The keys are the local context names
  , contexts :: Object ContextTranslation}

-- A singleton object. The key is the model name.
type ModelTranslation = 
  { namespace :: String
  , contexts :: Maybe (Object ContextTranslation)
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
    else Just <$> (for dr.contexts translateContext)
  eroles <- for (filterKeys isDefinedAtTopLevel dr.enumeratedRoles)
    (translateRole <<< E)
  croles <- for (filterKeys isDefinedAtTopLevel dr.calculatedRoles)
   (translateRole <<< C)
  allRoles <- pure $ fromFoldable ((toUnfoldable eroles <> toUnfoldable croles) :: Array (Tuple String RoleTranslation))
  roles <- if isEmpty allRoles then pure Nothing else pure $ Just allRoles
  pure { namespace: dr.namespace, contexts, roles}
  where
  isDefinedAtTopLevel :: String -> Boolean
  isDefinedAtTopLevel s = dr.namespace == typeUri2typeNameSpace_ s

translateContext :: Context -> Reader DomeinFileRecord ContextTranslation
translateContext (Context {id, gebruikerRol, contextRol, rolInContext, nestedContexts}) = do 
  users <- translateRoles gebruikerRol
  things <- translateRoles rolInContext
  contextroles <- translateRoles contextRol
  contexts <- fromFoldable <$> for nestedContexts \ct -> (lookupContextType ct >>= translateContext >>= pure <<< Tuple (unwrap ct))
  pure $ ContextTranslation
    { translations: Translations empty
    , users
    , things
    , contextroles
    , contexts}
  where
  translateRoles :: Array RoleType -> Reader DomeinFileRecord RolesTranslation
  translateRoles roles = fromFoldable <$> for roles 
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
  pure {translations: Translations empty, properties, actions}
translateRole (C (CalculatedRole rec)) = do 
  actions <- translateActions rec.actions
  pure {translations: Translations empty, properties: PropertiesTranslation empty, actions}

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
    tell $ namespace <> colonNl
    case contexts of 
      Nothing -> pure unit
      Just c -> do 
        tell (i tab "contexts" colonNl)
        void $ for (object2array c)
          (writeContext (tab <> tab))
  
  where
  writeContext :: String -> (Tuple String ContextTranslation) -> Writer String Unit
  writeContext indent (Tuple cname (ContextTranslation {translations, users, things, contextroles, contexts:ctxts})) = do 
    -- ContextName: 
    tell indent
    tell $ (typeUri2LocalName_ cname) <> colonNl
    -- translations: 
    --   en: Context name
    --   nl: Context naam
    writeTranslations (indent <> tab) translations
    -- users: 
    tell (i indent "users" colonNl)
    writeRoles (indent <> tab) users
    -- things: 
    tell (i indent "things" colonNl)
    writeRoles (indent <> tab) things
    -- contextroles: 
    tell (i indent "contextroles" colonNl)
    writeRoles (indent <> tab) contextroles
    -- Nested contexts
    tell (i indent "contexts" colonNl)
    writeNestedContexts (indent <> tab) ctxts

  writeNestedContexts :: String -> Object ContextTranslation -> Writer String Unit
  writeNestedContexts indent contextTypes = do 
    void $ for (object2array contextTypes)
      (writeContext  indent)

  writeRoles :: String -> RolesTranslation -> Writer String Unit
  writeRoles indent userRoles = do
    void $ for (object2array userRoles)
      \(Tuple roleName {translations, properties, actions}) -> do
        tell (i indent roleName colonSpace)
        writeTranslations (indent <> tab) translations
        case properties of 
          PropertiesTranslation properties' -> void $ for (object2array properties') (writeProperty (indent <> tab))
        case actions of
          ActionsPerStateTranslation none | isEmpty none -> pure unit
          ActionsPerStateTranslation actions' -> do
            tell (i indent tab "actions" colonNl)
            void $ for (object2array actions') (writeActions (indent <> tab <> tab))
  
  writeActions :: String -> Tuple String ActionsTranslation -> Writer String Unit
  writeActions indent (Tuple state (ActionsTranslation actions)) = do
    tell (i indent state colonNl)
    void $ for (object2array actions) (writeAction (indent <> tab))

  writeAction :: String -> Tuple String Translations -> Writer String Unit
  writeAction indent (Tuple action translations) = do 
    tell (i indent action colonNl)
    writeTranslations (indent <> tab) translations

  writeProperty :: String -> Tuple String Translations -> Writer String Unit
  writeProperty indent (Tuple prop translations) = do
    tell (i indent prop colonNl)
    writeTranslations (indent <> tab) translations
  
  writeTranslations :: String -> Translations -> Writer String Unit
  writeTranslations indent (Translations translations) = do
    tell (i indent "translations" colonNl)
    void $ for (object2array translations)
      \(Tuple lang translation) -> do
        tell (i indent tab lang colonSpace translation nl)

object2array :: forall a. Object a -> Array (Tuple String a)
object2array = toUnfoldable