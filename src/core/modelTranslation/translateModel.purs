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

module Perspectives.ModelTranslation where

import Prelude

import Control.Monad.Reader (Reader, runReader, ask)
import Control.Monad.State (get, lift, modify, runStateT)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Array (filter, head, null, catMaybes)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, notNull, null, toMaybe) as NULL
import Data.Set (toUnfoldable) as SET
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (Error)
import Foreign.Object (Object, empty, filterKeys, fromFoldable, isEmpty, keys, lookup, mapWithKey, singleton, toUnfoldable, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Data.EncodableMap (keys, lookup, values) as EM
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (typeUri2LocalName_, typeUri2typeNameSpace_, startsWithSegments, buitenRol)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (StateSpec, stateSpec2StateIdentifier)
import Perspectives.Representation.State (Notification(..), State(..), StateFulObject(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), PropertyType(..), RoleType, roletype2string)
import Purescript.YAML (load)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write)

-------------------------------------------------------------------------------
---- MODEL TRANSLATION REPRESENTATION
-------------------------------------------------------------------------------

-- The keys are the languages; the values are the translations of the local type name.
-- They can be translations of any type.
newtype Translations = Translations (Object String)
derive newtype instance WriteForeign Translations
derive newtype instance ReadForeign Translations

instance Semigroup Translations where
  append (Translations t1) (Translations t2) = Translations (t1 <> t2)

-- The keys are the original notification texts.
newtype NotificationsTranslation = NotificationsTranslation (Object Translations)
derive newtype instance WriteForeign NotificationsTranslation
derive newtype instance ReadForeign NotificationsTranslation

-- The translations of many properties.
-- The keys are the property names; the values are their translations.
newtype PropertiesTranslation = PropertiesTranslation (Object Translations)
derive newtype instance WriteForeign PropertiesTranslation
derive newtype instance ReadForeign PropertiesTranslation

-- The keys are the Action names. Even though Action names are not qualified in the 
-- DomeinFile, we have qualified them in the Translation.
newtype ActionsTranslation = ActionsTranslation (Object Translations)
derive newtype instance WriteForeign ActionsTranslation
derive newtype instance ReadForeign ActionsTranslation

-- Keys are the string representations of StateSpecs.
-- In the translation table we will generate a pseudo-qualified action name from the stateSpec 
-- and the local action name.
newtype ActionsPerStateTranslation = ActionsPerStateTranslation (Object ActionsTranslation)
derive newtype instance WriteForeign ActionsPerStateTranslation
derive newtype instance ReadForeign ActionsPerStateTranslation

newtype RoleTranslation = RoleTranslation
  { translations :: Translations
  , properties :: PropertiesTranslation
  , actions :: ActionsPerStateTranslation
  , notifications :: NotificationsTranslation
  }
derive newtype instance WriteForeign RoleTranslation
derive newtype instance ReadForeign RoleTranslation

-- The keys are the local role names.
newtype RolesTranslation = RolesTranslation (Object RoleTranslation)
derive newtype instance WriteForeign RolesTranslation
derive newtype instance ReadForeign RolesTranslation

newtype ContextTranslation = ContextTranslation
  { translations :: Translations
  , external :: RoleTranslation
  , users :: RolesTranslation
  , things :: RolesTranslation
  , contextroles :: RolesTranslation
  , notifications :: NotificationsTranslation
  -- The keys are the local context names
  , contexts :: ContextsTranslation}

instance WriteForeign ContextTranslation where
  writeImpl (ContextTranslation rec) = write rec
derive newtype instance ReadForeign ContextTranslation

newtype ContextsTranslation = ContextsTranslation (Object ContextTranslation)
instance WriteForeign ContextsTranslation where
  writeImpl (ContextsTranslation obj) = write obj
instance ReadForeign ContextsTranslation where
  readImpl f = do 
    (ct :: Object ContextTranslation) <- readImpl f
    pure $ ContextsTranslation ct

-- A singleton object. The key is the model name.
newtype ModelTranslation = ModelTranslation
  { namespace :: String
  , contexts :: Maybe ContextsTranslation
  , roles :: Maybe RolesTranslation
  }

derive newtype instance WriteForeign ModelTranslation
derive newtype instance ReadForeign ModelTranslation

-------------------------------------------------------------------------------
---- YAML FILE PARSE REPRESENTATION
---- In a YAML file we omit members that are empty in the ModelTranslation.
---- On reconstructing the Model Translation from the YAML, we should add them again.
-------------------------------------------------------------------------------
newtype ModelTranslation_ = ModelTranslation_
  { namespace :: NULL.Nullable String
  , contexts :: NULL.Nullable ContextsTranslation_
  , roles :: NULL.Nullable RolesTranslation_
  }

newtype ContextTranslation_ = ContextTranslation_
  { translations :: Translations_
  , external :: NULL.Nullable RoleTranslation_
  , users :: NULL.Nullable RolesTranslation_
  , things :: NULL.Nullable RolesTranslation_
  , contextroles :: NULL.Nullable RolesTranslation_
  -- The keys are the local context names
  , contexts :: NULL.Nullable ContextsTranslation_
  -- The keys are generated numbers and are of no interest.
  -- The original texts are stored under the key 'orig'.
  -- These notifications are lexically embedded in context state transitions.
  , notifications :: NULL.Nullable Notifications_
  }

newtype ContextsTranslation_ = ContextsTranslation_ (Object ContextTranslation_)

newtype RolesTranslation_ = RolesTranslation_ (Object RoleTranslation_)

newtype RoleTranslation_ = RoleTranslation_
  { translations :: Translations_
  , properties :: NULL.Nullable PropertiesTranslation_
  , actions :: NULL.Nullable ActionsPerStateTranslation_
  , markdowns :: NULL.Nullable (Object Translations_)
  -- These notifications are lexically embedded in role state transitions.
  , notifications :: NULL.Nullable Notifications_
  }

newtype PropertiesTranslation_ = PropertiesTranslation_ (Object {translations :: Translations_})

newtype ActionsPerStateTranslation_ = ActionsPerStateTranslation_ (Object ActionsTranslation_)

newtype ActionsTranslation_ = ActionsTranslation_ (Object { translations :: Translations_})

newtype Translations_ = Translations_ (Object (NULL.Nullable String))

-- The keys are generated numbers and are of no interest.
-- The original texts are stored under the key 'orig'.
newtype Notifications_ = Notifications_ (Object Translations_)
-------------------------------------------------------------------------------
---- CLASS REHYDRATE
-------------------------------------------------------------------------------

class (Translation modeltranslation) <= Rehydrate fromyaml modeltranslation | fromyaml -> modeltranslation where 
  hydrate :: fromyaml -> modeltranslation

instance Rehydrate Translations_ Translations where
  hydrate (Translations_ obj) = Translations $ fromFoldable $ catMaybes ((toUnfoldable obj) <#> \(Tuple language ntranslation) -> case NULL.toMaybe ntranslation of 
    Just translation -> Just (Tuple language translation)
    _ -> Nothing)

instance Rehydrate Notifications_ NotificationsTranslation where
  hydrate (Notifications_ obj) = NotificationsTranslation $ fromFoldable $ (values obj) <#> (\t -> let 
    Translations t' = hydrate t 
    orig = unsafePartial fromJust $ lookup "orig" t'
    in Tuple orig (Translations t')
  )

instance Rehydrate ActionsTranslation_ ActionsTranslation where
  -- Do away with the extra "translations" layer.
  hydrate (ActionsTranslation_ obj) = ActionsTranslation (obj <#> _.translations >>> hydrate)

instance Rehydrate RoleTranslation_ RoleTranslation where
  hydrate (RoleTranslation_ {translations, properties, actions, markdowns, notifications}) = 
    RoleTranslation { translations: translations', properties: properties', actions: actions', notifications: notifications'}
    where
    properties' = case NULL.toMaybe properties of 
      Nothing -> PropertiesTranslation empty
      Just (PropertiesTranslation_ obj) -> PropertiesTranslation (obj <#> _.translations >>> hydrate)
    actions' = case NULL.toMaybe actions of 
      Nothing -> ActionsPerStateTranslation empty
      Just (ActionsPerStateTranslation_ acts) -> ActionsPerStateTranslation (hydrate <$> acts)
    translations' = hydrate translations
    notifications' = case NULL.toMaybe notifications of 
      Nothing -> NotificationsTranslation empty
      Just (Notifications_ notifications_) -> NotificationsTranslation (hydrate <$> notifications_)

instance Rehydrate ContextTranslation_ ContextTranslation where
  hydrate (ContextTranslation_ {translations, external, users, things, contextroles, contexts, notifications}) = let
    translations' = hydrate translations
    external' = case NULL.toMaybe external of 
      Nothing -> RoleTranslation 
        { translations: Translations empty
        , properties: PropertiesTranslation empty
        , actions: ActionsPerStateTranslation empty
        , notifications: NotificationsTranslation empty}
      Just e@(RoleTranslation_ _) -> (hydrate e)
    users' = case NULL.toMaybe users of 
      Nothing -> RolesTranslation empty
      Just (RolesTranslation_ u) -> RolesTranslation (map hydrate u)
    things' = case NULL.toMaybe things of 
      Nothing -> RolesTranslation empty
      Just (RolesTranslation_ t) -> RolesTranslation (map hydrate t)
    contextroles' = case NULL.toMaybe contextroles of 
      Nothing -> RolesTranslation empty
      Just (RolesTranslation_ t) -> RolesTranslation (map hydrate t)
    contexts' = case NULL.toMaybe contexts of 
      Nothing -> ContextsTranslation empty
      Just (ContextsTranslation_ t) -> ContextsTranslation (map hydrate t)
    notifications' = case NULL.toMaybe notifications of 
      Nothing -> NotificationsTranslation empty
      Just (Notifications_ notifications_) -> NotificationsTranslation (hydrate <$> notifications_)
    in 
    ContextTranslation 
      { translations:translations'
      , external:external'
      , notifications: notifications'
      , users:users'
      , things:things'
      , contextroles:contextroles'
      , contexts:contexts'}

instance Rehydrate ModelTranslation_ ModelTranslation where
  hydrate (ModelTranslation_ { namespace, contexts, roles}) = let 
    namespace' = case NULL.toMaybe namespace of 
      Nothing -> "MissingNamespace" 
      Just n -> n
    contexts' = case NULL.toMaybe contexts of 
      Nothing -> ContextsTranslation empty
      Just (ContextsTranslation_ t) -> ContextsTranslation (map hydrate t)
    roles' = case NULL.toMaybe roles of 
      Nothing -> RolesTranslation empty
      Just (RolesTranslation_ t) -> RolesTranslation (map hydrate t)
    in 
    ModelTranslation {namespace:namespace', contexts:Just contexts', roles:Just roles'}

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
    else Just <<< ContextsTranslation <$> (for (filterKeys (not <<< eq dr.namespace) toplevelContexts) translateContext)
  eroles <- for (filterKeys isDefinedAtTopLevel dr.enumeratedRoles)
    (translateRole <<< E)
  croles <- for (filterKeys isDefinedAtTopLevel dr.calculatedRoles)
   (translateRole <<< C)
  allRoles <- pure $ fromFoldable ((toUnfoldable eroles <> toUnfoldable croles) :: Array (Tuple String RoleTranslation))
  roles <- if isEmpty allRoles then pure Nothing else pure $ Just $ RolesTranslation allRoles
  pure $ ModelTranslation { namespace: dr.namespace, contexts, roles}
  where
  isDefinedAtTopLevel :: String -> Boolean
  isDefinedAtTopLevel s = dr.namespace == typeUri2typeNameSpace_ s && (isNothing $ match (unsafeRegex "External$" noFlags) s )

translateContext :: Context -> Reader DomeinFileRecord ContextTranslation
translateContext (Context {id:contextId, gebruikerRol, contextRol, rolInContext, nestedContexts}) = do 
  {enumeratedRoles} <- ask
  external <- unsafePartial case lookup (buitenRol $ unwrap contextId) enumeratedRoles of 
    Just e -> translateRole (E e)
  users <- translateRoles gebruikerRol
  things <- translateRoles rolInContext
  contextroles <- translateRoles contextRol
  contexts <- ContextsTranslation <<< fromFoldable <$> for 
    (filter (\ct -> (unwrap ct) `startsWithSegments` (unwrap contextId) ) nestedContexts) 
    \ct -> (lookupContextType ct >>= translateContext >>= pure <<< Tuple (unwrap ct))
  notifications <- do
    {states} <- ask
    pure $ NotificationsTranslation (execWriter $ for_ states (\(State {stateFulObject, notifyOnEntry, notifyOnExit}) -> case stateFulObject of 
      Cnt c | c == contextId -> for_ (EM.values notifyOnEntry) translateNotification
      _ -> pure unit))
  pure $ ContextTranslation
    { translations: Translations empty
    , external
    , notifications
    , users
    , things
    , contextroles
    , contexts}
  where
  -- Writes singletons with the original sentence and empty translations

  translateRoles :: Array RoleType -> Reader DomeinFileRecord RolesTranslation
  translateRoles roles = RolesTranslation <$> fromFoldable <$> for 
    (filter (\rt -> (roletype2string rt) `startsWithSegments` (unwrap contextId)) roles)
    (\rt -> (lookupRoleType rt >>= translateRole >>= pure <<< Tuple (roletype2string rt)))

translateNotification :: Notification -> Writer (Object Translations) Unit
translateNotification (ContextNotification {sentence}) = tell $ singleton (_.sentence $ unwrap sentence) (Translations empty)
translateNotification (RoleNotification {sentence}) = tell $ singleton (_.sentence $ unwrap sentence) (Translations empty)

-- Aspect roles that have been added as is will turn up here, but should not be translated.
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
  notifications <- do
    {states} <- ask
    pure $ NotificationsTranslation (execWriter $ for_ states (\(State {stateFulObject, notifyOnEntry, notifyOnExit}) -> case stateFulObject of 
      Orole r | r == rec.id -> for_ (EM.values notifyOnEntry) translateNotification
      Srole r | r == rec.id -> for_ (EM.values notifyOnEntry) translateNotification
      _ -> pure unit))
  pure $ RoleTranslation {translations: Translations empty, properties, actions, notifications}
translateRole (C (CalculatedRole rec)) = do 
  actions <- translateActions rec.actions
  pure $ RoleTranslation {translations: Translations empty, properties: PropertiesTranslation empty, actions, notifications: NotificationsTranslation empty}

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
---- CLASS TRANSLATION 
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
    translations' <- case object2array translations of
      none | null none -> pure [Tuple "en" "", Tuple "nl" ""]
      ts -> pure ts
    void $ for translations'
      \(Tuple lang translation) -> do
        tell (i indent tab lang colonSpace translation nl)
  addTranslations _ t = t 

instance Translation NotificationsTranslation where
  qualify _ n = n
  writeKeys (NotificationsTranslation notifications) = forWithIndex_ notifications
    \original translations -> tell (singleton original translations)
  writeYaml indent (NotificationsTranslation notifications) = if isEmpty notifications
    then pure unit
    else do
      tell (i indent "notifications" colonNl)
      void $ runStateT
        (forWithIndex notifications
          \original translations -> do
            nr <- get
            lift $ tell (i indent tab nr colonNl)
            lift $ writeYaml (indent <> tab <> tab) translations
            modify ((+) 1)
            )
        0
  addTranslations (TranslationTable table) (NotificationsTranslation notifications) = NotificationsTranslation $
    flip mapWithKey notifications \original translations -> case lookup original table of 
      Nothing -> translations
      Just ts -> ts

instance Translation PropertiesTranslation where
  qualify namespace (PropertiesTranslation propsAndTranslations) = PropertiesTranslation $ qualifyObjectKeys namespace propsAndTranslations
  writeKeys (PropertiesTranslation propTranslations) = forWithIndex_ propTranslations
    \propertyName translations -> tell (singleton propertyName translations)
  writeYaml indent (PropertiesTranslation properties) = if isEmpty properties
    then pure unit
    else do 
      tell (i indent "properties" colonNl)
      void $ forWithIndex properties
        \propName translations -> do
          tell (i (indent <> tab) (typeUri2LocalName_ propName) colonNl)
          writeYaml (indent <> tab <> tab) translations
  addTranslations (TranslationTable table) (PropertiesTranslation obj) = PropertiesTranslation $
    flip mapWithKey obj \propName translations -> case lookup propName table of 
      Nothing -> translations
      Just ts -> ts

instance Translation ActionsTranslation where
  qualify namespace (ActionsTranslation obj) = ActionsTranslation $ qualifyObjectKeys namespace obj
  writeKeys (ActionsTranslation actionTranslations) = forWithIndex_ actionTranslations
    \actionName translations -> tell (singleton actionName translations)
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
          writeYaml (indent <> tab <> tab) acts
  addTranslations table (ActionsPerStateTranslation a) = ActionsPerStateTranslation $ addTranslations table <$> a

instance Translation RolesTranslation where
  qualify namespace (RolesTranslation obj) = 
    RolesTranslation $ fromFoldable
      (( toUnfoldable obj <#> \(Tuple unqualifiedName role) -> let 
        qualifiedName = namespace <> "$" <> unqualifiedName
        in Tuple qualifiedName (qualify qualifiedName role)
      ) :: Array (Tuple String RoleTranslation))
  writeKeys (RolesTranslation roleTranslations) = forWithIndex_ roleTranslations 
    \roleName (RoleTranslation {translations, properties, actions}) -> do 
      tell (singleton roleName translations)
      writeKeys properties
      writeKeys actions
  writeYaml indent (RolesTranslation userRoles) = void $ for (object2array userRoles)
    \(Tuple roleName roleTranslation) -> do
      tell (i indent (typeUri2LocalName_ roleName) colonNl)
      writeYaml (indent <> tab) roleTranslation
  addTranslations tbl@(TranslationTable table) (RolesTranslation a) = RolesTranslation $ flip mapWithIndex a 
    \roleName r@(RoleTranslation rec) -> case lookup roleName table of 
      Nothing -> addTranslations tbl r
      Just ts -> addTranslations tbl (RoleTranslation rec {translations = ts})

instance Translation RoleTranslation where
  qualify namespace (RoleTranslation {translations, properties, actions, notifications}) = let 
    properties' = qualify namespace properties
    actions' = qualify namespace actions
    in
    RoleTranslation {translations, properties: properties', actions: actions', notifications}
  writeKeys _ = pure unit
  writeYaml indent (RoleTranslation {translations, properties, actions}) = do
    writeYaml indent translations
    writeYaml indent properties
    writeYaml indent actions
  addTranslations table (RoleTranslation {translations, properties, actions, notifications}) = RoleTranslation $ 
    { translations
    , properties: addTranslations table properties
    , actions: addTranslations table actions
    , notifications: addTranslations table notifications}

instance Translation ContextTranslation where
  qualify namespace (ContextTranslation {translations, external:ext, users, things, contextroles, contexts, notifications}) = let 
    external = qualify namespace ext
    users' = qualify namespace users
    things' = qualify namespace things
    contextroles' = qualify namespace contextroles
    contexts' = qualify namespace contexts
    in
    ContextTranslation {translations, external, users: users', things: things', contextroles: contextroles', contexts: contexts', notifications}
  writeKeys _ = pure unit
  writeYaml indent (ContextTranslation {translations, external, users, things, contextroles, contexts:ctxts}) = do 
    writeYaml indent translations
    case external of 
      RoleTranslation {properties, actions} -> case properties, actions of
        PropertiesTranslation properties', ActionsPerStateTranslation actions' -> if isEmpty properties' && isEmpty actions'
          then pure unit
          else do 
            tell (i indent "external" colonNl)
            writeYaml (indent <> tab) properties
            writeYaml (indent <> tab) actions
    case users of
      RolesTranslation users' -> if isEmpty users'
        then pure unit
        else do 
          tell (i indent "users" colonNl)
          writeYaml (indent <> tab) users
    case things of
      RolesTranslation things' -> if isEmpty things'
        then pure unit
        else do 
          tell (i indent "things" colonNl)
          writeYaml (indent <> tab) things
    case contextroles of
      RolesTranslation contextroles' -> if isEmpty contextroles'
        then pure unit
        else do 
          tell (i indent "contextroles" colonNl)
          writeYaml (indent <> tab) contextroles
    case ctxts of
      ContextsTranslation ctxts' -> if isEmpty ctxts'
        then pure unit
        else do 
          tell (i indent "contexts" colonNl)
          writeYaml (indent <> tab) ctxts
  addTranslations table (ContextTranslation {translations, external, users, things, contextroles, contexts:ctxts, notifications}) = ContextTranslation 
    { translations
    , external: addTranslations table external
    , users: addTranslations table users
    , things: addTranslations table things
    , contextroles: addTranslations table contextroles
    , contexts: addTranslations table ctxts
    , notifications: addTranslations table notifications
    }

instance Translation ContextsTranslation where
  qualify namespace (ContextsTranslation obj) = 
    ContextsTranslation $ fromFoldable
      (( toUnfoldable obj <#> \(Tuple unqualifiedName ctxt) -> let 
        qualifiedName = namespace <> "$" <> unqualifiedName
        in Tuple qualifiedName (qualify qualifiedName ctxt)
      ) :: Array (Tuple String ContextTranslation))
  writeKeys (ContextsTranslation contextTranslations) = forWithIndex_ contextTranslations
    \contextName (ContextTranslation {translations, users, things, contextroles, contexts}) -> do
      tell (singleton contextName translations)
      writeKeys users
      writeKeys things
      writeKeys contextroles
      writeKeys contexts
  writeYaml indent (ContextsTranslation ctxts) = void $ for (object2array ctxts)
    \(Tuple contextName contextTranslation) -> do
      tell (i indent (typeUri2LocalName_ contextName) colonNl)
      writeYaml (indent <> tab) contextTranslation
  addTranslations tbl@(TranslationTable table) (ContextsTranslation a) = ContextsTranslation $ flip mapWithIndex a 
    \contextName r@(ContextTranslation rec) -> case lookup contextName table of 
      Nothing -> addTranslations tbl r
      Just ts -> addTranslations tbl (ContextTranslation rec {translations = ts})

instance Translation ModelTranslation where
  qualify namespace (ModelTranslation{contexts, roles}) = let
    contexts' = (qualify namespace) <$> contexts 
    roles' = (qualify namespace) <$> roles
    in
    ModelTranslation {namespace, contexts:contexts', roles:roles'}
  writeKeys (ModelTranslation {contexts, roles}) = do
    case contexts of 
      Just cs -> writeKeys cs
      Nothing -> pure unit
    case roles of 
      Just rs -> writeKeys rs
      Nothing -> pure unit
  writeYaml indent (ModelTranslation {namespace, contexts, roles}) = do
    tell (namespace <> colonNl)
    for_ contexts \contexts' -> do
      tell (i tab "contexts" colonNl)
      writeYaml (tab <> tab) contexts'
    for_ roles \roles' -> do
      tell (i tab "roles" colonNl)
      writeYaml (tab <> tab) roles'
  addTranslations table (ModelTranslation {namespace, contexts, roles}) = ModelTranslation
    { namespace
    , contexts: addTranslations table <$> contexts
    , roles: addTranslations table <$> roles}

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
colonNl = ":\n"

nl :: String
nl = "\n"

writeTranslationYaml :: ModelTranslation -> String
writeTranslationYaml modeltranslation = execWriter (writeYaml "" modeltranslation)

-------------------------------------------------------------------------------
---- QUALIFY NAMES IN TRANSLATION
-------------------------------------------------------------------------------
qualifyModelTranslation :: ModelTranslation -> ModelTranslation
qualifyModelTranslation m@(ModelTranslation{namespace}) = qualify namespace m

-------------------------------------------------------------------------------
---- CONSTRUCT TRANSLATIONTABLE FROM TRANSLATION
-------------------------------------------------------------------------------
-- Each key is the string representation of a type (ContextType, EnumeratedRoleType,
-- CalculatedRoleType, EnumeratedPropertyType, CalculatedPropertyType) or of the combination 
-- of a StateIdentifier and an Action name (separated by '$').
newtype TranslationTable = TranslationTable (Object Translations)
derive newtype instance ReadForeign TranslationTable
derive newtype instance WriteForeign TranslationTable

generateTranslationTable :: ModelTranslation -> TranslationTable
generateTranslationTable modeltranslation = TranslationTable $ execWriter (writeKeys modeltranslation)

-------------------------------------------------------------------------------
---- PARSE YAML TO TRANSLATION
-------------------------------------------------------------------------------
parseTranslation :: String -> Effect (Either Error ModelTranslation)
parseTranslation source = do 
  parseResult :: Either Error ParsedYaml <- load source
  modelTranslation :: Either Error ModelTranslation <- pure (map (hydrate <<< toModelTranslation_) parseResult)
  pure (map qualifyModelTranslation modelTranslation)
  where
  toModelTranslation_ :: ParsedYaml -> ModelTranslation_
  toModelTranslation_ yaml = case head $ keys yaml of 
    Just namespace -> let 
      {contexts, roles} = unsafePartial fromJust $ lookup namespace yaml
      in 
      ModelTranslation_ {namespace: NULL.notNull namespace, contexts, roles}
    Nothing -> ModelTranslation_ { namespace: NULL.notNull "IllegalYaml", contexts: NULL.null, roles: NULL.null}

type ParsedYaml = Object {contexts :: NULL.Nullable ContextsTranslation_, roles :: NULL.Nullable RolesTranslation_}
-------------------------------------------------------------------------------
---- ADD TRANSLATIONS
-- Add existing translations from a TranslationTable to a new Translations derived from a DomeinFile
-------------------------------------------------------------------------------
augmentModelTranslation :: TranslationTable -> ModelTranslation -> ModelTranslation
augmentModelTranslation = addTranslations

-------------------------------------------------------------------------------
---- RETRIEVE THE LOCAL TRANSLATION TABLE FOR A MODEL
-------------------------------------------------------------------------------
