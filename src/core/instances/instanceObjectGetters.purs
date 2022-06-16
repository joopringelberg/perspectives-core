-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Instances.ObjectGetters where

import Control.Monad.Error.Class (try)
import Control.Monad.Writer (lift, tell)
import Control.Plus (empty)
import Data.Array (elemIndex, filterA, findIndex, foldMap, foldl, head, index, length, null, singleton)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Map (Map, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap, ala)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_me, context_preferredUserRoleType, context_pspType, context_rolInContext, context_rolInContext_, rol_binding, rol_context, rol_gevuldeRol, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MonadPerspectives, (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Error.Boundaries (handlePerspectContextError', handlePerspectRolError')
import Perspectives.Identifiers (LocalName, deconstructBuitenRol, deconstructLocalName, deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectRol(..), externalRole, states) as IP
import Perspectives.Instances.Combinators (disjunction)
import Perspectives.Persistence.API (getViewOnDatabase)
import Perspectives.Persistent (entitiesDatabaseName, getPerspectContext, getPerspectEntiteit, getPerspectRol)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Role (actionsOfRoleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (StateSpec(..)) as SP
import Perspectives.Representation.TypeIdentifiers (ActionIdentifier(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..), StateIdentifier)
import Perspectives.TypesForDeltas (SubjectOfAction(..))
import Prelude (bind, discard, eq, flip, identity, map, not, pure, show, ($), (&&), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>))

-----------------------------------------------------------
-- FUNCTIONS FROM CONTEXT
-----------------------------------------------------------
-- | Because we never change the ExternalRole of a Context, we have no need
-- | to track it as a dependency.
externalRole :: ContextInstance ~~> RoleInstance
externalRole ci = ArrayT $ (try $ lift $ getContextMember IP.externalRole ci) >>=
  handlePerspectContextError' "externalRole" []
    \erole -> (tell $ ArrayWithoutDoubles [External ci]) *> pure [erole]

getEnumeratedRoleInstances :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getEnumeratedRoleInstances rn c = ArrayT $ (lift $ try $ getPerspectEntiteit c >>= flip context_rolInContext rn) >>=
  handlePerspectContextError' "getEnumeratedRoleInstances" []
    \(Tuple rn' instances) -> (tell $ ArrayWithoutDoubles [RoleAssumption c (EnumeratedRoleType rn')]) *> pure instances

getEnumeratedRoleInstances_ :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getEnumeratedRoleInstances_ rn c = ArrayT $ (lift $ try $ getPerspectEntiteit c >>= flip context_rolInContext_ rn) >>=
  handlePerspectContextError' "getEnumeratedRoleInstances" []
    \instances -> (tell $ ArrayWithoutDoubles [RoleAssumption c rn]) *> pure instances

getUnlinkedRoleInstances :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getUnlinkedRoleInstances rn c = ArrayT $ try ((lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/roleFromContext" (Just $ [unwrap rn, unwrap c])) >>=
  handlePerspectRolError' "getUnlinkedRoleInstances" []
    \(roles :: Array RoleInstance) -> (tell $ ArrayWithoutDoubles [RoleAssumption c rn]) *> pure roles

-- | Because we never change the type of a Context, we have no real need
-- | to track it as a dependency.
contextType :: ContextInstance ~~> ContextType
contextType cid  = ArrayT $ (lift $ try $ getContextMember (\c -> [context_pspType c]) cid) >>=
  handlePerspectRolError' "contextType" [] (pure <<< identity)

-- TODO. Fix the issue that an unlinked role does not show up for
-- a public context.
getMe :: ContextInstance ~~> RoleInstance
getMe ctxt = ArrayT $ (try $ lift $ getPerspectContext ctxt) >>=
  handlePerspectContextError' "getMe" []
    \c -> do
      tell $ ArrayWithoutDoubles [Me ctxt]
      pure $ maybe [] singleton (context_me c)

getPreferredUserRoleType :: ContextInstance ~~> RoleType
getPreferredUserRoleType ctxt = ArrayT $ (try $ lift $ getPerspectContext ctxt) >>=
  handlePerspectRolError' "getPreferredUserRoleType" []
    \c -> do
      tell $ ArrayWithoutDoubles [Me ctxt]
      pure $ maybe [] singleton (context_preferredUserRoleType c)

getActiveStates :: ContextInstance ~~> StateIdentifier
getActiveStates ci = ArrayT $ (try $ lift $ getContextMember IP.states ci) >>=
  handlePerspectContextError' "getActiveStates" []
    \states -> (tell $ ArrayWithoutDoubles [State ci]) *> pure states

getActiveStates_ :: ContextInstance -> MonadPerspectives (Array StateIdentifier)
getActiveStates_ ci = (try $ getContextMember IP.states ci) >>=
  handlePerspectContextError' "getActiveStates_" [] pure <<< identity

contextIsInState :: StateIdentifier -> ContextInstance -> MonadPerspectives Boolean
contextIsInState stateId ci = getActiveStates_ ci >>= pure <<< isJust <<< elemIndex stateId

-- | Get the ContextAction names for the context and user role instance, depending on the state
-- | of both.
getContextActions :: RoleType -> RoleInstance -> ContextInstance ~~> ActionIdentifier
getContextActions userRoleType userRoleInstance cid = ArrayT do
  (stateActionMap :: Map.Map SP.StateSpec (Object Action)) <- lift $ actionsOfRoleType userRoleType
  (contextStates :: Array StateIdentifier) <- runArrayT $ getActiveStates cid
  (userStates :: Array StateIdentifier) <- runArrayT $ getActiveRoleStates userRoleInstance
  -- Try each state of the subject and each state of the context
  pure $ ActionIdentifier <$> foldl
    (\(cumulatedActions :: Array String) (nextState :: SP.StateSpec) -> case Map.lookup nextState stateActionMap of
      Nothing -> cumulatedActions
      Just (actions :: Object Action) -> cumulatedActions <> keys actions)
    []
    ((SP.ContextState <$> contextStates) <> (SP.SubjectState <$> userStates))

-- | Returns the name of the model that defines the context type as a String Value.
contextModelName :: ContextInstance ~~> Value
contextModelName (ContextInstance cid) = maybe empty (pure <<< Value) (deconstructModelName cid)

-----------------------------------------------------------
-- FUNCTIONS FROM ROLE
-----------------------------------------------------------
-- | The ability to retrieve the Context of a RoleInstance depends on that RoleInstance being a Role of that Context.
context :: RoleInstance ~~> ContextInstance
context rid = ArrayT $ (lift $ try $ getPerspectEntiteit rid) >>= handlePerspectRolError' "context" []
  \(r :: IP.PerspectRol) -> do
  -- See: Implementing the Functional Reactive Pattern for a full justification of not
  -- recording an assumption.
  -- In short: a client who requests the context of rid, must have another request
  -- that yields rid in the first place. This request is dependent on that other
  -- request, client side. This means that, if rid is removed, the client is notified
  -- of that change and consequently is no longer interested in its context.
  tell $ ArrayWithoutDoubles [Context rid]
  pure $ [rol_context r]

context_ :: RoleInstance -> MonadPerspectives (Array ContextInstance)
context_ rid = (try $ getPerspectEntiteit rid) >>=
  handlePerspectRolError' "context_" []
  (pure <<< singleton <<< rol_context)

binding :: RoleInstance ~~> RoleInstance
binding r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "binding" []
  \(role :: IP.PerspectRol) -> do
    tell $ ArrayWithoutDoubles [Binding r]
    case rol_binding role of
      Nothing -> pure []
      (Just b) -> pure [b]

binding_ :: RoleInstance -> MonadPerspectives (Maybe RoleInstance)
binding_ r = (try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "binding_" Nothing
    \(role :: IP.PerspectRol) -> do
      case rol_binding role of
        Nothing -> pure Nothing
        (Just b) -> pure $ Just b

-- | Just the fillers that come instances of a particular ContextType.
bindingInContext :: ContextType -> RoleInstance ~~> RoleInstance
bindingInContext cType r = ArrayT do
  (fillers :: Array RoleInstance) <- runArrayT $ binding r
  filterA
    (\filler -> (lift $ try $ getPerspectEntiteit filler) >>=
      handlePerspectRolError' "bindingInContext" false
        \(role :: IP.PerspectRol) -> do
          fillerContextType <-  lift ((rol_context role) ##>> contextType)
          pure (eq cType fillerContextType))
    fillers

bottom :: RoleInstance ~~> RoleInstance
bottom r = ArrayT do
  (bs :: Array RoleInstance) <- runArrayT $ binding r
  case head bs of
    Nothing -> pure [r]
    Just b -> runArrayT $ bottom b

-- | From the instance of a Role (fillerId) of any kind, find the instances of the Role of the given
-- | type (filledId) that are filled with it. The type of rname (EnumeratedRoleType) may
-- | be psp:Context$externalRole.
-- getFilledRoles
getFilledRoles :: ContextType -> EnumeratedRoleType -> (RoleInstance ~~> RoleInstance)
getFilledRoles filledContextType filledType fillerId = ArrayT $ (lift $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError' "getFilledRoles" []
    \(filler :: IP.PerspectRol) -> case rol_gevuldeRol filler filledContextType filledType of
      {context:ct, role, instances} -> do
        tell $ ArrayWithoutDoubles [FilledRolesAssumption fillerId ct role]
        pure instances

getProperty :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getProperty pn r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getProperty" []
    \((IP.PerspectRol{properties}) :: IP.PerspectRol) -> do
      tell $ ArrayWithoutDoubles [Property r pn]
      case (lookup (unwrap pn) properties) of
        Nothing -> pure []
        (Just p) -> pure p

-- | Get a property on a chain of EnumeratedRole instances that are filled by each other.
-- | The function [getDynamicPropertyGetter](Perspectives.Query.UnsafeCompiler.html#t:getDynamicPropertyGetter)
-- | will compute the Values for a PropertyType (Enumerated or Calculated).
getPropertyFromTelescope :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getPropertyFromTelescope pn r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getPropertyFromTelescope" []
    \((IP.PerspectRol{properties, binding: bnd}) :: IP.PerspectRol) -> do
      tell $ ArrayWithoutDoubles [Property r pn]
      case (lookup (unwrap pn) properties) of
        Nothing -> do
          case bnd of
            Nothing -> pure []
            Just b -> runArrayT $ getPropertyFromTelescope pn b
        (Just p) -> pure p

-- | From a Property value getter, create a function that tries to find a value for the property
-- | on the role or its binding, recursively.
makeChainGetter :: (RoleInstance ~~> Value) -> (RoleInstance ~~> Value)
-- A beautiful definition that will not terminate:
-- makeChainGetter getter = disjunction getter (makeChainGetter (binding >=> getter))
makeChainGetter getter r = ArrayT do
  results <- runArrayT $ getter r
  if null results
    then do
      bnd <- runArrayT $ binding r
      case head bnd of
        Nothing -> pure []
        Just b -> runArrayT $ makeChainGetter getter b
    else pure results

-- | Turn a function that returns strings into one that returns Booleans.
makeBoolean :: forall a. (a ~~> Value) -> (a ~~> Boolean)
makeBoolean f = f >>> map (((==) "true") <<< unwrap)

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r, including AspectProperties.
-- | E.g. getUnqualifiedProperty "voornaam"
-- | NOTE: only adds an Assumption if a property value with a matching name can be found.
getUnqualifiedProperty :: LocalName -> (RoleInstance ~~> Value)
getUnqualifiedProperty ln r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getUnqualifiedProperty" []
    \(role@(IP.PerspectRol{properties}) :: IP.PerspectRol) -> case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys properties) of
      Nothing -> pure []
      (Just i) -> do
        pn <- pure (unsafePartial $ fromJust (index (keys $ rol_properties role) i))
        tell $ ArrayWithoutDoubles [Property r (EnumeratedPropertyType pn)]
        -- tell [assumption (unwrap r) pn]
        case (lookup pn properties) of
          Nothing -> pure []
          (Just p) -> pure p

-- | Because we never change the type of a Role, we have no real need
-- | to track it as a dependency.
roleType :: RoleInstance ~~> EnumeratedRoleType
roleType = ArrayT <<< lift <<< (getRolMember \r -> [rol_pspType r])

roleType_ :: RoleInstance -> MP EnumeratedRoleType
roleType_ = (getRolMember \r -> rol_pspType r)

hasType :: EnumeratedRoleType -> RoleInstance ~~> Boolean
hasType rt rid = ArrayT do
  t <- lift $ getRolMember rol_pspType rid
  pure $ [eq t rt]

-- | All the roles that bind the role instance.
allRoleBinders :: RoleInstance ~~> RoleInstance
allRoleBinders r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "allRoleBinders" []
    \((IP.PerspectRol{filledRoles}) :: IP.PerspectRol) ->
      foldWithIndexM
        (\cIndex (vals :: Array RoleInstance) (roleMap :: Object (Array RoleInstance)) -> do
          vals' <- foldWithIndexM
            (\rIndex (cum :: Array RoleInstance) (vals' :: Array RoleInstance) -> do
              tell $ ArrayWithoutDoubles [FilledRolesAssumption r (ContextType cIndex)(EnumeratedRoleType rIndex)]
              pure (cum <> vals'))
            []
            roleMap
          pure (vals <> vals'))
        []
        filledRoles

-- | `isMe` has an internal error boundary. On failure, it returns false.
isMe :: RoleInstance -> MP Boolean
isMe ri = (try $ getPerspectRol ri) >>=
  handlePerspectRolError' "isMe" false
    \(IP.PerspectRol{isMe: me, binding: bnd, pspType}) -> if me
      then pure true
      else case bnd of
        Nothing -> pure false
        Just b -> isMe b

notIsMe :: RoleInstance -> MP Boolean
notIsMe = isMe >=> pure <<< not

-- | Binding `boundBy` Binder is true,
-- | iff either Binding is the direct binding of Binder, or (binding Binder) `binds` Binding is true.
boundBy :: (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> Value)
boundBy sourceOfBoundRoles binder = ArrayT do
  (bools :: Array Boolean) <- runArrayT $ (sourceOfBoundRoles >=> bindsRole binder) binder
  -- If there are no boundRoles, this function must return false.
  if null bools
    then pure [Value $ show false]
    else pure [Value $ show $ ala Conj foldMap bools]

boundByOperator :: (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> Value)
boundByOperator sourceOfBoundRoles sourceOfBindingRoles originRole = ArrayT do
  boundRoles <- runArrayT (sourceOfBoundRoles originRole)
  bindingRoles <- runArrayT (sourceOfBindingRoles originRole)
  case head boundRoles, head bindingRoles of
    Just boundRole, Just bindingRole | length boundRoles == 1 && length bindingRoles == 1 -> do
      result <- (unsafePartial fromJust <<< head) <$> (runArrayT $ (bindsRole bindingRole) boundRole)
      pure [Value $ show result]
    _, _ -> pure [Value $ show false]

bindsOperator :: (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> Value)
bindsOperator sourceOfBindingRoles sourceOfBoundRoles = boundByOperator sourceOfBoundRoles sourceOfBindingRoles

-- role `bindsRole` bnd'
bindsRole :: RoleInstance -> RoleInstance ~~> Boolean
bindsRole role bnd' = ArrayT $
  if role == bnd'
    then pure [true]
    else do
      (bs :: Array RoleInstance) <- runArrayT $ binding role
      case head bs of
        Nothing -> pure [false]
        Just b -> runArrayT $ bindsRole b bnd'

-- | Binder `binds` Binding is true,
-- | iff either Binding is the direct binding of Binder, or (binding Binder) `binds` Binding is true.
-- | Query syntax: {step-that-produces-binder} >> binds {step-that-produces-binding}
binds :: (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> Value)
binds sourceOfBindingRoles bnd = ArrayT do
  (bools :: Array Boolean) <- runArrayT $ (sourceOfBindingRoles >=> boundByRole bnd) bnd
  -- If there are no bindingRoles, this function must return false.
  if null bools
    then pure [Value $ show false]
    else pure [Value $ show $ ala Conj foldMap bools]

-- bnd' `boundByRole` role
boundByRole :: RoleInstance -> RoleInstance ~~> Boolean
boundByRole bnd' role = ArrayT $
  if role == bnd'
    then pure [true]
    else do
      (bs :: Array RoleInstance) <- runArrayT $ binding role
      case head bs of
        Nothing -> pure [false]
        Just b -> runArrayT $ boundByRole bnd' b

-- subjectForContextInstance :: ContextInstance -> MonadPerspectivesTransaction SubjectOfAction
-- subjectForContextInstance contextId = do
--   msubject <- lift $ lift (contextId ##> getMe)
--   lift $ lift $ case msubject of
--     Nothing -> UserType <$> (contextId ##>> getMyType)
--     Just me -> pure $ UserInstance me
--
-- subjectForRoleInstance :: RoleInstance -> MonadPerspectivesTransaction SubjectOfAction
-- subjectForRoleInstance roleId = do
--   msubject <- lift $ lift (roleId ##> context >=> getMe)
--   lift $ lift $ case msubject of
--     Nothing -> UserType <$> (roleId ##>> context >=> getMyType)
--     Just me -> pure $ UserInstance me

typeOfSubjectOfAction :: SubjectOfAction -> MonadPerspectives RoleType
typeOfSubjectOfAction (UserInstance r) = (r ##>> roleType) >>= pure <<< ENR
typeOfSubjectOfAction (UserType t) = pure t

-- | Returns all the instances of the same role (including the argument instance!).
siblings :: RoleInstance ~~> RoleInstance
siblings rid = ArrayT $ (lift $ try $ getPerspectRol rid) >>=
  handlePerspectRolError' "siblings" []
    \(IP.PerspectRol{pspType, context:ctxt}) -> runArrayT $ getEnumeratedRoleInstances pspType ctxt

getActiveRoleStates :: RoleInstance ~~> StateIdentifier
getActiveRoleStates ri = ArrayT $ (try $ lift $ getRolMember (_.states <<< unwrap) ri) >>=
  handlePerspectRolError' "getActiveRoleStates" []
    \states -> (tell $ ArrayWithoutDoubles [RoleState ri]) *> pure states

getActiveRoleStates_ :: RoleInstance -> MonadPerspectives (Array StateIdentifier)
getActiveRoleStates_ ci = (try $ getRolMember (_.states <<< unwrap) ci) >>=
  handlePerspectRolError' "getActiveRoleStates_" [] pure <<< identity

roleIsInState :: StateIdentifier -> RoleInstance -> MonadPerspectives Boolean
roleIsInState stateId ri = getActiveRoleStates_ ri >>= pure <<< isJust <<< elemIndex stateId

-- | Returns the name of the model that defines the role type as a String Value.
roleModelName :: RoleInstance ~~> Value
roleModelName (RoleInstance rid) = maybe empty (pure <<< Value) (deconstructModelName rid)

-- | Return the value of the local property "Name", or return the last segment of the role type name.
getRoleName :: RoleInstance ~~> Value
getRoleName = disjunction
  (getUnqualifiedProperty "Name")
  (roleType >=> ArrayT <<< pure <<< map Value <<< (maybe [] singleton) <<< deconstructLocalName <<< deconstructBuitenRol <<< unwrap)
