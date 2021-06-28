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

import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Control.Monad.Writer (lift, tell)
import Control.Plus (empty)
import Data.Array (elemIndex, findIndex, foldMap, head, index, null, singleton)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap, ala)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_me, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MonadPerspectives, MonadPerspectivesTransaction, liftToInstanceLevel, (##=), (##>), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Error.Boundaries (handlePerspectContextError', handlePerspectRolError')
import Perspectives.Identifiers (LocalName, deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectRol(..), externalRole, states) as IP
import Perspectives.Instances.Combinators (conjunction, disjunction)
import Perspectives.Persistence.API (getViewOnDatabase)
import Perspectives.Persistent (entitiesDatabaseName, getPerspectContext, getPerspectEntiteit, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..), StateIdentifier)
import Perspectives.Types.ObjectGetters (lookForUnqualifiedRoleType)
import Perspectives.TypesForDeltas (SubjectOfAction(..))
import Prelude (bind, discard, flip, identity, join, map, not, pure, show, ($), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>))

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
getEnumeratedRoleInstances rn c = ArrayT $ (lift $ try $ getContextMember (flip context_rolInContext rn) c) >>=
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

getMe :: ContextInstance ~~> RoleInstance
getMe ctxt = ArrayT $ (try $ lift $ getPerspectContext ctxt) >>=
  handlePerspectContextError' "getMe" []
    \c -> do
      tell $ ArrayWithoutDoubles [Me ctxt (context_me c)]
      pure $ maybe [] singleton (context_me c)

-- | If the user has no role, return the role with the Aspect "model:System$Invitation$Guest".
getMyType :: ContextInstance ~~> RoleType
getMyType ctxt = (getMe >=> map ENR <<< roleType) ctxt
  <|>
  (contextType >=> liftToInstanceLevel (lookForUnqualifiedRoleType "Guest")) ctxt

getActiveStates :: ContextInstance ~~> StateIdentifier
getActiveStates ci = ArrayT $ (try $ lift $ getContextMember IP.states ci) >>=
  handlePerspectContextError' "getActiveStates" []
    \states -> (tell $ ArrayWithoutDoubles [State ci]) *> pure states

getActiveStates_ :: ContextInstance -> MonadPerspectives (Array StateIdentifier)
getActiveStates_ ci = (try $ getContextMember IP.states ci) >>=
  handlePerspectContextError' "getActiveStates_" [] pure <<< identity

contextIsInState :: StateIdentifier -> ContextInstance -> MonadPerspectives Boolean
contextIsInState stateId ci = getActiveStates_ ci >>= pure <<< isJust <<< elemIndex stateId

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

bottom :: RoleInstance ~~> RoleInstance
bottom r = ArrayT do
  (bs :: Array RoleInstance) <- runArrayT $ binding r
  case head bs of
    Nothing -> pure [r]
    Just b -> runArrayT $ bottom b

-- | From the instance of a Role of any kind, find the instances of the Role of the given
-- | type that bind it (that have it as their binding). The type of rname (EnumeratedRoleType) may
-- | be psp:Context$externalRole.
getRoleBinders :: EnumeratedRoleType -> (RoleInstance ~~> RoleInstance)
getRoleBinders rname r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getRoleBinders" []
    \((IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) -> do
      tell $ ArrayWithoutDoubles [Binder r rname]
      case (lookup (unwrap rname) gevuldeRollen) of
        Nothing -> pure []
        (Just bs) -> pure bs

-- | From the instance of a Rol of any kind, find the instances of the Rol with the given local name
-- | that bind it (that have it as their binding). The type of ln can be 'externalRole'.
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRoleBinders :: LocalName -> (RoleInstance ~~> RoleInstance)
getUnqualifiedRoleBinders ln r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getUnqualifiedRoleBinders" []
    \(role@(IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) -> do
      case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys gevuldeRollen) of
        Nothing -> pure []
        (Just i) -> do
          rn <- pure (unsafePartial $ fromJust (index (keys gevuldeRollen) i))
          tell $ ArrayWithoutDoubles [Binder r (EnumeratedRoleType rn)]
          -- tell [assumption (unwrap r) rn]
          case lookup rn gevuldeRollen of
            Nothing -> pure []
            (Just bs) -> pure bs

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

-- | All the roles that bind the role instance.
allRoleBinders :: RoleInstance ~~> RoleInstance
allRoleBinders r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "allRoleBinders" []
    \((IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) -> do
      for_ (keys gevuldeRollen) (\key -> tell $ ArrayWithoutDoubles [Binder r (EnumeratedRoleType key)]) -- tell [assumption (unwrap r) key])
      pure $ join $ values gevuldeRollen

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
  (bools :: Array Boolean) <- lift (binder ##= sourceOfBoundRoles >=> bindsRole binder)
  -- If there are no boundRoles, this function must return false.
  if null bools
    then pure [Value $ show false]
    else pure [Value $ show $ ala Conj foldMap bools]

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
  (bools :: Array Boolean) <- lift (bnd ##= sourceOfBindingRoles >=> boundByRole bnd)
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

subjectForContextInstance :: ContextInstance -> MonadPerspectivesTransaction SubjectOfAction
subjectForContextInstance contextId = do
  msubject <- lift $ lift (contextId ##> getMe)
  lift $ lift $ case msubject of
    Nothing -> UserType <$> (contextId ##>> getMyType)
    Just me -> pure $ UserInstance me

subjectForRoleInstance :: RoleInstance -> MonadPerspectivesTransaction SubjectOfAction
subjectForRoleInstance roleId = do
  msubject <- lift $ lift (roleId ##> context >=> getMe)
  lift $ lift $ case msubject of
    Nothing -> UserType <$> (roleId ##>> context >=> getMyType)
    Just me -> pure $ UserInstance me

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
  handlePerspectContextError' "getActiveRoleStates" []
    \states -> (tell $ ArrayWithoutDoubles [RoleState ri]) *> pure states

getActiveRoleStates_ :: RoleInstance -> MonadPerspectives (Array StateIdentifier)
getActiveRoleStates_ ci = (try $ getRolMember (_.states <<< unwrap) ci) >>=
  handlePerspectContextError' "getActiveRoleStates_" [] pure <<< identity

roleIsInState :: StateIdentifier -> RoleInstance -> MonadPerspectives Boolean
roleIsInState stateId ri = getActiveRoleStates_ ri >>= pure <<< isJust <<< elemIndex stateId

-- | Returns the name of the model that defines the role type as a String Value.
roleModelName :: RoleInstance ~~> Value
roleModelName (RoleInstance rid) = maybe empty (pure <<< Value) (deconstructModelName rid)

-- | Returns the name of the model that defines the context type as a String Value.
contextModelName :: ContextInstance ~~> Value
contextModelName (ContextInstance cid) = maybe empty (pure <<< Value) (deconstructModelName cid)
