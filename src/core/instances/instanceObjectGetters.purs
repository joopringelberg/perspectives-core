-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Instances.ObjectGetters where

import Control.Monad.Writer (lift, tell)
import Data.Array (findIndex, head, index, singleton)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (insert, keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_me, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (##>), type (~~>), MP, MonadPerspectives, assumption)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Identifiers (LocalName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..), externalRole) as IP
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol, saveEntiteit_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ActionType, ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (Unit, bind, discard, flip, identity, join, map, pure, void, ($), (*>), (<<<), (<>), (==), (>>=), (>>>))

-----------------------------------------------------------
-- FUNCTIONS FROM CONTEXT
-----------------------------------------------------------
trackContextDependency :: EnumeratedRoleType -> (ContextInstance ##> RoleInstance) -> (ContextInstance ~~> RoleInstance)
trackContextDependency roleName f c = (lift $ tell [(assumption (unwrap c) (unwrap roleName))]) *> (ArrayT $ lift $ f c)

-- | Because we never change the ExternalRole of a Context, we have no need
-- | to track it as a dependency.
externalRole :: ContextInstance ~~> RoleInstance
externalRole = lift <<< lift <<< getContextMember IP.externalRole

getRole :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getRole rn c = ArrayT do
  tell [assumption (unwrap c)(unwrap rn)]
  lift $ getContextMember (flip context_rolInContext rn) c

-- getRole rn = trackContextDependency rn (getContextMember \(ctxt :: PerspectContext) -> (context_rolInContext ctxt rn))

-- | Because we never change the type of a Context, we have no real need
-- | to track it as a dependency.
contextType :: ContextInstance ~~> ContextType
contextType = ArrayT <<< lift <<< (getContextMember \c -> [context_pspType c])

getConditionState :: ActionType -> ContextInstance -> MonadPerspectives Boolean
getConditionState a c = getPerspectContext c >>= \(IP.PerspectContext{actionConditionState}) -> pure $ maybe false identity (lookup (unwrap a) actionConditionState)

setConditionState :: ActionType -> ContextInstance -> Boolean -> MonadPerspectives Unit
setConditionState a c b = do
  (IP.PerspectContext r@{actionConditionState}) <- getPerspectContext c
  void $ saveEntiteit_ c (IP.PerspectContext r {actionConditionState = insert (unwrap a) b actionConditionState})

getMe :: ContextInstance ~~> RoleInstance
getMe ctxt = ArrayT do
  c <- lift $ getPerspectContext ctxt
  tell [assumption (unwrap ctxt) "model:System$Context$Me"]
  pure $ maybe [] singleton (context_me c)

-----------------------------------------------------------
-- FUNCTIONS FROM ROLE
-----------------------------------------------------------
-- | The ability to retrieve the Context of a RoleInstance depends on that RoleInstance being a Role of that Context.
context :: RoleInstance ~~> ContextInstance
context rid = ArrayT do
  (r :: IP.PerspectRol) <- lift $ getPerspectEntiteit rid
  -- See: Implementing the Functional Reactive Pattern for a full justification of not
  -- recording an assumption.
  -- In short: a client who requests the context of rid, must have another request
  -- that yields rid in the first place. This request is dependent on that other
  -- request, client side. This means that, if rid is removed, the client is notified
  -- of that change and consequently is no longer interested in its context.
  pure $ [rol_context r]

binding :: RoleInstance ~~> RoleInstance
binding r = ArrayT do
  (role :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  tell [assumption (unwrap r) "model:System$Role$binding"]
  case rol_binding role of
    Nothing -> pure []
    (Just b) -> pure [b]

bottom :: RoleInstance ~~> RoleInstance
bottom r = ArrayT do
  (bs :: Array RoleInstance) <- runArrayT $ binding r
  case head bs of
    Nothing -> pure [r]
    Just b -> runArrayT $ bottom b

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given
-- | type that bind it (that have it as their binding). The type of rname (RolDef) may
-- | be psp:Context$externalRole.
getRoleBinders :: EnumeratedRoleType -> (RoleInstance ~~> RoleInstance)
getRoleBinders rname r = ArrayT do
  ((IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  tell [assumption (unwrap r) (unwrap rname)]
  case (lookup (unwrap rname) gevuldeRollen) of
    Nothing -> pure []
    (Just bs) -> pure bs

-- | From the instance of a Rol of any kind, find the instances of the Rol with the given local name
-- | that bind it (that have it as their binding). The type of ln can be 'externalRole'.
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRoleBinders :: LocalName -> (RoleInstance ~~> RoleInstance)
getUnqualifiedRoleBinders ln r = ArrayT do
    (role@(IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
    case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys gevuldeRollen) of
      Nothing -> pure []
      (Just i) -> do
        rn <- pure (unsafePartial $ fromJust (index (keys gevuldeRollen) i))
        case lookup rn gevuldeRollen of
          Nothing -> pure []
          (Just bs) -> do
            for_ bs \b -> tell [assumption (unwrap b) "model:System$Role$binding"]
            pure bs

getProperty :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getProperty pn r = ArrayT do
  ((IP.PerspectRol{properties}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  tell [assumption (unwrap r)(unwrap pn)]
  case (lookup (unwrap pn) properties) of
    Nothing -> pure []
    (Just p) -> pure p

-- | Turn a function that returns strings into one that returns Booleans.
makeBoolean :: forall a. (a ~~> Value) -> (a ~~> Boolean)
makeBoolean f = f >>> map (((==) "true") <<< unwrap)

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r, including AspectProperties.
-- | E.g. getUnqualifiedProperty "voornaam"
getUnqualifiedProperty :: LocalName -> (RoleInstance ~~> Value)
getUnqualifiedProperty ln r = ArrayT do
  (role@(IP.PerspectRol{properties}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys properties) of
    Nothing -> pure []
    (Just i) -> do
      pn <- pure (unsafePartial $ fromJust (index (keys $ rol_properties role) i))
      tell [assumption (unwrap r) pn]
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
allRoleBinders r = ArrayT do
  ((IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  for_ (keys gevuldeRollen) (\key -> tell [assumption (unwrap r) key])
  pure $ join $ values gevuldeRollen

isMe :: RoleInstance -> MP Boolean
isMe ri = do
  (IP.PerspectRol{isMe: me, binding: bnd, pspType}) <- getPerspectRol ri
  if me
    then pure true
    else case bnd of
      Nothing -> pure false
      Just b -> isMe b
