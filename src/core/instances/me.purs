module Perspectives.Instances.Me where

import Control.Alt ((<|>))
import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Writer (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.ContextAndRole (rol_binding, rol_isMe)
import Perspectives.CoreTypes (type (~~>), MP, liftToInstanceLevel)
import Perspectives.Instances.Combinators (filter, some) as Combinators
import Perspectives.Instances.Combinators (logicalAnd_)
import Perspectives.Instances.ObjectGetters (contextType, getMe, getPreferredUserRoleType, roleType, roleType_)
import Perspectives.Persistent (tryGetPerspectRol)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Role (roleTypeIsFunctional)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ResourceType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (isInPublicScheme, takeGuid)
import Perspectives.StrippedDelta (addSchemeToResourceIdentifier)
import Perspectives.Types.ObjectGetters (calculatedUserRole, contextAspectsClosure, enumeratedUserRole, isUnlinked_, userRole)
import Prelude (bind, map, not, pure, ($), (<<<), (>=>), (>>=))

-- | `isMe` has an internal error boundary. On failure, it returns false.
-- | If the role instance is a public resource, checks the binding regardless of the value of member 'me'.
-- | This is because 'me' is a purely local optimization that is never synchronized, but this fails for 
-- | obvious reasons for public resources.
-- TODO. If the identifier is in the pub: scheme, try to find a local resource with the same GUID.
-- This involves getting the type of the role instance and looking up where I store its instances.
-- If found, apply isMe to it.
-- Otherwise return false.
-- We must do this because the public version will never have a useful version of 'isMe'. Neither will it 
-- bottom out in a role that has a value for 'isMe': it will be public roles all to the bottom.
isMe :: RoleInstance -> MP Boolean
isMe ri = if isInPublicScheme (unwrap ri)
  then isPublicIdentifierMe ri
  else tryGetPerspectRol ri >>= case _ of
    Nothing -> pure false
    Just rol -> if rol_isMe rol
      then pure true
      else case rol_binding rol of
        Nothing -> pure false
        Just b -> isMe b

-- | From a RoleInstance with an identifier in the public scheme, 
-- | lookup its type and use that to create a new schemed identifier.
-- | Then apply isMe.
isPublicIdentifierMe :: RoleInstance -> MP Boolean
isPublicIdentifierMe rid@(RoleInstance s) = do
  t <- roleType_ rid
  storageSchemes <- gets _.typeToStorage
  s' <- addSchemeToResourceIdentifier storageSchemes (RType t) (takeGuid s)
  isMe (RoleInstance s')

notIsMe :: RoleInstance -> MP Boolean
notIsMe = isMe >=> pure <<< not

-- | Preferrably returns the value of the type of 'me' of the context instance:
-- | this will be the enumerated role instance that is filled (ultimately) with sys:Me.
-- | Otherwise returns all Calculated roles that are functional and ultimately filled with sys:Me.
-- | The Guest and Visitor conventions tap in here.
getMyType :: ContextInstance ~~> RoleType
getMyType ctxt = getPreferredUserRoleType ctxt
  <|>
  (getMe >=> map ENR <<< roleType) ctxt
  <|>
  -- NOTE: this is a safety measure that catches cases where the 'me' administration has gone wrong.
  findMeInEnumeratedRoles ctxt
  <|>
  findMeInUnlinkedRoles ctxt
  <|>
  findMeInCalculatedRoles ctxt
  where
    findMeInEnumeratedRoles :: ContextInstance ~~> RoleType
    findMeInEnumeratedRoles = (contextType >=> Combinators.filter (liftToInstanceLevel $ contextAspectsClosure >=> enumeratedUserRole) (computesMe ctxt))

    findMeInCalculatedRoles :: ContextInstance ~~> RoleType
    findMeInCalculatedRoles = (contextType >=> Combinators.filter 
      (liftToInstanceLevel $ contextAspectsClosure >=> calculatedUserRole)
      (logicalAnd_ 
        (lift <<< lift <<< roleTypeIsFunctional :: RoleType ~~> Boolean)
        (computesMe ctxt)))

    findMeInUnlinkedRoles ::  ContextInstance ~~> RoleType
    findMeInUnlinkedRoles = Combinators.filter (Combinators.filter (contextType >=> liftToInstanceLevel (contextAspectsClosure >=> enumeratedUserRole)) isUnlinked) (computesMe ctxt)
      where
        isUnlinked :: RoleType ~~> Boolean
        isUnlinked (ENR rt) = lift $ lift $ isUnlinked_ rt
        isUnlinked (CR _) = pure false

getAllMyRoleTypes :: ContextInstance ~~> RoleType
getAllMyRoleTypes ctxt = ((contextType >=> Combinators.filter (liftToInstanceLevel userRole) (computesMe ctxt)) ctxt)

computesMe :: ContextInstance -> RoleType ~~> Boolean
computesMe ctxt' rt = Combinators.some (getRoleInstances rt >=> lift <<< lift <<< isMe) ctxt'

getMeInRoleAndContext :: RoleType -> ContextInstance ~~> RoleInstance
getMeInRoleAndContext rt = Combinators.filter (getRoleInstances rt) (lift <<< lift <<< isMe)
