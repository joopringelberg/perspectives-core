module Perspectives.Instances.ObjectGetters where

import Control.Monad.Writer (lift, tell)
import Data.Array (findIndex, index)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_pspType, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), assumption, type (##>), MP)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Identifiers (LocalName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..), externalRole) as IP
import Perspectives.Instances (getPerspectContext, getPerspectEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (($), (<>), (<<<), pure, (*>), bind, discard, (>=>), (>>>), map, (==))

-----------------------------------------------------------
-- FUNCTIONS FROM CONTEXT
-----------------------------------------------------------
trackContextDependency :: EnumeratedRoleType -> (ContextInstance ##> RoleInstance) -> (ContextInstance ~~> RoleInstance)
trackContextDependency roleName f c = (lift $ tell [(assumption (unwrap c) (unwrap roleName))]) *> (ArrayT $ lift $ f c)

-- | Because we never change the ExternalRole of a Context, we have no need
-- | to track it as a dependency.
externalRole :: ContextInstance ~~> RoleInstance
externalRole = lift <<< lift <<< (getPerspectContext >=> pure <<< IP.externalRole)

getRole :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getRole rn c = ArrayT do
  ((IP.PerspectContext{rolInContext}) :: IP.PerspectContext) <- lift $ getPerspectEntiteit c
  case (lookup (unwrap rn) rolInContext) of
    Nothing -> pure []
    (Just r) -> do
      tell [assumption (unwrap c)(unwrap rn)]
      pure r

-- getRole rn = trackContextDependency rn (getContextMember \(ctxt :: PerspectContext) -> (context_rolInContext ctxt rn))

-- | Because we never change the type of a Context, we have no real need
-- | to track it as a dependency.
contextType :: ContextInstance ~~> ContextType
contextType = ArrayT <<< lift <<< (getContextMember \c -> [context_pspType c])

-----------------------------------------------------------
-- FUNCTIONS FROM ROLE
-----------------------------------------------------------
-- | The ability to retrieve the Context of a RoleInstance depends on that RoleInstance being a Role of that Context.
context :: RoleInstance ~~> ContextInstance
context rid = ArrayT do
  (r :: IP.PerspectRol) <- lift $ getPerspectEntiteit rid
  tell [(assumption (unwrap $ rol_context r) (unwrap $ rol_pspType r))]
  pure $ [rol_context r]

binding :: RoleInstance ~~> RoleInstance
binding r = ArrayT do
  (role :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  case rol_binding role of
    Nothing -> pure []
    (Just b) -> do
      tell [assumption (unwrap r) "model:Perspectives$Role$binding"]
      pure [b]

getProperty :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getProperty pn r = ArrayT do
  ((IP.PerspectRol{properties}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  case (lookup (unwrap pn) properties) of
    Nothing -> pure []
    (Just p) -> do
      tell [assumption (unwrap r)(unwrap pn)]
      pure p

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
      case (lookup pn properties) of
        Nothing -> pure []
        (Just p) -> do
          tell [assumption (unwrap r) pn]
          pure p

-- | Because we never change the type of a Role, we have no real need
-- | to track it as a dependency.
roleType :: RoleInstance ~~> EnumeratedRoleType
roleType = ArrayT <<< lift <<< (getRolMember \r -> [rol_pspType r])

roleType_ :: RoleInstance -> MP EnumeratedRoleType
roleType_ = (getRolMember \r -> rol_pspType r)

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given
-- | type that bind it (that have it as their binding). The type of rname (RolDef) may
-- | be psp:Context$externalRole.
getRoleBinders :: EnumeratedRoleType -> (RoleInstance ~~> RoleInstance)
getRoleBinders rname r = ArrayT do
  ((IP.PerspectRol{gevuldeRollen}) :: IP.PerspectRol) <- lift $ getPerspectEntiteit r
  case (lookup (unwrap rname) gevuldeRollen) of
    Nothing -> pure []
    (Just g) -> do
      tell [assumption (unwrap r) (unwrap rname)]
      pure g

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
          (Just b) -> do
            tell [assumption (unwrap r) rn]
            pure b
