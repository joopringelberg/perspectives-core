module Perspectives.Instances.ObjectGetters where

import Data.Array (findIndex, index, nub, singleton)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_property, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (MonadPerspectives, type (~~>))
import Perspectives.Identifiers (LocalName)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (identity, join, ($), (<>), (>=>), (<<<), pure)

buitenRol :: ContextInstance ~~> RoleInstance
buitenRol = (getContextMember \c -> [context_buitenRol c])

context :: RoleInstance ~~> ContextInstance
context = getRolMember \rol -> [rol_context rol]

iedereRolInContext :: (ContextInstance ~~> RoleInstance)
iedereRolInContext = getContextMember \ctxt -> nub $ join $ values (context_iedereRolInContext ctxt)

binding :: RoleInstance ~~> RoleInstance
binding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

getProperty :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getProperty pn = getRolMember \(rol :: PerspectRol) -> rol_property rol (unwrap pn)

getRole :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getRole rn = getContextMember \(ctxt :: PerspectContext) -> (context_rolInContext ctxt rn)

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r, including AspectProperties.
-- | E.g. getUnqualifiedProperty "voornaam"
getUnqualifiedProperty :: LocalName -> (RoleInstance ~~> Value)
getUnqualifiedProperty ln = getRolMember $ getUnQualifiedPropertyFromPerspectRol ln

getUnQualifiedPropertyFromPerspectRol :: LocalName -> PerspectRol -> Array Value
getUnQualifiedPropertyFromPerspectRol ln rol =
  case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys $ rol_properties rol) of
    Nothing -> []
    (Just i) -> maybe [] identity (lookup (unsafePartial $ fromJust (index (keys $ rol_properties rol) i)) (rol_properties rol))

contextType :: ContextInstance -> MonadPerspectives ContextType
contextType = getPerspectEntiteit >=> pure <<< context_pspType

roleType :: RoleInstance -> MonadPerspectives EnumeratedRoleType
roleType = getPerspectEntiteit >=> pure <<< rol_pspType
