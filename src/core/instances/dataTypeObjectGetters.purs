module Perspectives.Instances.ObjectGetters where

import Data.Array (findIndex, index, nub, singleton)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, context_pspType, rol_binding, rol_context, rol_properties)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (ObjectsGetter, MonadPerspectives)
import Perspectives.Identifiers (LocalName)
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Instances.Aliases (AnyContext)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType)
import Prelude (identity, join, ($), (<>), (>=>), (<<<), pure)

buitenRol :: ObjectsGetter
buitenRol = (getContextMember \c -> [context_buitenRol c])

context :: ObjectsGetter
context = getRolMember \rol -> [rol_context rol]

iedereRolInContext :: ObjectsGetter
iedereRolInContext = getContextMember \ctxt -> nub $ join $ values (context_iedereRolInContext ctxt)

binding :: ObjectsGetter
binding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

getProperty :: EnumeratedPropertyType -> ObjectsGetter
getProperty pn = getRolMember \(rol :: PerspectRol) -> maybe [] identity (lookup (unwrap pn) (rol_properties rol))

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r, including AspectProperties.
-- | E.g. getUnqualifiedProperty "voornaam"
getUnqualifiedProperty :: LocalName -> ObjectsGetter
getUnqualifiedProperty ln = getRolMember $ getUnQualifiedPropertyFromPerspectRol ln

getUnQualifiedPropertyFromPerspectRol :: LocalName -> PerspectRol -> Array String
getUnQualifiedPropertyFromPerspectRol ln rol =
  case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys $ rol_properties rol) of
    Nothing -> []
    (Just i) -> maybe [] identity (lookup (unsafePartial $ fromJust (index (keys $ rol_properties rol) i)) (rol_properties rol))

contextType :: AnyContext -> MonadPerspectives ContextType
contextType = getPerspectEntiteit >=> pure <<< context_pspType
