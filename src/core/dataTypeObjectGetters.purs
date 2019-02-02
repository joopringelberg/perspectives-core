module Perspectives.DataTypeObjectGetters where

import Data.Array (nub, singleton)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.StrMap (keys, values, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_binnenRol, context_buitenRol, context_displayName, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, TypedObjectGetter, TypedObjectsGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class RolType, BuitenRol, Context, ContextDef, PString, PropertyDef, RolDef, RolInContext, typeWithPerspectivesTypes)
import Prelude (bind, join, pure, ($), id)

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of context), others roles (such as the result of binding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall s o e. Partial => TypedObjectsGetter s o e -> TypedObjectGetter s o e
toSingle og id = do
  (ar :: Array o) <- og id
  pure $ ArrayPartial.head ar

contextType :: forall e. (Context ~~> ContextDef) e
contextType = typeWithPerspectivesTypes $ getContextMember \context -> [context_pspType context]

getRol' :: forall e. (Context ~~> RolInContext) e
getRol' rn = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (unwrap rn) (context_rolInContext context))

-- Returns an empty array if the context does not exist.
buitenRol :: forall e. (Context ~~> BuitenRol) e
buitenRol = typeWithPerspectivesTypes $ getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
buitenRol' :: forall e. Context -> MonadPerspectives (AjaxAvarCache e) (Maybe BuitenRol)
buitenRol' = typeWithPerspectivesTypes $ getContextMember' \c -> context_buitenRol c

iedereRolInContext :: forall e. (Context ~~> RolInContext) e
iedereRolInContext = typeWithPerspectivesTypes $ getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
typeVanIedereRolInContext :: forall e. (Context ~~> RolDef) e
typeVanIedereRolInContext = typeWithPerspectivesTypes $ getContextMember \context -> keys (context_rolInContext context)

-- | The names of every property given to this rol.
propertyTypen :: forall rt e. RolType rt => (rt ~~> PropertyDef) e
propertyTypen = typeWithPerspectivesTypes $ getRolMember \rol -> keys (rol_properties rol)

-- | The names of every internal property given to this context.
internePropertyTypen :: forall e. (Context ~~> PropertyDef) e
internePropertyTypen = typeWithPerspectivesTypes $ getContextMember \context -> keys (rol_properties (context_binnenRol context))

label :: forall e. (Context ~~> PString) e
label = typeWithPerspectivesTypes $ getContextMember \context -> [(context_displayName context)]

rolType :: forall rt e. RolType rt => (rt ~~> RolDef) e
rolType = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_pspType rol]

binding :: forall rt b e. RolType rt => Binding b => (rt ~~> b) e
binding = typeWithPerspectivesTypes $ getRolMember \rol -> maybe [] singleton (rol_binding rol)

context :: forall rt e. RolType rt => (rt ~~> Context) e
context = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_context rol]

-- | TODO. Oorspronkelijk definieerde ik dit als (rt ~~> ContextDef) e, maar dat klopt niet. Het kan zijn dat er semantische fouten zijn waar deze functie is gebruikt.
-- | NOTE. Without the type annotation of binding, the existence of type b is inferred by
-- | the compiler but it cannot derive its constraint (Binding).
rolBindingDef :: forall b rt e. Binding b => RolType rt => (rt ~~> Context) e
rolBindingDef = (binding :: (rt ~~> b) e) /-/ context

binding' :: forall rt b e. RolType rt => Binding b => TypedObjectGetter rt b e
binding' = unsafePartial $ toSingle binding
