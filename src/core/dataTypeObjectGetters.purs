module Perspectives.DataTypeObjectGetters where

import Data.Array (nub, singleton)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe, maybe)
import Data.StrMap (keys, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_binnenRol, context_buitenRol, context_displayName, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, TypedObjectGetter, TypedObjectsGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolType, class RolVanContext, BuitenRol, ContextDef, PString, PropertyDef, RolDef, RolInContext, binding, context, contextType, typeWithPerspectivesTypes)
import Prelude (bind, join, pure, ($))

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of context), others roles (such as the result of binding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall s o e. TypedObjectsGetter s o e -> TypedObjectGetter s o e
toSingle og id = do
  (ar :: Array o) <- og id
  pure $ unsafePartial $ ArrayPartial.head ar

-- contextType :: forall s o e. ContextType s => ContextType o => (s ~~> o) e
-- contextType = typeWithPerspectivesTypes $ getContextMember \context -> [context_pspType context]

-- Returns an empty array if the context does not exist.
buitenRol :: forall r s e. ContextType s => (s ~~> BuitenRol) e
buitenRol = typeWithPerspectivesTypes $ getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
buitenRol' :: forall r s e. ContextType s => s -> MonadPerspectives (AjaxAvarCache e) (Maybe BuitenRol)
buitenRol' = typeWithPerspectivesTypes $ getContextMember' \c -> context_buitenRol c

iedereRolInContext :: forall r s e. ContextType s => (s ~~> RolInContext) e
iedereRolInContext = typeWithPerspectivesTypes $ getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
typeVanIedereRolInContext :: forall s o e. ContextType s => RolType o => (s ~~> o) e
typeVanIedereRolInContext = typeWithPerspectivesTypes $ getContextMember \context -> keys (context_rolInContext context)

-- | The names of every property given to this rol.
propertyTypen :: forall rt e. RolType rt => (rt ~~> PropertyDef) e
propertyTypen = typeWithPerspectivesTypes $ getRolMember \rol -> keys (rol_properties rol)

-- | The names of every internal property given to this context.
internePropertyTypen :: forall s e. ContextType s => (s ~~> PropertyDef) e
internePropertyTypen = typeWithPerspectivesTypes $ getContextMember \context -> keys (rol_properties (context_binnenRol context))

label :: forall s e. ContextType s => (s ~~> PString) e
label = typeWithPerspectivesTypes $ getContextMember \context -> [(context_displayName context)]

rolType :: forall s o e. RolType s => (s ~~> RolDef) e
rolType = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_pspType rol]

-- binding :: forall s o e. RolType s => (s ~~> RolDef) e
-- binding = typeWithPerspectivesTypes $ getRolMember \rol -> maybe [] singleton (rol_binding rol)
--
-- binding_ :: forall s o e. RolType s => RolType o => (s ~~> o) e
-- binding_ = typeWithPerspectivesTypes $ getRolMember \rol -> maybe [] singleton (rol_binding rol)

-- | Notice how this type is true for model:Perspectives and model:QueryAst, as all defined contexts in those models are types!
-- context :: forall rt s e. RolType rt => (rt ~~> ContextDef) e
-- context = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_context rol]
--
-- context_ :: forall rt ct e. RolType rt => ContextType ct => (rt ~~> ct) e
-- context_ = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_context rol]

rolBindingDef :: forall b c d e.
  Binding b c =>
  RolType c =>
  ContextType d =>
  RolVanContext c d =>
  (b ~~> d) e
rolBindingDef = binding /-/ context

-- rolBindingDef_ :: forall b c e. RolType b => ContextType c => (b ~~> c) e
-- rolBindingDef_ = binding /-/ context

binding' :: forall bi bo e.
  RolType bi =>
  RolType bo =>
  Binding bi bo =>
  TypedObjectGetter bi bo e
binding' = toSingle binding
