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
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolKind, BuitenRol, PString, PropertyDef, RolDef, RolInContext, typeWithPerspectivesTypes)
import Prelude (bind, join, pure, ($))

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of context), others roles (such as the result of binding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall s o e. TypedObjectsGetter s o e -> TypedObjectGetter s o e
toSingle og id = do
  (ar :: Array o) <- og id
  pure $ unsafePartial $ ArrayPartial.head ar

contextType :: forall s o e. ContextType s => ContextType o => (s ~~> o) e
contextType = typeWithPerspectivesTypes $ getContextMember \context -> [context_pspType context]

-- Returns an empty array if the context does not exist.
buitenRol :: forall s e. ContextType s => (s ~~> BuitenRol) e
buitenRol = typeWithPerspectivesTypes $ getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
buitenRol' :: forall s e. ContextType s => s -> MonadPerspectives (AjaxAvarCache e) (Maybe BuitenRol)
buitenRol' = typeWithPerspectivesTypes $ getContextMember' \c -> context_buitenRol c

iedereRolInContext :: forall s e. ContextType s => (s ~~> RolInContext) e
iedereRolInContext = typeWithPerspectivesTypes $ getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
typeVanIedereRolInContext :: forall s e. ContextType s => (s ~~> RolDef) e
typeVanIedereRolInContext = typeWithPerspectivesTypes $ getContextMember \context -> keys (context_rolInContext context)

-- | The names of every property given to this rol.
propertyTypen :: forall rt e. RolKind rt => (rt ~~> PropertyDef) e
propertyTypen = typeWithPerspectivesTypes $ getRolMember \rol -> keys (rol_properties rol)

-- | The names of every internal property given to this context.
internePropertyTypen :: forall s e. ContextType s => (s ~~> PropertyDef) e
internePropertyTypen = typeWithPerspectivesTypes $ getContextMember \context -> keys (rol_properties (context_binnenRol context))

label :: forall s e. ContextType s => (s ~~> PString) e
label = typeWithPerspectivesTypes $ getContextMember \context -> [(context_displayName context)]

rolType :: forall rt e. RolKind rt => (rt ~~> RolDef) e
rolType = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_pspType rol]

binding :: forall s o e. RolKind s => RolKind o => (s ~~> o) e
binding = typeWithPerspectivesTypes $ getRolMember \rol -> maybe [] singleton (rol_binding rol)

-- | Notice how this type is true for model:Perspectives and model:QueryAst, as all defined contexts in those models are types!
context :: forall rt s e. RolKind rt => ContextType s => (rt ~~> s) e
context = typeWithPerspectivesTypes $ getRolMember \rol -> [rol_context rol]

-- | TODO. Oorspronkelijk definieerde ik dit als (rt ~~> ContextDef) e, maar dat klopt niet. Het kan zijn dat er semantische fouten zijn waar deze functie is gebruikt.
-- NOTE.
-- the definition:
--  rolBindingDef = binding /-/ context
-- will not do, as the compiler infers the existence of a type variable that is the result of 'binding', but
-- cannot infer a type for it.
-- Originally, I had solved this with:
--  rolBindingDef :: forall c b rt e. Binding b => RolKind rt => ContextType c => (rt ~~> c) e
--  rolBindingDef = (binding :: (rt ~~> b) e) /-/ context
-- However, that exports the problem to any function that calls rolBindingDef.
-- I've now solved it with an arbitrary choice for the type of the subexpression 'binding'.
-- It seems to produce no problem when the result of applying 'binding' is not actually an instance of RolInContext.
rolBindingDef :: forall c b e. Binding b => ContextType c => (b ~~> c) e
rolBindingDef = binding /-/ context

binding' :: forall b e. Binding b => TypedObjectGetter b b e
binding' = toSingle binding
