module Perspectives.DataTypeObjectGetters where

import Data.Array (nub, singleton)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.StrMap (keys, values)
import Perspectives.ContextAndRole (context_binnenRol, context_buitenRol, context_displayName, context_pspType, context_rolInContext, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter, ObjectGetter, type (~~>))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (binnenRol) as PI
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BinnenRol(..), BuitenRol(..), binding)
import Prelude (bind, join, pure, ($), (>=>), (<<<), map)

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of context), others roles (such as the result of binding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall e. Partial => ObjectsGetter e -> ObjectGetter e
toSingle og id = do
  (ar :: Array String) <- og id
  pure $ ArrayPartial.head ar

-- | All we know is that we extract the member `pspType` from a PerspectContext identified by a String.
-- | We could type that String as a Context (the instance). But that prohibits us from applying contextType
-- | to a ContextDef, for example - even while that is represented by a PerspectContext just as well!
-- | We have no type class that encompasses both Context, ContextDef, RolDef, PropertyDef, SimpleValueDef and PBool.
-- | In the same vein, the result of this function could be any of ContextDef, RolDef, PropertyDef and SimpleValueDef.
-- | So at this level, we leave the Perspectives data untyped. We'll have to type the argument and result of this
-- | function in the context of its application.
contextType :: forall e. (AnyContext ~~> AnyDefinition) e
contextType = getContextMember \context -> [context_pspType context]

-- | We know that, as long as we apply this function to an identifier that represents a PerspectContext, we'll
-- | get a BuitenRol. We cannot constrain the argument, however.
buitenRol :: forall e. (AnyContext ~~> BuitenRol) e
buitenRol = (getContextMember \c -> [context_buitenRol c]) >=> pure <<< map BuitenRol

-- | Returns Nothing if the context does not exist.
buitenRol' :: forall e. AnyContext -> MonadPerspectives (AjaxAvarCache e) (Maybe BuitenRol)
buitenRol' = (getContextMember' \c -> context_buitenRol c) >=> pure <<< map BuitenRol

binnenRol :: forall e. (AnyContext ~~> BinnenRol) e
binnenRol = pure <<< singleton <<< BinnenRol <<< PI.binnenRol

-- | We cannot type the result, as it can be either a RolInContext, or a ContextRol. Neither can we type the argument.
iedereRolInContext :: forall e. ObjectsGetter e
iedereRolInContext = getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
typeVanIedereRolInContext :: forall e. ObjectsGetter e
typeVanIedereRolInContext = getContextMember \context -> keys (context_rolInContext context)

-- | The types of every property for which this rol has a value.
propertyTypen :: forall e. ObjectsGetter e
propertyTypen = getRolMember \rol -> keys (rol_properties rol)

-- | The types of every property for which the BinnenRol has a value.
internePropertyTypen :: forall e. ObjectsGetter e
internePropertyTypen = getContextMember \context -> keys (rol_properties (context_binnenRol context))

label :: forall e. ObjectsGetter e
label = getContextMember \context -> [(context_displayName context)]

rolType :: forall e. ObjectsGetter e
rolType = getRolMember \rol -> [rol_pspType rol]

rolBindingDef :: forall r b e. Binding r b => (r ~~> AnyContext) e
rolBindingDef = binding /-/ context

context :: forall r e. RolClass r => (r ~~> String) e
context = pure <<< unwrap >=> getRolMember \rol -> [rol_context rol]

-- binding' :: forall e. ObjectGetter e
-- binding' = unsafePartial $ toSingle binding
