module Perspectives.DataTypeObjectGetters where

import Data.Array (nub, singleton)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Foreign.Object (keys, values)
import Perspectives.ContextAndRole (context_buitenRol, context_displayName, context_pspType, context_iedereRolInContext, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter, ObjectGetter, type (~~>))
import Perspectives.Identifiers (binnenRol) as PI
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BinnenRol(..), BuitenRol(..), RolDef, binding, typeWithPerspectivesTypes)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Prelude (bind, join, pure, ($), (>=>), (<<<), map)

identity :: forall o. (o ~~> o)
identity x = pure [x]

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of context), others roles (such as the result of binding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: Partial => ObjectsGetter -> ObjectGetter
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
contextType :: (AnyContext ~~> ContextType)
contextType = getContextMember \context -> [context_pspType context]

-- | We know that, as long as we apply this function to an identifier that represents a PerspectContext, we'll
-- | get a BuitenRol. We cannot constrain the argument, however.
buitenRol :: (AnyContext ~~> BuitenRol)
buitenRol = (getContextMember \c -> [context_buitenRol c]) >=> pure <<< map BuitenRol

-- | Returns Nothing if the context does not exist.
buitenRol' :: AnyContext -> MonadPerspectives (Maybe BuitenRol)
buitenRol' = (getContextMember' \c -> context_buitenRol c) >=> pure <<< map BuitenRol

isBuitenRol :: forall r. RolClass r => r -> MonadPerspectives Boolean
isBuitenRol = typeWithPerspectivesTypes (getPerspectEntiteit >=> \r -> pure $ test (unsafeRegex "buitenRolBeschrijving$" noFlags) (rol_pspType r))

binnenRol :: (AnyContext ~~> BinnenRol)
binnenRol = pure <<< singleton <<< BinnenRol <<< PI.binnenRol

-- | We cannot type the result, as it can be either a RolInContext, or a ContextRol. Neither can we type the argument.
iedereRolInContext :: ObjectsGetter
iedereRolInContext = getContextMember \context -> nub $ join $ values (context_iedereRolInContext context)

-- | The names of every rol given to this context.
typeVanIedereRolInContext :: ObjectsGetter
typeVanIedereRolInContext = getContextMember \context -> keys (context_iedereRolInContext context)

-- | The types of every property for which this rol has a value.
propertyTypen :: ObjectsGetter
propertyTypen = getRolMember \rol -> keys (rol_properties rol)

label :: ObjectsGetter
label = getContextMember \context -> [(context_displayName context)]

rolType :: forall r. RolClass r => (r ~~> EnumeratedRoleType)
rolType = getPerspectEntiteit >=> pure <<< rol_pspType

genericRolType :: (String ~~> String)
genericRolType = getRolMember \rol -> [rol_pspType rol]

rolBindingDef :: forall r b. Binding r b => (r ~~> AnyContext)
rolBindingDef = binding /-/ context

context :: forall r. RolClass r => (r ~~> String)
context = pure <<< unwrap >=> getRolMember \rol -> [rol_context rol]

genericContext :: ObjectsGetter
genericContext = getRolMember \rol -> [rol_context rol]
