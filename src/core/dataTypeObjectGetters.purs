module Perspectives.DataTypeObjectGetters where

import Control.Monad.Eff.Exception (error)
import Data.Array (head, nub, singleton)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe, maybe)
import Data.StrMap (keys, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_binnenRol, context_buitenRol, context_displayName, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter, ObjectGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.Utilities (onNothing)
import Prelude (bind, join, pure, ($), (<>), (>=>))

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of context), others roles (such as the result of binding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall e. Partial => ObjectsGetter e -> ObjectGetter e
toSingle og id = do
  (ar :: Array String) <- og id
  pure $ ArrayPartial.head ar

makeFunction :: forall e. String -> ObjectsGetter e -> ObjectGetter e
makeFunction name og = og >=> (\ta -> onNothing (error $ "Function yields no value: " <> name) (pure (head ta)))

contextType :: forall e. ObjectsGetter e
contextType = getContextMember \context -> [context_pspType context]

-- | `psp:ContextInstance -> psp:Context`
contextTypeF :: forall e. ObjectGetter e
contextTypeF = makeFunction "contextTypeF" contextType

-- Returns an empty array if the context does not exist.
buitenRol :: forall e. ObjectsGetter e
buitenRol = getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
buitenRol' :: forall e. ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String)
buitenRol' = getContextMember' \c -> context_buitenRol c

iedereRolInContext :: forall e. ObjectsGetter e
iedereRolInContext = getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
typeVanIedereRolInContext :: forall e. ObjectsGetter e
typeVanIedereRolInContext = getContextMember \context -> keys (context_rolInContext context)

-- | The names of every property given to this rol.
getPropertyTypen :: forall e. ObjectsGetter e
getPropertyTypen = getRolMember \rol -> keys (rol_properties rol)

-- | The names of every internal property given to this context.
getInternePropertyTypen :: forall e. ObjectsGetter e
getInternePropertyTypen = getContextMember \context -> keys (rol_properties (context_binnenRol context))

getDisplayName :: forall e. ObjectsGetter e
getDisplayName = getContextMember \context -> [(context_displayName context)]

rolType :: forall e. ObjectsGetter e
rolType = getRolMember \rol -> [rol_pspType rol]

getRolTypeF :: forall e. ObjectGetter e
getRolTypeF = makeFunction "getRolTypeF" rolType

binding :: forall e. ObjectsGetter e
binding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

getRolBindingDef :: forall e. ObjectsGetter e
getRolBindingDef = binding /-/ context

context :: forall e. ObjectsGetter e
context = getRolMember \rol -> [rol_context rol]

binding' :: forall e. ObjectGetter e
binding' = unsafePartial $ toSingle binding
