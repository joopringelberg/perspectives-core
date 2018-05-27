module Perspectives.SystemObjectGetters where

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
-- | of getRolContext), others roles (such as the result of getRolBinding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall e. Partial => ObjectsGetter e -> ObjectGetter e
toSingle og id = do
  (ar :: Array String) <- og id
  pure $ ArrayPartial.head ar

makeFunction :: forall e. String -> ObjectsGetter e -> ObjectGetter e
makeFunction name og = og >=> (\ta -> onNothing (error $ "Function yields no value: " <> name) (pure (head ta)))

getContextType :: forall e. ObjectsGetter e
getContextType = getContextMember \context -> [context_pspType context]

-- | `psp:ContextInstance -> psp:Context`
getContextTypeF :: forall e. ObjectGetter e
getContextTypeF = makeFunction "getContextTypeF" getContextType

-- Returns an empty array if the context does not exist.
getBuitenRol :: forall e. ObjectsGetter e
getBuitenRol = getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
getBuitenRol' :: forall e. ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String)
getBuitenRol' = getContextMember' \c -> context_buitenRol c

getRollen :: forall e. ObjectsGetter e
getRollen = getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
getRolTypen :: forall e. ObjectsGetter e
getRolTypen = getContextMember \context -> keys (context_rolInContext context)

-- | The names of every property given to this rol.
getPropertyTypen :: forall e. ObjectsGetter e
getPropertyTypen = getRolMember \rol -> keys (rol_properties rol)

-- | The names of every internal property given to this context.
getInternePropertyTypen :: forall e. ObjectsGetter e
getInternePropertyTypen = getContextMember \context -> keys (rol_properties (context_binnenRol context))

getDisplayName :: forall e. ObjectsGetter e
getDisplayName = getContextMember \context -> [(context_displayName context)]

getRolType :: forall e. ObjectsGetter e
getRolType = getRolMember \rol -> [rol_pspType rol]

getRolTypeF :: forall e. ObjectGetter e
getRolTypeF = makeFunction "getRolTypeF" getRolType

getRolBinding :: forall e. ObjectsGetter e
getRolBinding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

getRolBindingDef :: forall e. ObjectsGetter e
getRolBindingDef = getRolBinding /-/ getRolContext

getRolContext :: forall e. ObjectsGetter e
getRolContext = getRolMember \rol -> [rol_context rol]

getRolBinding' :: forall e. ObjectGetter e
getRolBinding' = unsafePartial $ toSingle getRolBinding
