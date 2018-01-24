module Perspectives.Property where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Data.Argonaut (toArray, toString)
import Data.Array (singleton)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (keys, lookup, values)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_buitenRol, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.Identifiers (isWellFormedIdentifier)
import Perspectives.Resource (PROPDEFS, ResourceDefinitions, getContext, getPropDefs, getRole)
import Perspectives.ResourceTypes (PropDefs(..), Resource, DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), ID, PerspectContext(..), PerspectRol, RoleName, propertyValue)

{-
Property values are represented by Arrays.
We need functions that give us an array of values for a given property for a given resource.
-}

type PropertyName = String

type PropDefsEffects e = DomeinFileEffects (st :: ST ResourceDefinitions, prd :: PROPDEFS | e)

type PerspectEffects e = (PropDefsEffects e)

type ObjectsGetter e = Resource -> Aff (PropDefsEffects e) (Array String)

type ObjectGetter e = Resource -> Aff (PropDefsEffects e) String

-- | A function that takes a property name and returns an ObjectsGetter for that property.
-- | The getter takes a Resource (an ID) and returns a computation of an Array of string values. It can throw one of these errors:
-- | - the propertyname is not well-formed;
-- | - the value is not an Array;
-- | - not all elements in the Array are strings.
-- | The computation has the PropDefsEffects in Aff.
getObjectsGetter :: forall e. PropertyName -> ObjectsGetter e
getObjectsGetter pn r =
  -- Is the propertyname well formed?
  case isWellFormedIdentifier pn of
    false -> throwError $ error ("getObjectsGetter: property '" <> pn <> "' is not a wellformed standard CURIE or DomeinURI!" )
    true -> do
      (PropDefs pd) <- getPropDefs r
      case lookup pn pd of
        -- Property is not available. This is not an error.
        Nothing -> pure []
        -- This must be an array filled with zero or more values that toString recognizes.
        (Just json) -> case toArray json of
          Nothing -> throwError $ error ("getObjectsGetter: property " <> pn <> " of resource " <> show r <> " is not an array!" )
          (Just arr) -> case traverse toString arr of
            Nothing -> throwError $ error ("getObjectsGetter: property " <> pn <> " of resource " <> show r <> " has an element that is not of the required type" )
            (Just a) -> pure a

getContextMember :: forall e. (PerspectContext -> Array String) -> ObjectsGetter e
getContextMember f c = do
  maybeContext <- getContext c
  case maybeContext of
    (Just perspectContext) -> pure $ f perspectContext
    otherwise -> pure []

-- Even though members of a context will always be present, the context itself may not. Hence we return a Maybe value.
getContextMember' :: forall a e. (PerspectContext -> a) -> (ID -> Aff (PropDefsEffects e) (Maybe a))
getContextMember' f c = do
  maybeContext <- getContext c
  case maybeContext of
    (Just perspectContext) -> pure $ Just $ f perspectContext
    otherwise -> pure Nothing

getContextType :: forall e. ObjectsGetter e
getContextType = getContextMember \context -> [context_pspType context]

-- Returns an empty array if the context does not exist.
getBuitenRol :: forall e. ObjectsGetter e
getBuitenRol = getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
getBuitenRol' :: forall e. ID -> Aff (PropDefsEffects e) (Maybe String)
getBuitenRol' = getContextMember' \c -> context_buitenRol c

getRol :: forall e. RoleName -> ObjectsGetter e
getRol rn = getContextMember \context -> maybe [] id (lookup rn (context_rolInContext context))

getRollen :: forall e. ObjectsGetter e
getRollen = getContextMember \context -> join $ values (context_rolInContext context)

getRolTypen :: forall e. ObjectsGetter e
getRolTypen = getContextMember \context -> keys (context_rolInContext context)

getPublicProperty :: forall e. PropertyName -> ObjectsGetter e
getPublicProperty pn id = do
  mbr <- getBuitenRol' id
  case mbr of
    Nothing -> pure []
    (Just br) -> getProperty pn br

getPrivateProperty :: forall e. PropertyName -> ObjectsGetter e
getPrivateProperty pn ident = do
  (mbr :: Maybe BinnenRol) <- getContextMember' (\(PerspectContext{binnenRol}) -> binnenRol) ident
  case mbr of
    Nothing -> pure []
    -- TODO: vervang de pattern matching zodra binnenRol een 'echte' rol is.
    (Just (BinnenRol{properties})) -> pure $ (maybe [] propertyValue) (lookup pn properties)

getRolMember :: forall e. (PerspectRol -> Array String) -> ObjectsGetter e
getRolMember f c = do
  maybeRol <- getRole c
  case maybeRol of
    (Just perspectRol) -> pure $ f perspectRol
    otherwise -> pure []

getRolType :: forall e. ObjectsGetter e
getRolType = getRolMember \rol -> [rol_pspType rol]

getRolBinding :: forall e. ObjectsGetter e
getRolBinding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

getRolContext :: forall e. ObjectsGetter e
getRolContext = getRolMember \rol -> [rol_context rol]

getProperty :: forall e. PropertyName -> ObjectsGetter e
getProperty pn = getRolMember \rol -> maybe [] propertyValue (lookup pn (rol_properties rol))

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of getRolContext), others roles (such as the result of getRolBinding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall e. Partial => ObjectsGetter e -> ObjectGetter e
toSingle og id = do
  (ar :: Array String) <- og id
  pure $ ArrayPartial.head ar

getRolBinding' :: forall e. ObjectGetter e
getRolBinding' = unsafePartial $ toSingle getRolBinding
