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
import Perspectives.Identifiers (isWellFormedIdentifier)
import Perspectives.Resource (PROPDEFS, ResourceDefinitions, getContext, getPropDefs, getRole)
import Perspectives.ResourceTypes (PropDefs(..), Resource, DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), ID, PerspectContext(..), PerspectRol(..), RoleName, propertyValue)

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

getContextMember' :: forall a e. (PerspectContext -> a) -> (ID -> Aff (PropDefsEffects e) (Maybe a))
getContextMember' f c = do
  maybeContext <- getContext c
  case maybeContext of
    (Just perspectContext) -> pure $ Just $ f perspectContext
    otherwise -> pure Nothing

getContextType :: forall e. ObjectsGetter e
getContextType = getContextMember \(PerspectContext{pspType}) -> [pspType]

getBuitenRol :: forall e. ObjectsGetter e
getBuitenRol = getContextMember \(PerspectContext{buitenRol}) -> [buitenRol]

getBuitenRol' :: forall e. ID -> Aff (PropDefsEffects e) (Maybe String)
getBuitenRol' = getContextMember' \(PerspectContext{buitenRol}) -> buitenRol

getRol :: forall e. RoleName -> ObjectsGetter e
getRol rn = getContextMember \(PerspectContext{rolInContext}) -> maybe [] id (lookup rn rolInContext)

getRollen :: forall e. ObjectsGetter e
getRollen = getContextMember \(PerspectContext{rolInContext}) -> join $ values rolInContext

getRolTypen :: forall e. ObjectsGetter e
getRolTypen = getContextMember \(PerspectContext{rolInContext}) -> keys rolInContext

getPublicProperty :: forall e. RoleName -> ObjectsGetter e
getPublicProperty pn id = do
  mbr <- getBuitenRol' id
  case mbr of
    Nothing -> pure []
    (Just br) -> getProperty pn br

getPrivateProperty :: forall e. RoleName -> ObjectsGetter e
getPrivateProperty pn ident = do
  (mbr :: Maybe BinnenRol) <- getContextMember' (\(PerspectContext{binnenRol}) -> binnenRol) ident
  case mbr of
    Nothing -> pure []
    (Just (BinnenRol{properties})) -> pure $ (maybe [] propertyValue) (lookup pn properties)

getRolMember :: forall e. (PerspectRol -> Array String) -> ObjectsGetter e
getRolMember f c = do
  maybeRol <- getRole c
  case maybeRol of
    (Just perspectRol) -> pure $ f perspectRol
    otherwise -> pure []

getRolType :: forall e. ObjectsGetter e
getRolType = getRolMember \(PerspectRol{pspType}) -> [pspType]

getRolBinding :: forall e. ObjectsGetter e
getRolBinding = getRolMember \(PerspectRol{binding}) -> maybe [] singleton binding

getRolContext :: forall e. ObjectsGetter e
getRolContext = getRolMember \(PerspectRol{context}) -> [context]

getProperty :: forall e. PropertyName -> ObjectsGetter e
getProperty pn = getRolMember \(PerspectRol{properties}) -> maybe [] propertyValue (lookup pn properties)

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of getRolContext), others roles (such as the result of getRolBinding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall e. Partial => ObjectsGetter e -> ObjectGetter e
toSingle og id = do
  (ar :: Array String) <- og id
  pure $ ArrayPartial.head ar

getRolBinding' :: forall e. ObjectGetter e
getRolBinding' = unsafePartial $ toSingle getRolBinding
