module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeVar', peekVar, AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Perspectives.Location (Location, THEORYDELTA, saveInLocation, locationValue, setLocationValue, saveResource)
import Perspectives.ResourceRetrieval (fetchPropDefs)
import Perspectives.ResourceTypes (AsyncDomeinFile, PropDefs, Resource(..), ResourceId, ResourceLocation(..))

-- | The global index of all Resource-Location pairs, indexed by ResourceId.
type ResourceIndex = GLStrMap ResourceLocation

resourceIndex :: ResourceIndex
resourceIndex = new unit

-- | From a Resource, find its Location.
locationFromResource :: forall e. Resource -> Aff (gm :: GLOBALMAP | e ) (Location (Maybe Resource))
locationFromResource res@(Resource{id}) = do
  x <- liftEff $ peek resourceIndex id
  case x of
    (Just (ResourceLocation{ loc })) -> pure loc
    Nothing -> liftEff $ storeResourceInIndex res

locationFromMaybeResource :: forall e. Maybe Resource -> Aff (gm :: GLOBALMAP | e ) (Location (Maybe Resource))
locationFromMaybeResource Nothing = pure $ saveInLocation Nothing
locationFromMaybeResource (Just res@(Resource{id})) = do
  x <- liftEff $ peek resourceIndex id
  case x of
    (Just (ResourceLocation{ loc })) -> pure loc
    Nothing -> liftEff $ storeResourceInIndex res

-- | From a Location, return the resource.
resourceFromLocation :: Location (Maybe Resource) -> Resource
resourceFromLocation loc = unsafePartial resourceFromLocation' loc where
    resourceFromLocation' :: Partial => Location (Maybe Resource) -> Resource
    resourceFromLocation' locR =
      case locationValue locR of
        (Just r) -> r

representResource :: forall e. ResourceId -> Eff (gm :: GLOBALMAP | e) (Maybe Resource)
representResource id = representResourceInLocation id >>= (pure <<< locationValue)

-- | Look up a resource or create a new resource without definition and store it in the index.
representResourceInLocation :: forall e. ResourceId -> Eff (gm :: GLOBALMAP | e) (Location (Maybe Resource))
representResourceInLocation id = do
  x <- peek resourceIndex id
  case x of
    Nothing -> do
      loc <- storeResourceInIndex (Resource{ id: id, propDefs: Nothing})
      pure loc
    (Just (ResourceLocation {loc})) -> pure loc

-- | Create a new resource with definitions and store it in the index.
newResource :: ResourceId -> PropDefs -> Aff ( gm :: GLOBALMAP, avar :: AVAR ) (Location (Maybe Resource))
newResource id defs = do
    v <- makeVar' defs
    res <- pure (Resource{ id: id, propDefs: Just v})
    liftEff $ storeResourceInIndex res

storeResourceInIndex :: forall e. Resource -> Eff (gm :: GLOBALMAP | e) (Location (Maybe Resource))
storeResourceInIndex res@(Resource{id}) =
  let loc = saveResource id (Just res)
  in do
    poke resourceIndex id (ResourceLocation{ res: res, loc: loc}) *> pure loc

addPropertyDefinitions :: forall e. Resource -> AVar PropDefs -> Aff (td :: THEORYDELTA, gm :: GLOBALMAP | e) Unit
addPropertyDefinitions r@(Resource{id}) av =
  do
    loc <- locationFromResource r
    _ <- liftEff $ setLocationValue loc (Just (Resource{ id: id, propDefs: (Just av)}))
    _ <- liftEff $ poke resourceIndex id (ResourceLocation{ res: r, loc: loc })
    pure unit

-- | Get the property definitions of a Resource.
getPropDefs :: forall e. Resource -> AsyncDomeinFile e PropDefs
getPropDefs r@(Resource {id, propDefs}) = case propDefs of
  Nothing -> do
              pd <- fetchPropDefs id
              av <- makeVar' pd
              -- set av as the value of propDefs in the resource!
              _ <- pure (addPropertyDefinitions r av)
              pure pd
  (Just avar) -> do
                  pd <- peekVar avar
                  pure pd
