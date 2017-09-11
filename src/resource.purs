module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeVar', peekVar, AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
-- import Data.StrMap.ST (poke, STStrMap)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Partial.Unsafe (unsafePartial)
import Perspectives.Location (Location, THEORYDELTA, locate, setLocationValue)
import Perspectives.ResourceRetrieval (fetchPropDefs)
import Perspectives.ResourceTypes (PropDefs, Resource(..), ResourceId, ResourceLocation(..), AsyncDomeinFile)

-- | The global index of all Resource-Location pairs, indexed by ResourceId.
type ResourceIndex = GLStrMap ResourceLocation

resourceIndex :: ResourceIndex
resourceIndex = new unit

-- | From a Resource, find its Location.
resourceLocation :: Resource -> Location Resource
resourceLocation (Resource{id}) = unsafePartial resourceLocation' id where
    resourceLocation' :: Partial => String -> Location Resource
    resourceLocation' rid = case peek resourceIndex id of
      (Just (ResourceLocation{ loc })) -> loc

-- | Look up a resource or create a new resource without definition and store it in the index.
representResource :: forall e. ResourceId -> Eff (gm :: GLOBALMAP | e) Resource
representResource id = case peek resourceIndex id of
  Nothing -> storeResourceInIndex $ Resource{ id: id, propDefs: Nothing}
  (Just (ResourceLocation {res})) -> pure res

-- | Create a new resource with definitions and store it in the index.
newResource :: ResourceId -> PropDefs -> Aff ( gm :: GLOBALMAP, avar :: AVAR ) Resource
newResource id defs = do
    v <- makeVar' defs
    res <- pure (Resource{ id: id, propDefs: Just v})
    liftEff $ storeResourceInIndex res

storeResourceInIndex :: forall e. Resource -> Eff (gm :: GLOBALMAP | e) Resource
storeResourceInIndex res@(Resource{id}) =
  let loc = locate res
  in do
    poke resourceIndex id (ResourceLocation{ res: res, loc: loc}) *> pure res

addPropertyDefinitions :: forall e. Resource -> AVar PropDefs -> Eff (td :: THEORYDELTA, gm :: GLOBALMAP | e) Unit
addPropertyDefinitions r@(Resource{id}) av =
  let
    loc = resourceLocation r
    newR = Resource{ id: id, propDefs: (Just av)}
    newResourceLocation = ResourceLocation{ res: r, loc: loc }
  in do
    _ <- setLocationValue loc newR
    _ <- poke resourceIndex id newResourceLocation
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

-----------------------------------------------------------------------------------------------
-- | EXAMPLES
-----------------------------------------------------------------------------------------------
{-r1 :: Resource
r1 = Resource{
  id : "r1",
  propDefs : PropDefs $ fromFoldable [ (Tuple "maten" (toForeign [ 1, 2])), (Tuple "naam" (toForeign ["Joop", "Johannes"]))]
}

j :: Resource
j = representResource "Joop"

r1Stored :: Resource
r1Stored = newResource "r1" (Just propDefs) where (Resource{propDefs}) = r1
-}
