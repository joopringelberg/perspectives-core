module Perspectives.Resource where

import Prelude
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeVar, putVar, peekVar, AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.ST (ST)
import Data.StrMap (StrMap, empty, lookup)
import Data.StrMap.ST (poke, STStrMap)
import Network.HTTP.Affjax (AJAX)
import Perspectives.Location (Location, locate)
import Perspectives.ResourceRetrieval( PropDefs, ResourceId, fetchDefinition, stringToPropDefs)

-- | Basic representation for Resource, complete with its definition.
newtype Resource = Resource
  { id :: String
  , propDefs :: Maybe (AVar PropDefs)
  }

-- | The show instance cannot inspect the content of the AVar, because that needs running
-- | the asynchronous computation to yield a result synchronously, which we cannot do.
instance showResource :: Show Resource where
    show (Resource{id, propDefs} ) = case propDefs of
      Nothing -> id <> " (No definition)"
      (Just avar) -> id <> " (with definition)"

-- | We store the combination of Resource and Location in an Index.
newtype ResourceLocation = ResourceLocation{ res :: Resource, loc :: Location Resource}

--instance showResourceLocation :: Show (ResourceLocation e) where
--  show (ResourceLocation{ res }) = show res

-- | The global index of all Resource-Location pairs, indexed by ResourceId.
type ResourceIndex = StrMap ResourceLocation
resourceIndex :: ResourceIndex
resourceIndex = empty

-- | From a Resource, find its Location.
resourceLocation :: Resource -> Location Resource
resourceLocation (Resource{id}) = unsafePartial resourceLocation' id where
    resourceLocation' :: Partial => String -> Location Resource
    resourceLocation' rid = case lookup id resourceIndex of
      (Just (ResourceLocation{ loc })) -> loc

-- | Look up a resource or create a new resource without definition and store it in the index.
representResource :: ResourceId -> Resource
representResource id = case lookup id resourceIndex of
  Nothing -> Resource{ id: id, propDefs: Nothing}
  (Just (ResourceLocation {res})) -> res

-- | Create a new resource with definitions and store it in the index.
newResource :: ResourceId -> PropDefs -> Aff ( st :: ST ResourceIndex, avar :: AVAR ) Resource
newResource id defs = do
    v <- makeVar
    _ <- putVar v defs
    res <- pure (Resource{ id: id, propDefs: Just v})
    loc <- pure (locate res)
    ri <- liftEff $ (thawST' resourceIndex)
    _ <- liftEff $ (poke ri id (ResourceLocation{ res: res, loc: loc}))
    pure res

-- | A version of thawST that does not copy.
foreign import thawST' :: forall a h r. StrMap a -> Eff (st :: ST h | r) (STStrMap h a)

-- | Get the property definitions of a Resource.
getPropDefs :: Resource -> Aff (ajax :: AJAX, avar :: AVAR) PropDefs
getPropDefs (Resource {id, propDefs}) = case propDefs of
  Nothing -> do
              defstring <- fetchDefinition id
              def <- pure (stringToPropDefs defstring)
              av <- makeVar
              -- set av as the value of propDefs in the resource!
              putVar av def
              pure def
  (Just avar) -> do peekVar avar

{--- | Look up a resource or create a new resource and store it in the index.
representResource' :: ResourceId -> Maybe PropDefs -> Resource
representResource' id pd = case lookup id resourceIndex of
  Nothing -> let
      r = case pd of
            Nothing -> Resource{ id: id, propDefs: PropDefs empty }
            (Just defs) -> Resource{ id: id, propDefs: defs}
      l = locate r
      m = runPure $ runST do
        ri <- thawST' resourceIndex -- thawST kopieert, thawST' niet.
        poke ri id (ResourceLocation{ res: r, loc: l})
    in r
  (Just (ResourceLocation {res})) -> res-}

-----------------------------------------------------------------------------------------------
-- | EXAMPLES
-----------------------------------------------------------------------------------------------
{-r1 :: Resource
r1 = Resource{
  id : "r1",
  propDefs : PropDefs $ fromFoldable [ (Tuple "maten" (toForeign [ 1, 2])), (Tuple "naam" (toForeign ["Joop", "Johannes"]))]
}

j :: Resource
j = representResource "Joop" Nothing

r1Stored :: Resource
r1Stored = representResource "r1" (Just propDefs) where (Resource{propDefs}) = r1
-}
