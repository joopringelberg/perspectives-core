module Perspectives.Resource where

import Prelude
import Control.Monad.Eff (runPure, Eff)
import Control.Monad.ST (ST)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldable, lookup, runST, keys)
import Data.StrMap.ST (poke, STStrMap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Control.Monad.Aff.AVar
import Perspectives.Location (Location, locate)

{-
  Resources are put into Locations, so we can lift (and memoize) operations over Location.
-}

-- | A newtype for the property definitions so we can show them.
--newtype PropDefs = PropDefs (StrMap (Array Foreign))
newtype PropDefs = PropDefs (StrMap Foreign)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = show $ keys s

-- | Basic representation for Resource, complete with its definition.
newtype Resource = Resource
  { id :: String
--  , propDefs :: Maybe (AVar PropDefs)
  , propDefs :: PropDefs
  }

type ResourceId = String

instance showResource :: Show Resource where
    show (Resource{id, propDefs} ) = id <> " ("<> show propDefs <> ")"

-- | We store the combination of Resource and Location in an Index.
newtype ResourceLocation = ResourceLocation{ res :: Resource, loc :: Location Resource}

instance showResourceLocation :: Show ResourceLocation where
  show (ResourceLocation{ res }) = show res

resourceIndex :: StrMap ResourceLocation
resourceIndex = empty

-- | Look up a resource or create a new resource and store it in the index.
representResource :: ResourceId -> Maybe PropDefs -> Resource
representResource id pd = case lookup id resourceIndex of
  Nothing -> let
      r = case pd of
            Nothing -> Resource{ id: id, propDefs: PropDefs empty }
            (Just defs) -> Resource{ id: id, propDefs: defs}
      l = locate r
      m = runPure $ runST do
        ri <- thawST' resourceIndex -- thawST kopieert.
        poke ri id (ResourceLocation{ res: r, loc: l})
    in r
  (Just (ResourceLocation {res})) -> res

-- | A version of thawST that does not copy.
foreign import thawST' :: forall a h r. StrMap a -> Eff (st :: ST h | r) (STStrMap h a)

-- | From a Resource, find its Location.
resourceLocation :: Resource -> Location Resource
resourceLocation (Resource{id}) = unsafePartial resourceLocation' id where
    resourceLocation' :: Partial => String -> Location Resource
    resourceLocation' rid = case lookup id resourceIndex of
      (Just (ResourceLocation{ loc })) -> loc

-----------------------------------------------------------------------------------------------
-- | EXAMPLES
-----------------------------------------------------------------------------------------------
r1 :: Resource
r1 = Resource{
  id : "r1",
  propDefs : PropDefs $ fromFoldable [ (Tuple "maten" (toForeign [ 1, 2])), (Tuple "naam" (toForeign ["Joop", "Johannes"]))]
}

j :: Resource
j = representResource "Joop" Nothing

r1Stored :: Resource
r1Stored = representResource "r1" (Just propDefs) where (Resource{propDefs}) = r1
