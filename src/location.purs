-- | A location for values that is in contact with the network store in a way that is invisible for
-- | the type system.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Location
  ( Location(Location)
  , Node )
where

newtype Location a = Location { value:: a, node :: Node }

-- | The node that contains the network information. Its structure and content is invisible
-- | for the type system.
foreign import data Node :: Type
