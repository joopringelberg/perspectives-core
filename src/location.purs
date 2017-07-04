-- | A location for values that is in contact with the network store in a way that is invisible for
-- | the type system.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Location where
import Prelude

foreign import data Node :: Type

foreign import createNode :: Node -> Node

newtype Location a = Location { value:: a, node :: Node }

instance functorLocation :: Functor Location where
  map fn (Location {value, node}) = Location { value: (fn value), node: createNode node }

--l1 = Location 1 createNode
