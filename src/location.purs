-- | A location for values that is in contact with the network store in a way that is invisible for
-- | the type system.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Location where

import Prelude
import Data.Maybe
import Perspectives.Node

-- | Save the Location in the Node. We need this to be able to retrieve it.
-- | Return the Location for chaining.
foreign import saveLocation :: forall a. Location a -> Node -> Node

-- | From a Node holding a Location with value v, follow the link labeled with a
-- | function a -> b, to the Location that holds the result of applying the function to v.
foreign import retrieveLocation :: forall a b. Node -> (a -> b) -> Undefined (Location b)

-- | From a Node retrieve its Location.
foreign import nodeLocation :: forall a. Node -> Location a

-- | The primary representation of Location holds a value and a node for side effects.
data Location a = Location a Node

-- | Encapsulate a value in a fresh Location without any dependencies.
locate :: forall a. a -> Location a
locate a = let
      node = createNode unit
      location = Location a node
      ignore = saveLocation location node
    in location

-- | From a location, create a fresh dependent location connected to it.
createDependentLocation :: forall a b. Node -> b -> (a -> b) -> Node -> Location b
createDependentLocation node value fn origin = let
    location@(Location value target) = locate value
  in nodeLocation (saveLocation location (linkNode origin fn target))

-- | Returns a Location holding the result of applying the function to the value of the
-- | Location of the node, if it exists; Nothing otherwise.
maybeLocation :: forall a b. Node -> (a -> b) -> Maybe (Location b)
maybeLocation node fn = handle (retrieveLocation node fn) where
  handle :: Undefined (Location b) -> Maybe (Location b)
  handle r | isUndefined r = Nothing
          | otherwise = Just (fromUndefined r)

instance showLocation :: Show a => Show (Location a) where
  show (Location value node) = "Location(" <> show (getIndex node) <> ") "<> show value

instance functorLocation :: Functor Location where
  map fn (Location value node) =
    case maybeLocation node fn of
      Nothing -> createDependentLocation node (fn value) fn node
      Just l -> l

l1 = locate 1 :: Location Int
l2 = map (add 1) l1 :: Location Int
l3 = map (add 1) l1 :: Location Int

{-}
instance applyLocation :: Apply Location where
  apply (Location {value: fn, node: functionNode}) (Location {value, node: valueNode}) =
    case maybeLocation valueNode fn of
      Nothing -> createDependentLocation
      Just l -> l
-}
