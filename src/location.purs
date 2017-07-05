-- | A location for values that is in contact with the network store in a way that is invisible for
-- | the type system.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Location where
import Prelude
import Data.Maybe

-- | Undefined represents values for foreign function results that can be undefined.
foreign import data Undefined :: Type -> Type

-- | Test whether an Undefined value is, in fact, undefined.
foreign import isUndefined :: forall a. Undefined a -> Boolean

-- | Unwrap a value from the Undefined data type.
foreign import fromUndefined :: forall a. Undefined a -> a

-- | The node that contains the network information. Its structure and content is invisible
-- | for the type system.
foreign import data Node :: Type

type Origin = Node
type Target = Node

-- | Connect a Node through a function to another, new Node. This represents, on the level of
-- | the network, the application of the function to the value of a Location, where the
-- | application result is contained in another Location
foreign import linkNode :: forall a b. Origin -> (a -> b) -> Target

-- | Save the Location in the Node. We need this to be able to retrieve it.
-- | Return the Location for chaining.
foreign import saveLocation :: forall a. Location a -> Node -> Location a

-- | From a Node holding a Location with value v, follow the link labeled with a
-- | function a -> b, to the Location that holds the result of applying the function to v.
foreign import retrieveLocation :: forall a b. Node -> (a -> b) -> Undefined (Location b)

-- | Create a Node from nothing.
foreign import createNode :: Unit -> Node

foreign import getIndex :: Node -> Int

newtype Location a = Location { value:: a, node :: Node }

-- | Encapsulate a value in a fresh Location.
locate :: forall a. a -> Location a
locate a = saveLocation l node where
  l@(Location {node}) = Location { value: a, node: createNode unit }

instance showLocation :: Show a => Show (Location a) where
  show (Location { value, node }) = "Location(" <> show (getIndex node) <> ") "<> show value
{-}
instance functorLocation :: Functor Location where
  map fn (Location {value, node : origin}) = saveLocation l node where
    l@(Location {node}) = Location { value: fn value, node: linkNode origin fn }
    retrieved = maybeLocation origin fn
-}
instance functorLocation :: Functor Location where
  map fn (Location {value, node}) =
    case maybeLocation node fn of
      Nothing -> createLocation node (fn value) fn node
      Just l -> l

-- | Create a fresh Location holding the value and linked to the (origin) node through the function.
createLocation :: forall a b. Node -> b -> (a -> b) -> Node -> Location b
createLocation node value fn origin = saveLocation location targetNode where
  location = Location { value: value, node: targetNode }
  targetNode = linkNode origin fn

-- | Returns a Location holding the result of applying the function to the value of the
-- | Location of the node, if it exists; Nothing otherwise.
maybeLocation :: forall a b. Node -> (a -> b) -> Maybe (Location b)
maybeLocation node fn = handle (retrieveLocation node fn) where
  handle :: Undefined (Location b) -> Maybe (Location b)
  handle r | isUndefined r = Nothing
          | otherwise = Just (fromUndefined r)

l1 = locate 1
l2 = map (add 1) l1
l3 = map (add 1) l1

instance applyLocation :: Apply Location where
  apply (Location {value: fn, node: functionNode}) (Location {value, node: valueNode}) =
    case maybeLocation valueNode fn of
      Nothing ->
      Just l -> l
