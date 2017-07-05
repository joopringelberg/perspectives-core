-- | A structure that is invisible for the type system. We only use a (foreign) functional interface.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Node where

import Prelude
import Perspectives.Location (Node, Location)

-- | Undefined represents values for foreign function results that can be undefined.
foreign import data Undefined :: Type -> Type

-- | Test whether an Undefined value is, in fact, undefined.
foreign import isUndefined :: forall a. Undefined a -> Boolean

-- | Unwrap a value from the Undefined data type.
foreign import fromUndefined :: forall a. Undefined a -> a

type Origin = Node
type Target = Node

-- | Connect a Node through a function to another, new Node. This represents, on the level of
-- | the network, the application of the function to the value of a Location, where the
-- | application result is contained in another Location.
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
