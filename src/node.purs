-- | A structure that is invisible for the type system. We only use a (foreign) functional interface.
-- |
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Node where

import Prelude
import Data.StrMap (StrMap)

-- | The node that contains the network information. Its structure and content is invisible
-- | for the type system.

-- | A stand-in for Location a. We cannot use the type constructor Location here to specialize Node.
-- | Its structure and content is invisible for the type system.
foreign import data AnyLocation :: Type

-- | The node that contains the network information.
newtype Node = Node
  { location :: AnyLocation
  , dependents :: StrMap( Node)
  , supports :: Array Node
  , index :: Int
  }

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

-- | Create a Node from nothing.
foreign import createNode :: Unit -> Node

foreign import getIndex :: Node -> Int
