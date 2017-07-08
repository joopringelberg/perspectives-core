-- | A structure that is invisible for the type system. We only use a (foreign) functional interface.
-- |
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Node where

import Prelude
import Data.Foldable (elem, foldMap)
import Data.Monoid.Disj (Disj(..))
import Data.Ord (class Ord, Ordering(..))
import Data.StrMap (StrMap, values)

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
  , recompute :: (Unit -> Node)
  , set :: forall a. (a -> Node)
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
-- | The update function is constructed and saved, too.
foreign import linkNode :: forall a b. Array Origin -> (a -> b) -> Target -> Target

-- | Create a Node from nothing.
foreign import createNode :: Unit -> Node

foreign import getIndex :: Node -> Int

foreign import equalNodes :: Node -> Node -> Boolean

instance eqNode :: Eq Node where
  eq n1 n2 = equalNodes n1 n2

instance ordNode :: Ord Node where
  compare n1 n2 | n1 == n2 = EQ
  compare (Node {dependents}) n | elem n (values dependents) = LT
  compare n (Node {dependents}) | elem n (values dependents) = GT
  compare n (Node {dependents}) = if lessThenSomeDependent then LT else GT where
    (Disj lessThenSomeDependent) = foldMap (\dep -> Disj ((compare n dep) == LT)) dependents
