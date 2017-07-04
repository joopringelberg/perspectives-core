-- | A test with a network structure

module Test.Network where

import Perspectives.DestructiveArray
import Control.Bind
import Control.Apply
import Perspectives.DestructiveMap (DestructiveMap, setInMap, getFromMap, deleteFromMap)

type Name = String

type Edge =
  { name :: String
  , target :: Node
  }

type EdgeMap = DestructiveMap Edge
type Supports = DestructiveArray Node

newtype Node = Node
  { name :: String
  , dependents :: EdgeMap
  , supports :: Supports
  }

--getNodeName :: forall a. Node a -> Name
getNodeName :: forall r. { name :: String | r } -> Name
getNodeName {name} = name


getDependent :: Node -> Name -> Edge
getDependent (Node { dependents }) k = getFromMap dependents k

addDependent :: Node -> Name -> Edge -> EdgeMap
addDependent	source@(Node { dependents }) name edge@{ target : Node({ supports }) }
  = setInMap dependents name edge where
      discard = pushInDestructiveArray supports source

removeDependent :: Node -> Name -> EdgeMap
removeDependent	source@( Node { dependents }) name
  = let
        edge = getDependent source name
        discard = deleteFromMap dependents name
    in
      deleteFromMap dependents name

data NetworkNode a = NetworkNode a Node

instance functorNetworkNode :: Functor NetworkNode where
  map fn (NetworkNode x n) = NetworkNode (fn x) n

instance applyNetworkNode :: Apply NetworkNode where
  apply (NetworkNode fn n) x = fn <$> x

instance bindNetworkNode :: Bind NetworkNode where
  bind (NetworkNode a n) f = f a
