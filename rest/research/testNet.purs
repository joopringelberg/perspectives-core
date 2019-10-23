-- | A test with a network structure

module Test.Network where

import Prelude
import Perspectives.DestructiveArray
import Control.Bind
import Control.Apply
import Perspectives.DestructiveMap (DestructiveMap, createMap, deleteFromMap, getFromMap, setInMap)
import Perspectives.DestructiveArray (createDestructiveArray)


type Name = String

type Edge a =
  { name :: String
  , target :: Node a
  }

type EdgeMap a = DestructiveMap (Edge a)
type Supports a = DestructiveArray (Node a)

newtype Node a = Node
  { name :: String
  , payload :: a
  , dependents :: EdgeMap a
  , supports :: Array a
  }

--getNodeName :: forall a. Node a -> Name
getNodeName :: forall r. { name :: String | r } -> Name
getNodeName {name} = name


getDependent :: forall a. Node a -> Name -> Edge a
getDependent (Node { dependents }) k = getFromMap dependents k

addDependent :: Node -> Name -> Edge -> EdgeMap
addDependent	source@(Node { dependents }) name edge@{ target : Node({ supports }) }
  = setInMap dependents name edge where
      discard = pushInDestructiveArray supports source

removeDependent :: forall a. Node a -> Name -> EdgeMap a
removeDependent	source@( Node { dependents }) name
  = let
        edge = getDependent source name
        discard = deleteFromMap dependents name
    in
      deleteFromMap dependents name

instance functorNetworkNode :: Functor Node where
  map fn n@(Node {payload}) = Node { payload: (fn payload), name: "anything", dependents: createMap unit, supports: [n] }

instance applyNetworkNode :: Apply Node where
  apply (Node {payload}) x = Node( {payload: payload <$> x })

instance bindNetworkNode :: Bind Node where
  bind (Node {payload}) f = f payload
