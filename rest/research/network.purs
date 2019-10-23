-- | The network consists of NetworkNodes that are connected by Edges.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Network
  ( NetworkNode
  , Name
  , Edge
  , EdgeMap)

where
import Prelude
import Perspectives.DestructiveArray
import Delta (Delta, Deltas)
import Perspectives.DestructiveMap (DestructiveMap, setInMap, getFromMap, deleteFromMap)

type Name = String

-- | The update function embodies the update of a dependent NetworkNode, expressed as an Array of Deltas, based on a Delta coming from a support.
type Update a b = Delta a -> Deltas b

-- | An Edge connects two NetworkNodes. It contains an update function that will propell a change from a support to a dependent.
newtype Edge b = Edge
  { name :: String
  , update :: forall a b. Delta a -> Deltas b
  , node :: NetworkNode b
}

type EdgeMap a = DestructiveMap (Edge a)


type NetworkNode a =
  { name :: String
  , payload :: a
  , changeHandlers :: Array (Deltas a -> Unit)
  , unprocessedDeltas :: forall b. Deltas b
  , dependents :: forall b. EdgeMap b
  , supports :: forall b. DestructiveArray (NetworkNode b)
}

--getNodeName :: forall a. NetworkNode a -> Name
getNodeName :: forall r. { name :: String | r } -> Name
getNodeName {name} = name


getDependent :: forall a. NetworkNode a -> Name -> Edge
getDependent (NetworkNode { dependents } ) k = getFromMap dependents k

addDependent :: forall a. NetworkNode a -> Name -> Edge -> EdgeMap
addDependent	source@(NetworkNode { dependents }) name edge@(Edge { node : NetworkNode { supports } })
  = setInMap dependents name edge where
      discard = pushInDestructiveArray supports source

removeDependent :: forall a. NetworkNode a -> Name -> EdgeMap
removeDependent	source@(NetworkNode { dependents }) name
  = let
        edge = getDependent source name
        discard = deleteFromMap dependents name
    in
      deleteFromMap dependents name
