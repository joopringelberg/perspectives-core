-- | Propagate Location update through the node network, respecting topological sorting.
-- | This we implement using a breadth-first strategy. Start a queue with support-less nodes.
-- | Pick the first node and update it.
-- | Add its dependents, topologically ordered, to the back of the queue and repeat.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Propagate where

import Control.Monad.Eff
import Perspectives.FifoQueue
import Data.Array (elem, filter, sort)
import Data.StrMap (values)
import Data.Unit (Unit,unit)
import Perspectives.Location (Location(Location), nodeLocation)
import Perspectives.Node (Node(Node), THEORYDELTA, setNode, recomputeNode)
import Prelude ((<<<), map, bind, pure)

type ChangedNode = Eff (td :: THEORYDELTA) Node
type QueueOfChangedNodes = Queue ChangedNode
-- | A Queue of Nodes wrapped in the THEORYDELTA effect.
currentQueue :: QueueOfChangedNodes
currentQueue = queue []

{-}
-- | Propagate update through the network, respecting topological ordering of nodes.
propagateChanges :: forall a eff.
  Eff (td :: THEORYDELTA | eff) (Queue Node)
  -> Eff eff (Array Unit)
--propagateChanges = (map (recomputeLocation <<< nodeLocation)) <<< nodesToRecompute
propagateChanges queue = do
  map recomputeLocation (nodesToRecompute queue)

nodesToRecompute :: forall eff.
  Eff (td :: THEORYDELTA | eff) (Queue Node)
  -> Eff (td :: THEORYDELTA | eff) (Array Node)
nodesToRecompute queue = bind queue (\q ->
  if ( empty q)
  then pure (cumulator q)
  else
    let (Node {dependents}) = next q
    in
      nodesToRecompute (pure (appendToEnd q
            (sort (filter (\n -> elem n (cumulator q)) (values dependents))))))

-- | Recompute the value of a Location based on its changed supports.
recomputeLocation :: forall a eff. Eff (td ::THEORYDELTA | eff) (Location a) -> Eff eff Unit
recomputeLocation l = do
    recomputedNode <- recomputeNode (getNode l)
    pure unit
  where
    getNode :: forall b e. Eff (td ::THEORYDELTA | e) (Location b) -> ChangedNode
    getNode loc = do
      (Location _ n) <- loc
      pure n

-- | Set the value of a Location to a new value.
setLocation :: forall a. Location a -> a -> QueueOfChangedNodes
setLocation l@(Location _ n) a = appendToEnd currentQueue [setNode n a]
-}
