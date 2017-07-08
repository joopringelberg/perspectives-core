-- | Propagate Location update through the node network, respecting topological sorting.
-- | This we implement using a breadth-first strategy. Start a queue with support-less nodes.
-- | Pick the first node and update it.
-- | Add its dependents, topologically ordered, to the back of the queue and repeat.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Propagate where

import Prelude ((<<<), map)
import Perspectives.FifoQueue
import Data.Array (elem, filter, sort)
import Data.StrMap (values)
import Data.Unit (unit)
import Perspectives.Location (Location(Location), nodeLocation)
import Perspectives.Node (Node(Node))


-- | Propagate update through the network, respecting topological ordering of nodes.
propagate :: Queue Node -> Queue Node
propagate q | empty q = q
propagate q = propagate extendedQueue where
  (Node {dependents}) = next q
  extendedQueue = appendToEnd q (values dependents)

propagateChanges :: forall a. Queue Node -> Array (Location a)
propagateChanges = (map (recomputeLocation <<< nodeLocation)) <<< nodesToRecompute

nodesToRecompute :: Queue Node -> Array Node
nodesToRecompute q | empty q = cumulator q
nodesToRecompute q = nodesToRecompute (appendToEnd q
      (sort (filter (\n -> elem n (cumulator q)) (values dependents))))
    where
      (Node {dependents}) = next q

-- | Recompute the value of a Location based on its changed supports.
recomputeLocation :: forall a. Location a -> Location a
recomputeLocation (Location v (Node{recompute})) = nodeLocation (recompute unit )

-- | Set the value of a Location to a new value.
setLocation :: forall a. Location a -> a -> Location a
setLocation (Location v (Node{set})) a = nodeLocation (set a )
