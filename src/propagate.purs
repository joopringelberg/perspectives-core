-- | Propagate Location update through the node network, respecting topological sorting.
-- | This we implement using a breadth-first strategy. Start a queue with support-less nodes.
-- | Pick the first node and update it.
-- | Add its dependents, topologically ordered, to the back of the queue and repeat.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Propagate where

import Perspectives.Location (Location(Location), nodeLocation)
import Perspectives.Node (Node(Node))
import Perspectives.FifoQueue
import Data.Unit (Unit, unit)
import Data.StrMap (values)

type PropagationQueue = Queue Node

propagate :: Queue Node -> Unit
propagate q | empty q = unit
propagate q =
      let
        next@(Node {dependents}) = popFromFront q
        ignore = appendToEnd q (values dependents)
        ignore2 = recomputeLocation (nodeLocation next)
      in propagate q

-- | Recompute the value of a Location based on its changed supports.
recomputeLocation :: forall a. Location a -> Location a
recomputeLocation (Location v (Node{recompute})) = nodeLocation (recompute unit )

-- | Set the value of a Location to a new value.
setLocation :: forall a. Location a -> a -> Location a
setLocation (Location v (Node{set})) a = nodeLocation (set a )
