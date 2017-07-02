-- | The network consists of NetworkNodes that are connected by Edges.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Sequence where
import Perspectives.Network (NetworkNode)
import Perspectives.ContextWithDeltas

-- A sequence is a NetworkNode whose payload is an array of things;
newtype Sequence a = Sequence (NetworkNode (Array a))


-- The Sequence instance of ContextWithDeltas. Note: no serious implementation!
instance contextWithDeltasSequence :: ContextWithDeltas (Sequence a) where
  shortName a = "constant"
  processDeltas a = a
  add a b = a
  remove a b = a
