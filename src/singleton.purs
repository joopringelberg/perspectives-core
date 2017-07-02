-- | Singleton is a NetworkNode with a payload that is a single value.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Singleton where
import Perspectives.Network (NetworkNode)
import Perspectives.ContextWithDeltas

-- A singleton is a NetworkNode whose payload is just a thing.
type Singleton a = Singleton (NetworkNode a) 
-- newtype Singleton a = Singleton (Maybe (NetworkNode a))

-- The Singleton instance of ContextWithDeltas. Note: no serious implementation!
instance contextWithDeltasSequence :: ContextWithDeltas (Singleton a) where
  shortName a = "constant"
  processDeltas a = a
  add a b = a
  remove a b = a
  addDependent (Singleton s) n e = addDependent s n e

-- In a function defined on Singleton, one must deconstruct a Singleton to a NetworkNode to be able to call a NetworkNode function.
--singletonName :: forall a. Singleton a -> String
--singletonName (Singleton n) = getNodeName n
