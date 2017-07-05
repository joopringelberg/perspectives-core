-- | The typeclass ContextWithDeltas provides an abstraction for NetworkNodes.
-- | Singleton and Sequence are instances.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.ContextWithDeltas where

import Perspectives.Network

class ContextWithDeltas a where
  shortName :: a -> String
  processDeltas :: a -> a
  add :: forall b . a -> (Array b) -> a
  remove :: forall b . a -> (Array b) -> a
  addDependent :: forall b. a -> Name -> Edge b -> EdgeMap b
