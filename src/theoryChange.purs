module Perspectives.TheoryChange where

import Data.Array (difference, elemIndex, head, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..))
import Perspectives.TripleAdministration (Triple(..), TripleRef(..))
import Prelude (Ordering(..), Unit, unit)

pushIntoQueue :: forall a. Array a -> a -> Array a
pushIntoQueue = snoc

popFromQueue :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
popFromQueue = uncons

-- | dependsOn t1 t2 returns GT if t1 depends on t2, that is, t1 is one of t2's dependencies.
-- | In the graph, draw t1 above t2 with an arrow pointing from t1 downwards to t2. Hence, t1 is GT than t2.
-- | (dependsOn is the inverse of hasDependency, in other words, dependencies)
dependsOn :: Triple -> Triple -> Ordering
dependsOn (Triple{subject, predicate}) (Triple{dependencies}) =
  case elemIndex (TripleRef{subject: subject, predicate: predicate}) dependencies of
    Nothing -> EQ
    otherwise -> GT

type TripleQueue = Array Triple

addToQueue :: TripleQueue -> Array Triple -> TripleQueue
addToQueue q triples = union q (sortBy dependsOn (difference triples q))

-- propagateTheoryDeltas :: TripleQueue -> Unit
-- propagateTheoryDeltas q = case popFromQueue q of
--   Nothing -> unit
--   (Just {head, tail}) -> do
--     triple <- recompute head
--     pure propagateTheoryDeltas tail
