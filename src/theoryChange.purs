module Perspectives.TheoryChange where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, difference, elemIndex, foldr, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Property (PropDefsEffects, lookupInObjectsGetterIndex)
import Perspectives.TripleAdministration (Triple(..), TripleRef(..), lookupInTripleIndex)
import Prelude (Ordering(..), Unit, bind, id, pure, (+))

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

propagateTheoryDeltas :: forall e. TripleQueue -> Aff (PropDefsEffects e) (Array String)
propagateTheoryDeltas q = case popFromQueue q of
  Nothing -> pure []
  (Just {head, tail}) -> do
    (obj :: Array String) <- recompute head
    _ <- saveChangedObject head obj
    (deps :: Array Triple) <- liftEff (getDependencies head)
    propagateTheoryDeltas (addToQueue tail deps)
  where

    getDependencies :: Triple ->  Eff (PropDefsEffects e) (Array Triple)
    getDependencies (Triple{dependencies}) = do
      x <- traverse lookupRef dependencies
      pure (foldr (maybe id cons) [] x)

    lookupRef :: forall eff. TripleRef -> Eff (gm :: GLOBALMAP | eff) (Maybe Triple)
    lookupRef (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

recompute :: forall e. Triple -> Aff (PropDefsEffects e) (Array String)
recompute (Triple{subject, predicate}) = do
  mp <- liftEff (lookupInObjectsGetterIndex predicate)
  case mp of
    Nothing -> pure []
    (Just p) -> p subject

saveChangedObject :: forall e. Triple -> Array String -> Aff e Unit
saveChangedObject t obj = liftEff (saveChangedObject_ t obj)

foreign import saveChangedObject_ :: forall e. Triple -> Array String -> Eff e Unit
