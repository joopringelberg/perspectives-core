module Perspectives.TheoryChange (updateFromSeeds, modifyTriple, propagate, addTripleToQueue) where

import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Array (cons, delete, difference, elemIndex, foldr, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, Triple(..), TripleQueue, TripleQueueElement(..), TripleRef(..))
import Perspectives.PerspectivesState (addToRecomputed, setTripleQueue, tripleQueue)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery)
import Perspectives.TripleAdministration (lookupInTripleIndex, setSupports_)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Prelude (Ordering(..), Unit, bind, identity, join, pure, void, ($), discard, map, (*>))
import Unsafe.Coerce (unsafeCoerce)

tripleToTripleQueueElement :: Triple String String -> TripleQueueElement
tripleToTripleQueueElement = unsafeCoerce

tripleQueueElementToTripleRef :: TripleQueueElement -> TripleRef
tripleQueueElementToTripleRef = unsafeCoerce

tripleQueueElementToTriple :: TripleQueueElement -> Triple String String
tripleQueueElementToTriple = unsafeCoerce

tripleRefToTripleQueueElement :: TripleRef -> TripleQueueElement
tripleRefToTripleQueueElement = unsafeCoerce

addTripleToQueue :: TripleQueueElement ->  MonadPerspectives Unit
addTripleToQueue t = do
  q <- tripleQueue
  setTripleQueue $ cons t q

pushIntoQueue :: forall a. Array a -> a -> Array a
pushIntoQueue = snoc

popFromQueue :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
popFromQueue = uncons

-- | dependsOn t1 t2 returns GT if t1 depends on t2, that is, t1 is one of t2's dependencies.
-- | In the graph, draw t1 above t2 with an arrow pointing from t1 downwards to t2. Hence, t1 is GT than t2.
-- | (dependsOn is the inverse of hasDependency, in other words, dependencies)
dependsOn :: TripleQueueElement -> TripleQueueElement -> Ordering
dependsOn tqe (TripleQueueElement{dependencies}) =
  case elemIndex (tripleQueueElementToTripleRef tqe) dependencies of
    Nothing -> EQ
    otherwise -> GT

addToQueue :: TripleQueue -> TripleQueue -> TripleQueue
addToQueue q triples = union q (sortBy dependsOn (difference triples q))

propagate :: MonadPerspectives Unit
propagate = do
  (q :: Array TripleQueueElement) <- tripleQueue
  setTripleQueue []
  void $ updateFromSeeds q

updateFromSeeds :: TripleQueue -> MonadPerspectives (Array String)
updateFromSeeds ts = do
  x <- pure (map (\(TripleQueueElement{dependencies}) -> map tripleRefToTripleQueueElement dependencies) ts)
  propagateTheoryDeltas (join x)

propagateTheoryDeltas :: TripleQueue -> MonadPerspectives (Array String)
propagateTheoryDeltas q = case popFromQueue q of
  Nothing -> pure []
  (Just {head, tail}) -> do
    tr@(Triple{dependencies}) <- pure $ tripleQueueElementToTriple head
    -- Note: the recomputed triple will not have dependencies. These are computed on adding other triples,
    -- from their supports. And when triples are removed, their supports should lose a depencency.
    t@(Triple{object, supports} :: Triple String String) <- recompute tr
    _ <- liftAff $ saveChangedObject tr object
    _ <- liftAff $ liftEffect $ setSupports_ tr supports
    propagateTheoryDeltas (addToQueue tail (map tripleRefToTripleQueueElement dependencies))

getDependencies :: Triple String String ->  Effect (Array (Triple String String))
getDependencies (Triple{dependencies}) = do
  x <- traverse lookupRef dependencies
  pure (foldr (maybe identity cons) [] x)
  where
    -- lookupRef :: TripleRef -> Effect (gm :: GLOBALMAP | eff) (Maybe (Triple String String))
    lookupRef (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

recompute :: Triple String String -> MonadPerspectives (Triple String String)
recompute (Triple{subject, predicate, tripleGetter}) = addToRecomputed (TripleRef {subject: subject, predicate: predicate}) *> runMonadPerspectivesQuery subject tripleGetter

-- Change the object of the triple to the array of IDs passed to the function.
saveChangedObject :: Triple String String -> Array String -> Aff (Triple String String)
saveChangedObject t obj = liftEffect (saveChangedObject_ t obj)

foreign import saveChangedObject_ :: Triple String String -> Array String -> Effect (Triple String String)

-- | Actually modify the triple according to the Delta. Return the changed Triple.
modifyTriple :: Delta -> Aff (Maybe TripleQueueElement)
modifyTriple (Delta{id: rid, memberName: pid, value, deltaType}) =
  do
    value' <- pure (unsafePartial (fromJust value))
    mt <- liftEffect $ lookupInTripleIndex rid pid
    case mt of
      Nothing -> pure Nothing
      (Just t@(Triple tr@{object})) -> do
        changedObject <- case deltaType of
          Add -> pure $ cons value' object
          Remove -> pure $ delete value' object
          Change -> pure [value']
        changedTriple <- (saveChangedObject t changedObject)
        pure $ Just $ tripleToTripleQueueElement changedTriple
