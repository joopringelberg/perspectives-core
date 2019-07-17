module Perspectives.TheoryChange (updateFromSeeds, modifyTriple, propagate, addTripleToQueue, addToQueue, tripleRefToTripleQueueElement, tripleToTripleQueueElement) where

import Data.Array (cons, delete, difference, elemIndex, foldr, null, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, Triple(..), TripleQueue, TripleQueueElement(..), TripleRef(..))
import Perspectives.PerspectivesState (addToRecomputed, setTripleQueue, tripleQueue)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery)
import Perspectives.TripleAdministration (addDependency, getRef, lookupInTripleIndex, removeDependency, setSupports_, unRegisterBasicTriple)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Prelude (Ordering(..), Unit, bind, discard, identity, join, map, pure, unit, void, ($), (*>))
import Unsafe.Coerce (unsafeCoerce)

tripleToTripleQueueElement :: Triple String String -> TripleQueueElement
tripleToTripleQueueElement = unsafeCoerce

tripleToTripleRef :: Triple String String -> TripleRef
tripleToTripleRef = unsafeCoerce

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
-- addToQueue q triples = (sortBy dependsOn (union q (difference triples q)))

propagate :: MonadPerspectives Unit
propagate = do
  (q :: Array TripleQueueElement) <- tripleQueue
  setTripleQueue []
  void $ updateFromSeeds q

updateFromSeeds :: TripleQueue -> MonadPerspectives (Array String)
updateFromSeeds ts = do
  x <- pure (map (\(TripleQueueElement{dependencies}) -> map tripleRefToTripleQueueElement dependencies) ts)
  propagateTheoryDeltas (join x)

-- | Recompute the triples supported by triples that have changed, in the queue.
-- | This is the only place where triples supported by other triples are changed destructively.
propagateTheoryDeltas :: TripleQueue -> MonadPerspectives (Array String)
propagateTheoryDeltas q = case popFromQueue q of
  Nothing -> pure []
  (Just {head, tail}) -> do
    tr@(Triple{dependencies, supports: oldSupports}) <- pure $ tripleQueueElementToTriple head
    -- Note: the recomputed triple will not have dependencies. These are computed on adding other triples,
    -- from their supports. And when triples are removed, their supports should lose a depencency.
    t@(Triple{object, supports} :: Triple String String) <- recompute tr
    -- For each triple that no longer supports tr, remove tr as a dependency.
    liftEffect $ for_ (difference oldSupports supports)
      (removeDependency (getRef tr))
    -- For each new triple that supports tr, add tr as a dependency
    liftEffect $ for_ (difference supports oldSupports)
      (addDependency (getRef tr))
    _ <- liftEffect $ saveChangedObject tr object
    _ <- liftEffect $ setSupports_ tr supports
    -- If the new triple has no supports, we can remove it, as we only have computed triples here.
    if (null supports)
      then void $ liftEffect $ unRegisterBasicTriple $ tripleToTripleRef t
      else pure unit
    propagateTheoryDeltas (addToQueue tail (map tripleRefToTripleQueueElement dependencies))

getDependencies :: Triple String String ->  Effect (Array (Triple String String))
getDependencies (Triple{dependencies}) = do
  x <- traverse lookupRef dependencies
  pure (foldr (maybe identity cons) [] x)
  where
    lookupRef :: TripleRef -> Effect (Maybe (Triple String String))
    lookupRef (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

recompute :: Triple String String -> MonadPerspectives (Triple String String)
recompute (Triple{subject, predicate, tripleGetter}) =
  addToRecomputed (TripleRef {subject: subject, predicate: predicate}) *>
  runMonadPerspectivesQuery subject tripleGetter

-- Change the object of the triple to the array of IDs passed to the function.
foreign import saveChangedObject :: Triple String String -> Array String -> Effect (Triple String String)

-- | Actually modify the triple according to the Delta. Return the changed Triple.
-- | This is the only place where Triples without support are changed destructively.
modifyTriple :: Delta -> Effect (Maybe TripleQueueElement)
modifyTriple (Delta{id: rid, memberName: pid, value, deltaType}) =
  do
    value' <- pure (unsafePartial (fromJust value))
    mt <- lookupInTripleIndex rid pid
    case mt of
      Nothing -> pure Nothing
      (Just t@(Triple tr@{object})) -> do
        changedObject <- case deltaType of
          Add -> pure $ cons value' object
          Remove -> pure $ delete value' object
          Change -> pure [value']
        changedTriple <- (saveChangedObject t changedObject)
        pure $ Just $ tripleToTripleQueueElement changedTriple
