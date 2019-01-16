module Perspectives.TheoryChange (updateFromSeeds, modifyTriple, propagate, addTripleToQueue) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, delete, difference, elemIndex, foldr, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, Triple(..), TripleQueue, TripleQueueElement(..), TripleRef(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.PerspectivesState (setTripleQueue, tripleQueue)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery)
import Perspectives.TripleAdministration (getRef, getTriple, lookupInTripleIndex, removeDependency_, setSupports_)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Prelude (Ordering(..), Unit, bind, id, join, pure, void, ($), discard, map)
import Unsafe.Coerce (unsafeCoerce)

tripleToTripleQueueElement :: forall e. Triple e -> TripleQueueElement
tripleToTripleQueueElement = unsafeCoerce

tripleQueueElementToTripleRef :: forall e. TripleQueueElement -> TripleRef
tripleQueueElementToTripleRef = unsafeCoerce

tripleQueueElementToTriple :: forall e. TripleQueueElement -> Triple e
tripleQueueElementToTriple = unsafeCoerce

tripleRefToTripleQueueElement :: TripleRef -> TripleQueueElement
tripleRefToTripleQueueElement = unsafeCoerce

addTripleToQueue :: forall e. TripleQueueElement ->  MonadPerspectives (avar :: AVAR | e) Unit
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

addToQueue :: forall e. TripleQueue -> TripleQueue -> TripleQueue
addToQueue q triples = union q (sortBy dependsOn (difference triples q))

propagate :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
propagate = do
  (q :: Array TripleQueueElement) <- tripleQueue
  setTripleQueue []
  void $ updateFromSeeds q

updateFromSeeds :: forall e. TripleQueue -> MonadPerspectives (AjaxAvarCache e) (Array String)
updateFromSeeds ts = do
  x <- pure (map (\(TripleQueueElement{dependencies}) -> map tripleRefToTripleQueueElement dependencies) ts)
  propagateTheoryDeltas (join x)

propagateTheoryDeltas :: forall e. TripleQueue -> MonadPerspectives (AjaxAvarCache e) (Array String)
propagateTheoryDeltas q = case popFromQueue q of
  Nothing -> pure []
  (Just {head, tail}) -> do
    tr <- pure $ tripleQueueElementToTriple head
    t@(Triple{object, supports, dependencies} :: Triple e) <- recompute tr
    _ <- liftAff $ saveChangedObject tr object
    _ <- liftAff $ liftEff $ updateDependencies tr t
    _ <- liftAff $ liftEff $ setSupports_ tr supports
    propagateTheoryDeltas (addToQueue tail (map tripleRefToTripleQueueElement dependencies))

updateDependencies :: forall e e1. Triple e -> Triple e -> Eff (gm :: GLOBALMAP | e1) Unit
updateDependencies t@(Triple{supports: old}) (Triple{supports: new}) =
  foreachE (difference old new) remove where
    remove :: TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
    remove ref = void do
      mt <- getTriple ref
      case mt of
        (Just supportingTriple) -> removeDependency_ supportingTriple (getRef t)
        Nothing -> pure ref

getDependencies :: forall e eff. Triple e ->  Eff (gm :: GLOBALMAP | eff) (Array (Triple e))
getDependencies (Triple{dependencies}) = do
  x <- traverse lookupRef dependencies
  pure (foldr (maybe id cons) [] x)
  where
    -- lookupRef :: TripleRef -> Eff (gm :: GLOBALMAP | eff) (Maybe (Triple e))
    lookupRef (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

recompute :: forall e. Triple e -> MonadPerspectives (AjaxAvarCache e) (Triple e)
recompute (Triple{subject, tripleGetter}) = runMonadPerspectivesQuery subject tripleGetter

-- Change the object of the triple to the array of IDs passed to the function.
saveChangedObject :: forall e1 e2. Triple e2 -> Array String -> Aff e1 (Triple e2)
saveChangedObject t obj = liftEff (saveChangedObject_ t obj)

foreign import saveChangedObject_ :: forall e1 e2. Triple e2 -> Array String -> Eff e1 (Triple e2)

-- | Actually modify the triple according to the Delta. Return the changed Triple.
modifyTriple :: forall e1 e2. Delta -> Aff (gm :: GLOBALMAP | e1) (Maybe TripleQueueElement)
modifyTriple (Delta{id: rid, memberName: pid, value, deltaType}) =
  do
    value' <- pure (unsafePartial (fromJust value))
    mt <- liftEff $ lookupInTripleIndex rid pid
    case mt of
      Nothing -> pure Nothing
      (Just t@(Triple tr@{object})) -> do
        changedObject <- case deltaType of
          Add -> pure $ cons value' object
          Remove -> pure $ delete value' object
          Change -> pure [value']
        changedTriple <- (saveChangedObject t changedObject)
        pure $ Just $ tripleToTripleQueueElement changedTriple
