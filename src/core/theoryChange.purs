module Perspectives.TheoryChange (updateFromSeeds, modifyTriple, propagate, addTripleToQueue) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, delete, difference, elemIndex, foldr, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, Triple(..), TripleQueue, TripleQueueElement(..), TripleRef(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.PerspectivesState (addToRecomputed, setTripleQueue, tripleQueue)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery)
import Perspectives.TripleAdministration (lookupInTripleIndex, setSupports_)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Prelude (Ordering(..), Unit, bind, id, join, pure, void, ($), discard, map, (*>))
import Unsafe.Coerce (unsafeCoerce)

tripleToTripleQueueElement :: forall e. Triple String String e -> TripleQueueElement
tripleToTripleQueueElement = unsafeCoerce

tripleQueueElementToTripleRef :: forall e. TripleQueueElement -> TripleRef
tripleQueueElementToTripleRef = unsafeCoerce

tripleQueueElementToTriple :: forall e. TripleQueueElement -> Triple String String e
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
    tr@(Triple{dependencies}) <- pure $ tripleQueueElementToTriple head
    -- Note: the recomputed triple will not have dependencies. These are computed on adding other triples,
    -- from their supports. And when triples are removed, their supports should lose a depencency.
    t@(Triple{object, supports} :: Triple String String e) <- recompute tr
    _ <- liftAff $ saveChangedObject tr object
    _ <- liftAff $ liftEff $ setSupports_ tr supports
    propagateTheoryDeltas (addToQueue tail (map tripleRefToTripleQueueElement dependencies))

getDependencies :: forall e eff. Triple String String e ->  Eff (gm :: GLOBALMAP | eff) (Array (Triple String String e))
getDependencies (Triple{dependencies}) = do
  x <- traverse lookupRef dependencies
  pure (foldr (maybe id cons) [] x)
  where
    -- lookupRef :: TripleRef -> Eff (gm :: GLOBALMAP | eff) (Maybe (Triple String String e))
    lookupRef (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

recompute :: forall e. Triple String String e -> MonadPerspectives (AjaxAvarCache e) (Triple String String e)
recompute (Triple{subject, predicate, tripleGetter}) = addToRecomputed (TripleRef {subject: subject, predicate: predicate}) *> runMonadPerspectivesQuery subject tripleGetter

-- Change the object of the triple to the array of IDs passed to the function.
saveChangedObject :: forall e1 e2. Triple String String e2 -> Array String -> Aff e1 (Triple String String e2)
saveChangedObject t obj = liftEff (saveChangedObject_ t obj)

foreign import saveChangedObject_ :: forall e1 e2. Triple String String e2 -> Array String -> Eff e1 (Triple String String e2)

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
