module Perspectives.TheoryChange where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, delete, difference, elemIndex, foldr, snoc, sortBy, uncons, union)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (Triple(..), TripleRef(..), MonadPerspectives, runMonadPerspectivesQuery)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (Subject, Predicate)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.TripleAdministration (getRef, getTriple, lookupInTripleIndex, removeDependency_, setSupports_)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Prelude (Ordering(..), Unit, bind, id, join, pure, void, ($), (<<<), (>>=))

pushIntoQueue :: forall a. Array a -> a -> Array a
pushIntoQueue = snoc

popFromQueue :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
popFromQueue = uncons

-- | dependsOn t1 t2 returns GT if t1 depends on t2, that is, t1 is one of t2's dependencies.
-- | In the graph, draw t1 above t2 with an arrow pointing from t1 downwards to t2. Hence, t1 is GT than t2.
-- | (dependsOn is the inverse of hasDependency, in other words, dependencies)
dependsOn :: forall e1 e2. Triple e1 -> Triple e2 -> Ordering
dependsOn (Triple{subject, predicate}) (Triple{dependencies}) =
  case elemIndex (TripleRef{subject: subject, predicate: predicate}) dependencies of
    Nothing -> EQ
    otherwise -> GT

type TripleQueue e = Array (Triple e)

addToQueue :: forall e. TripleQueue e -> Array (Triple e) -> TripleQueue e
addToQueue q triples = union q (sortBy dependsOn (difference triples q))

updateFromSeeds :: forall e. Array (Triple e) -> MonadPerspectives (AjaxAvarCache e) (Array String)
updateFromSeeds ts = do
  x <- liftEff (traverse getDependencies ts)
  propagateTheoryDeltas (join x)

propagateTheoryDeltas :: forall e. TripleQueue e -> MonadPerspectives (AjaxAvarCache e) (Array String)
propagateTheoryDeltas q = case popFromQueue q of
  Nothing -> pure []
  (Just {head, tail}) -> do
    t@(Triple{object, supports} :: Triple e) <- recompute head
    _ <- liftAff $ saveChangedObject head object
    _ <- liftAff $ liftEff $ updateDependencies head t
    _ <- liftAff $ liftEff $ setSupports_ head supports
    (deps :: Array (Triple e)) <- liftEff (getDependencies head)
    propagateTheoryDeltas (addToQueue tail deps)

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

modifyTriple :: forall e1 e2. Delta -> Aff (gm :: GLOBALMAP | e1) (Maybe (Triple e2))
modifyTriple (Delta{id: rid, memberName: pid, value, deltaType}) =
  do
    value' <- pure (unsafePartial (fromJust (unNullOrUndefined value)))
    mt <- liftEff $ lookupInTripleIndex rid pid
    case mt of
      Nothing -> pure Nothing
      (Just t@(Triple tr@{object})) -> do
        changedObject <- case deltaType of
          Add -> pure $ cons value' object
          Remove -> pure $ delete value' object
          Change -> pure [value']
        changedTriple <- (saveChangedObject t changedObject)
        pure $ Just changedTriple


-- Destructively change the objects of an existing triple.
-- Returns a Triple that can be used as a seed for delta propagation, i.e. (wrapped in an array) as argument to updateFromSeeds.
-- NOTA BENE. This function is superceded by the functions in deltas.purs.
setProperty :: forall e. Subject -> Predicate -> Array String -> (Aff (gm :: GLOBALMAP | e)) (Maybe (Triple e))
setProperty rid pid object =
  do
    mt <- liftEff $ lookupInTripleIndex rid pid
    case mt of
      Nothing -> pure Nothing
      (Just t) -> liftAff $ (saveChangedObject t object) >>= pure <<< Just
