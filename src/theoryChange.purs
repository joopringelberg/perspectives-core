module Perspectives.TheoryChange where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (StateT)
import Data.Array (cons, difference, elemIndex, foldr, snoc, sortBy, uncons, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Property (PropDefsEffects)
import Perspectives.TripleAdministration (Triple(..), TripleRef(..), FlexTriple, getRef, getTriple, lookupInTripleIndex, removeDependency, setSupports)
import Prelude (Ordering(..), Unit, bind, id, join, pure, void, ($))

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

updateFromSeeds :: forall e. Array (Triple e) -> StateT Boolean (Aff (PropDefsEffects e)) (Array String)
updateFromSeeds ts = do
  x <- liftEff (traverse getDependencies ts)
  propagateTheoryDeltas (join x)

propagateTheoryDeltas :: forall e. TripleQueue e -> StateT Boolean (Aff (PropDefsEffects e)) (Array String)
propagateTheoryDeltas q = case popFromQueue q of
  Nothing -> pure []
  (Just {head, tail}) -> do
    t@(Triple{object, supports} :: Triple e) <- recompute head
    _ <- liftAff $ saveChangedObject head object
    _ <- liftAff $ liftEff $ updateDependencies head t
    _ <- liftAff $ liftEff $ setSupports head supports
    (deps :: Array (Triple e)) <- liftEff (getDependencies head)
    propagateTheoryDeltas (addToQueue tail deps)

updateDependencies :: forall e e1. Triple e -> Triple e -> Eff (gm :: GLOBALMAP | e1) Unit
updateDependencies t@(Triple{supports: old}) (Triple{supports: new}) =
  foreachE (difference old new) remove where
    remove :: TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
    remove ref = void do
      mt <- getTriple ref
      case mt of
        (Just supportingTriple) -> removeDependency supportingTriple (getRef t)
        Nothing -> pure ref

getDependencies :: forall e eff. Triple e ->  Eff (gm :: GLOBALMAP | eff) (Array (Triple e))
getDependencies (Triple{dependencies}) = do
  x <- traverse lookupRef dependencies
  pure (foldr (maybe id cons) [] x)
  where
    -- lookupRef :: TripleRef -> Eff (gm :: GLOBALMAP | eff) (Maybe (Triple e))
    lookupRef (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

recompute :: forall e. Triple e -> FlexTriple e
recompute (Triple{subject, tripleGetter}) = tripleGetter subject

saveChangedObject :: forall e1 e2. Triple e2 -> Array String -> Aff e1 (Triple e2)
saveChangedObject t obj = liftEff (saveChangedObject_ t obj)

foreign import saveChangedObject_ :: forall e1 e2. Triple e2 -> Array String -> Eff e1 (Triple e2)

-- TODO Deze functie is niet langer nuttig, maar is wel model voor een serie van andere.
-- Met RDF konden we alleen resources maken en properties veranderen. Met CRL kunnen
-- we contexten en rollen maken, rollen binden en properties een waarde geven.
-- setProperty :: forall e. Resource -> Predicate -> Array String -> FlexTriple e
-- setProperty rid pid object =
--   do
--     mt <- liftEff $ lookupInTripleIndex rid pid
--     case mt of
--       -- Case Nothing will not happen when we have contexts.
--       Nothing -> do
--         t <- applyNamedFunction (constructTripleGetter pid) rid
--         liftAff $ saveChangedObject t object
--       (Just t) -> liftAff $ saveChangedObject t object
