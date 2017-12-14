module Perspectives.TripleAdministration where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (StateT, get)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Property (PropDefsEffects)
import Perspectives.ResourceTypes (Resource)
import Prelude (class Eq, Unit, bind, pure, unit, void, ($), (&&), (<>), (==))

type Predicate = String

-- type TripleGetter e = Resource -> Aff (PropDefsEffects e) (Triple e)

type FlexTriple e = StateT Boolean (Aff (PropDefsEffects e)) (Triple e)

type TripleGetter e = Resource -> FlexTriple e

data NamedFunction f = NamedFunction String f

newtype Triple e = Triple
  { subject :: Resource
  , predicate :: Predicate
  , object :: Array String
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter e}

tripleObjects :: forall e. Triple e -> Array String
tripleObjects (Triple{object}) = object

instance showTriple :: Show (Triple e) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: Eq (Triple e) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

newtype TripleRef = TripleRef { subject :: Resource, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

getRef :: forall e. Triple e -> TripleRef
getRef (Triple{subject, predicate}) = TripleRef{subject: subject, predicate: predicate}

-- | An index of Predicate-Object combinations, indexed by Resource.
type TripleIndex e = GLStrMap (PredicateIndex e)

-- An index of objects indexed by Predicate.
type PredicateIndex e = GLStrMap (Triple e)

-- | A global store of triples, indexed by Resource and Predicate.
tripleIndex :: forall e. TripleIndex e
tripleIndex = new unit

lookupInTripleIndex :: forall e1 e2. Resource -> Predicate -> Eff (gm :: GLOBALMAP | e1) (Maybe (Triple e2))
lookupInTripleIndex rid pid = do
  preds <- peek tripleIndex rid
  case preds of
    Nothing ->
      pure Nothing
    (Just p) -> do
      objls <- peek p pid
      case objls of
        Nothing ->
          pure Nothing
        (Just o) -> pure (Just o)

getTriple :: forall e1 e2. TripleRef -> Eff (gm :: GLOBALMAP | e1) (Maybe (Triple e2))
getTriple (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

-- | Add a triple to the index. The object value of the triple should comply to the type of the TripleIndex!
-- | Will add an entry for the Resource if it is not yet present.
addToTripleIndex :: forall e1 e2.
  Resource ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  Array TripleRef ->
  TripleGetter e2 ->
  Eff (gm :: GLOBALMAP | e1) (Triple e2)
addToTripleIndex rid pid val deps sups tripleGetter =
    do
      (m :: PredicateIndex e2) <- ensureResource rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                , supports : sups
                , tripleGetter: tripleGetter
                })
      predIndex <- poke m pid triple
      _ <- poke tripleIndex rid predIndex
      _ <- foreachE sups (addDependency_ (getRef triple))
      pure triple

registerTriple :: forall e1 e2. Triple e2 -> Eff (gm :: GLOBALMAP | e1) (Triple e2)
registerTriple (Triple{subject, predicate, object, dependencies, supports, tripleGetter}) = addToTripleIndex subject predicate object dependencies supports tripleGetter

ensureResource :: forall e1 e2. Resource -> Eff (gm :: GLOBALMAP | e1) (PredicateIndex e2)
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex e2) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

memorize :: forall e. TripleGetter e -> String -> NamedFunction (TripleGetter e)
memorize getter name = NamedFunction name \id -> do
  remember <- get
  case remember of
    true -> do
      mt <- liftEff (lookupInTripleIndex id name)
      case mt of
        Nothing -> do
          t <- getter id
          liftEff $ registerTriple t
        (Just t) -> pure t
    false -> do
      t <- getter id
      liftEff $ registerTriple t

-- TODO: dit moet eigenlijk een apart effect zijn, b.v.: DEPENDENCY.
foreign import addDependency :: forall e1 e2. Triple e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef
foreign import removeDependency :: forall e1 e2. Triple e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef
foreign import setSupports ::  forall e1 e2. Triple e2 -> Array TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit

addDependency_ :: forall e1. TripleRef -> TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
addDependency_ dependentRef supportingRef = do
  ms <- getTriple supportingRef
  case ms of
    (Just support) -> void (addDependency support dependentRef)
    Nothing -> pure unit
