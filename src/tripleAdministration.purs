module Perspectives.TripleAdministration where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.ResourceTypes (Resource)
import Prelude (class Eq, bind, pure, unit, (&&), (<>), (==))

type Predicate = String

newtype Triple = Triple
  { subject :: Resource
  , predicate :: Predicate
  , object :: Array String
  , dependencies :: Array TripleRef}

instance showTriple :: Show Triple where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: Eq Triple where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

newtype TripleRef = TripleRef { subject :: Resource, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

-- | An index of Predicate-Object combinations, indexed by Resource.
type TripleIndex = GLStrMap PredicateIndex

-- An index of objects indexed by Predicate.
type PredicateIndex = GLStrMap Triple

-- | A global store of triples, indexed by Resource and Predicate.
tripleIndex :: TripleIndex
tripleIndex = new unit

lookupInTripleIndex :: forall e. Resource -> Predicate -> Eff (gm :: GLOBALMAP | e) (Maybe Triple)
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

-- | Add a triple to the index. The object value of the triple should comply to the type of the TripleIndex!
-- | Will add an entry for the Resource if it is not yet present.
addToTripleIndex :: forall e.
  Resource ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  Eff (gm :: GLOBALMAP | e) Triple
addToTripleIndex rid pid val deps =
    do
      (m :: PredicateIndex) <- ensureResource rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                })
      predIndex <- poke m pid triple
      _ <- poke tripleIndex rid predIndex
      pure triple

registerTriple :: forall e. Triple -> Eff (gm :: GLOBALMAP | e) Triple
registerTriple (Triple{subject, predicate, object, dependencies}) = addToTripleIndex subject predicate object dependencies

ensureResource :: forall e. Resource -> Eff (gm :: GLOBALMAP | e) PredicateIndex
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

foreign import addDependency :: forall e. Triple -> TripleRef -> Eff (gm :: GLOBALMAP | e) TripleRef
