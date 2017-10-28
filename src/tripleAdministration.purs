module Perspectives.TripleAdministration where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Property (ObjectsGetter)
import Perspectives.ResourceTypes (Resource)
import Prelude (class Eq, bind, pure, unit, (&&), (<>), (==))

type Predicate = String

newtype Triple e = Triple
  { subject :: Resource
  , predicate :: Predicate
  , object :: Array String
  , dependencies :: Array TripleRef
  , objectsGetter :: ObjectsGetter e}

instance showTriple :: Show (Triple e) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: Eq (Triple e) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

newtype TripleRef = TripleRef { subject :: Resource, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

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

-- | Add a triple to the index. The object value of the triple should comply to the type of the TripleIndex!
-- | Will add an entry for the Resource if it is not yet present.
addToTripleIndex :: forall e1 e2.
  Resource ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  ObjectsGetter e2 ->
  Eff (gm :: GLOBALMAP | e1) (Triple e2)
addToTripleIndex rid pid val deps objsGetter =
    do
      (m :: PredicateIndex e2) <- ensureResource rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                , objectsGetter: objsGetter
                })
      predIndex <- poke m pid triple
      _ <- poke tripleIndex rid predIndex
      pure triple

registerTriple :: forall e1 e2. Triple e2 -> Eff (gm :: GLOBALMAP | e1) (Triple e2)
registerTriple (Triple{subject, predicate, object, dependencies, objectsGetter}) = addToTripleIndex subject predicate object dependencies objectsGetter

ensureResource :: forall e1 e2. Resource -> Eff (gm :: GLOBALMAP | e1) (PredicateIndex e2)
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex e2) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

-- TODO: dit moet eigenlijk een apart effect zijn, b.v.: DEPENDENCY.
foreign import addDependency :: forall e1 e2. Triple e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef
