module Perspectives.TripleAdministration where

import Control.Monad.Eff (Eff)
import Data.Array (head, null)
import Data.Maybe (Maybe(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.ResourceTypes (Resource(..), ResourceId)
import Prelude (class Functor, bind, id, pure, unit)

class Functor ef <= PossiblyEmptyFunctor ef where
  empty :: forall a. ef a
  isEmpty :: forall a. ef a -> Boolean
  fromArray :: forall a. Array a -> ef a

instance possiblyEmptyMaybe :: PossiblyEmptyFunctor Maybe where
  empty = Nothing
  isEmpty Nothing = true
  isEmpty otherwise = false
  fromArray = head

instance possiblyEmptyArray :: PossiblyEmptyFunctor Array where
  empty = []
  isEmpty = null
  fromArray = id

type PredicateId = String

newtype Triple a = Triple
  { subject :: ResourceId
  , predicate :: PredicateId
  , object :: a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

newtype TripleRef = TripleRef { subject :: ResourceId, predicate :: PredicateId}

-- | A global index of Triples, indexed by ResourceId.
type ResourceIndex a = GLStrMap (PredicateIndex a)

-- A store of objects indexed by PredicateId.
type PredicateIndex a = GLStrMap (Triple a)

lookup :: forall e a ef. PossiblyEmptyFunctor ef => ResourceIndex (ef a) -> ResourceId -> PredicateId -> Eff (gm :: GLOBALMAP | e) (Triple (ef a))
lookup index rid pid = do
  preds <- peek index rid
  case preds of
    Nothing ->
      pure (Triple{ subject: rid
              , predicate: pid
              , object: empty
              , supports: []
              , dependencies: []
              })
    (Just p) -> do
      objls <- peek p pid
      case objls of
        Nothing ->
          pure (Triple{ subject: rid
                  , predicate: pid
                  , object: empty
                  , supports: []
                  , dependencies: []
                  })
        (Just o) -> pure o

-- | Add a triple to the index. The object value of the triple should comply to the type of the ResourceIndex!
-- | Will add an entry for the resourceId if it is not yet present.
addTriple :: forall e a ef. PossiblyEmptyFunctor ef =>
  ResourceIndex (ef a) ->
  ResourceId ->
  PredicateId ->
  (ef a) ->
  Array TripleRef ->
  Array TripleRef ->
  Eff (gm :: GLOBALMAP | e) (Triple (ef a))
addTriple index rid pid val sups deps =
    do
      (m :: PredicateIndex (ef a)) <- ensureResource index rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , supports: sups
                , dependencies: deps
                })
      predIndex <- poke m pid triple
      _ <- poke index rid predIndex
      pure triple

ensureResource :: forall e a ef. PossiblyEmptyFunctor ef => ResourceIndex (ef a) -> ResourceId -> Eff (gm :: GLOBALMAP | e) (PredicateIndex (ef a))
ensureResource index rid = do
  pid <- peek index rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex (ef a)) <- pure (new unit)
        _ <- poke index rid m
        pure m
    (Just m) -> pure m

-- The global index of triples with a single Int value.
intIndex :: ResourceIndex (Maybe Int)
intIndex = new unit

-- The global index of triples with multiple Int values.
intsIndex :: ResourceIndex (Array Int)
intsIndex = new unit

-- The global index of triples with a single String value.
stringIndex :: ResourceIndex (Maybe String)
stringIndex = new unit

-- The global index of triples with multiple String values.
stringsIndex :: ResourceIndex (Array String)
stringsIndex = new unit

-- The global index of triples with a single Number value.
numberIndex :: ResourceIndex (Maybe Number)
numberIndex = new unit

-- The global index of triples with multiple Number values.
numbersIndex :: ResourceIndex (Array Number)
numbersIndex = new unit

-- The global index of triples with a single Boolean value.
booleanIndex :: ResourceIndex (Maybe Boolean)
booleanIndex = new unit

-- The global index of triples with a single Resource value.
resourceIndex :: ResourceIndex (Maybe Resource)
resourceIndex = new unit

-- The global index of triples with multiple Resource values.
resourcesIndex :: ResourceIndex (Array Resource)
resourcesIndex = new unit
