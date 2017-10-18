module Perspectives.TripleAdministration where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.ResourceTypes (ResourceId)
import Prelude (bind, pure, unit)

type PredicateId = String

newtype Triple a = Triple
  { subject :: ResourceId
  , predicate :: PredicateId
  , object :: a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

newtype TripleRef = TripleRef { subject :: ResourceId, predicate :: PredicateId}

-- | A global index of Triples, indexed by ResourceId.
newtype ResourceIndex a = ResourceIndex (GLStrMap (PredicateIndex a))

-- A store of objects indexed by PredicateId.
type PredicateIndex a = GLStrMap (Triple a)

class TripleStore ts where
  lookup :: forall e a. ts a -> ResourceId -> PredicateId -> Eff (gm :: GLOBALMAP | e) (Triple a)
  -- addTriple :: forall e.
  --   ResourceId ->
  --   PredicateId ->
  --   a ->
  --   Array TripleRef ->
  --   Array TripleRef ->
  --   Eff (gm :: GLOBALMAP | e) (ResourceIndex a)
  -- addResource :: forall e. ResourceIndex a -> ResourceId -> PredicateIndex a -> Eff (gm :: GLOBALMAP | e) (ResourceIndex a)
  -- newPredicateIndex :: forall e. ResourceId -> PredicateId -> a -> Eff (gm :: GLOBALMAP | e) (PredicateIndex a)

defaultLookup :: forall a e. a -> ResourceIndex a -> ResourceId -> PredicateId -> Eff (gm :: GLOBALMAP | e) (Triple a)
defaultLookup emptyRepresentation (ResourceIndex index) rid pid = do
  (preds :: Maybe (PredicateIndex a)) <- peek index rid
  case preds of
    Nothing ->
      pure (Triple{ subject: rid
              , predicate: pid
              , object: emptyRepresentation
              , supports: []
              , dependencies: []
              })
    (Just p) -> do
      (objls :: Maybe (Triple a)) <- peek p pid
      case objls of
        Nothing ->
          pure (Triple{ subject: rid
                  , predicate: pid
                  , object: emptyRepresentation
                  , supports: []
                  , dependencies: []
                  })
        (Just o) -> pure o

-- defaultAddTriple :: forall e a.
--   ResourceIndex a ->
--   ResourceId ->
--   PredicateId ->
--   a ->
--   Array TripleRef ->
--   Array TripleRef ->
--   Eff (gm :: GLOBALMAP | e) (ResourceIndex a)
-- defaultAddTriple index rid pid val sups deps =
--     do
--       (m :: PredicateIndex a) <- pure (new unit)
--       predIndex <- poke m pid (Triple{ subject: rid
--                 , predicate: pid
--                 , object: val
--                 , supports: sups
--                 , dependencies: deps
--                 })
--       poke index rid predIndex
--
-- defaultAddResource :: forall e a. ResourceIndex a -> ResourceId -> PredicateIndex a -> Eff (gm :: GLOBALMAP | e) (ResourceIndex a)
-- defaultAddResource = poke
--
-- defaultNewPredicateIndex :: forall e a. ResourceId -> PredicateId -> a -> Eff (gm :: GLOBALMAP | e) (PredicateIndex a)
-- defaultNewPredicateIndex rid pid val = do
--   (m :: PredicateIndex a) <- pure (new unit)
--   poke m pid (Triple{ subject: rid
--             , predicate: pid
--             , object: val
--             , supports: []
--             , dependencies: []
--             })

-- The global index of triples with a single Int value.
intIndex :: ResourceIndex (Maybe Int)
intIndex = ResourceIndex (new unit)

intsIndex :: ResourceIndex (Array Int)
intsIndex = ResourceIndex (new unit)

instance intStore :: TripleStore ResourceIndex where
  lookup = defaultLookup Nothing
  -- addTriple = defaultAddTriple intIndex
  -- addResource = defaultAddResource
  -- newPredicateIndex = defaultNewPredicateIndex

instance intsStore :: TripleStore (Array Int) where
  lookup = defaultLookup []
  -- addTriple = defaultAddTriple intsIndex
  -- addResource = defaultAddResource
  -- newPredicateIndex = defaultNewPredicateIndex
