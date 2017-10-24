module Perspectives.TripleAdministration where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Property (AsyncPropDefsM, PropertyName, getGetter)
import Perspectives.ResourceTypes (Resource)
import Prelude (bind, pure, unit, (<>))

type Predicate = String

newtype Triple = Triple
  { subject :: Resource
  , predicate :: Predicate
  , object :: Array String
  , dependencies :: Array TripleRef}

instance showTriple :: Show Triple where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

newtype TripleRef = TripleRef { subject :: Resource, predicate :: Predicate}

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

-- | A global index of Triples, indexed by Resource.
type ResourceIndex = GLStrMap PredicateIndex

tripleIndex :: ResourceIndex
tripleIndex = new unit

-- | The global store of triples, indexed by Resource and Predicate.

-- A store of objects indexed by Predicate.
type PredicateIndex = GLStrMap Triple

data NamedFunction f = NamedFunction String f

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

type TripleGetter e = Resource -> AsyncPropDefsM e Triple

-- | Use this function to construct property getters that memorize in the triple administration.
constructTripleGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructTripleGetter pn = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    t@(Triple{object} :: Triple) <- liftEff (lookup tripleIndex id pn)
    case null object of
      true -> do
        (object' :: Array String) <- getGetter pn id
        liftEff (addTriple id pn object' [])
      false -> pure t

lookup :: forall e. ResourceIndex -> Resource -> Predicate -> Eff (gm :: GLOBALMAP | e) Triple
lookup index rid pid = do
  preds <- peek index rid
  case preds of
    Nothing ->
      pure (Triple{ subject: rid
              , predicate: pid
              , object: []
              , dependencies: []
              })
    (Just p) -> do
      objls <- peek p pid
      case objls of
        Nothing ->
          pure (Triple{ subject: rid
                  , predicate: pid
                  , object: []
                  , dependencies: []
                  })
        (Just o) -> pure o

-- | Add a triple to the index. The object value of the triple should comply to the type of the ResourceIndex!
-- | Will add an entry for the Resource if it is not yet present.
addTriple :: forall e.
  Resource ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  Eff (gm :: GLOBALMAP | e) Triple
addTriple rid pid val deps =
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

-- runTripleGetter :: forall e ef.
--   NamedFunction (TripleGetter ef) ->
--   Maybe Resource ->
--   AsyncPropDefsM e (ef String)
-- runTripleGetter (NamedFunction _ tg) mr = bind (tg mr) (\(Triple{object}) -> pure object)
