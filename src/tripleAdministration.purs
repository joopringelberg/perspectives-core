module Perspectives.TripleAdministration where

import Perspectives.ObjectCollection
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Property (AsyncPropDefsM, PropertyName, getGetter)
import Perspectives.ResourceTypes (Resource)
import Prelude (class Functor, bind, pure, unit)

type Predicate = String

newtype Triple ef = Triple
  { subject :: Resource
  , predicate :: Predicate
  , object :: ef String
  , dependencies :: Array TripleRef}

newtype TripleRef = TripleRef { subject :: Resource, predicate :: Predicate}

-- | A global index of Triples, indexed by Resource.
type ResourceIndex ef = GLStrMap (PredicateIndex ef)

tripleIndex :: forall ef. ObjectCollection ef => ResourceIndex ef
tripleIndex = new unit

-- | The global store of triples, indexed by Resource and Predicate.

-- A store of objects indexed by Predicate.
type PredicateIndex ef = GLStrMap (Triple ef)

data NamedFunction f = NamedFunction String f

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

type TripleGetter e ef = Maybe Resource -> AsyncPropDefsM e (Triple ef)

-- | Use this function to construct property getters that memorize in the triple administration.
constructTripleGetter :: forall e ef. ObjectCollection ef =>
  PropertyName ->
  NamedFunction (TripleGetter e ef)
constructTripleGetter pn = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e ef
  tripleGetter res@(Just id) = do
    t@(Triple{object} :: Triple ef) <- liftEff (lookup tripleIndex id pn)
    case isEmpty object of
      true -> do
        (object' :: ef String) <- getGetter pn res
        liftEff (addTriple id pn object' [] [])
      false -> pure t
  tripleGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: empty
          , dependencies: []
          })

lookup :: forall e ef. ObjectCollection ef => ResourceIndex ef -> Resource -> Predicate -> Eff (gm :: GLOBALMAP | e) (Triple ef)
lookup index rid pid = do
  preds <- peek index rid
  case preds of
    Nothing ->
      pure (Triple{ subject: rid
              , predicate: pid
              , object: empty
              , dependencies: []
              })
    (Just p) -> do
      objls <- peek p pid
      case objls of
        Nothing ->
          pure (Triple{ subject: rid
                  , predicate: pid
                  , object: empty
                  , dependencies: []
                  })
        (Just o) -> pure o

-- | Add a triple to the index. The object value of the triple should comply to the type of the ResourceIndex!
-- | Will add an entry for the Resource if it is not yet present.
addTriple :: forall e ef. ObjectCollection ef =>
  Resource ->
  Predicate ->
  (ef String) ->
  Array TripleRef ->
  Array TripleRef ->
  Eff (gm :: GLOBALMAP | e) (Triple ef)
addTriple rid pid val sups deps =
    do
      (m :: PredicateIndex ef) <- ensureResource rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                })
      predIndex <- poke m pid triple
      _ <- poke tripleIndex rid predIndex
      pure triple

ensureResource :: forall e ef. ObjectCollection ef => Resource -> Eff (gm :: GLOBALMAP | e) (PredicateIndex ef)
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex ef) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

foreign import addDependency :: forall e ef. Triple ef -> TripleRef -> Eff (gm :: GLOBALMAP | e) TripleRef

-- runTripleGetter :: forall e ef.
--   NamedFunction (TripleGetter ef) ->
--   Maybe Resource ->
--   AsyncPropDefsM e (ef String)
-- runTripleGetter (NamedFunction _ tg) mr = bind (tg mr) (\(Triple{object}) -> pure object)
