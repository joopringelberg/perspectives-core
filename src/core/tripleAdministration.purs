module Perspectives.TripleAdministration
  ( memorizeQueryResults
  , setMemorizeQueryResults
  , getRef
  , lookupInTripleIndex
  , getTriple
  , addToTripleIndex
  , registerTriple
  , unRegisterTriple
  , memorize
  , removeDependency_
  , setSupports_
  )
  where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, delete, new, peek, poke)
import Prelude (Unit, bind, discard, pure, unit, void, ($))
import Unsafe.Coerce (unsafeCoerce)

-- | If memorizeQueryResults == true, we will look up a result in the triple cache
-- | before computing it.
memorizeQueryResults :: forall e. MonadPerspectivesQuery (avar :: AVAR | e) Boolean
memorizeQueryResults = lift $ gets _.memorizeQueryResults

setMemorizeQueryResults :: forall e. Boolean -> MonadPerspectivesQuery (avar :: AVAR | e) Unit
setMemorizeQueryResults b = lift $ modify \ps -> ps {memorizeQueryResults = b}

getRef :: forall s p o e. Triple s p o e -> TripleRef
getRef = unsafeCoerce

-- | An index of Triples.
-- | They are indexed by Subject and Predicate - in other words, by two Strings.
-- | Here we've dropped the typing of s and o in terms of Perspectives types.
type TripleIndex s p o e = GLStrMap (PredicateIndex s p o e)

-- An index of Triples indexed by Predicate (for a single Subject).
type PredicateIndex s p o e = GLStrMap (Triple s p o e)

-- | A global store of Triples, indexed by Subject and Predicate.
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
tripleIndex :: forall s p o e. TripleIndex s p o e
tripleIndex = new unit

-- | To look up a triple, we provide Strings representing a Subject and an Object.
-- | However, we do not type these two in terms of Perspectives types.
lookupInTripleIndex :: forall e1 s p o e2.
  String -> String -> Eff (gm :: GLOBALMAP | e1) (Maybe (Triple s p o e2))
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

getTriple :: forall e1 s p o e2.
  TripleRef -> Eff (gm :: GLOBALMAP | e1) (Maybe (Triple s p o e2))
getTriple (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

-- | Construct a triple and add it to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
addToTripleIndex :: forall e1 s p o e2.
  Newtype s String =>
  Newtype p String =>
  s ->
  p ->
  (Array o) ->
  Array TripleRef ->
  Array TripleRef ->
  TripleGetter s p o e2 ->
  Eff (gm :: GLOBALMAP | e1) (Triple s p o e2)
addToTripleIndex rid pid val deps sups tripleGetter =
    do
      (m :: PredicateIndex s p o e2) <- ensureResource (unwrap rid)
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                , supports : sups
                , tripleGetter: tripleGetter
                })
      predIndex <- poke m (unwrap pid) triple
      _ <- foreachE sups (addDependency (getRef triple))
      pure triple

-- | Add the triple to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
registerTriple :: forall e1 e2 s p o c r b.
  Newtype s String =>
  Newtype p String =>
  (Triple s p o e2) ->
  Eff (gm :: GLOBALMAP | e1) (Triple s p o e2)
registerTriple triple@(Triple{subject, predicate, supports}) = do
  (m :: PredicateIndex s p o e2) <- ensureResource (unwrap subject)
  predIndex <- poke m (unwrap predicate) triple
  _ <- foreachE supports (addDependency (getRef triple))
  pure triple

-- | Remove the triple identified by the reference from the index (removes the dependency from its supports, too)
unRegisterTriple :: forall s p o e1. TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
unRegisterTriple (TripleRef{subject, predicate}) = do
  preds <- peek tripleIndex subject
  case preds of
    Nothing ->
      pure unit
    (Just (p :: PredicateIndex s p o e1)) -> do
      objls <- peek p predicate
      case objls of
        Nothing ->
          pure unit
        (Just t@(Triple{supports})) -> do
          void $ delete p predicate
          foreachE supports (removeDependency (getRef t))

-- | Make sure an entry for the given resource identifier is in the tripleIndex. Return the PredicateIndex for the
-- | resource.
ensureResource :: forall e1 s p o e2. String -> Eff (gm :: GLOBALMAP | e1) (PredicateIndex s p o e2)
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex s p o e2) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

memorize :: forall s p o e.
  Newtype s String =>
  Newtype p String =>
  TripleGetter s p o e ->
  String ->
  TypedTripleGetter s p o e
memorize getter name = TypedTripleGetter name
  \(id :: s) -> do
    remember <- memorizeQueryResults
    case remember of
      true -> do
        mt <- lift $ liftEff (lookupInTripleIndex (unwrap id) name)
        case mt of
          Nothing -> do
            t <- getter id
            lift $ liftEff $ registerTriple t
          (Just t) -> pure t
      false -> getter id

-- | Add the reference to the triple.
foreign import addDependency_ :: forall e1 s p o e2. Triple s p o e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef

-- | Remove the reference from the triple.
foreign import removeDependency_ :: forall e1 s p o e2. Triple s p o e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef
foreign import setSupports_ ::  forall e1 s p o e2. Triple s p o e2 -> Array TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit

-- | Add the dependentRef (first argument) as a dependency to the triple identified by the supportingRef (second argument).
addDependency :: forall e1. TripleRef -> TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
addDependency dependentRef supportingRef = do
  ms <- getTriple supportingRef
  case ms of
    (Just support) -> void (addDependency_ support dependentRef)
    Nothing -> pure unit

-- | Remove the dependentRef (first argument) as a dependency from the triple identified by the supportingRef (second argument).
removeDependency :: forall e1. TripleRef -> TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
removeDependency dependentRef supportingRef = do
  ms <- getTriple supportingRef
  case ms of
    (Just support) -> void (removeDependency_ support dependentRef)
    Nothing -> pure unit
