module Perspectives.TripleAdministration1 where

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives, TrackedObjects(..), TripleRef(..), ObjectsGetter, tripleRef)
import Perspectives.EntiteitAndRDFAliases (Predicate, Subject, ID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, delete, new, peek, poke)
import Perspectives.ObjectGetterLookup (lookupObjectsGetterByName, lookupObjectsGetterName)
import Perspectives.PerspectivesState (getsGlobalState, modifyGlobalState)
import Prelude (Unit, bind, discard, pure, unit, void, ($))

---------------------------------------------------------------------------------------
-- TRACKEDOBJECTSINDEX
---------------------------------------------------------------------------------------
-- | An index of Predicate-Object combinations, indexed by Subject.
type TrackedObjectsIndex = GLStrMap PredicateIndex

-- An index of objects indexed by Predicate (for a single Subject).
type PredicateIndex = GLStrMap TrackedObjects

-- | A global store of triples, indexed by Subject and Predicate.
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
trackedObjectsIndex :: TrackedObjectsIndex
trackedObjectsIndex = new unit

-- | Apply the objectsGetter and register the result if memorizeQueryResults == true.
withTracking :: forall e. ObjectsGetter e -> ObjectsGetter e
withTracking g id =
  case lookupObjectsGetterName g of
    -- No name, no tracking.
    Nothing -> g id
    (Just predicate) -> do
      remember <- memorizeQueryResults
      case remember of
        true -> do
          mto <- lift $ liftEff (lookupInTrackedObjectsIndex id predicate)
          case mto of
            Nothing -> do
              objects <- g id
              _ <- lift $ liftEff $ addToTrackedObjectsIndex id predicate objects [] []
              pure objects
            (Just (TrackedObjects{objects})) -> pure objects
        false -> g id

-- | Obtain the getter used to compute a particular tracked set of objects.
trackedObjectsGetter :: forall e. TrackedObjects -> Maybe (ObjectsGetter e)
trackedObjectsGetter (TrackedObjects{predicate}) = lookupObjectsGetterByName predicate

---------------------------------------------------------------------------------------
-- MEMORIZE OR NOT
---------------------------------------------------------------------------------------
-- | If memorizeQueryResults == true, we will look up a result in the tracked objects
-- | cache before computing it.
memorizeQueryResults :: forall e. MonadPerspectives e Boolean
memorizeQueryResults = getsGlobalState _.memorizeQueryResults

-- | If true, memorize results from now on.
setMemorizeQueryResults :: forall e. Boolean -> MonadPerspectives e Unit
setMemorizeQueryResults b = modifyGlobalState \ps -> ps {memorizeQueryResults = b}

---------------------------------------------------------------------------------------
-- LOOKUP, ADD
---------------------------------------------------------------------------------------
-- | From a resource ID and a predicate (name for an ObjectsGetter), find
-- | TrackedObjects.
lookupInTrackedObjectsIndex :: forall e. Subject -> Predicate -> Eff (gm :: GLOBALMAP | e) (Maybe TrackedObjects)
lookupInTrackedObjectsIndex rid pid = do
  preds <- peek trackedObjectsIndex rid
  case preds of
    Nothing ->
      pure Nothing
    (Just p) -> do
      objls <- peek p pid
      case objls of
        Nothing ->
          pure Nothing
        (Just o) -> pure (Just o)

getTriple :: forall e. TripleRef -> Eff (gm :: GLOBALMAP | e) (Maybe TrackedObjects)
getTriple (TripleRef{subject, predicate}) = lookupInTrackedObjectsIndex subject predicate

-- | Construct TrackedObjects and add it to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
addToTrackedObjectsIndex :: forall e.
  ID ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  Array TripleRef ->
  Eff (gm :: GLOBALMAP | e) TrackedObjects
addToTrackedObjectsIndex rid getterName objects deps sups = do
  (m :: PredicateIndex) <- ensureResource rid
  to <- pure (TrackedObjects
              { predicate: getterName
              , objects: objects
              , dependencies: deps
              , supports : sups
              })
  predIndex <- poke m getterName to
  _ <- foreachE sups (addDependency (tripleRef rid getterName))
  pure to

-- | Add the TrackedObjects to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
registerTrackedObjects :: forall e. TrackedObjects -> Subject -> Predicate -> Eff (gm :: GLOBALMAP | e) TrackedObjects
registerTrackedObjects to@(TrackedObjects{supports}) subject predicate = do
  (m :: PredicateIndex) <- ensureResource subject
  predIndex <- poke m predicate to
  _ <- foreachE supports (addDependency (tripleRef subject predicate))
  pure to

-- | Remove the TrackedObjects identified by the reference from the index (removes the dependency from its supports, too)
unRegisterTrackedObjects :: forall e1. TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
unRegisterTrackedObjects (TripleRef{subject, predicate}) = do
  preds <- peek trackedObjectsIndex subject
  case preds of
    Nothing ->
      pure unit
    (Just (p :: PredicateIndex)) -> do
      objls <- peek p predicate
      case objls of
        Nothing ->
          pure unit
        (Just t@(TrackedObjects{supports})) -> do
          void $ delete p predicate
          foreachE supports (removeDependency (tripleRef subject predicate))

-- | Make sure an entry for the given resource identifier is in the trackedObjectsIndex. Return the PredicateIndex for the
-- | resource.
ensureResource :: forall e. Subject -> Eff (gm :: GLOBALMAP | e) PredicateIndex
ensureResource rid = do
  pid <- peek trackedObjectsIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex) <- pure (new unit)
        _ <- poke trackedObjectsIndex rid m
        pure m
    (Just m) -> pure m

-- | Add the reference to the triple.
foreign import addDependency_ :: forall e1 e2. TrackedObjects -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef

-- | Remove the reference from the triple.
foreign import removeDependency_ :: forall e1 e2. TrackedObjects -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef
foreign import setSupports_ ::  forall e1 e2. TrackedObjects -> Array TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit

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
