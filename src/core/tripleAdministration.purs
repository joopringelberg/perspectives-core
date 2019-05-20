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
  , removeDependency
  , addDependency
  , setSupports_
  , detectCycles
  , lookupSubject
  )
  where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.State (lift)
import Data.Array (cons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect, foreachE)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..))
import Perspectives.EntiteitAndRDFAliases (Predicate, Subject)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, delete, new, peek, poke)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (==), (<>), show, (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | If memorizeQueryResults == true, we will look up a result in the triple cache
-- | before computing it.
memorizeQueryResults :: MonadPerspectivesQuery Boolean
memorizeQueryResults = lift $ gets _.memorizeQueryResults

setMemorizeQueryResults :: Boolean -> MonadPerspectivesQuery Unit
setMemorizeQueryResults b = lift $ modify \ps -> ps {memorizeQueryResults = b}

getRef :: forall s o. Triple s o -> TripleRef
getRef = unsafeCoerce

-- | An index of Predicate-Object combinations, indexed by Subject.
type TripleIndex = GLStrMap PredicateIndex

-- An index of objects indexed by Predicate (for a single Subject).
type PredicateIndex = GLStrMap (Triple String String)

-- | A global store of triples, indexed by Subject and Predicate.
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
tripleIndex :: TripleIndex
tripleIndex = new unit

lookupSubject :: Subject -> Effect (Maybe PredicateIndex)
lookupSubject = peek tripleIndex

lookupInTripleIndex :: forall s o. Subject -> Predicate -> Effect (Maybe (Triple s o))
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
        (Just o) -> pure (Just (typeWithPerspectivesTypes o))

getTriple :: TripleRef -> Effect (Maybe (Triple String String))
getTriple (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

-- | Construct a triple and add it to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
addToTripleIndex ::
  Subject ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  Array TripleRef ->
  TripleGetter String String ->
  Effect (Triple String String)
addToTripleIndex rid pid val deps sups tripleGetter =
    do
      (m :: PredicateIndex) <- ensureResource rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                , supports : sups
                , tripleGetter: tripleGetter
                })
      predIndex <- poke m pid triple
      _ <- foreachE sups (addDependency (getRef triple))
      pure triple

-- | Add the triple to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
registerTriple :: Triple String String -> Effect (Triple String String)
registerTriple triple@(Triple{subject, predicate, supports}) = do
  (m :: PredicateIndex) <- ensureResource subject
  predIndex <- poke m predicate triple
  _ <- foreachE supports (addDependency (getRef triple))
  pure triple

-- | Remove the triple identified by the reference from the index (removes the dependency from its supports, too)
unRegisterTriple :: TripleRef -> Effect Unit
unRegisterTriple (TripleRef{subject, predicate}) = do
  preds <- peek tripleIndex subject
  case preds of
    Nothing ->
      pure unit
    (Just (p :: PredicateIndex)) -> do
      objls <- peek p predicate
      case objls of
        Nothing ->
          pure unit
        (Just t@(Triple{supports})) -> do
          void $ delete p predicate
          foreachE supports (removeDependency (getRef t))

-- | Make sure an entry for the given resource identifier is in the tripleIndex. Return the PredicateIndex for the
-- | resource.
ensureResource :: Subject -> Effect (PredicateIndex)
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

memorize :: forall s o. TripleGetter s o -> String -> TypedTripleGetter s o
memorize getter name = TypedTripleGetter name
  \(id :: s) -> do
    remember <- memorizeQueryResults
    case remember of
      true -> do
        mt <- lift $ liftEffect (lookupInTripleIndex (typeWithPerspectivesTypes id) name)
        case mt of
          Nothing -> do
            t <- getter id
            (stringTriple :: Triple String String) <- pure $ typeWithPerspectivesTypes t
            _ <- lift $ liftEffect $ registerTriple stringTriple
            pure t
          (Just t) -> pure $ typeWithPerspectivesTypes t
      false -> getter id

-- | Add the reference to the triple.
foreign import addDependency_ :: Triple String String -> TripleRef -> Effect TripleRef

-- | Remove the reference from the triple.
foreign import removeDependency_ :: Triple String String -> TripleRef -> Effect TripleRef
foreign import setSupports_ ::  Triple String String -> Array TripleRef -> Effect Unit

-- | Add the dependentRef (first argument) as a dependency to the triple identified by the supportingRef (second argument).
addDependency :: TripleRef -> TripleRef -> Effect Unit
addDependency dependentRef supportingRef = do
  ms <- getTriple supportingRef
  case ms of
    (Just support) -> void (addDependency_ support dependentRef)
    Nothing -> pure unit

detectCycles :: TripleRef -> Aff Unit
detectCycles source = getDependents source >>= detectCycles' source [source]

detectCycles' :: TripleRef ->  Array TripleRef -> Array TripleRef -> Aff Unit
detectCycles' source path dependents =
  for_ dependents
    \dep -> if (dep == source)
      then throwError (error $ "Cycle in dependencies: " <> show (cons dep path))
      else getDependents dep >>= detectCycles' source (cons dep path)

getDependents :: TripleRef -> Aff (Array TripleRef)
getDependents tr = do
  ms <- liftEffect $ getTriple tr
  case ms of
    (Just (Triple{dependencies})) -> pure dependencies
    Nothing -> pure []

-- | Remove the dependentRef (first argument) as a dependency from the triple identified by the supportingRef (second argument).
removeDependency :: TripleRef -> TripleRef -> Effect Unit
removeDependency dependentRef supportingRef = do
  ms <- getTriple supportingRef
  case ms of
    (Just support) -> void (removeDependency_ support dependentRef)
    Nothing -> pure unit
