module Perspectives.TripleAdministration where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.State (lift)
import Data.Array (cons, null)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect, foreachE)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Foreign.Object (values)
import Perspectives.CoreTypes (MonadPerspectivesQuery, StringTriple, StringTripleGetter, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), StringTypedTripleGetter)
import Perspectives.EntiteitAndRDFAliases (Predicate, Subject)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, delete, new, peek, poke, clear)
import Perspectives.Representation.Resource (subjectString, Subject, Objects) as PRR
import Prelude (Unit, bind, discard, pure, unit, void, ($), (==), (<>), show, (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | If memorizeQueryResults == true, we will look up a result in the triple cache
-- | before computing it.
memorizeQueryResults :: MonadPerspectivesQuery Boolean
memorizeQueryResults = lift $ gets _.memorizeQueryResults

setMemorizeQueryResults :: Boolean -> MonadPerspectivesQuery Unit
setMemorizeQueryResults b = lift $ modify \ps -> ps {memorizeQueryResults = b}

getRef :: StringTriple -> TripleRef
getRef = unsafeCoerce

-- | An index of Predicate-Object combinations, indexed by Subject.
-- | These are untyped, or rather, String-typed.
type TripleIndex = GLStrMap PredicateIndex

-- An index of objects indexed by Predicate (for a single Subject).
type PredicateIndex = GLStrMap (Triple String String)

-- | A global store of triples, indexed by Subject and Predicate.
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
tripleIndex :: TripleIndex
tripleIndex = new unit

-- | A very destructive action! Only used for testing purposes.
clearTripleIndex :: Effect Unit
clearTripleIndex = void $ clear tripleIndex

lookupSubject :: Subject -> Effect (Maybe PredicateIndex)
lookupSubject = peek tripleIndex

lookupInTripleIndex :: Subject -> Predicate -> Effect (Maybe StringTriple)
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
        (Just o) -> pure (Just ( o))

getTriple :: TripleRef -> Effect (Maybe (Triple String String))
getTriple (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

-- | Construct a triple and add it to the index.
-- | Will add an entry for the Subject if it is not yet present.
-- | Adds a dependency to each of the supports.
addToTripleIndex ::
  PRR.Subject ->
  Predicate ->
  PRR.Objects ->
  Array TripleRef ->
  Array TripleRef ->
  TripleGetter Subject PRR.Objects ->
  Effect (Triple Subject PRR.Objects)
addToTripleIndex rid pid val deps sups tripleGetter =
    do
      (m :: PredicateIndex) <- ensureResource $ PRR.subjectString rid
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
-- | Recursively remove the supports if they have no dependencies left.
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
          foreachE supports
            \s -> do
              removeDependency (getRef t) s
              b <- (hasDependencies s)
              if b
                then (pure unit)
                else (unRegisterTriple s)

-- | Remove the triple and return all its dependencies.
unRegisterBasicTriple :: TripleRef -> Effect (Maybe (Triple String String))
unRegisterBasicTriple (TripleRef{subject, predicate}) = do
  preds <- peek tripleIndex (PRR.subjectString subject)
  case preds of
    Nothing ->
      pure Nothing
    (Just (p :: PredicateIndex)) -> do
      objls <- peek p predicate
      case objls of
        Nothing -> pure Nothing
        (Just t@(Triple{dependencies})) -> do
          void $ delete p predicate
          pure $ Just t

-- | Remove the subject and all its relations from the TripleIndex.
-- | Remove all triples with the given subject from the TripleIndex.
-- | Returns all removed triples.
unRegisterSubject :: Subject -> Effect (Array TripleRef)
unRegisterSubject subject = do
  preds <- peek tripleIndex subject
  case preds of
    Nothing ->
      pure []
    (Just (p :: PredicateIndex)) -> do
      void $ delete tripleIndex subject
      -- For all predicates, return all triples.
      pure $ values (unsafeCoerce p)

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

memorize :: forall s t. Newtype s String => TripleGetter s t -> String -> TypedTripleGetter s t
memorize getter name = TypedTripleGetter name
  \(id :: s) -> do
    remember <- memorizeQueryResults
    case remember of
      true -> do
        mt <- lift $ liftEffect (lookupInTripleIndex (unwrap id) name)
        case mt of
          Nothing -> do
            (t :: Triple s t) <- getter id
            (stringTriple :: Triple String String) <- pure $ unsafeCoerce t
            _ <- lift $ liftEffect $ registerTriple stringTriple
            pure t
          (Just t) -> pure $ unsafeCoerce t
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

hasDependencies :: TripleRef -> Effect Boolean
hasDependencies tr = do
  ms <- getTriple tr
  case ms of
    (Just (Triple{dependencies})) -> pure $ null dependencies
    Nothing -> pure false

-- | Remove the dependentRef (first argument) as a dependency from the triple identified by the supportingRef (second argument).
removeDependency :: TripleRef -> TripleRef -> Effect Unit
removeDependency dependentRef supportingRef = do
  ms <- getTriple supportingRef
  case ms of
    (Just support) -> void (removeDependency_ support dependentRef)
    Nothing -> pure unit
