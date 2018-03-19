module Perspectives.TripleAdministration where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (StateT, lift)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.StrMap (StrMap)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, delete, new, peek, poke)
import Perspectives.PerspectivesState (MonadPerspectives, getsGlobalState, modifyGlobalState)
import Prelude (class Eq, Unit, bind, discard, pure, unit, void, ($), (&&), (<>), (==))

-- | The QueryEnvironment is a set of bindings of variableNames (Strings) to either ID's or the string representation of simple values.
type QueryEnvironment = StrMap (Array String)

type MonadPerspectivesQuery e =  StateT QueryEnvironment (MonadPerspectives e)

type TripleGetter e = Subject -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)

data NamedFunction f = NamedFunction String f

newtype Triple e = Triple
  { subject :: Subject
  , predicate :: Predicate
  , object :: Array Value
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter e}

tripleObjects :: forall e. Triple e -> Array String
tripleObjects (Triple{object}) = object

type MonadPerspectivesObjects e = MonadPerspectives (AjaxAvarCache e) (Array ID)

-- | If memorizeQueryResults == true, we will look up a result in the triple cache
-- | before computing it.
memorizeQueryResults :: forall e. MonadPerspectivesQuery e Boolean
memorizeQueryResults = lift $ getsGlobalState _.memorizeQueryResults

setMemorizeQueryResults :: forall e. Boolean -> MonadPerspectivesQuery e Unit
setMemorizeQueryResults b = lift $ modifyGlobalState \ps -> ps {memorizeQueryResults = b}


tripleObjects_  :: forall e. Triple e -> MonadPerspectives (AjaxAvarCache e) (Array ID)
tripleObjects_ (Triple{object}) = pure object

instance showTriple :: Show (Triple e) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: Eq (Triple e) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

newtype TripleRef = TripleRef { subject :: Subject, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

getRef :: forall e. Triple e -> TripleRef
getRef (Triple{subject, predicate}) = TripleRef{subject: subject, predicate: predicate}

-- | An index of Predicate-Object combinations, indexed by Subject.
type TripleIndex e = GLStrMap (PredicateIndex e)

-- An index of objects indexed by Predicate (for a single Subject).
type PredicateIndex e = GLStrMap (Triple e)

-- | A global store of triples, indexed by Subject and Predicate.
tripleIndex :: forall e. TripleIndex e
tripleIndex = new unit

lookupInTripleIndex :: forall e1 e2. Subject -> Predicate -> Eff (gm :: GLOBALMAP | e1) (Maybe (Triple e2))
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

getTriple :: forall e1 e2. TripleRef -> Eff (gm :: GLOBALMAP | e1) (Maybe (Triple e2))
getTriple (TripleRef{subject, predicate}) = lookupInTripleIndex subject predicate

-- | Add a triple to the index. The object value of the triple should comply to the type of the TripleIndex!
-- | Will add an entry for the Subject if it is not yet present.
addToTripleIndex :: forall e1 e2.
  Subject ->
  Predicate ->
  (Array String) ->
  Array TripleRef ->
  Array TripleRef ->
  TripleGetter e2 ->
  Eff (gm :: GLOBALMAP | e1) (Triple e2)
addToTripleIndex rid pid val deps sups tripleGetter =
    do
      (m :: PredicateIndex e2) <- ensureResource rid
      triple <- pure (Triple{ subject: rid
                , predicate: pid
                , object: val
                , dependencies: deps
                , supports : sups
                , tripleGetter: tripleGetter
                })
      predIndex <- poke m pid triple
      -- _ <- poke tripleIndex rid predIndex
      _ <- foreachE sups (addDependency (getRef triple))
      pure triple

-- | Remove the triple identified by the reference from the index (removes the dependency from its supports, too)
unRegisterTriple :: forall e1. TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit
unRegisterTriple (TripleRef{subject, predicate}) = do
  preds <- peek tripleIndex subject
  case preds of
    Nothing ->
      pure unit
    (Just (p :: PredicateIndex e1)) -> do
      objls <- peek p predicate
      case objls of
        Nothing ->
          pure unit
        (Just t@(Triple{supports})) -> do
          void $ delete p predicate
          foreachE supports (removeDependency (getRef t))

registerTriple :: forall e1 e2. Triple e2 -> Eff (gm :: GLOBALMAP | e1) (Triple e2)
registerTriple (Triple{subject, predicate, object, dependencies, supports, tripleGetter}) = addToTripleIndex subject predicate object dependencies supports tripleGetter

-- | Make sure an entry for the given resource identifier is in the tripleIndex. Return the PredicateIndex for the
-- | resource.
ensureResource :: forall e1 e2. Subject -> Eff (gm :: GLOBALMAP | e1) (PredicateIndex e2)
ensureResource rid = do
  pid <- peek tripleIndex rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex e2) <- pure (new unit)
        _ <- poke tripleIndex rid m
        pure m
    (Just m) -> pure m

memorize :: forall e. TripleGetter e -> String -> NamedFunction (TripleGetter e)
memorize getter name = NamedFunction name \id -> do
  remember <- memorizeQueryResults
  case remember of
    true -> do
      mt <- lift $ liftEff (lookupInTripleIndex id name)
      case mt of
        Nothing -> do
          t <- getter id
          lift $ liftEff $ registerTriple t
        (Just t) -> pure t
    false -> do
      t <- getter id
      lift $ liftEff $ registerTriple t

-- | Add the reference to the triple.
foreign import addDependency_ :: forall e1 e2. Triple e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef

-- | Remove the reference from the triple.
foreign import removeDependency_ :: forall e1 e2. Triple e2 -> TripleRef -> Eff (gm :: GLOBALMAP | e1) TripleRef
foreign import setSupports_ ::  forall e1 e2. Triple e2 -> Array TripleRef -> Eff (gm :: GLOBALMAP | e1) Unit

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
