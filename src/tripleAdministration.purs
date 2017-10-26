module Perspectives.TripleAdministration where

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Property (Getter, PropertyName, PropDefsEffects, getGetter)
import Perspectives.ResourceTypes (Resource)
import Prelude (class Eq, Ordering(..), Unit, bind, conj, eq, pure, unit, void, (&&), (<<<), (<>), (==), (>>=))

type Predicate = String

newtype Triple = Triple
  { subject :: Resource
  , predicate :: Predicate
  , object :: Array String
  , dependencies :: Array TripleRef}

instance showTriple :: Show Triple where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: Eq Triple where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

newtype TripleRef = TripleRef { subject :: Resource, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

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

applyToNamedFunction :: forall a b. a -> NamedFunction (a -> b) -> b
applyToNamedFunction a (NamedFunction _ f)= f a

infix 0 applyToNamedFunction as ##

type TripleGetter e = Resource -> Aff (PropDefsEffects e) Triple

type NamedTripleGetter = forall e. NamedFunction (TripleGetter e)

constructTripleGetterFromArbitraryFunction :: forall e.
  PropertyName ->
  Getter ->
  NamedFunction (TripleGetter e)
constructTripleGetterFromArbitraryFunction pn getter = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    mt <- liftEff (lookupTriple id pn)
    case mt of
      Nothing -> do
        (object' :: Array String) <- getter id
        liftEff (addTriple id pn object' [])
      (Just t) -> pure t

-- | Use this function to construct property getters that memorize in the triple administration.
constructTripleGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructTripleGetter pn = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    mt <- liftEff (lookupTriple id pn)
    case mt of
      Nothing -> do
        (object' :: Array String) <- getGetter pn id
        liftEff (addTriple id pn object' [])
      (Just t) -> pure t

lookupTriple :: forall e. Resource -> Predicate -> Eff (gm :: GLOBALMAP | e) (Maybe Triple)
lookupTriple rid pid = do
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

registerTriple :: forall e. Triple -> Eff (gm :: GLOBALMAP | e) Triple
registerTriple (Triple{subject, predicate, object, dependencies}) = addTriple subject predicate object dependencies

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
