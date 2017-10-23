module Perspectives.TripleAdministration where

import Perspectives.ObjectCollection
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, toBoolean, toNumber, toString)
import Data.Array (head, null)
import Data.Eq (class Eq)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Property (AsyncPropDefsM, PropertyName, getGetter)
import Perspectives.ResourceTypes (Resource(..), ResourceId)
import Prelude (class Functor, bind, id, pure, unit)

type PredicateId = String

newtype Triple a = Triple
  { subject :: ResourceId
  , predicate :: PredicateId
  , object :: a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

class TypedObjectCollection ef a where
  tripleStore :: ObjectCollection ef => Lazy (GLStrMap (GLStrMap (Triple (ef a))))
  lookup1 :: forall e. ObjectCollection ef =>
    ResourceId ->
    PredicateId ->
    Eff (gm :: GLOBALMAP | e) (Triple (ef a))
  addTriple1 :: forall e. ObjectCollection ef =>
    ResourceId ->
    PredicateId ->
    (ef a) ->
    Array TripleRef ->
    Array TripleRef ->
    Eff (gm :: GLOBALMAP | e) (Triple (ef a))
  constructTripleGetter1 :: forall e. ObjectCollection ef =>
    (Json -> Maybe a) ->
    PropertyName ->
    NamedFunction (Maybe Resource -> AsyncPropDefsM e (Triple (ef a)))
  -- sTos1 :: forall e. Eq a => NamedFunction (TripleGetter e (Maybe Resource)) -> NamedFunction (TripleGetter e (Maybe a)) -> NamedFunction (TripleGetter e (Maybe a))
  -- sTop1 :: forall e. Eq a => NamedFunction (TripleGetter e (Maybe Resource)) -> NamedFunction (TripleGetter e (Array a)) -> NamedFunction (TripleGetter e (Array a))
  -- pTop1 :: forall e. Eq a => NamedFunction (TripleGetter e (Array Resource)) -> NamedFunction (TripleGetter e (Array a)) -> NamedFunction (TripleGetter e (Array a))
  -- pTos1 :: forall e. Eq a => NamedFunction (TripleGetter e (Array Resource)) -> NamedFunction (TripleGetter e (Maybe a)) -> NamedFunction (TripleGetter e (Array a))

newtype TripleRef = TripleRef { subject :: ResourceId, predicate :: PredicateId}

-- | A global index of Triples, indexed by ResourceId.
type ResourceIndex a = GLStrMap (PredicateIndex a)

-- A store of objects indexed by PredicateId.
type PredicateIndex a = GLStrMap (Triple a)

lookup :: forall e a ef. ObjectCollection ef => ResourceIndex (ef a) -> ResourceId -> PredicateId -> Eff (gm :: GLOBALMAP | e) (Triple (ef a))
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
addTriple :: forall e a ef. ObjectCollection ef =>
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

ensureResource :: forall e a ef. ObjectCollection ef => ResourceIndex (ef a) -> ResourceId -> Eff (gm :: GLOBALMAP | e) (PredicateIndex (ef a))
ensureResource index rid = do
  pid <- peek index rid
  case pid of
    Nothing -> do
        (m :: PredicateIndex (ef a)) <- pure (new unit)
        _ <- poke index rid m
        pure m
    (Just m) -> pure m

foreign import addDependency :: forall e a. Triple (Maybe a) -> TripleRef -> Eff (gm :: GLOBALMAP | e) TripleRef

data NamedFunction f = NamedFunction String f

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

type TripleGetter e a = Maybe Resource -> AsyncPropDefsM e (Triple a)

runTripleGetter :: forall a e. NamedFunction (TripleGetter e a) -> Maybe Resource -> AsyncPropDefsM e a
runTripleGetter (NamedFunction _ tg) mr = bind (tg mr) (\(Triple{object}) -> pure object)

-- | Use this function to construct property getters that memorize in the triple administration.
constructTripleGetter :: forall a e ef. ObjectCollection ef =>
  ResourceIndex (ef a) ->
  (Json -> Maybe a) ->
  PropertyName ->
  NamedFunction (Maybe Resource -> AsyncPropDefsM e (Triple (ef a)))
constructTripleGetter tripleStore tofn pn = NamedFunction pn tripleGetter where
  tripleGetter ::  (Maybe Resource -> AsyncPropDefsM e (Triple (ef a)))
  tripleGetter res@(Just (Resource{id})) = do
    t@(Triple{object} :: Triple (ef a)) <- liftEff (lookup tripleStore id pn)
    case isEmpty object of
      true -> do
        (object' :: ef a) <- getGetter tofn pn res
        liftEff (addTriple tripleStore id pn object' [] [])
      false -> pure t
  tripleGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: empty
          , supports: []
          , dependencies: []
          })

instance singleIntCollection :: TypedObjectCollection Maybe Int where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

instance pluralIntCollection :: TypedObjectCollection Array Int where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

instance singleStringCollection :: TypedObjectCollection Maybe String where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

instance pluralStringCollection :: TypedObjectCollection Array String where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

instance singleNumberCollection :: TypedObjectCollection Maybe Number where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

instance pluralNumberCollection :: TypedObjectCollection Array Number where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

instance singleBooleanCollection :: TypedObjectCollection Maybe Boolean where
  tripleStore = defer (\_ -> new unit)
  lookup1 = lookup (force tripleStore)
  addTriple1 = addTriple (force tripleStore)
  constructTripleGetter1 = constructTripleGetter (force tripleStore)

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
