module Perspectives.QueryCombinators where

import Control.Monad.Trans.Class (lift)
import Data.Array (cons, difference, elemIndex, findIndex, foldr, head, intersect, last, null, singleton, filter, union) as Arr
import Data.HeytingAlgebra (not, conj, disj, implies) as HA
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), applyTypedTripleGetterToMaybeObject, putQueryVariable, readQueryVariable, type (**>))
import Perspectives.PerspectivesTypes (PBool(..), typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, lookupInTripleIndex, memorize, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.TripleGetterFromObjectGetter (trackedAs, tripleGetterFromTripleGetter)
import Prelude (class Eq, class Show, bind, const, discard, eq, flip, identity, map, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=))
import Type.Data.Boolean (kind Boolean)
import Unsafe.Coerce (unsafeCoerce)

-- | Return the last element in the chain
-- | `psp:SingularFunction -> psp:SingularFunction`
closure' :: forall o.
  Eq o =>
  (o **> o) ->
  (o **> o)
closure' (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array o -> o -> MonadPerspectivesQuery (Triple o o)
    getter cumulator id = do
      t@(Triple{object : objectsOfP}) <- p id
      case Arr.elemIndex id cumulator of
        Nothing -> do
          case Arr.head objectsOfP of
            Nothing -> pure t
            (Just o) -> do
              pt@(Triple{object:bottom}) <- getter (Arr.cons o cumulator) o
              case Arr.head bottom of
                Nothing -> pure t
                otherwise ->
                  pure $ Triple { subject: id
                                , predicate : name
                                , object : bottom
                                , dependencies : []
                                , supports : [typeWithPerspectivesTypes getRef pt]
                                , tripleGetter : getter cumulator}
        otherwise -> pure t

    name :: String
    name = "closure('" <>  nameOfp <> ")"

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe identity Arr.cons

-- | A selection of the results of the second query using the first (boolean) query as a criterium.
-- | `psp:Constraint -> psp:Function -> psp:Function`
filter :: forall s o.
  (o **> PBool) ->
  (s **> o) ->
  (s **> o)
filter (TypedTripleGetter nameOfc criterium) (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: TripleGetter s o
    getter id = do
      t@(Triple{object}) <- p id
      (triples :: Array (Triple o PBool)) <- traverse criterium object
      (objects :: Array o) <- pure $ Arr.foldr addSubjectIfTrue [] triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : Arr.cons (typeWithPerspectivesTypes getRef t)
                      (map (typeWithPerspectivesTypes getRef) triples)
                    , tripleGetter : getter}

    addSubjectIfTrue :: Triple o PBool -> Array o -> Array o
    addSubjectIfTrue (Triple{subject, object}) arr = case Arr.elemIndex (PBool "true") object of
      Nothing -> arr
      _ -> Arr.cons subject arr

    name :: String
    name = "filter(" <> nameOfc <> ", " <> nameOfp <> ")"

-- | A selection of the results of the query using a simple (boolean) function as a criterium.
-- Test.Perspectives.TripleGetterConstructors, via getUnqualifiedRolDefinition
filter_ :: forall s o.
  (o -> Boolean) ->
  String ->
  (s **> o) ->
  (s **> o)
filter_ criterium criteriumName (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: TripleGetter s o
    getter id = do
      t@(Triple{object}) <- p id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : Arr.filter criterium object
                    , dependencies : []
                    , supports : [(typeWithPerspectivesTypes getRef t)]
                    , tripleGetter : getter}
    name :: String
    name = "(filter_" <> nameOfp <> "_" <> criteriumName <> ")"

-- Returns true iff the results of both TripleGetters applied to the same origin yield exactly the same values in the same order.
-- | `psp:Function -> psp:Function -> psp:Function`
equal :: forall s o.
  Eq o =>
  (s **> o) ->
  (s **> o) ->
  (s **> PBool)
equal (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s PBool
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : [PBool $ show $ ps == qs]
                    , dependencies : []
                    , supports : map (typeWithPerspectivesTypes getRef) [pt, qt]
                    , tripleGetter : typeWithPerspectivesTypes getter}

    name = "(equal " <> nameOfp <> " " <> nameOfq <> ")"

cond :: forall s o.
  (s **> PBool) ->
  (s **> o) ->
  (s **> o) ->
  (s **> o)
cond cd@(TypedTripleGetter nameOfCondition condition) (TypedTripleGetter nameOfThenPart thenPart) (TypedTripleGetter nameOfElsePart elsePart) = memorize getter name where

  name :: String
  name = "(cond(" <> nameOfCondition <> ", " <> nameOfThenPart <> ", " <> nameOfElsePart <> ")"

  getter :: TripleGetter s o
  getter id = do
    c@(Triple{object : cs}) <- condition id
    case Arr.head cs of
      Just (PBool "true") -> do
        (Triple{object, supports}) <- thenPart id
        pure $ Triple { subject: id
                      , predicate : name
                      , object : object
                      , dependencies : []
                      , supports : Arr.cons (getRef c) supports
                      , tripleGetter : typeWithPerspectivesTypes getter}
      otherwise -> do
        (Triple{object, supports}) <- elsePart id
        pure $ Triple { subject: id
                      , predicate : name
                      , object : object
                      , dependencies : []
                      , supports : Arr.cons (getRef c) supports
                      , tripleGetter : typeWithPerspectivesTypes getter}

-- Applies the logical binary operator (such as OR, AND and IMPLIES) to the results of two queries applied to the same origin.
logicalBinaryOperator :: forall s.
  String ->
  (Boolean -> Boolean -> Boolean) ->
  (s **> PBool) ->
  (s **> PBool) ->
  ((s **> PBool))
logicalBinaryOperator n op (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s PBool
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : fromBool $ op (toBool ps) (toBool qs)
                    , dependencies : []
                    , supports : map (typeWithPerspectivesTypes getRef) [pt, qt]
                    , tripleGetter : typeWithPerspectivesTypes getter}
    name :: String
    name = n <> "(" <> nameOfp <> " " <> nameOfq <> ")"

    fromBool :: Boolean -> Array PBool
    fromBool = Arr.singleton <<< PBool <<< show

    toBool :: Array PBool -> Boolean
    toBool s = maybe false ((==) (PBool "true")) (Arr.head s)

conj :: forall s. (s **> PBool) -> (s **> PBool) -> ((s **> PBool))
conj = logicalBinaryOperator "conj" HA.conj

disj :: forall s. (s **> PBool) -> (s **> PBool) -> ((s **> PBool))
disj = logicalBinaryOperator "disj" HA.disj

implies :: forall s. (s **> PBool) -> (s **> PBool) -> ((s **> PBool))
implies = logicalBinaryOperator "implies" HA.implies

-- A set operation applied to the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
setOperation :: forall s o. Eq o =>
  (Array o -> Array o -> Array o) ->
  (s **> o) ->
  (s **> o) ->
  (s **> o)
setOperation op (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name where
    getter :: TripleGetter s o
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (op ps qs)
                    , dependencies : []
                    , supports : map (typeWithPerspectivesTypes getRef) [pt, qt]
                    , tripleGetter : getter}
    name = "intersect(" <> nameOfp <> ", " <> nameOfq <> ")"

intersect :: forall s o. Eq o =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
intersect = setOperation Arr.intersect

difference :: forall s o. Eq o =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
difference = setOperation Arr.difference

union :: forall s o. Eq o =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
union = setOperation Arr.union

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isSomething :: forall s o. Triple s o -> MonadPerspectivesQuery (Triple s PBool)
isSomething (Triple r@{subject, predicate, object, dependencies, supports, tripleGetter}) = pure $ Triple
  { subject: subject
  , predicate: predicate
  , object: [PBool $ show (HA.not $ Arr.null object)]
  , dependencies: dependencies
  , supports: supports
  , tripleGetter: typeWithPerspectivesTypes tripleGetter
}

-- | A constraint constructed by checking whether the value of the query is empty.
-- | psp:Function -> psp:Constraint
notEmpty :: forall s o. (s **> o) -> (s **> PBool)
notEmpty (TypedTripleGetter nameOfp p) = memorize getter name where

  getter :: TripleGetter s PBool
  getter = p >=> isSomething >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "notEmpty(" <> nameOfp <> ")"

-- | Construct a function that returns a bool in MonadPerspectivesQuery, from a TypedTripleGetter.
toBoolean :: forall s. (s **> PBool) -> s -> MonadPerspectivesQuery Boolean
toBoolean tg = flip applyTypedTripleGetterToMaybeObject tg >=> pure <<< maybe false (eq (PBool "true"))

-- | This query constructor takes an argument that can be an PerspectEntiteit id or a simpleValue, and returns
-- | a triple whose object is boolean value.
contains :: forall s o.
  Eq o => Show s =>
  o ->
  TypedTripleGetter s o ->
  TypedTripleGetter s PBool
-- Test.Perspectives.TripleGetterConstructors
contains id' tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("contains(" <> typeWithPerspectivesTypes id' <> ")") f where
  f :: Array o -> s -> Array PBool
  f os _ = do
    case Arr.elemIndex id' os of
      Nothing -> [PBool "false"]
      otherwise -> [PBool "true"]

containsMatching :: forall s o. Show s => (s -> o -> Boolean) -> String -> TypedTripleGetter s o -> TypedTripleGetter s PBool
containsMatching criterium criteriumName tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containsMatching(" <> criteriumName <> ")") f where
  f :: Array o -> s -> Array PBool
  f os subject = maybe [PBool "true"] (const [PBool "false"]) (Arr.findIndex (criterium subject) os)

-- | Apply to a query and retrieve a boolean query that returns true iff its subject occurs in its result.
-- | `psp:Function -> psp:Constraint`
containedIn :: forall o. Eq o => Show o => (o **> o) -> (o **> PBool)
containedIn tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containedIn(" <> nameOfp <> ")") f where
  f :: Array o -> o -> Array PBool
  f os id = do
    case Arr.elemIndex id os of
      Nothing -> [PBool "false"]
      otherwise -> [PBool "true"]

-- | The logical negation of a Constraint.
-- | `psp:Constraint -> psp:Constraint`
not :: forall s. (s **> PBool) -> (s **> PBool)
not tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("not(" <> nameOfp <> ")") f where
  f :: Array PBool -> s -> Array PBool
  f object _ = case Arr.head object of
      (Just (PBool "true")) -> [PBool "false"]
      otherwise -> [PBool "true"] -- NOTE: type checking guarantees we only have two values.

not' :: forall s.  Show s => (s **> String) -> (s **> String)
not' tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("not(" <> nameOfp <> ")") f where
  f :: Array String -> s -> Array String
  f object _ = case Arr.head object of
      (Just "true") -> ["false"]
      otherwise -> ["true"] -- NOTE: type checking guarantees we only have two values.

-- | Turn a query of many arguments into a query of a single element.
-- | The selected element depends on the ordering returned by the query.
-- | `psp:Function -> psp:SingularFunction`
lastElement :: forall s o.  Show s => (s **> o) -> (s **> o)
lastElement tg@(TypedTripleGetter nameOfp (p :: TripleGetter s o)) = tripleGetterFromTripleGetter tg
  ("lastElement(" <> nameOfp <> ")") f where
    f :: Array o -> s -> Array o
    f os _ = maybe [] Arr.singleton (Arr.last os)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
-- | The resulting query returns exactly the same result as the argument query.
-- | `psp:Function -> psp:Function`
ignoreCache :: forall s o. (s **> o) -> (s **> o)
ignoreCache (TypedTripleGetter nameOfp p) = TypedTripleGetter ("ignore(" <> nameOfp <> ")") go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults false
      result <- p r
      setMemorizeQueryResults remember
      pure result


-- | Use the cache of query results for the given named function.
-- | The resulting query returns exactly the same result as the argument query.
-- | `psp:Function -> psp:Function`
useCache :: forall s o. (s **> o) -> (s **> o)
useCache (TypedTripleGetter nameOfp p) = TypedTripleGetter ("useCache(" <> nameOfp <> ")") go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result

constant :: forall s. Show s => s -> TypedTripleGetter s s
constant subject = (\_ -> pure [subject]) `trackedAs` ("constant()" <> unsafeCoerce subject <> ")")

-----------------------------------------------------------
-- VARIABLES
-----------------------------------------------------------
-- | Save the query result under the given name in the query execution environment for future use.
-- | The resulting query returns exactly the same result as the argument query.
-- | `String -> psp:Function -> psp:Function`
var :: forall s o. String -> (s **> o) -> (s **> o)
var name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("var(" <> nameOfp <> ")") go where
  go subject = do
    r <- p subject
    putQueryVariable name $ (typeWithPerspectivesTypes getRef) r
    pure r

-- | Retrieve the result stored in the query environment under the given name.
-- | Returns exactly the same value as the query used to store the value.
-- | `String -> psp:Function`
ref :: forall s o. String -> TypedTripleGetter s o
ref name = TypedTripleGetter ("ref(" <> name <> ")") (ref' name)

ref' :: forall s o. String -> s -> MonadPerspectivesQuery (Triple s o)
ref' name ignore = readQueryVariable name >>= \(TripleRef{subject, predicate}) ->
    do
      mref <- lift $ liftEffect $ lookupInTripleIndex subject predicate
      unsafePartial $ pure $ fromJust $ mref

-- | Save the query variable value and restore it after computing the query.
-- | Use this combinator to protect a query variable value.
-- | `String -> psp:Function -> psp:Function`
saveVar :: forall s o. String -> (s **> o) -> (s **> o)
saveVar name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("saveVar(" <> name <> ", " <> nameOfp <> ")") go where
  go subject = do
    variableValue <- readQueryVariable name
    r <- p subject
    putQueryVariable name variableValue
    pure r
