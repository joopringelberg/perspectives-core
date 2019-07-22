module Perspectives.QueryCombinators where

import Control.Monad.Trans.Class (lift)
import Data.Array (cons, difference, elemIndex, findIndex, foldr, head, intersect, last, null, singleton, filter, union) as Arr
import Data.HeytingAlgebra (not, conj, disj, implies) as HA
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), StringTriple, applyTypedTripleGetterToMaybeObject, putQueryVariable, readQueryVariable, type (**>))
import Perspectives.Instances.Aliases (PBool)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.TripleAdministration (getRef, lookupInTripleIndex, memorize, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.TripleGetters.TrackedAs (trackedAs, tripleGetterFromTripleGetter)
import Prelude (bind, const, discard, eq, flip, identity, map, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=), class Eq)
import Type.Data.Boolean (kind Boolean)
import Unsafe.Coerce (unsafeCoerce)


-- | Return the last element in the chain
-- | `psp:SingularFunction -> psp:SingularFunction`
closure' :: forall s. Newtype s String => Eq s =>
  s **> s ->
  s **> s
closure' (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array s -> s -> MonadPerspectivesQuery (Triple s s)
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
                                , supports : [getRef $ unsafeCoerce pt]
                                , tripleGetter : getter cumulator}
        otherwise -> pure t

    name :: String
    name = "closure('" <>  nameOfp <> ")"

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe identity Arr.cons

-- | A selection of the results of the second query using the first (boolean) query as a criterium.
-- | `psp:Constraint -> psp:Function -> psp:Function`
filter :: forall s o. Newtype s String =>
  (o **> Value) ->
  (s **> o) ->
  (s **> o)
filter (TypedTripleGetter nameOfc criterium) (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: TripleGetter s o
    getter id = do
      t@(Triple{object}) <- p id
      (triples :: Array (Triple o Value)) <- traverse criterium object
      (objects :: Array o) <- pure $ unsafeCoerce $ Arr.foldr addSubjectIfTrue [] (unsafeCoerce triples :: Array StringTriple)
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : Arr.cons (getRef $ unsafeCoerce t)
                      (map getRef (unsafeCoerce triples))
                    , tripleGetter : getter}

    addSubjectIfTrue :: Triple String PBool -> Array String -> Array String
    addSubjectIfTrue (Triple{subject, object}) arr = case Arr.elemIndex ("true") object of
      Nothing -> arr
      _ -> Arr.cons subject arr

    name :: String
    name = "filter(" <> nameOfc <> ", " <> nameOfp <> ")"

-- | A selection of the results of the query using a simple (boolean) function as a criterium.
-- Test.Perspectives.TripleGetterConstructors, via getUnqualifiedRolDefinition
filter_ :: forall s o. Newtype s String =>
  (s -> Boolean) ->
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
                    , object : unsafeCoerce (Arr.filter criterium (unsafeCoerce object))
                    , dependencies : []
                    , supports : [( getRef $ unsafeCoerce t)]
                    , tripleGetter : getter}
    name :: String
    name = "(filter_" <> nameOfp <> "_" <> criteriumName <> ")"

-- Returns true iff the results of both TripleGetters applied to the same origin yield exactly the same values in the same order.
-- | `psp:Function -> psp:Function -> psp:Function`
equal :: forall s o. Newtype s String => Eq o =>
  (s **> o) ->
  (s **> o) ->
  (s **> Value)
equal (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s Value
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : [Value $ show $ ps == qs]
                    , dependencies : []
                    , supports : map (unsafeCoerce getRef) [pt, qt]
                    , tripleGetter :  getter}

    name = "(equal " <> nameOfp <> " " <> nameOfq <> ")"

cond :: forall s o. Newtype s String =>
  (s **> Value) ->
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
      Just (Value "true") -> do
        (Triple{object, supports}) <- thenPart id
        pure $ Triple { subject: id
                      , predicate : name
                      , object : object
                      , dependencies : []
                      , supports : Arr.cons (getRef $ unsafeCoerce c) supports
                      , tripleGetter :  getter}
      otherwise -> do
        (Triple{object, supports}) <- elsePart id
        pure $ Triple { subject: id
                      , predicate : name
                      , object : object
                      , dependencies : []
                      , supports : Arr.cons (getRef $ unsafeCoerce c) supports
                      , tripleGetter :  getter}

-- Applies the logical binary operator (such as OR, AND and IMPLIES) to the results of two queries applied to the same origin.
logicalBinaryOperator :: forall s. Newtype s String =>
  String ->
  (Boolean -> Boolean -> Boolean) ->
  (s **> Value) ->
  (s **> Value) ->
  (s **> Value)
logicalBinaryOperator n op (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s Value
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : fromBool $ op (toBool ps) (toBool qs)
                    , dependencies : []
                    , supports : map (unsafeCoerce getRef) [pt, qt]
                    , tripleGetter :  getter}
    name :: String
    name = n <> "(" <> nameOfp <> " " <> nameOfq <> ")"

    fromBool :: Boolean -> Array Value
    fromBool = Arr.singleton <<< Value <<< show

    toBool :: Array Value -> Boolean
    toBool s = maybe false ((==) (Value "true")) (Arr.head s)

conj :: forall s. Newtype s String =>
  (s **> Value) ->
  (s **> Value) ->
  (s **> Value)
conj = logicalBinaryOperator "conj" HA.conj

disj :: forall s. Newtype s String =>
  (s **> Value) ->
  (s **> Value) ->
  (s **> Value)
disj = logicalBinaryOperator "disj" HA.disj

implies :: forall s. Newtype s String =>
  (s **> Value) ->
  (s **> Value) ->
  (s **> Value)
implies = logicalBinaryOperator "implies" HA.implies

-- A set operation applied to the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
setOperation :: forall s o. Newtype s String =>
  (Array String -> Array String -> Array String) ->
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
                    , object : unsafeCoerce (op (unsafeCoerce ps) (unsafeCoerce qs))
                    , dependencies : []
                    , supports : map (unsafeCoerce getRef) [pt, qt]
                    , tripleGetter : getter}
    name = "intersect(" <> nameOfp <> ", " <> nameOfq <> ")"

intersect :: forall s o. Newtype s String =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
intersect = setOperation Arr.intersect

difference :: forall s o. Newtype s String =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
difference = setOperation Arr.difference

union :: forall s o. Newtype s String =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
union = setOperation Arr.union

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isSomething :: forall s o. Triple s o -> MonadPerspectivesQuery (Triple s Value)
isSomething (Triple r@{subject, predicate, object, dependencies, supports, tripleGetter}) = pure $ Triple
  { subject: subject
  , predicate: predicate
  , object: [Value $ show (HA.not $ Arr.null object)]
  , dependencies: dependencies
  , supports: supports
  , tripleGetter:  tripleGetter >=> isSomething
}

-- | A constraint constructed by checking whether the value of the query is empty.
-- | psp:Function -> psp:Constraint
notEmpty :: forall s o. Newtype s String => (s **> o) -> (s **> Value)
notEmpty (TypedTripleGetter nameOfp p) = memorize getter name where

  getter :: TripleGetter s Value
  getter = p >=> isSomething >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "notEmpty(" <> nameOfp <> ")"

-- | Construct a function that returns a bool in MonadPerspectivesQuery, from a TypedTripleGetter.
toBoolean :: forall s. Newtype s String => s **> Value -> s -> MonadPerspectivesQuery Boolean
toBoolean tg = flip applyTypedTripleGetterToMaybeObject tg >=> pure <<< maybe false (eq (Value "true"))

-- | This query constructor takes an argument that can be an PerspectEntiteit id or a simpleValue, and returns
-- | a triple whose object is boolean value.
containedIn :: forall s o. Eq o => Newtype s String => Newtype o String =>
  o ->
  s **> o ->
  (s **> Value)
-- Test.Perspectives.TripleGetterConstructors
containedIn id' tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containedIn(" <>  unwrap id' <> ")") f where
  f :: Array o -> s -> Array Value
  f os _ = do
    case Arr.elemIndex id' os of
      Nothing -> [Value "false"]
      otherwise -> [Value "true"]

containsMatching :: forall s o. Newtype s String => (s -> o -> Boolean) -> String -> (s **> o) -> TypedTripleGetter s Value
containsMatching criterium criteriumName tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containsMatching(" <> criteriumName <> ")") f where
  f :: Array o -> s -> Array Value
  f os subject = maybe [Value "true"] (const [Value "false"]) (Arr.findIndex (criterium subject) os)

-- | Apply to a query and retrieve a boolean query that returns true iff its subject occurs in its result.
-- | `psp:Function -> psp:Constraint`
contains :: forall s o. Newtype s String => Eq s => (s **> o) -> (s **> Value)
contains tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containedIn(" <> nameOfp <> ")") f where
  f :: Array o -> s -> Array Value
  f os id = do
    case (Arr.elemIndex id (unsafeCoerce os)) of
      Nothing -> [Value "false"]
      otherwise -> [Value "true"]

-- | The logical negation of a Constraint.
not' :: forall s. Newtype s String => (s **> Value) -> (s **> Value)
not' tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("not(" <> nameOfp <> ")") f where
  f :: Array Value -> s -> Array Value
  f object _ = case Arr.head object of
      (Just (Value "true")) -> [Value "false"]
      otherwise -> [Value "true"] -- NOTE: type checking guarantees we only have two values.

-- | Turn a query of many arguments into a query of a single element.
-- | The selected element depends on the ordering returned by the query.
-- | `psp:Function -> psp:SingularFunction`
lastElement :: forall s o. Newtype s String => (s **> o) -> (s **> o)
lastElement tg@(TypedTripleGetter nameOfp (p :: TripleGetter s o)) = tripleGetterFromTripleGetter tg
  ("lastElement(" <> nameOfp <> ")") f where
    f :: Array o -> s -> Array o
    f os _ = maybe [] Arr.singleton (Arr.last os)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
-- | The resulting query returns exactly the same result as the argument query.
-- | `psp:Function -> psp:Function`
ignoreCache :: forall s o. Newtype s String => (s **> o) -> (s **> o)
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
useCache :: forall s o. Newtype s String => (s **> o) -> (s **> o)
useCache (TypedTripleGetter nameOfp p) = TypedTripleGetter ("useCache(" <> nameOfp <> ")") go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result

constant :: forall s. Newtype s String => Value -> s **> Value
constant subject = (\_ -> pure [subject]) `trackedAs` ("constant()" <> unsafeCoerce subject <> ")")

-----------------------------------------------------------
-- VARIABLES
-----------------------------------------------------------
-- | Save the query result under the given name in the query execution environment for future use.
-- | The resulting query returns exactly the same result as the argument query.
-- | `String -> psp:Function -> psp:Function`
var :: forall s o. Newtype s String => String -> s **> o -> s **> o
var name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("var(" <> nameOfp <> ")") go where
  go subject = do
    r <- p subject
    putQueryVariable name $ (unsafeCoerce getRef) r
    pure r

-- | Retrieve the result stored in the query environment under the given name.
-- | Returns exactly the same value as the query used to store the value.
-- | `String -> psp:Function`
ref :: forall s o. Newtype s String => String -> s **> o
ref name = TypedTripleGetter ("ref(" <> name <> ")") (ref' name)

ref' :: forall s o. String -> s -> MonadPerspectivesQuery (Triple s o)
ref' name _ = readQueryVariable name >>= \(TripleRef{subject, predicate}) ->
    do
      mref <- lift $ liftEffect $ lookupInTripleIndex subject predicate
      pure (unsafeCoerce (unsafePartial $ fromJust $ mref) :: Triple s o)

-- | Save the query variable value and restore it after computing the query.
-- | Use this combinator to protect a query variable value.
-- | `String -> psp:Function -> psp:Function`
saveVar :: forall s o. Newtype s String => String -> s **> o -> s **> o
saveVar name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("saveVar(" <> name <> ", " <> nameOfp <> ")") go where
  go subject = do
    variableValue <- readQueryVariable name
    r <- p subject
    putQueryVariable name variableValue
    pure r
