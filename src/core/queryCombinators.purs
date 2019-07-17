module Perspectives.QueryCombinators where

import Control.Monad.Trans.Class (lift)
import Data.Array (cons, difference, elemIndex, findIndex, foldr, head, intersect, last, null, singleton, filter, union) as Arr
import Data.HeytingAlgebra (not, conj, disj, implies) as HA
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesQuery, StringTripleGetter, StringTypedTripleGetter, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), StringTriple, applyTypedTripleGetterToMaybeObject, putQueryVariable, readQueryVariable)
import Perspectives.Instances.Aliases (PBool)
import Perspectives.TripleAdministration (getRef, lookupInTripleIndex, memorize, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.TripleGetters.TrackedAs (trackedAs, tripleGetterFromTripleGetter)
import Prelude (bind, const, discard, eq, flip, identity, map, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=))
import Type.Data.Boolean (kind Boolean)
import Unsafe.Coerce (unsafeCoerce)


-- | Return the last element in the chain
-- | `psp:SingularFunction -> psp:SingularFunction`
closure' ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter
closure' (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array String -> String -> MonadPerspectivesQuery StringTriple
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
                                , supports : [ getRef pt]
                                , tripleGetter : getter cumulator}
        otherwise -> pure t

    name :: String
    name = "closure('" <>  nameOfp <> ")"

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe identity Arr.cons

-- | A selection of the results of the second query using the first (boolean) query as a criterium.
-- | `psp:Constraint -> psp:Function -> psp:Function`
filter ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
filter (TypedTripleGetter nameOfc criterium) (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: StringTripleGetter
    getter id = do
      t@(Triple{object}) <- p id
      (triples :: Array (Triple String PBool)) <- traverse criterium object
      (objects :: Array String) <- pure $ Arr.foldr addSubjectIfTrue [] triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : Arr.cons ( getRef t)
                      (map ( getRef) triples)
                    , tripleGetter : getter}

    addSubjectIfTrue :: Triple String PBool -> Array String -> Array String
    addSubjectIfTrue (Triple{subject, object}) arr = case Arr.elemIndex ("true") object of
      Nothing -> arr
      _ -> Arr.cons subject arr

    name :: String
    name = "filter(" <> nameOfc <> ", " <> nameOfp <> ")"

-- | A selection of the results of the query using a simple (boolean) function as a criterium.
-- Test.Perspectives.TripleGetterConstructors, via getUnqualifiedRolDefinition
filter_ ::
  (String -> Boolean) ->
  String ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
filter_ criterium criteriumName (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: StringTripleGetter
    getter id = do
      t@(Triple{object}) <- p id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : Arr.filter criterium object
                    , dependencies : []
                    , supports : [( getRef t)]
                    , tripleGetter : getter}
    name :: String
    name = "(filter_" <> nameOfp <> "_" <> criteriumName <> ")"

-- Returns true iff the results of both TripleGetters applied to the same origin yield exactly the same values in the same order.
-- | `psp:Function -> psp:Function -> psp:Function`
equal ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
equal (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter String PBool
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : [show $ ps == qs]
                    , dependencies : []
                    , supports : map ( getRef) [pt, qt]
                    , tripleGetter :  getter}

    name = "(equal " <> nameOfp <> " " <> nameOfq <> ")"

cond ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
cond cd@(TypedTripleGetter nameOfCondition condition) (TypedTripleGetter nameOfThenPart thenPart) (TypedTripleGetter nameOfElsePart elsePart) = memorize getter name where

  name :: String
  name = "(cond(" <> nameOfCondition <> ", " <> nameOfThenPart <> ", " <> nameOfElsePart <> ")"

  getter :: StringTripleGetter
  getter id = do
    c@(Triple{object : cs}) <- condition id
    case Arr.head cs of
      Just ("true") -> do
        (Triple{object, supports}) <- thenPart id
        pure $ Triple { subject: id
                      , predicate : name
                      , object : object
                      , dependencies : []
                      , supports : Arr.cons (getRef c) supports
                      , tripleGetter :  getter}
      otherwise -> do
        (Triple{object, supports}) <- elsePart id
        pure $ Triple { subject: id
                      , predicate : name
                      , object : object
                      , dependencies : []
                      , supports : Arr.cons (getRef c) supports
                      , tripleGetter :  getter}

-- Applies the logical binary operator (such as OR, AND and IMPLIES) to the results of two queries applied to the same origin.
logicalBinaryOperator ::
  String ->
  (Boolean -> Boolean -> Boolean) ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  (StringTypedTripleGetter)
logicalBinaryOperator n op (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter String PBool
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : fromBool $ op (toBool ps) (toBool qs)
                    , dependencies : []
                    , supports : map ( getRef) [pt, qt]
                    , tripleGetter :  getter}
    name :: String
    name = n <> "(" <> nameOfp <> " " <> nameOfq <> ")"

    fromBool :: Boolean -> Array PBool
    fromBool = Arr.singleton <<< show

    toBool :: Array PBool -> Boolean
    toBool s = maybe false ((==) ("true")) (Arr.head s)

conj :: StringTypedTripleGetter -> StringTypedTripleGetter -> (StringTypedTripleGetter)
conj = logicalBinaryOperator "conj" HA.conj

disj :: StringTypedTripleGetter -> StringTypedTripleGetter -> (StringTypedTripleGetter)
disj = logicalBinaryOperator "disj" HA.disj

implies :: StringTypedTripleGetter -> StringTypedTripleGetter -> (StringTypedTripleGetter)
implies = logicalBinaryOperator "implies" HA.implies

-- A set operation applied to the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
setOperation ::
  (Array String -> Array String -> Array String) ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
setOperation op (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name where
    getter :: StringTripleGetter
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (op ps qs)
                    , dependencies : []
                    , supports : map ( getRef) [pt, qt]
                    , tripleGetter : getter}
    name = "intersect(" <> nameOfp <> ", " <> nameOfq <> ")"

intersect ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
intersect = setOperation Arr.intersect

difference ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
difference = setOperation Arr.difference

union ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
union = setOperation Arr.union

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isSomething :: StringTriple -> MonadPerspectivesQuery (Triple String PBool)
isSomething (Triple r@{subject, predicate, object, dependencies, supports, tripleGetter}) = pure $ Triple
  { subject: subject
  , predicate: predicate
  , object: [show (HA.not $ Arr.null object)]
  , dependencies: dependencies
  , supports: supports
  , tripleGetter:  tripleGetter
}

-- | A constraint constructed by checking whether the value of the query is empty.
-- | psp:Function -> psp:Constraint
notEmpty :: StringTypedTripleGetter -> StringTypedTripleGetter
notEmpty (TypedTripleGetter nameOfp p) = memorize getter name where

  getter :: TripleGetter String PBool
  getter = p >=> isSomething >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "notEmpty(" <> nameOfp <> ")"

-- | Construct a function that returns a bool in MonadPerspectivesQuery, from a TypedTripleGetter.
toBoolean :: StringTypedTripleGetter -> String -> MonadPerspectivesQuery Boolean
toBoolean tg = flip applyTypedTripleGetterToMaybeObject tg >=> pure <<< maybe false (eq ("true"))

-- | This query constructor takes an argument that can be an PerspectEntiteit id or a simpleValue, and returns
-- | a triple whose object is boolean value.
containedIn ::
  String ->
  StringTypedTripleGetter ->
  TypedTripleGetter String PBool
-- Test.Perspectives.TripleGetterConstructors
containedIn id' tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containedIn(" <>  id' <> ")") f where
  f :: Array String -> String -> Array PBool
  f os _ = do
    case Arr.elemIndex id' os of
      Nothing -> ["false"]
      otherwise -> ["true"]

containsMatching :: (String -> String -> Boolean) -> String -> StringTypedTripleGetter -> TypedTripleGetter String PBool
containsMatching criterium criteriumName tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containsMatching(" <> criteriumName <> ")") f where
  f :: Array String -> String -> Array PBool
  f os subject = maybe ["true"] (const ["false"]) (Arr.findIndex (criterium subject) os)

-- | Apply to a query and retrieve a boolean query that returns true iff its subject occurs in its result.
-- | `psp:Function -> psp:Constraint`
contains :: StringTypedTripleGetter -> StringTypedTripleGetter
contains tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("containedIn(" <> nameOfp <> ")") f where
  f :: Array String -> String -> Array PBool
  f os id = do
    case Arr.elemIndex id os of
      Nothing -> ["false"]
      otherwise -> ["true"]

-- | The logical negation of a Constraint.
-- | `psp:Constraint -> psp:Constraint`
not :: StringTypedTripleGetter -> StringTypedTripleGetter
not tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("not(" <> nameOfp <> ")") f where
  f :: Array PBool -> String -> Array PBool
  f object _ = case Arr.head object of
      (Just ("true")) -> ["false"]
      otherwise -> ["true"] -- NOTE: type checking guarantees we only have two values.

not' :: StringTypedTripleGetter -> StringTypedTripleGetter
not' tg@(TypedTripleGetter nameOfp p) = tripleGetterFromTripleGetter tg ("not(" <> nameOfp <> ")") f where
  f :: Array String -> String -> Array String
  f object _ = case Arr.head object of
      (Just "true") -> ["false"]
      otherwise -> ["true"] -- NOTE: type checking guarantees we only have two values.

-- | Turn a query of many arguments into a query of a single element.
-- | The selected element depends on the ordering returned by the query.
-- | `psp:Function -> psp:SingularFunction`
lastElement :: StringTypedTripleGetter -> StringTypedTripleGetter
lastElement tg@(TypedTripleGetter nameOfp (p :: StringTripleGetter)) = tripleGetterFromTripleGetter tg
  ("lastElement(" <> nameOfp <> ")") f where
    f :: Array String -> String -> Array String
    f os _ = maybe [] Arr.singleton (Arr.last os)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
-- | The resulting query returns exactly the same result as the argument query.
-- | `psp:Function -> psp:Function`
ignoreCache :: StringTypedTripleGetter -> StringTypedTripleGetter
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
useCache :: StringTypedTripleGetter -> StringTypedTripleGetter
useCache (TypedTripleGetter nameOfp p) = TypedTripleGetter ("useCache(" <> nameOfp <> ")") go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result

constant :: String -> StringTypedTripleGetter
constant subject = (\_ -> pure [subject]) `trackedAs` ("constant()" <> unsafeCoerce subject <> ")")

-----------------------------------------------------------
-- VARIABLES
-----------------------------------------------------------
-- | Save the query result under the given name in the query execution environment for future use.
-- | The resulting query returns exactly the same result as the argument query.
-- | `String -> psp:Function -> psp:Function`
var :: String -> StringTypedTripleGetter -> StringTypedTripleGetter
var name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("var(" <> nameOfp <> ")") go where
  go subject = do
    r <- p subject
    putQueryVariable name $ ( getRef) r
    pure r

-- | Retrieve the result stored in the query environment under the given name.
-- | Returns exactly the same value as the query used to store the value.
-- | `String -> psp:Function`
ref :: String -> StringTypedTripleGetter
ref name = TypedTripleGetter ("ref(" <> name <> ")") (ref' name)

ref' :: String -> String -> MonadPerspectivesQuery StringTriple
ref' name ignore = readQueryVariable name >>= \(TripleRef{subject, predicate}) ->
    do
      mref <- lift $ liftEffect $ lookupInTripleIndex subject predicate
      unsafePartial $ pure $ fromJust $ mref

-- | Save the query variable value and restore it after computing the query.
-- | Use this combinator to protect a query variable value.
-- | `String -> psp:Function -> psp:Function`
saveVar :: String -> StringTypedTripleGetter -> StringTypedTripleGetter
saveVar name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("saveVar(" <> name <> ", " <> nameOfp <> ")") go where
  go subject = do
    variableValue <- readQueryVariable name
    r <- p subject
    putQueryVariable name variableValue
    pure r
