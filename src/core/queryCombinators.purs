module Perspectives.QueryCombinators where

import Control.Alt ((<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array (cons, difference, elemIndex, findIndex, foldr, head, intersect, last, null, singleton, union, filter) as Arr
import Data.Array (foldMap)
import Data.HeytingAlgebra (not, conj, disj, implies) as HA
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, alaF, unwrap)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), applyTypedTripleGetterToMaybeObject, putQueryVariable, readQueryVariable, tripleObjects, type (**>), type (~~>), (@@))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.ObjectGetterConstructors (directAspectProperties, directAspectRoles, directAspects, searchContextRol)
import Perspectives.PerspectivesTypes (class Binding, AnyContext, BuitenRol(..), ContextDef, ContextRol, PBool(..), PropertyDef(..), RolDef, typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, lookupInTripleIndex, memorize, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.TripleGetterComposition ((>->), composeMonoidal)
import Perspectives.TripleGetterFromObjectGetter (constructTripleGetterFromEffectExpression, constructTripleGetterFromObjectsGetter, trackedAs)
import Prelude (class Eq, bind, const, discard, eq, flip, id, join, map, pure, show, ($), (<<<), (<>), (>=>), (>>=), (==), (>>>))
import Type.Data.Boolean (kind Boolean)

-- | Return the last element in the chain
-- | `psp:SingularFunction -> psp:SingularFunction`
closure' :: forall o e.
  Eq o =>
  (o **> o) e ->
  (o **> o) e
closure' (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array o -> o -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple o o e)
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
    name = "(closure' " <>  nameOfp <> ")"

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe id Arr.cons

-- | A selection of the results of the second query using the first (boolean) query as a criterium.
-- | `psp:Constraint -> psp:Function -> psp:Function`
filter :: forall s o e.
  (o **> PBool) e ->
  (s **> o) e ->
  (s **> o) e
filter (TypedTripleGetter nameOfc criterium) (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: TripleGetter s o e
    getter id = do
      t@(Triple{object}) <- p id
      (triples :: Array (Triple o PBool e)) <- traverse criterium object
      (objects :: Array o) <- pure $ Arr.foldr addSubjectIfTrue [] triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : Arr.cons (typeWithPerspectivesTypes getRef t)
                      (map (typeWithPerspectivesTypes getRef) triples)
                    , tripleGetter : getter}

    addSubjectIfTrue :: Triple o PBool e -> Array o -> Array o
    addSubjectIfTrue (Triple{subject, object}) arr = case Arr.elemIndex (PBool "true") object of
      Nothing -> arr
      _ -> Arr.cons subject arr

    name :: String
    name = "(filter " <> nameOfc <> " " <> nameOfp <> ")"

-- | A selection of the results of the query using a simple (boolean) function as a criterium.
filter_ :: forall s o e.
  (o -> Boolean) ->
  String ->
  (s **> o) e ->
  (s **> o) e
filter_ criterium criteriumName (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: TripleGetter s o e
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
equal :: forall s o e.
  Eq o =>
  (s **> o) e ->
  (s **> o) e ->
  (s **> PBool) e
equal (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s PBool e
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : [PBool $ show $ ps == qs]
                    , dependencies : []
                    , supports : map (typeWithPerspectivesTypes getRef) [pt, qt]
                    , tripleGetter : typeWithPerspectivesTypes getter}

    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

-- Applies the logical binary operator (such as OR, AND and IMPLIES) to the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
logicalBinaryOperator :: forall s o e.
  (Boolean -> Boolean -> Boolean) ->
  (s **> PBool) e ->
  (s **> PBool) e ->
  ((s **> PBool) e)
logicalBinaryOperator op (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s PBool e
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
    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

    fromBool :: Boolean -> Array PBool
    fromBool = Arr.singleton <<< PBool <<< show

    toBool :: Array PBool -> Boolean
    toBool s = maybe false ((==) (PBool "true")) (Arr.head s)

conj :: forall s e. (s **> PBool) e -> (s **> PBool) e -> ((s **> PBool) e)
conj = logicalBinaryOperator HA.conj

disj :: forall s e. (s **> PBool) e -> (s **> PBool) e -> ((s **> PBool) e)
disj = logicalBinaryOperator HA.disj

implies :: forall s e. (s **> PBool) e -> (s **> PBool) e -> ((s **> PBool) e)
implies = logicalBinaryOperator HA.implies

-- The intersection of the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
intersect :: forall s o e. Eq o => (s **> o) e -> (s **> o) e -> (s **> o) e
intersect (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name where
    getter :: TripleGetter s o e
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (Arr.intersect ps qs)
                    , dependencies : []
                    , supports : map (typeWithPerspectivesTypes getRef) [pt, qt]
                    , tripleGetter : getter}
    name = "(intersect " <> nameOfp <> " " <> nameOfq <> ")"

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isNothing :: forall s o e. Triple s o e -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple s PBool e)
isNothing (Triple r@{subject, predicate, object, dependencies, supports, tripleGetter}) = pure $ Triple
  { subject: subject
  , predicate: predicate
  , object: [PBool $ show (HA.not $ Arr.null object)]
  , dependencies: dependencies
  , supports: supports
  , tripleGetter: typeWithPerspectivesTypes tripleGetter
}

-- | A constraint constructed by checking whether the value of the query is empty.
-- | psp:Function -> psp:Constraint
notEmpty :: forall s o e. (s **> o) e -> (s **> PBool) e
notEmpty (TypedTripleGetter nameOfp p) = memorize getter name where

  getter :: TripleGetter s PBool e
  getter = p >=> isNothing >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "(notEmpty " <> nameOfp <> ")"

-- | Construct a function that returns a bool in MonadPerspectivesQuery, from a TypedTripleGetter.
toBoolean :: forall s e. (s **> PBool) e -> s -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
toBoolean tg = flip applyTypedTripleGetterToMaybeObject tg >=> pure <<< maybe false (eq (PBool "true"))

-- | This query constructor takes an argument that can be an PerspectEntiteit id or a simpleValue, and returns
-- | a triple whose object is boolean value.
contains :: forall s o e.
  Eq o =>
  Newtype o String =>
  o ->
  TypedTripleGetter s o e ->
  TypedTripleGetter s PBool e
contains id' (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$contains_" <> unwrap id') f where
  f :: (s -> MonadPerspectivesQuery (AjaxAvarCache e) (Array PBool))
  f id = do
    (Triple{object}) <- p id
    case Arr.elemIndex id' object of
      Nothing -> pure [PBool "false"]
      otherwise -> pure [PBool "true"]

containsMatching :: forall s o e. (s -> o -> Boolean) -> String -> TypedTripleGetter s o e -> TypedTripleGetter s PBool e
containsMatching criterium criteriumName (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$contains" <> criteriumName) f where
  f :: (s -> MonadPerspectivesQuery (AjaxAvarCache e) (Array PBool))
  f subject = do
    (Triple{object}) <- p subject
    pure $ maybe [PBool "true"] (const [PBool "false"]) (Arr.findIndex (criterium subject) object)

-- | Apply to a query and retrieve a boolean query that returns true iff its subject occurs in its result.
-- | `psp:Function -> psp:Constraint`
containedIn :: forall o e. Eq o => (o **> o) e -> (o **> PBool) e
containedIn (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$containedIn_" <> nameOfp) f where
  f :: (o -> MonadPerspectivesQuery (AjaxAvarCache e) (Array PBool))
  f id = do
    (Triple{object}) <- p id
    case Arr.elemIndex id object of
      Nothing -> pure [PBool "false"]
      otherwise -> pure [PBool "true"]

-- | The logical negation of a Constraint.
-- | `psp:Constraint -> psp:Constraint`
not :: forall s e. (s **> PBool) e -> (s **> PBool) e
not (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$not_" <> nameOfp) f where
  f :: (s -> MonadPerspectivesQuery (AjaxAvarCache e) (Array PBool))
  f id = do
    (Triple{object}) <- p id
    case Arr.head object of
      (Just (PBool "true")) -> pure [PBool "false"]
      otherwise -> pure [PBool "true"] -- NOTE: type checking guarantees we only have two values.

-- | Turn a query of many arguments into a query of a single element.
-- | The selected element depends on the ordering returned by the query.
-- | `psp:Function -> psp:SingularFunction`
lastElement :: forall s o e. (s **> o) e -> (s **> o) e
lastElement (TypedTripleGetter nameOfp (p :: TripleGetter s o e)) = constructTripleGetterFromEffectExpression
  ("(lastElement_" <> nameOfp <> ")")
  (p >=> pure <<< (maybe [] Arr.singleton) <<< Arr.last <<< tripleObjects)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
-- | The resulting query returns exactly the same result as the argument query.
-- | `psp:Function -> psp:Function`
ignoreCache :: forall s o e. (s **> o) e -> (s **> o) e
ignoreCache (TypedTripleGetter nameOfp p) = TypedTripleGetter nameOfp go where
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
useCache :: forall s o e. (s **> o) e -> (s **> o) e
useCache (TypedTripleGetter nameOfp p) = TypedTripleGetter nameOfp go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result

constant :: forall s e. Newtype s String => s -> TypedTripleGetter s s e
constant subject = constructTripleGetterFromObjectsGetter
  ("model:Perspectives$constant$_" <> unwrap subject)
  (\_ -> pure [subject])

-----------------------------------------------------------
-- VARIABLES
-----------------------------------------------------------
-- | Save the query result under the given name in the query execution environment for future use.
-- | The resulting query returns exactly the same result as the argument query.
-- | `String -> psp:Function -> psp:Function`
var :: forall s o e. String -> (s **> o) e -> (s **> o) e
var name (TypedTripleGetter nameOfp p) = TypedTripleGetter nameOfp go where
  go subject = do
    r <- p subject
    putQueryVariable name $ (typeWithPerspectivesTypes getRef) r
    pure r

-- | Retrieve the result stored in the query environment under the given name.
-- | Returns exactly the same value as the query used to store the value.
-- | `String -> psp:Function`
ref :: forall s o e. String -> TypedTripleGetter s o e
ref name = TypedTripleGetter name (ref' name)

ref' :: forall s o e. String -> s -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple s o e)
ref' name ignore = readQueryVariable name >>= \(TripleRef{subject, predicate}) ->
    do
      mref <- lift $ liftEff $ lookupInTripleIndex subject predicate
      unsafePartial $ pure $ fromJust $ mref

-- | Save the query variable value and restore it after computing the query.
-- | Use this combinator to protect a query variable value.
-- | `String -> psp:Function -> psp:Function`
saveVar :: forall s o e. String -> (s **> o) e -> (s **> o) e
saveVar name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("saveVar_" <> name <> nameOfp) go where
  go subject = do
    variableValue <- readQueryVariable name
    r <- p subject
    putQueryVariable name variableValue
    pure r
