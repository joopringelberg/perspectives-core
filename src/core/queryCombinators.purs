module Perspectives.QueryCombinators where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, difference, elemIndex, findIndex, foldr, head, intersect, last, nub, null, singleton, union) as Arr
import Data.HeytingAlgebra (not) as HA
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, ObjectsGetter, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), applyTypedTripleGetterToMaybeObject, putQueryVariable, readQueryVariable, tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, Object, RolID, Subject, Value)
import Perspectives.ObjectGetterConstructors (getRol)
import Perspectives.TripleAdministration (getRef, lookupInTripleIndex, memorize, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.TripleGetterConstructors (constructTripleGetterFromObjectsGetter, constructTripleGetterFromEffectExpression)
import Prelude (bind, const, discard, eq, flip, id, join, map, pure, show, ($), (<<<), (<>), (>=>), (>>=))
import Type.Data.Boolean (kind Boolean)

-- | The recursive closure of a query, bottoming out when it has no results.
-- | The result only contains the argument id if it can be obtained by applying p,
-- | never because it is the starting point of the computation.
-- | `psp:Function -> psp:Function`
closure :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e
closure (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array ID -> Subject -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
    getter cumulator id = do
      t@(Triple{object : objectsOfP}) <- p id
      case Arr.elemIndex id cumulator of
        Nothing -> do
          (triples :: Array (Triple e)) <- (traverse (getter (Arr.union cumulator objectsOfP))) (Arr.difference objectsOfP cumulator)
          objects <- pure $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object: Arr.union objectsOfP objects
                        , dependencies : []
                        , supports : map getRef (Arr.cons t triples)
                        , tripleGetter : getter []}
        otherwise -> pure t

    name :: String
    name = "(closure " <>  nameOfp <> ")"

-- | Return the last element in the chain
-- | `psp:SingularFunction -> psp:SingularFunction`
closure' :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e
closure' (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array ID -> Subject -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
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
                                , supports : [getRef pt]
                                , tripleGetter : getter cumulator}
        otherwise -> pure t

    name :: String
    name = "(closure' " <>  nameOfp <> ")"

-- | The closure of an ObjectsGetter.
closure_ :: forall e.
  ObjectsGetter e ->
  ObjectsGetter e
closure_ p = getter [] where
  getter :: Array ID -> ID -> MonadPerspectives (AjaxAvarCache e) (Array Value)
  getter cumulator id = do
    objectsOfP <- p id
    case Arr.elemIndex id cumulator of
      Nothing -> do
        (results :: Array (Array String)) <- traverse (getter (Arr.union cumulator objectsOfP)) (Arr.difference objectsOfP cumulator)
        pure $ Arr.nub $ join (Arr.cons objectsOfP results)
      otherwise -> pure objectsOfP

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe id Arr.cons

-- | A selection of the results of the second query using the first (boolean) query as a criterium.
-- | `psp:Constraint -> psp:Function -> psp:Function`
filter :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e ->
  TypedTripleGetter e
filter (TypedTripleGetter nameOfc criterium) (TypedTripleGetter nameOfp p) =
  memorize getter name where
    getter :: TripleGetter e
    getter id = do
      t@(Triple{object}) <- p id
      (triples :: Array (Triple e)) <- traverse criterium (Arr.difference object [id])
      (objects :: Array String) <- pure $ Arr.foldr addSubjectIfTrue [] triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : map getRef (Arr.cons t triples)
                    , tripleGetter : getter}

    addSubjectIfTrue :: Triple e -> Array String -> Array String
    addSubjectIfTrue (Triple{subject, object}) arr = case Arr.elemIndex "true" object of
      Nothing -> arr
      _ -> Arr.cons subject arr

    name :: String
    name = "(filter " <> nameOfc <> " " <> nameOfp <> ")"

-- The concatenation of the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
concat :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e ->
  (TypedTripleGetter e)
concat (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter e
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (Arr.union ps qs)
                    , dependencies : []
                    , supports : map getRef [pt, qt]
                    , tripleGetter : getter}

    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

-- The intersection of the results of two queries applied to the same origin.
-- | `psp:Function -> psp:Function -> psp:Function`
intersect :: forall e. TypedTripleGetter e -> TypedTripleGetter e -> TypedTripleGetter e
intersect (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name where
    getter :: TripleGetter e
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (Arr.intersect ps qs)
                    , dependencies : []
                    , supports : map getRef [pt, qt]
                    , tripleGetter : getter}
    name = "(intersect " <> nameOfp <> " " <> nameOfq <> ")"

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isNothing :: forall e. Triple e -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
isNothing (Triple r@{object}) = pure (Triple(r {object = [show (HA.not $ Arr.null object)]}))

-- | A constraint constructed by checking of the value of the query is empty.
-- | psp:Function -> psp:Constraint
notEmpty :: forall e. TypedTripleGetter e -> TypedTripleGetter e
notEmpty (TypedTripleGetter nameOfp p) = memorize getter name where

  getter :: TripleGetter e
  getter = p >=> isNothing >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "(notEmpty " <> nameOfp <> ")"

-- | Construct a function that returns a bool in Aff, from a TypedTripleGetter.
toBoolean :: forall e. TypedTripleGetter e -> RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
toBoolean tg = flip applyTypedTripleGetterToMaybeObject tg >=> pure <<< maybe false (eq "true")

-- | This query constructor takes a context id as argument. The query step that results can be applied to a role id
-- | and will result in all instances of that role for the given context.
-- | For domain we just take psp:Context. Range can only be psp:Rol because we have no
-- | other knowledge on it.
-- | psp:ContextInstance -> psp:Function
rolesOf :: forall e. ContextID -> TypedTripleGetter e
rolesOf cid = constructTripleGetterFromObjectsGetter
  ("model:Perspectives$rolesOf" <> cid) f where
  f :: ObjectsGetter e
  f rolName = getRol rolName cid

-- | This query constructor takes an argument that can be an PerspectEntiteit id or a simpleValue, and returns
-- | a triple whose object is boolean value.
contains :: forall e. ID -> TypedTripleGetter e -> TypedTripleGetter e
contains id' (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$contains" <> id') f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f id = do
    (Triple{object}) <- p id
    case Arr.elemIndex id' object of
      Nothing -> pure ["false"]
      otherwise -> pure ["true"]

containsMatching :: forall e. (Subject -> Object -> Boolean) -> String -> TypedTripleGetter e -> TypedTripleGetter e
containsMatching criterium criteriumName (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$contains" <> criteriumName) f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f subject = do
    (Triple{object}) <- p subject
    pure $ maybe ["true"] (const ["false"]) (Arr.findIndex (criterium subject) object)

-- | Apply to a query and retrieve a boolean query that returns true iff its subject is
-- | a member of the result of the argument query.
-- | `psp:Function -> psp:Constraint`
containedIn :: forall e. TypedTripleGetter e -> TypedTripleGetter e
containedIn (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$containedIn_" <> nameOfp) f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f id = do
    (Triple{object}) <- p id
    case Arr.elemIndex id object of
      Nothing -> pure ["false"]
      otherwise -> pure ["true"]

-- | The logical negation of a Constraint.
-- | `psp:Constraint -> psp:Constraint`
not :: forall e. TypedTripleGetter e -> TypedTripleGetter e
not (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$not_" <> nameOfp) f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f id = do
    (Triple{object}) <- p id
    case Arr.head object of
      (Just "true") -> pure ["false"]
      otherwise -> pure ["true"] -- NOTE: type checking guarantees we only have two values.

-- | Turn a query of many arguments into a query of a single element.
-- | The selected element depends on the ordering returned by the query.
-- | `psp:Function -> psp:SingularFunction`
lastElement :: forall e. TypedTripleGetter e -> TypedTripleGetter e
lastElement (TypedTripleGetter nameOfp (p :: TripleGetter e)) = constructTripleGetterFromEffectExpression
  ("(lastElement " <> nameOfp <> ")")
  (p >=> pure <<< (maybe [] Arr.singleton) <<< Arr.last <<< tripleObjects)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
-- | The resulting query returns exactly the same result as the argument query.
-- | `psp:Function -> psp:Function`
ignoreCache :: forall e. TypedTripleGetter e -> TypedTripleGetter e
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
useCache :: forall e. TypedTripleGetter e -> TypedTripleGetter e
useCache (TypedTripleGetter nameOfp p) = TypedTripleGetter nameOfp go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result

constant :: forall e. ID -> TypedTripleGetter e
constant subject = constructTripleGetterFromObjectsGetter
  ("model:Perspectives$constant$_" <> subject)
  (\_ -> pure [subject])

-----------------------------------------------------------
-- VARIABLES
-----------------------------------------------------------
-- | Save the query result under the given name in the query execution environment for future use.
-- | The resulting query returns exactly the same result as the argument query.
-- | `String -> psp:Function -> psp:Function`
var :: forall e. String -> TypedTripleGetter e -> TypedTripleGetter e
var name (TypedTripleGetter nameOfp p) = TypedTripleGetter nameOfp go where
  go subject = do
    r <- p subject
    putQueryVariable name (TripleRef{subject: subject, predicate: nameOfp})
    pure r

-- | Retrieve the result stored in the query environment under the given name.
-- | Returns exactly the same value as the query used to store the value.
-- | `String -> psp:Function`
ref :: forall e. String -> TypedTripleGetter e
ref name = TypedTripleGetter name (ref' name)

ref' :: forall e. String -> String -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
ref' name ignore = readQueryVariable name >>= \(TripleRef{subject, predicate}) ->
    do
      mref <- lift $ liftEff $ lookupInTripleIndex subject predicate
      unsafePartial $ pure $ fromJust $ mref

-- | Save the query variable value and restore it after computing the query.
-- | Use this combinator to protect a query variable value.
-- | `String -> psp:Function -> psp:Function`
saveVar :: forall e. String -> TypedTripleGetter e -> TypedTripleGetter e
saveVar name (TypedTripleGetter nameOfp p) = TypedTripleGetter ("saveVar_" <> name <> nameOfp) go where
  go subject = do
    variableValue <- readQueryVariable name
    r <- p subject
    putQueryVariable name variableValue
    pure r
