module Perspectives.QueryCombinators where

import Data.Array (cons, difference, elemIndex, foldr, head, last, null, singleton, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (Triple(..), TripleGetter, NamedFunction(..), NamedTripleGetter, MonadPerspectivesQuery)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolID, ID)
import Perspectives.Property (ObjectsGetter, getRol)
import Perspectives.TripleAdministration (getRef, memorize, memorizeQueryResults, setMemorizeQueryResults, tripleObjects)
import Perspectives.TripleGetter (constructTripleGetterFromObjectsGetter, constructTripleGetterFromEffectExpression)
import Prelude (bind, discard, id, join, map, not, pure, show, ($), (<<<), (<>), (==), (>=>))

closure :: forall e.
  NamedTripleGetter e ->
  NamedTripleGetter e
closure (NamedFunction nameOfp p) =
  memorize getter name
  where
    getter :: TripleGetter e
    getter id' = do
      t@(Triple{subject, object : objectsOfP}) <- p id'
      (triples :: Array (Triple e)) <- traverse getter (difference objectsOfP [id'])
      objects <- pure $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id'
                    , predicate : name
                    , object : cons subject objects
                    , dependencies : []
                    , supports : map getRef (cons t triples)
                    , tripleGetter : getter}

    name :: String
    name = "(closure " <>  nameOfp <> ")"

-- | Return the last element in the chain
closure' :: forall e.
  NamedTripleGetter e ->
  NamedTripleGetter e
closure' (NamedFunction nameOfp p) =
  memorize getter name
  where
    getter :: TripleGetter e
    getter id = do
      t@(Triple{subject, object : objectsOfP}) <- p id
      case head objectsOfP of
        Nothing -> pure t
        (Just o) -> do
          pt@(Triple{object:bottom}) <- getter o
          case head bottom of
            Nothing -> pure t
            otherwise ->
              pure $ Triple { subject: id
                            , predicate : name
                            , object : bottom
                            , dependencies : []
                            , supports : [getRef pt]
                            , tripleGetter : getter}

    name :: String
    name = "(closure " <>  nameOfp <> ")"

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe id cons

filter :: forall e.
  NamedTripleGetter e ->
  NamedTripleGetter e ->
  NamedTripleGetter e
filter (NamedFunction nameOfc criterium) (NamedFunction nameOfp p) =
  memorize getter name where
    getter :: TripleGetter e
    getter id = do
      t@(Triple{object}) <- p id
      (triples :: Array (Triple e)) <- traverse criterium (difference object [id])
      (objects :: Array String) <- pure $ foldr addSubjectIfTrue [] triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : map getRef (cons t triples)
                    , tripleGetter : getter}

    addSubjectIfTrue :: Triple e -> Array String -> Array String
    addSubjectIfTrue (Triple{subject, object}) arr = case elemIndex "true" object of
      Nothing -> arr
      _ -> cons subject arr

    name :: String
    name = "(closure " <>  nameOfp <> ")"

concat :: forall e.
  NamedTripleGetter e ->
  NamedTripleGetter e ->
  NamedTripleGetter e
concat (NamedFunction nameOfp p) (NamedFunction nameOfq q) = memorize getter name
  where
    getter :: TripleGetter e
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (union ps qs)
                    , dependencies : []
                    , supports : map getRef [pt, qt]
                    , tripleGetter : getter}

    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isNothing :: forall e. Triple e -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
isNothing (Triple r@{object}) = pure (Triple(r {object = [show (not $ null object)]}))

notEmpty :: forall e. NamedTripleGetter e -> NamedTripleGetter e
notEmpty (NamedFunction nameOfp p) = memorize getter name where

  getter :: TripleGetter e
  getter = p >=> isNothing >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "(notEmpty " <> nameOfp <> ")"

-- | Construct a function that returns a bool in Aff, from a TripleGetter.
toBoolean :: forall e. NamedTripleGetter e -> RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
toBoolean (NamedFunction nameOfp p) r = do
  result <- p r
  arrWithBool <- pure $ tripleObjects result
  case head arrWithBool of
    Nothing -> pure false
    (Just x) -> pure (x == "true")

-- | This query constructor takes a context id as argument. The query step that results can be applied to a role id
-- | and will result in all instances of that role for the given context.
rolesOf :: forall e. ContextID -> NamedTripleGetter e
rolesOf cid = constructTripleGetterFromObjectsGetter
  ("model:Perspectives$rolesOf" <> cid) f where
  f :: ObjectsGetter e
  f rolName = getRol rolName cid
-- rolesOf cid = NamedFunction ("rolesOf_" <> cid) \rolname -> applyNamedFunction (constructRolGetter rolname) cid

-- | This query constructor takes an argument that can be an PerspectEntiteit id or a simpleValue, and returns
-- | a triple whose object is boolean value.
contains :: forall e. ID -> NamedTripleGetter e -> NamedTripleGetter e
contains id' (NamedFunction nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$contains" <> id') f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f id = do
    (Triple{object}) <- p id
    case elemIndex id' object of
      Nothing -> pure ["false"]
      otherwise -> pure ["true"]

lastElement :: forall e. NamedTripleGetter e -> NamedTripleGetter e
lastElement (NamedFunction nameOfp (p :: TripleGetter e)) = constructTripleGetterFromEffectExpression
  ("(lastElement " <> nameOfp <> ")")
  (p >=> pure <<< (maybe [] singleton) <<< last <<< tripleObjects)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
ignoreCache :: forall e. NamedTripleGetter e -> NamedTripleGetter e
ignoreCache (NamedFunction nameOfp p) = NamedFunction nameOfp go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults false
      result <- p r
      setMemorizeQueryResults remember
      pure result

-- | Use the cache of query results for the given named function.
useCache :: forall e. NamedTripleGetter e -> NamedTripleGetter e
useCache (NamedFunction nameOfp p) = NamedFunction nameOfp go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result

constant :: forall e. ID -> NamedTripleGetter e
constant subject = constructTripleGetterFromObjectsGetter
  ("model:Perspectives$constant$_" <> subject)
  \_ -> pure [subject]
