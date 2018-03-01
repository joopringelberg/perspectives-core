module Perspectives.QueryCombinators where

import Data.Array (cons, difference, elemIndex, foldr, head, null, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.ContextAndRole (rol_pspType)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolID, ID)
import Perspectives.PerspectivesState (MonadPerspectives, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.Property (ObjectsGetter, getRol)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, getRef, memorize, tripleObjects)
import Perspectives.TripleGetter (constructTripleGetterFromArbitraryFunction)
import Prelude (bind, discard, id, join, map, not, pure, show, ($), (<>), (==), (>=>))

closure :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
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
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
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
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
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
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
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
isNothing :: forall e. Triple e -> MonadPerspectives (AjaxAvarCache e) (Triple e)
isNothing (Triple r@{object}) = pure (Triple(r {object = [show (not $ null object)]}))

hasValue :: forall e. NamedFunction (TripleGetter e) -> NamedFunction (TripleGetter e)
hasValue (NamedFunction nameOfp p) = memorize getter name where

  getter :: TripleGetter e
  getter = p >=> isNothing >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "(hasValue " <> nameOfp <> ")"

-- | Construct a function that returns a bool in Aff, from a TripleGetter.
toBoolean :: forall e. NamedFunction (TripleGetter e) -> RolID -> MonadPerspectives (AjaxAvarCache e) Boolean
toBoolean (NamedFunction nameOfp p) r = do
  result <- p r
  arrWithBool <- pure $ tripleObjects result
  case head arrWithBool of
    Nothing -> pure false
    (Just x) -> pure (x == "true")

rolesOf :: forall e. ContextID -> NamedFunction (TripleGetter e)
rolesOf cid = constructTripleGetterFromArbitraryFunction
  ("model:Perspectives$rolesOf" <> cid) f where
  f :: ObjectsGetter e
  f rolName = getRol rolName cid
-- rolesOf cid = NamedFunction ("rolesOf_" <> cid) \rolname -> applyNamedFunction (constructRolGetter rolname) cid

contains :: forall e. ID -> NamedFunction (TripleGetter e) -> NamedFunction (TripleGetter e)
contains id' (NamedFunction nameOfp p) = constructTripleGetterFromArbitraryFunction ("model:Perspectives$contains" <> id') f where
  f :: ObjectsGetter e
  f id = do
    (Triple{object}) <- p id
    case elemIndex id' object of
      Nothing -> pure ["false"]
      otherwise -> pure ["true"]

-- | Ignore the cache of query results for the given named function, i.e. always compute.
ignoreCache :: forall e. NamedFunction (TripleGetter e) -> NamedFunction (TripleGetter e)
ignoreCache (NamedFunction nameOfp p) = NamedFunction nameOfp go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults false
      result <- p r
      setMemorizeQueryResults remember
      pure result

-- | Use the cache of query results for the given named function.
useCache :: forall e. NamedFunction (TripleGetter e) -> NamedFunction (TripleGetter e)
useCache (NamedFunction nameOfp p) = NamedFunction nameOfp go where
  go r =
    do
      remember <- memorizeQueryResults
      setMemorizeQueryResults true
      result <- p r
      setMemorizeQueryResults remember
      pure result
