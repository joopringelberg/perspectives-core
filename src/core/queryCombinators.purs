module Perspectives.QueryCombinators where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, difference, elemIndex, findIndex, foldr, head, last, nub, null, singleton, union)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, ObjectsGetter, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), putQueryVariable, readQueryVariable, tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, Subject, Object)
import Perspectives.PerspectEntiteit (getType)
import Perspectives.Property (getRol)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext)
import Perspectives.TripleAdministration (getRef, lookupInTripleIndex, memorize, memorizeQueryResults, setMemorizeQueryResults)
import Perspectives.TripleGetter (constructTripleGetterFromObjectsGetter, constructTripleGetterFromEffectExpression)
import Prelude (bind, const, discard, id, join, map, not, pure, show, ($), (<<<), (<>), (==), (>>=), (>=>))
import Type.Data.Boolean (kind Boolean)

closure :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e
closure (TypedTripleGetter nameOfp p) =
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

-- | The closure of an ObjectsGetter.
closure_ :: forall e.
  ObjectsGetter e ->
  ObjectsGetter e
closure_ p = getter where
  getter :: ObjectsGetter e
  getter id = do
    objectsOfP <- p id
    (results :: Array (Array String)) <- traverse getter (difference objectsOfP [id])
    pure $ nub $ join results

-- | Return the last element in the chain
closure' :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e
closure' (TypedTripleGetter nameOfp p) =
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
  TypedTripleGetter e ->
  TypedTripleGetter e ->
  TypedTripleGetter e
filter (TypedTripleGetter nameOfc criterium) (TypedTripleGetter nameOfp p) =
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
  TypedTripleGetter e ->
  TypedTripleGetter e ->
  MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
concat (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  pure $ memorize getter name
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

-- TODO. Waarom niet memoriseren?
leastCommonAncestor :: forall e. Subject -> Subject -> MonadPerspectives (AjaxAvarCache e) Subject
leastCommonAncestor t1 t2 = do
  (ancestorsOfT1 :: Array ID) <- closure_ (type_ >=> (pure <<< singleton)) t1
  iterateUntilM (isTypeOfT1 ancestorsOfT1) type_ t2
  where
    -- TODO. getPerspectEntiteit moet geen maybe teruggeven, maar een error gooien als
    -- er niets gevonden wordt. Dan kan de maybe hieronder weg.
    type_ :: Subject -> MonadPerspectives (AjaxAvarCache e) Subject
    type_ = ((getPerspectEntiteit :: Subject -> MonadPerspectives (AjaxAvarCache e) (Maybe PerspectContext)) >=> (pure <<< maybe "" (getType :: PerspectContext -> String)))

    isTypeOfT1 :: Array Subject -> Subject -> Boolean
    isTypeOfT1 ancestors superOfT2 = isJust $ elemIndex superOfT2 ancestors


-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isNothing :: forall e. Triple e -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
isNothing (Triple r@{object}) = pure (Triple(r {object = [show (not $ null object)]}))

notEmpty :: forall e. TypedTripleGetter e -> TypedTripleGetter e
notEmpty (TypedTripleGetter nameOfp p) = memorize getter name where

  getter :: TripleGetter e
  getter = p >=> isNothing >=> \(Triple t) -> pure (Triple(t {predicate = name, tripleGetter = getter}))

  name :: String
  name = "(notEmpty " <> nameOfp <> ")"

-- | Construct a function that returns a bool in Aff, from a TripleGetter.
toBoolean :: forall e. TypedTripleGetter e -> RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
toBoolean (TypedTripleGetter nameOfp p) r = do
  result <- p r
  arrWithBool <- pure $ tripleObjects result
  case head arrWithBool of
    Nothing -> pure false
    (Just x) -> pure (x == "true")

-- | This query constructor takes a context id as argument. The query step that results can be applied to a role id
-- | and will result in all instances of that role for the given context.
-- | For domain we just take psp:Context. Range can only be psp:Rol because we have no
-- | other knowledge on it.
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
    case elemIndex id' object of
      Nothing -> pure ["false"]
      otherwise -> pure ["true"]

containsMatching :: forall e. (Subject -> Object -> Boolean) -> String -> TypedTripleGetter e -> TypedTripleGetter e
containsMatching criterium criteriumName (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$contains" <> criteriumName) f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f subject = do
    (Triple{object}) <- p subject
    pure $ maybe ["true"] (const ["false"]) (findIndex (criterium subject) object)

-- | Apply to a query and retrieve a boolean query that returns true iff its subject is a member of the result of
-- | the argument query.
containedIn :: forall e. TypedTripleGetter e -> TypedTripleGetter e
containedIn (TypedTripleGetter nameOfp p) = constructTripleGetterFromEffectExpression ("model:Perspectives$containedIn" <> nameOfp) f where
  f :: (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String))
  f id = do
    (Triple{object}) <- p id
    case elemIndex id object of
      Nothing -> pure ["false"]
      otherwise -> pure ["true"]

lastElement :: forall e. TypedTripleGetter e -> TypedTripleGetter e
lastElement (TypedTripleGetter nameOfp (p :: TripleGetter e)) = constructTripleGetterFromEffectExpression
  ("(lastElement " <> nameOfp <> ")")
  (p >=> pure <<< (maybe [] singleton) <<< last <<< tripleObjects)

-- | Ignore the cache of query results for the given named function, i.e. always compute.
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
useCache :: forall e a. TypedTripleGetter e -> TypedTripleGetter e
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
var :: forall e. String -> TypedTripleGetter e -> TypedTripleGetter e
var name (TypedTripleGetter nameOfp p) = TypedTripleGetter nameOfp go where
  go subject = do
    r <- p subject
    putQueryVariable name (TripleRef{subject: subject, predicate: nameOfp})
    pure r

ref :: forall e. String -> TypedTripleGetter e
ref name = TypedTripleGetter name ref'

ref' :: forall e. String -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)
ref' name = readQueryVariable name >>= \(TripleRef{subject, predicate}) ->
    do
      mref <- lift $ liftEff $ lookupInTripleIndex subject predicate
      unsafePartial $ pure $ fromJust $ mref
