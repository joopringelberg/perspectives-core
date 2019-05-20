module Perspectives.TripleGetterComposition where

import Data.Array (cons, difference, foldM, head, intersect, nub, null, uncons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence) as Trav
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TypedTripleGetter(..), applyTypedTripleGetter, tripleObjects)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (class Eq, class Ord, Unit, bind, join, map, pure, unit, ($), (<$>), (<<<), (<>), (>=>), (>>=))

followedBy :: forall s o a. TypedTripleGetter s o -> (o -> a) -> TypedTripleGetter s a
followedBy (TypedTripleGetter n g) f = TypedTripleGetter n fNag where
  fNag :: TripleGetter s a
  fNag = g >=> \(Triple{subject, predicate, object, dependencies, supports, tripleGetter}) -> pure $ Triple
    { subject: subject
    , predicate: predicate
    , object: map f object
    , supports: supports
    , dependencies: dependencies
    , tripleGetter: fNag
  }

before :: forall s o a. (a -> s) -> TypedTripleGetter s o -> TypedTripleGetter a o
before f (TypedTripleGetter n g) = TypedTripleGetter n fVoorg where
  fVoorg :: TripleGetter a o
  fVoorg a = g (f a) >>= \(Triple{subject, predicate, object, dependencies, supports, tripleGetter}) -> pure $ Triple
    { subject: a
    , predicate: predicate
    , object: object
    , supports: supports
    , dependencies: dependencies
    , tripleGetter: fVoorg
  }

-- | Compose two queries like composing two functions.
unionOfTripleObjects :: forall s o t.
  Eq t =>
  Eq o =>
  Ord o =>
  TypedTripleGetter s t ->
  TypedTripleGetter t o ->
  TypedTripleGetter s o
unionOfTripleObjects (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t) <- p id
      (triples :: Array (Triple t o)) <- Trav.traverse q objectsOfP
      objects <- pure $ nub $ join $ map tripleObjects triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                    , tripleGetter : getter}

    name :: String
    name = nameOfp <> " >-> " <> nameOfq

infixl 9 unionOfTripleObjects as >->

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
lazyUnionOfTripleObjects :: forall s o t.
  Eq t =>
  Eq o =>
  Ord o =>
  TypedTripleGetter s t ->
  (Unit -> TypedTripleGetter t o) ->
  String ->
  TypedTripleGetter s o
lazyUnionOfTripleObjects (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t) <- p id
      case head objectsOfP of
        Nothing -> pure $ typeWithPerspectivesTypes t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
          (triples :: Array (Triple t o)) <- Trav.traverse q (difference objectsOfP [typeWithPerspectivesTypes id])
          -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
          objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objects
                        , dependencies : []
                        , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                        , tripleGetter : getter}

    name :: String
    name = nameOfp <> " >->> " <> nameOfg

infixl 9 lazyUnionOfTripleObjects as >->>

intersectionOfTripleObjects :: forall s o t.
  Eq t =>
  Eq o =>
  TypedTripleGetter s t ->
  TypedTripleGetter t o ->
  TypedTripleGetter s o
intersectionOfTripleObjects (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t) <- p id
      (triples :: Array (Triple t o)) <- Trav.traverse q objectsOfP
      objects <-
        case uncons triples of
          Nothing -> pure []
          (Just {head, tail}) -> foldM
            (\r (Triple{object}) -> pure $ intersect object r)
            (tripleObjects head)
            tail
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                    , tripleGetter : getter}

    name :: String
    name = nameOfp <> " <-< " <> nameOfq

infixl 9 intersectionOfTripleObjects as <-<

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
lazyIntersectionOfTripleObjects :: forall s o t.
  Eq t =>
  Eq o =>
  TypedTripleGetter s t ->
  (Unit -> TypedTripleGetter t o) ->
  String ->
  TypedTripleGetter s o
lazyIntersectionOfTripleObjects (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t) <- p id
      case head objectsOfP of
        Nothing -> pure $ typeWithPerspectivesTypes t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          (triples :: Array (Triple t o)) <- Trav.traverse q objectsOfP
          objects <-
            case uncons triples of
              Nothing -> pure []
              (Just {head, tail}) -> foldM
                (\r (Triple{object}) -> pure $ intersect object r)
                (tripleObjects head)
                tail
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objects
                        , dependencies : []
                        , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                        , tripleGetter : getter}

    name :: String
    name = nameOfp <> " <<-< " <> nameOfg

infixl 9 lazyIntersectionOfTripleObjects as <<-<

-- | Traverse the results of the second TypedTripleGetter with function that yields a TypedTripleGetter.
traverse :: forall s o t.
  Eq t =>
  Eq o =>
  Ord o =>
  (t -> TypedTripleGetter s o) ->
  String ->
  TypedTripleGetter s t ->
  TypedTripleGetter s o
traverse f nameOfF (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (t@(Triple{object : (objectsOfQ :: Array t)}) :: Triple s t) <- q id
      (monadicValues :: Array (MonadPerspectivesQuery (Triple s o))) <- pure $ applyTypedTripleGetter id <<< f <$> objectsOfQ
      (triples :: Array (Triple s o)) <- Trav.sequence monadicValues
      objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                    , tripleGetter : getter}

    name :: String
    name = "traverse(" <>  nameOfF <> ", " <> nameOfq <> ")"

composeMonoidal :: forall s o a.
  TypedTripleGetter s o ->
  (Array o -> a) ->
  String ->
  TypedTripleGetter s a
composeMonoidal (TypedTripleGetter nameOfp p) f n = memorize getter name where
  getter :: TripleGetter s a
  getter id = do
    (t@(Triple{object : objectsOfP}) :: Triple s o) <- p id
    a <- pure $ f objectsOfP
    pure $ Triple { subject: id
                  , predicate : name
                  , object : [a]
                  , dependencies : []
                  , supports : [(getRef (typeWithPerspectivesTypes t))]
                  , tripleGetter : getter}

  name :: String
  name = "composeMonoidal(" <>  nameOfp <> "," <> n <> ")"

-- | Compose two queries. If the left query has a non-empty result, use that result; otherwise use the result of the
-- | right query.
preferLeft :: forall s o.
  TypedTripleGetter s o ->
  (Unit -> TypedTripleGetter s o) ->
  String ->
  TypedTripleGetter s o
preferLeft (TypedTripleGetter nameOfp p) lazyQ nameOfq =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (tp@(Triple{object : objectsOfP}) :: Triple s o) <- p id
      if (null objectsOfP)
        then do
          (TypedTripleGetter _ q) <- pure $ lazyQ unit
          (tq@(Triple{object : objectsOfQ}) :: Triple s o) <- q id
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objectsOfQ
                        , dependencies : []
                        , supports : [(getRef (typeWithPerspectivesTypes tp)), (getRef (typeWithPerspectivesTypes tq))]
                        , tripleGetter : getter}
        else
          pure $ Triple { subject: id
            , predicate : name
            , object : objectsOfP
            , dependencies : []
            , supports : [(getRef (typeWithPerspectivesTypes tp))]
            , tripleGetter : getter}

    name :: String
    name = "preferLeft(" <> nameOfp <> ", " <> nameOfq <> ")"
