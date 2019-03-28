module Perspectives.TripleGetterComposition where

import Data.Array (cons, difference, head, nub)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence) as Trav
import Perspectives.CoreTypes (Triple(..), TripleGetter, TypedTripleGetter(..), MonadPerspectivesQuery, applyTypedTripleGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (Unit, bind, join, map, pure, unit, ($), (<>), class Eq, (>=>), (>>=), (<$>), (<<<))

followedBy :: forall s o a e. TypedTripleGetter s o e -> (o -> a) -> TypedTripleGetter s a e
followedBy (TypedTripleGetter n g) f = TypedTripleGetter n fNag where
  fNag :: TripleGetter s a e
  fNag = g >=> \(Triple{subject, predicate, object, dependencies, supports, tripleGetter}) -> pure $ Triple
    { subject: subject
    , predicate: predicate
    , object: map f object
    , supports: supports
    , dependencies: dependencies
    , tripleGetter: fNag
  }

before :: forall s o a e. (a -> s) -> TypedTripleGetter s o e -> TypedTripleGetter a o e
before f (TypedTripleGetter n g) = TypedTripleGetter n fVoorg where
  fVoorg :: TripleGetter a o e
  fVoorg a = g (f a) >>= \(Triple{subject, predicate, object, dependencies, supports, tripleGetter}) -> pure $ Triple
    { subject: a
    , predicate: predicate
    , object: object
    , supports: supports
    , dependencies: dependencies
    , tripleGetter: fVoorg
  }

-- | Compose two queries like composing two functions.
composeTripleGetters :: forall s o t e.
  Eq t =>
  Eq o =>
  TypedTripleGetter s t e ->
  TypedTripleGetter t o e ->
  TypedTripleGetter s o e
composeTripleGetters (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s o e
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t e) <- p id
      (triples :: Array (Triple t o e)) <- Trav.traverse q objectsOfP
      objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                    , tripleGetter : getter}

    name :: String
    name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 9 composeTripleGetters as >->

-- | Traverse the results of the second TypedTripleGetter with function that yields a TypedTripleGetter.
traverse :: forall s o t e.
  Eq t =>
  Eq o =>
  (t -> TypedTripleGetter s o e) ->
  String ->
  TypedTripleGetter s t e ->
  TypedTripleGetter s o e
traverse f nameOfF (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s o e
    getter id = do
      (t@(Triple{object : (objectsOfQ :: Array t)}) :: Triple s t e) <- q id
      (monadicValues :: Array (MonadPerspectivesQuery (AjaxAvarCache e) (Triple s o e))) <- pure $ applyTypedTripleGetter id <<< f <$> objectsOfQ
      (triples :: Array (Triple s o e)) <- Trav.sequence monadicValues
      objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                    , tripleGetter : getter}

    name :: String
    name = "(" <>  nameOfF <> " >-> " <> nameOfq <> ")"

composeMonoidal :: forall s o a e.
  TypedTripleGetter s o e ->
  (Array o -> a) ->
  String ->
  TypedTripleGetter s a e
composeMonoidal (TypedTripleGetter nameOfp p) f n = memorize getter name where
  getter :: TripleGetter s a e
  getter id = do
    (t@(Triple{object : objectsOfP}) :: Triple s o e) <- p id
    a <- pure $ f objectsOfP
    pure $ Triple { subject: id
                  , predicate : name
                  , object : [a]
                  , dependencies : []
                  , supports : [(getRef (typeWithPerspectivesTypes t))]
                  , tripleGetter : getter}

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> n <> ")"

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
composeLazy :: forall s o t e.
  Eq t =>
  Eq o =>
  TypedTripleGetter s t e ->
  (Unit -> TypedTripleGetter t o e) ->
  String ->
  TypedTripleGetter s o e
composeLazy (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: TripleGetter s o e
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t e) <- p id
      case head objectsOfP of
        Nothing -> pure $ typeWithPerspectivesTypes t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
          (triples :: Array (Triple t o e)) <- Trav.traverse q (difference objectsOfP [typeWithPerspectivesTypes id])
          -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
          objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objects
                        , dependencies : []
                        , supports : cons (getRef (typeWithPerspectivesTypes t)) (map getRef (typeWithPerspectivesTypes triples))
                        , tripleGetter : getter}

    name :: String
    name = "(" <>  nameOfp <> " >-> " <> nameOfg <> ")"

infixl 9 composeLazy as >->>
