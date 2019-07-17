module Perspectives.TripleGetterComposition where

import Data.Array (cons, difference, elemIndex, foldM, head, intersect, nub, null, uncons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence) as Trav
import Perspectives.CoreTypes (MonadPerspectivesQuery, StringTripleGetter, StringTypedTripleGetter, Triple(..), TripleGetter, TypedTripleGetter(..), StringTriple, applyTypedTripleGetter, tripleObjects)
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (Unit, bind, join, map, pure, unit, ($), (<$>), (<<<), (<>))

-- | Compose two queries like composing two functions.
unionOfTripleObjects ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
unionOfTripleObjects (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: StringTripleGetter
    getter id = do
      (t@(Triple{object : objectsOfP}) :: StringTriple) <- p id
      (triples :: Array StringTriple) <- Trav.traverse q objectsOfP
      objects <- pure $ nub $ join $ map tripleObjects triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef ( t)) (map getRef ( triples))
                    , tripleGetter : getter}

    name :: String
    name = nameOfp <> " >-> " <> nameOfq

infixl 9 unionOfTripleObjects as >->

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
lazyUnionOfTripleObjects ::
  StringTypedTripleGetter ->
  (Unit -> StringTypedTripleGetter) ->
  String ->
  StringTypedTripleGetter
lazyUnionOfTripleObjects (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: StringTripleGetter
    getter id = do
      (t@(Triple{object : objectsOfP}) :: StringTriple) <- p id
      case head objectsOfP of
        Nothing -> pure $  t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
          (triples :: Array StringTriple) <- Trav.traverse q (difference objectsOfP [ id])
          -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
          objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objects
                        , dependencies : []
                        , supports : cons (getRef ( t)) (map getRef ( triples))
                        , tripleGetter : getter}

    name :: String
    name = nameOfp <> " >->> " <> nameOfg

infixl 9 lazyUnionOfTripleObjects as >->>

intersectionOfTripleObjects ::
  StringTypedTripleGetter ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
intersectionOfTripleObjects (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: StringTripleGetter
    getter id = do
      (t@(Triple{object : objectsOfP}) :: StringTriple) <- p id
      (triples :: Array StringTriple) <- Trav.traverse q objectsOfP
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
                    , supports : cons (getRef ( t)) (map getRef ( triples))
                    , tripleGetter : getter}

    name :: String
    name = nameOfp <> " <-< " <> nameOfq

infixl 9 intersectionOfTripleObjects as <-<

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
lazyIntersectionOfTripleObjects ::
  StringTypedTripleGetter ->
  (Unit -> StringTypedTripleGetter) ->
  String ->
  StringTypedTripleGetter
lazyIntersectionOfTripleObjects (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: StringTripleGetter
    getter id = do
      (t@(Triple{object : objectsOfP}) :: StringTriple) <- p id
      case head objectsOfP of
        Nothing -> pure $  t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          (triples :: Array StringTriple) <- Trav.traverse q objectsOfP
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
                        , supports : cons (getRef ( t)) (map getRef ( triples))
                        , tripleGetter : getter}

    name :: String
    name = nameOfp <> " <<-< " <> nameOfg

infixl 9 lazyIntersectionOfTripleObjects as <<-<

-- | Traverse the results of the second TypedTripleGetter with function that yields a TypedTripleGetter.
traverse :: (String -> StringTypedTripleGetter) ->
  String ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
traverse f nameOfF (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: StringTripleGetter
    getter id = do
      (t@(Triple{object : (objectsOfQ :: Array String)}) :: StringTriple) <- q id
      (monadicValues :: Array (MonadPerspectivesQuery StringTriple)) <- pure $ applyTypedTripleGetter id <<< f <$> objectsOfQ
      (triples :: Array StringTriple) <- Trav.sequence monadicValues
      objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef ( t)) (map getRef ( triples))
                    , tripleGetter : getter}

    name :: String
    name = "traverse(" <>  nameOfF <> ", " <> nameOfq <> ")"

composeMonoidal ::
  StringTypedTripleGetter ->
  (Array String -> String) ->
  String ->
  StringTypedTripleGetter
composeMonoidal (TypedTripleGetter nameOfp p) f n = memorize getter name where
  getter :: TripleGetter String String
  getter id = do
    (t@(Triple{object : objectsOfP}) :: Triple String String) <- p id
    a <- pure $ f objectsOfP
    pure $ Triple { subject: id
                  , predicate : name
                  , object : [a]
                  , dependencies : []
                  , supports : [(getRef ( t))]
                  , tripleGetter : getter}

  name :: String
  name = "composeMonoidal(" <>  nameOfp <> "," <> n <> ")"

-- | Compose two queries. If the left query has a non-empty result, use that result; otherwise use the result of the
-- | right query.
preferLeft ::
  StringTypedTripleGetter ->
  (Unit -> StringTypedTripleGetter) ->
  String ->
  StringTypedTripleGetter
preferLeft (TypedTripleGetter nameOfp p) lazyQ nameOfq =
  memorize getter name
    where
    getter :: StringTripleGetter
    getter id = do
      (tp@(Triple{object : objectsOfP}) :: StringTriple) <- p id
      if (null objectsOfP)
        then do
          (TypedTripleGetter _ q) <- pure $ lazyQ unit
          (tq@(Triple{object : objectsOfQ}) :: StringTriple) <- q id
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objectsOfQ
                        , dependencies : []
                        , supports : [(getRef ( tp)), (getRef ( tq))]
                        , tripleGetter : getter}
        else
          pure $ Triple { subject: id
            , predicate : name
            , object : objectsOfP
            , dependencies : []
            , supports : [(getRef ( tp))]
            , tripleGetter : getter}

    name :: String
    name = "preferLeft(" <> nameOfp <> ", " <> nameOfq <> ")"

unlessFalse :: forall s.
  StringTypedTripleGetter ->
  (Unit -> StringTypedTripleGetter) ->
  String ->
  StringTypedTripleGetter
unlessFalse (TypedTripleGetter nameOfp p) lazyQ nameOfq =
  memorize getter name where
    getter :: StringTripleGetter
    getter id = do
      (tp@(Triple{object : objectsOfP}) :: StringTriple) <- p id
      case (elemIndex "false" objectsOfP) of
        Nothing -> pure $ Triple { subject: id
          , predicate : name
          , object : objectsOfP
          , dependencies : []
          , supports : [(getRef ( tp))]
          , tripleGetter : getter}
        otherwise -> do
          (TypedTripleGetter _ q) <- pure $ lazyQ unit
          (tq@(Triple{object : objectsOfQ}) :: StringTriple) <- q id
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objectsOfQ
                        , dependencies : []
                        , supports : [(getRef ( tp)), (getRef ( tq))]
                        , tripleGetter : getter}

    name :: String
    name = "unlessFalse(" <> nameOfp <> ", " <> nameOfq <> ")"
