module Perspectives.TripleGetterComposition where

import Data.Array (cons, difference, elemIndex, foldM, head, intersect, nub, null, uncons)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse) as Trav
import Perspectives.CoreTypes (Triple(..), TripleGetter, TypedTripleGetter(..), tripleObjects, type (**>))
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (class Ord, class Eq, Unit, bind, join, map, pure, unit, ($), (<>))
import Unsafe.Coerce (unsafeCoerce)

-- | Compose two queries like composing two functions.
unionOfTripleObjects :: forall s o p. Ord p => Newtype s String =>
  (s **> o) ->
  (o **> p) ->
  (s **> p)
unionOfTripleObjects (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s p
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s o) <- p id
      (triples :: Array (Triple o p)) <- Trav.traverse q objectsOfP
      objects <- pure $ nub $ join $ map tripleObjects triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : cons (getRef (unsafeCoerce t)) (map getRef (unsafeCoerce triples))
                    , tripleGetter : getter}

    name :: String
    name = nameOfp <> " >-> " <> nameOfq

infixl 9 unionOfTripleObjects as >->

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
lazyUnionOfTripleObjects :: forall s o. Eq s => Ord o => Newtype s String =>
  (s **> o) ->
  (Unit -> (s **> o)) ->
  String ->
  (s **> o)
lazyUnionOfTripleObjects (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter (id :: s) = do
      (t@(Triple{object : objectsOfP}) :: Triple s o) <- p id
      case head objectsOfP of
        Nothing -> pure $  t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
          (triples :: Array (Triple s o)) <- Trav.traverse q (difference (unsafeCoerce objectsOfP) [unsafeCoerce id])
          -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
          objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objects
                        , dependencies : []
                        , supports : cons (getRef (unsafeCoerce t)) (map getRef (unsafeCoerce triples))
                        , tripleGetter : getter}

    name :: String
    name = nameOfp <> " >->> " <> nameOfg

infixl 9 lazyUnionOfTripleObjects as >->>

intersectionOfTripleObjects :: forall s o p. Eq p => Newtype s String =>
  (s **> o) ->
  (o **> p) ->
  (s **> p)
intersectionOfTripleObjects (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s p
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s o) <- p id
      (triples :: Array (Triple o p)) <- Trav.traverse q objectsOfP
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
                    , supports : cons (getRef (unsafeCoerce t)) (map getRef (unsafeCoerce triples))
                    , tripleGetter : getter}

    name :: String
    name = nameOfp <> " <-< " <> nameOfq

infixl 9 intersectionOfTripleObjects as <-<

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
lazyIntersectionOfTripleObjects :: forall s o. Eq o => Newtype s String =>
  (s **> o) ->
  (Unit -> (o **> o)) ->
  String ->
  (s **> o)
lazyIntersectionOfTripleObjects (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: TripleGetter s o
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s o) <- p id
      case head objectsOfP of
        Nothing -> pure t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          (triples :: Array (Triple o o)) <- Trav.traverse q objectsOfP
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
                        , supports : cons (getRef (unsafeCoerce t)) (map getRef (unsafeCoerce triples))
                        , tripleGetter : getter}

    name :: String
    name = nameOfp <> " <<-< " <> nameOfg

infixl 9 lazyIntersectionOfTripleObjects as <<-<

-- | Traverse the results of the second TypedTripleGetter with function that yields a TypedTripleGetter.
-- traverse :: forall s o a b. (b -> (a **> o)) ->
--   String ->
--   (a **> b) ->
--   (s **> o)
-- traverse f nameOfF (TypedTripleGetter nameOfq q) =
--   memorize getter name
--     where
--     getter :: TripleGetter a o
--     getter (id :: a) = do
--       (t@(Triple{object : (objectsOfQ :: Array b)}) :: Triple a b) <- q id
--       (monadicValues :: Array (MonadPerspectivesQuery (Triple a o))) <- pure $ applyTypedTripleGetter id <<< f <$> objectsOfQ
--       (triples :: Array (Triple a o)) <- Trav.sequence monadicValues
--       (objects :: Array o) <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
--       pure $ Triple { subject: (id :: a)
--                     , predicate : name
--                     , object : objects
--                     , dependencies : []
--                     , supports : cons (getRef (unsafeCoerce t)) (map getRef (unsafeCoerce triples))
--                     , tripleGetter : getter}
--
--     name :: String
--     name = "traverse(" <>  nameOfF <> ", " <> nameOfq <> ")"

composeMonoidal :: forall s o. Newtype s String =>
  (s **> o) ->
  (Array o -> o) ->
  String ->
  (s **> o)
composeMonoidal (TypedTripleGetter nameOfp p) f n = memorize getter name where
  getter :: TripleGetter s o
  getter id = do
    (t@(Triple{object : objectsOfP}) :: Triple s o) <- p id
    a <- pure $ f objectsOfP
    pure $ Triple { subject: id
                  , predicate : name
                  , object : [a]
                  , dependencies : []
                  , supports : [(getRef (unsafeCoerce t))]
                  , tripleGetter : getter}

  name :: String
  name = "composeMonoidal(" <>  nameOfp <> "," <> n <> ")"

-- | Compose two queries. If the left query has a non-empty result, use that result; otherwise use the result of the
-- | right query.
preferLeft :: forall s o. Newtype s String =>
  (s **> o) ->
  (Unit -> (s **> o)) ->
  String ->
  (s **> o)
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
                        , supports : [(getRef (unsafeCoerce tp)), (getRef (unsafeCoerce tq))]
                        , tripleGetter : getter}
        else
          pure $ Triple { subject: id
            , predicate : name
            , object : objectsOfP
            , dependencies : []
            , supports : [(getRef (unsafeCoerce tp))]
            , tripleGetter : getter}

    name :: String
    name = "preferLeft(" <> nameOfp <> ", " <> nameOfq <> ")"

unlessFalse :: forall s o. Newtype o String => Newtype s String =>
  (s **> o) ->
  (Unit -> (s **> o)) ->
  String ->
  (s **> o)
unlessFalse (TypedTripleGetter nameOfp p) lazyQ nameOfq =
  memorize getter name where
    getter :: TripleGetter s o
    getter id = do
      (tp@(Triple{object : objectsOfP}) :: Triple s o) <- p id
      case (elemIndex "false" (map unwrap objectsOfP)) of
        Nothing -> pure $ Triple { subject: id
          , predicate : name
          , object : objectsOfP
          , dependencies : []
          , supports : [(getRef (unsafeCoerce tp))]
          , tripleGetter : getter}
        otherwise -> do
          (TypedTripleGetter _ q) <- pure $ lazyQ unit
          (tq@(Triple{object : objectsOfQ}) :: Triple s o) <- q id
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objectsOfQ
                        , dependencies : []
                        , supports : [(getRef (unsafeCoerce tp)), (getRef (unsafeCoerce tq))]
                        , tripleGetter : getter}

    name :: String
    name = "unlessFalse(" <> nameOfp <> ", " <> nameOfq <> ")"
