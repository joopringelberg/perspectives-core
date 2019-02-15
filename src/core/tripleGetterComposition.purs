module Perspectives.TripleGetterComposition where


import Data.Array (cons, difference, head, nub)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (Triple(..), TripleGetter, TypedTripleGetter(..))
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (Unit, bind, join, map, pure, unit, ($), (<>), class Eq)

-- | Compose two queries like composing two functions.
-- | `psp:Function -> psp:Function -> psp:Function`
composeTripleGetters :: forall s o t e.
  Eq t =>
  Eq o =>
  Newtype s String =>
  Newtype t String =>
  Newtype o String =>
  TypedTripleGetter s t e ->
  TypedTripleGetter t o e ->
  TypedTripleGetter s o e
composeTripleGetters (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter s o e
    getter id = do
      (t@(Triple{object : objectsOfP}) :: Triple s t e) <- p id
      -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
      (triples :: Array (Triple t o e)) <- traverse q (difference objectsOfP [wrap $ unwrap id])
      -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
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

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
composeLazy :: forall s o t e.
  Eq t =>
  Eq o =>
  Newtype s String =>
  Newtype t String =>
  Newtype o String =>
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
          (triples :: Array (Triple t o e)) <- traverse q (difference objectsOfP [wrap $ unwrap id])
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
