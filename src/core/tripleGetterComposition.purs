module Perspectives.TripleGetterComposition where


import Data.Array (cons, difference, head, nub)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.CoreTypes (Triple(..), TripleGetter, TypedTripleGetter(..))
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (Unit, bind, join, map, pure, unit, ($), (<>))

-- | Compose two queries like composing two function.
-- | `psp:Function -> psp:Function -> psp:Function`
composeTripleGetters :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e ->
  TypedTripleGetter e
composeTripleGetters (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  memorize getter name
    where
    getter :: TripleGetter e
    getter id = do
      t@(Triple{object : objectsOfP}) <- p id
      -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
      (triples :: Array (Triple e)) <- traverse q (difference objectsOfP [id])
      -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
      objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : map getRef (cons t triples)
                    , tripleGetter : getter}

    name :: String
    name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 9 composeTripleGetters as >->

-- | TripleGetter composition where the second operand is treated as lazy
-- | (wrapped in a function). Useful for recursive queries that bottom out
-- | when the first operator yields no results.
-- | `psp:Function -> (Unit -> psp:Function) -> String -> psp:Function`
composeLazy :: forall e.
  TypedTripleGetter e ->
  (Unit -> TypedTripleGetter e) ->
  String ->
  TypedTripleGetter e
composeLazy (TypedTripleGetter nameOfp p) g nameOfg =
  memorize getter name
    where
    getter :: TripleGetter e
    getter id = do
      t@(Triple{object : objectsOfP}) <- p id
      case head objectsOfP of
        Nothing -> pure t
        otherwise -> do
          (TypedTripleGetter nameOfq q) <- pure (g unit)
          -- NOTE: (difference objectsOfP [id]) is our safety catch for cyclic graphs.
          (triples :: Array (Triple e)) <- traverse q (difference objectsOfP [id])
          -- some t' in triples may have zero objects under q. Their subjects contribute nothing to the objects of the composition.
          objects <- pure $ nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object : objects
                        , dependencies : []
                        , supports : map getRef (cons t triples)
                        , tripleGetter : getter}

    name :: String
    name = "(" <>  nameOfp <> " >-> " <> nameOfg <> ")"

infixl 9 composeLazy as >->>
