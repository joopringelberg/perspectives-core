module Perspectives.PropertyComposition where


import Data.Array (cons, difference, nub)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (Triple(..), TripleGetter, TypedTripleGetter(..))
import Perspectives.TripleAdministration (getRef, memorize)
import Prelude (bind, join, pure, ($), (<>), map)

compose :: forall e.
  TypedTripleGetter e ->
  TypedTripleGetter e ->
  TypedTripleGetter e
compose (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) =
  -- TODO: domain2 must subsume range1
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

infixl 9 compose as >->
