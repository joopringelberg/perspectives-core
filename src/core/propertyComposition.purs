module Perspectives.PropertyComposition where


import Data.Array (cons, difference, nub)
import Data.Traversable (traverse)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, getRef, memorize)
import Prelude (bind, join, pure, ($), (<>), map)

compose :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
compose (NamedFunction nameOfp p) (NamedFunction nameOfq q) =
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

-- type MagicProperty e = Array (Triple e) -> Aff (AjaxAvarCache e) (Array (Triple e))
--
-- magic :: forall e. NamedFunction( TripleGetter e) ->  MagicProperty e
-- magic ntg tripleArr = do
--   x <- traverse (magic' ntg) tripleArr
--   pure $ join x
--   where
--     magic' :: NamedFunction( TripleGetter e) -> Triple e -> Aff (AjaxAvarCache e) (Array (Triple e))
--     magic' (NamedFunction _ q) (Triple{subject, object}) = traverse q (difference object [subject])
