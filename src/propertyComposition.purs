module Perspectives.PropertyComposition where


import Control.Monad.Aff (Aff)
import Data.Array (cons, difference)
import Data.Traversable (traverse)
import Perspectives.Property (PropDefsEffects)
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
      objects <- pure $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : map getRef (cons t triples)
                    , tripleGetter : getter}

    name :: String
    name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 9 compose as >->

type MagicProperty e = Array (Triple e) -> Aff (PropDefsEffects e) (Array (Triple e))

magic :: forall e. NamedFunction( TripleGetter e) ->  MagicProperty e
magic ntg tripleArr = do
  x <- traverse (magic' ntg) tripleArr
  pure $ join x
  where
    magic' :: NamedFunction( TripleGetter e) -> Triple e -> Aff (PropDefsEffects e) (Array (Triple e))
    magic' (NamedFunction _ q) (Triple{subject, object}) = traverse q (difference object [subject])

-- TODO: weggooien zodra de combinators veranderd zijn.
foreign import unsafeHead :: forall a. Array a -> a
