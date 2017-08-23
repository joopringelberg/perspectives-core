module Perspectives.PropertyComposition where

import Control.Monad.Aff (Aff)
import Data.Array (singleton)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (class Traversable, traverse)
import Prelude (class Bind, bind, id, join, pure, ($), (>=>))

xTox :: forall a b c trav e. Traversable trav => Bind trav =>
  (a -> Aff e (trav b))
  -> (b -> Aff e (trav c))
  -> a
  -> Aff e (trav c)
xTox f g a = do
  x <- f a
  y <- traverse g x
  pure (join y)
-- xTox f g = f >=> traverse g >=> (pure <<< join)

infix 0 xTox as >->

sTop :: forall a b c e.
  (a -> Aff e (Maybe b))
  -> (b -> Aff e (Array c))
  -> a
  -> Aff e (Array c)
sTop f g a = do
  x <- f a
  y <- traverse g (maybeToArray x)
  pure (join y)
  where
    maybeToArray :: forall d. Maybe d -> Array d
    maybeToArray = maybe [] singleton

infix 0 sTop as >->>

pTos :: forall a b c e.
  (a -> Aff e (Array b))
  -> (b -> Aff e (Maybe c))
  -> a
  -> Aff e (Array c)
pTos f g a = do
  x <- f a
  -- x :: Array b
  y <- traverse g x
  -- y :: Array (Maybe c)
  pure $ maybe [] id (traverse id y)

infix 0 pTos as >>->

-- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
-- a = b
-- m = Aff e
-- b = Maybe c
-- t = Array
-- traverse :: (b -> Aff e (Maybe c)) -> Array b -> Aff e (Array (Maybe c))
--
-- t = Array
-- a = Maybe c
-- m = Maybe
-- b = c
-- traverse :: (Maybe c -> Maybe c) -> Array (Maybe c) -> Maybe (Array c)
