module Perspectives.PropertyComposition where

import Control.Monad.Aff (Aff)
import Data.Array (cons, foldr)
import Data.Maybe (Maybe(..), maybe)
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
  -- x :: Maybe b
  case x of
    Nothing -> pure []
    (Just b) -> g b
-- sTop f g = f >=> (maybe (pure []) g)

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
  pure $ foldr (maybe id cons) [] y

infix 0 pTos as >>->

-- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
-- a = b
-- m = Aff e
-- b = Maybe c
-- t = Array
-- traverse :: (b -> Aff e (Maybe c)) -> Array b -> Aff e (Array (Maybe c))
