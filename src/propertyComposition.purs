module Perspectives.PropertyComposition where

import Control.Bind ((>=>))
import Control.Monad (class Monad)
import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Prelude (bind, id, join, pure, ($))

sTos :: forall a b c m. Monad m =>
  (a -> m (Maybe b))
  -> (b -> m (Maybe c))
  -> a
  -> m (Maybe c)
sTos f g = f >=> (maybe (pure Nothing) g)

infix 0 sTos as >->

sTop :: forall a b c m. Monad m =>
  (a -> m (Maybe b))
  -> (b -> m (Array c))
  -> a
  -> m (Array c)
sTop f g a = do
  x <- f a
  case (x :: Maybe b) of
    Nothing -> pure []
    (Just b) -> g b
-- sTop f g = f >=> (maybe (pure []) g)

infix 0 sTop as >->>

pTos :: forall a b c m.  Monad m => Eq c =>
  (a -> m (Array b))
  -> (b -> m (Maybe c))
  -> a
  -> m (Array c)
pTos f g a = do
  x <- f a
  y <- traverse g (x :: Array b)
  pure $ nub $ foldr (maybe id cons) [] (y :: Array (Maybe c))

infix 0 pTos as >>->

pTop :: forall a b c m.  Monad m => Eq c =>
  (a -> m (Array b))
  -> (b -> m (Array c))
  -> a
  -> m (Array c)
pTop f g a = do
  x <- f a
  y <- traverse g (x :: Array b)
  pure $ nub (join (y :: Array (Array c)))
-- xTox f g = f >=> traverse g >=> (pure <<< nub <<< join)

infix 0 pTop as >>->>
