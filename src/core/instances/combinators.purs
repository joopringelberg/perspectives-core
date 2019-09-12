module Perspectives.Instances.Combinators where

import Control.MonadZero (guard)
import Data.Array (cons, elemIndex)
import Data.Maybe (maybe)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Prelude (class Monad, class Eq, bind, const, discard, pure, ($), (>=>))

-- | The closure of f, not including the root argument.
-- | The closure of f contains no double entries iff the result of
-- | f contains no double entries.
closure :: forall a m. Eq a => Monad m => (a -> ArrayT m a) -> (a -> ArrayT m a)
closure f = f >=> \a -> ArrayT do
  a' <- runArrayT $ closure f a
  pure $ maybe a' (const (cons a a')) (elemIndex a a')

-- | The closure of f, including the root argument.
-- | The closure of f contains no double entries iff the result of
-- | f contains no double entries.
closure_ :: forall a m. Eq a => Monad m => (a -> ArrayT m a) -> (a -> ArrayT m a)
closure_ f a = ArrayT $ do
  r <- runArrayT $ closure f a
  pure $ maybe r (const (cons a r)) (elemIndex a r)

filter :: forall m a o. Monad m =>
  (a -> ArrayT m o) ->
  (o -> ArrayT m Boolean) ->
  (a -> ArrayT m o)
filter source criterium a = do
  (r :: o) <- source a
  (passes :: Boolean) <- criterium r
  guard passes
  pure r

filter' :: forall m a o. Monad m =>
  (a -> ArrayT m o) ->
  (o -> Boolean) ->
  (a -> ArrayT m o)
filter' source criterium a = do
  (r :: o) <- source a
  guard (criterium r)
  pure r

cond :: forall m s o. Monad m =>
  (s -> ArrayT m Boolean) ->
  (s -> ArrayT m o) ->
  (s -> ArrayT m o) ->
  (s -> ArrayT m o)
cond condition thenPart elsePart id = do
  passes <- condition id
  if passes then thenPart id else elsePart id
