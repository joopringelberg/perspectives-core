-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Instances.Combinators where

import Control.MonadZero (guard)
import Data.Array (cons, elemIndex, null, union)
import Data.Maybe (maybe)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Prelude (class Monad, class Eq, bind, const, discard, pure, ($), (>=>))

-- | The closure of f, not including the root argument.
-- | The closure of f contains no double entries iff the result of
-- | f contains no double entries.
closure :: forall a m. Eq a => Monad m => (a -> ArrayT m a) -> (a -> ArrayT m a)
closure f = f >=> \a -> ArrayT do
  a' <- runArrayT $ closure f a
  pure $ maybe (cons a a') (const a') (elemIndex a a')

-- | The closure of f, including the root argument.
-- | The closure of f contains no double entries iff the result of
-- | f contains no double entries.
closure_ :: forall a m. Eq a => Monad m => (a -> ArrayT m a) -> (a -> ArrayT m a)
closure_ f a = ArrayT $ do
  r <- runArrayT $ closure f a
  pure $ maybe (cons a r) (const r) (elemIndex a r)

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

-- | Prefer the left solution over the right one.
disjunction :: forall m s o. Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m o) ->
  (s -> ArrayT m o)
disjunction left right id = ArrayT do
  r <- runArrayT $ left id
  if null r
    then runArrayT $ right id
    else pure r

-- | Join the results.
conjunction :: forall m s o. Eq o => Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m o) ->
  (s -> ArrayT m o)
conjunction left right id = ArrayT do
  l <- runArrayT $ left id
  r <- runArrayT $ right id
  pure $ union l r
