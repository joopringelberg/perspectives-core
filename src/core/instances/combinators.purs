-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Instances.Combinators where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Trans.Class (lift)
import Control.Alternative (guard, map)
import Data.Array (cons, elemIndex, foldM, foldMap, head, intersect, null, union)
import Data.HeytingAlgebra (not, (&&)) as HA
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, ala, unwrap)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Persistent (tryGetPerspectEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Prelude (class Eq, class Monad, class Show, bind, const, discard, pure, show, ($), (<$>), (<<<), (==), (>=>))

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

filter :: forall e m a o. Monad m => MonadError e m =>
  (a -> ArrayT m o) ->
  (o -> ArrayT m Boolean) ->
  (a -> ArrayT m o)
filter source criterium a = do
  (r :: o) <- source a
  (passes :: Boolean) <- criterium r
  guard passes
  pure r

filter' :: forall e m a o. Monad m => MonadError e m =>
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
orElse :: forall m s o. Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m o) ->
  (s -> ArrayT m o)
orElse left right id = ArrayT do
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

-- | Intersect the results.
intersection :: forall m s o. Eq o => Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m o) ->
  (s -> ArrayT m o)
intersection left right id = ArrayT do
  l <- runArrayT $ left id
  r <- runArrayT $ right id
  pure $ intersect l r

logicalAnd :: forall m s. Monad m =>
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
logicalAnd a b c = ArrayT do
  (as :: Array Value) <- runArrayT (a c)
  (bs :: Array Value) <- runArrayT (b c)
  case head as, head bs of
    Just (Value "true"), Just (Value "true") -> pure [Value "true"]
    _, _ -> pure [Value "false"]

logicalAnd_ :: forall m s. Monad m =>
  (s -> ArrayT m Boolean) ->
  (s -> ArrayT m Boolean) ->
  (s -> ArrayT m Boolean)
logicalAnd_ a b c = ArrayT do
  (as :: Array Boolean) <- runArrayT (a c)
  (bs :: Array Boolean) <- runArrayT (b c)
  case head as, head bs of
    Just true, Just true -> pure [true]
    _, _ -> pure [false]

logicalOr :: forall m s. Monad m =>
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
logicalOr a b c = ArrayT do
  (as :: Array Value) <- runArrayT (a c)
  (bs :: Array Value) <- runArrayT (b c)
  case head as, head bs of
    Just (Value "true"), _ -> pure [Value "true"]
    _, Just (Value "true") -> pure [Value "true"]
    _, _ -> pure [Value "false"]

exists :: forall m s o. Eq o => Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m Value)
exists source id = ArrayT do
  r <- runArrayT $ source id
  pure $ [Value $ show $ HA.not $ null r]

exists' :: forall m s o. Eq o => Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m Boolean)
exists' source id = ArrayT do
  r <- runArrayT $ source id
  pure $ [HA.not $ null r]

-- | Note that if source yields no results, the end result will still be true!
every :: forall m s. Monad m =>
  (s -> ArrayT m Boolean) ->
  (s -> ArrayT m Boolean)
every source id = ArrayT do
  (r :: Array Boolean) <- runArrayT $ source id
  pure [ala Conj foldMap r]

some :: forall m s. Monad m =>
  (s -> ArrayT m Boolean) ->
  (s -> ArrayT m Boolean)
some source id = ArrayT do
  (r :: Array Boolean) <- runArrayT $ source id
  pure [ala Disj foldMap r]

available :: forall s o. Eq o => Newtype o String => Show o =>
  (s -> MonadPerspectivesQuery o) ->
  (s -> MonadPerspectivesQuery Value)
available source id = available_ (source >=> pure <<< unwrap) id

available_ :: forall s.
  (s -> MonadPerspectivesQuery String) ->
  (s -> MonadPerspectivesQuery Value)
available_ source id = ArrayT do
  r <- runArrayT $ source id
  result <- lift $ available' r
  pure $ [Value $ show result]

available' :: Array String -> MonadPerspectives Boolean
available' ids = do
  result <- foldM
    (\allAvailable resId -> do
      mr <- tryGetPerspectEntiteit (RoleInstance resId)
      case mr of
        Nothing -> do
          mc <- tryGetPerspectEntiteit (ContextInstance resId)
          case mc of
            Nothing -> pure false
            _ -> pure allAvailable
        _ -> pure allAvailable)
    true
    ids
  pure (result HA.&& (HA.not (null ids)))

-- | Implements negation by failure in the sense that if the source returns no values, it is interpreted
-- | as `false`, hence `not` returns `true`.
-- | In other words: if we have a Boolean property, the absence of a value for that property is mapped to `false`.
not :: forall m s. Monad m =>
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
not source id = ArrayT do
  (r :: Array Value) <- runArrayT $ source id
  map (Value <<< show) <$> not_ r

not' :: forall m s. Monad m =>
  (s -> ArrayT m Boolean) ->
  (s -> ArrayT m Boolean)
not' source id = ArrayT do
  (r :: Array Boolean) <- runArrayT $ source id
  pure (HA.not <$> r)

-- | Implements negation by failure in the sense that if the source returns no values, it is interpreted
-- | as `false`, hence `not` returns `true`.
-- | In other words: if we have a Boolean property, the absence of a value for that property is understood to be `false`.
-- | not_ [] ==> [true]
-- | not_ [Value "false"] ==> [true]
-- | not_ [Value "true"] ==> [false]
not_ :: forall m. Monad m => Array Value -> m (Array Boolean)
not_ r = if null r
  then pure [true]
  else pure $ ((==) (Value "false")) <$> r
