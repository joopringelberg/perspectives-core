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

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (guard)
import Data.Array (cons, elemIndex, foldM, foldMap, null, union)
import Data.HeytingAlgebra (not, (&&)) as HA
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, ala, unwrap)
import Perspectives.CoreTypes (MonadPerspectivesQuery)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Persistent (tryGetPerspectEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Prelude (class Eq, class Monad, class Show, bind, const, discard, pure, show, ($), (<$>), (<<<), (==), (>=>), (<*>), (&&), (||))

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

logicalOperation :: forall m s. Monad m =>
  (Value -> Value -> Value) ->
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
logicalOperation f a b c = ArrayT do
  (as :: Array Value) <- runArrayT (a c)
  (bs :: Array Value) <- runArrayT (b c)
  (rs :: Array Value) <- pure $ f <$> as <*> bs
  if null rs
    then pure [Value "false"]
    else pure rs

wrapLogicalOperator :: (Boolean -> Boolean -> Boolean) -> (Value -> Value -> Value)
wrapLogicalOperator g (Value p) (Value q) = Value $ show (g (p == "true") (q == "true"))

logicalAnd :: forall m s. Monad m =>
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
logicalAnd = logicalOperation (wrapLogicalOperator (&&))

logicalOr :: forall m s. Monad m =>
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
logicalOr = logicalOperation (wrapLogicalOperator (||))

exists :: forall m s o. Eq o => Monad m =>
  (s -> ArrayT m o) ->
  (s -> ArrayT m Value)
exists source id = ArrayT do
  r <- runArrayT $ source id
  pure $ [Value $ show $ HA.not $ null r]

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
available source id = ArrayT do
  r <- runArrayT $ source id
  result <- foldM
    (\allAvailable resId -> lift do
      mr <- tryGetPerspectEntiteit (RoleInstance $ unwrap resId)
      case mr of
        Nothing -> do
          mc <- tryGetPerspectEntiteit (ContextInstance $ unwrap resId)
          case mc of
            Nothing -> pure false
            otherwise -> pure allAvailable
        otherwise -> pure allAvailable)
    true
    r
  pure $ [Value $ show (result HA.&& (HA.not (null r)))]

not :: forall m s. Monad m =>
  (s -> ArrayT m Value) ->
  (s -> ArrayT m Value)
not source id = ArrayT do
  r <- runArrayT $ source id
  pure $ (Value <<< show <<< ((==) (Value "false"))) <$> r
