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

module Perspectives.DependencyTracking.Array.Trans where

------------------------------------------------------------------------------------------------------------
---- THE ArrayT MONAD TRANSFORMER
------------------------------------------------------------------------------------------------------------

-- | The `ArrayT` monad transformer.
-- |
-- | This monad transformer extends the base monad.

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError, try)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Alternative (class Alternative)
import Control.Plus (class Alt, class Plus)
import Data.Array (catMaybes) as Arr
import Data.Array (concat, head, null, singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

------------------------------------------------------------------------------------------------------------
---- THE ArrayT MONAD TRANSFORMER
------------------------------------------------------------------------------------------------------------

-- | The `ArrayT` monad transformer.
-- |
-- | This monad transformer extends the base monad.
newtype ArrayT m a = ArrayT (m (Array a))

-- | Run a computation in the `ArrayT` monad.
runArrayT :: forall m a. ArrayT m a -> m (Array a)
runArrayT (ArrayT x) = x

derive instance newtypeArrayT :: Newtype (ArrayT m a) _

instance functorArrayT :: Functor m => Functor (ArrayT m) where
  map f (ArrayT ma) = ArrayT (map f <$> ma)

-- | Remove elements from an array which do not contain a value.
catMaybes :: forall f a. Monad f => ArrayT f (Maybe a) -> ArrayT f a
catMaybes (ArrayT ma) = ArrayT $ do
  (as :: Array (Maybe a)) <- ma
  pure (Arr.catMaybes as)

instance applyArrayT :: Monad m => Apply (ArrayT m) where
  apply = applyArrayT_

applyArrayT_ :: forall m a b. Monad m => ArrayT m (a -> b) -> ArrayT m a -> ArrayT m b
applyArrayT_ (ArrayT mfs) (ArrayT mas) = ArrayT do
  (fs :: Array (a -> b)) <- mfs
  (as :: Array a) <- mas
  pure (fs <*> as)

instance applicativeArrayT :: Monad m => Applicative (ArrayT m) where
  pure = ArrayT <<< pure <<< singleton

-- NOTE that the result may contain double entries. We cannot assume Ord for the elements of the arrays, however, nor restrict Bind to such elements.
instance bindArrayT :: Monad m => Bind (ArrayT m) where
  -- bind :: forall a b. ArrayT m a -> (a -> ArrayT m b) -> ArrayT m b
  bind (ArrayT x) f = ArrayT (x >>= g)
    where
      -- g :: Array a -> m (Array b)
      g as = map concat (traverse unwrap (map f as))
      -- g as = map concat (unwrap (traverse f as))

-- h :: forall a b m. Eq b => Monad m => (a -> ArrayT m b) -> Array a -> m (Array b)
-- h f as = map concat (unwrap (traverse f as))
-- h f as = map (foldl union []) (unwrap (traverse f as))

-- x :: forall a m. Applicative m => Array (m (Array a)) -> m (Array a)
-- x = traverse identity >>> map concat

instance monadArrayT :: Monad m => Monad (ArrayT m)

instance monadTransArrayT :: MonadTrans ArrayT where
  lift :: forall m a. Monad m => m a -> ArrayT m a
  lift = ArrayT <<< map singleton

-- | Use `liftArray` to lift a function of type `Array a -> Array b` to a function
-- | with type `ArrayT m a -> ArrayT m b`.
liftArrayFunction :: forall m a b. Monad m => (Array a -> Array b) -> ArrayT m a -> ArrayT m b
liftArrayFunction f as = ArrayT $ runArrayT as >>= (pure <<< f)

instance altArrayT :: (Monad m, MonadError e m) => Alt (ArrayT m) where
  alt a1 a2 = ArrayT $ do
    a1' <- try (runArrayT a1)
    case a1' of
      Left _ -> runArrayT a2
      Right candidates -> if null candidates
          then runArrayT a2
          else pure candidates

instance plusArrayT :: (Monad m, MonadError e m) => Plus (ArrayT m)  where
  empty = ArrayT $ pure []

instance alternativeArrayT :: (Monad m, MonadError e m) => Alternative (ArrayT m)

-- instance monadZeroArrayT :: (Monad m, MonadError e m) => MonadZero (ArrayT m)

instance monadThrowArrayT :: MonadThrow e m => MonadThrow e (ArrayT m) where
  throwError e = lift (throwError e)

instance monadErrorArrayT :: MonadError e m => MonadError e (ArrayT m) where
  catchError (ArrayT ma) h =
    ArrayT (catchError ma (\e -> unwrap $ h e))

instance monadEffectArrayT :: MonadEffect m => MonadEffect (ArrayT m) where
  liftEffect a = lift (liftEffect a)

instance monadAffArrayT ∷ MonadAff m => MonadAff (ArrayT m) where
  liftAff a = lift (liftAff a)

firstOfSequence :: forall f a. Monad f => Array a -> ArrayT f a
firstOfSequence as = case head as of 
  Nothing -> ArrayT $ pure []
  Just a -> ArrayT $ pure [a]