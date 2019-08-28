module Perspectives.DependencyTracking.Array.Trans where

------------------------------------------------------------------------------------------------------------
---- THE ArrayT MONAD TRANSFORMER
------------------------------------------------------------------------------------------------------------

-- | The `ArrayT` monad transformer.
-- |
-- | This monad transformer extends the base monad.
import Prelude

import Control.Monad.Trans.Class (class MonadTrans)
import Control.MonadZero (class Alternative, class MonadZero)
import Control.Plus (class Alt, class Plus)
import Data.Array (concat, singleton)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)

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

instance applyArrayT :: Monad m => Apply (ArrayT m) where
  apply = applyArrayT_

applyArrayT_ :: forall m a b. Monad m => ArrayT m (a -> b) -> ArrayT m a -> ArrayT m b
applyArrayT_ (ArrayT mfs) (ArrayT mas) = ArrayT do
  fs <- mfs
  as <- mas
  pure (fs <*> as)

instance applicativeArrayT :: Monad m => Applicative (ArrayT m) where
  pure = ArrayT <<< pure <<< singleton

-- NOTE that the result may contain double entries. We cannot assume Ord for the elements of the arrays, however, nor restrict Bind to such elements.
instance bindArrayT :: Monad m => Bind (ArrayT m) where
  bind (ArrayT x) f = ArrayT (x >>= g)
    where
      g as = map concat (traverse unwrap (map f as))

-- x :: forall a m. Applicative m => Array (m (Array a)) -> m (Array a)
-- x = traverse identity >>> map concat

instance monadArrayT :: Monad m => Monad (ArrayT m)

instance monadTransArrayT :: MonadTrans ArrayT where
  lift :: forall m a. Monad m => m a -> ArrayT m a
  lift = ArrayT <<< map singleton

lift' :: forall m a. Monad m => m (Array a) -> ArrayT m a
lift' = ArrayT

-- | Use `liftArray` to lift a function of type `Array a -> Array b` to a function
-- | with type `ArrayT m a -> ArrayT m b`.
liftArrayFunction :: forall m a b. Monad m => (Array a -> Array b) -> ArrayT m a -> ArrayT m b
liftArrayFunction f as = ArrayT $ runArrayT as >>= (pure <<< f)

instance altArrayT :: Monad m => Alt (ArrayT m) where
  alt a1 a2 = ArrayT $ do
    a1' <- runArrayT a1
    a2' <- runArrayT a2
    pure (append a1' a2')

instance plusArrayT :: Monad m => Plus (ArrayT m)  where
  empty = ArrayT $ pure []

instance alternativeArrayT :: Monad m => Alternative (ArrayT m)

instance monadZeroArrayT :: Monad m => MonadZero (ArrayT m)
