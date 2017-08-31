module Perspectives.LocationT where

import Perspectives.Location
import Control.Alt (class Functor)
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind, (>>=))
import Control.Monad (class Monad, ap)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Newtype (class Newtype)
import Prelude (map, pure, (<$>), (<<<), liftM1)

newtype LocationT m a = LocationT (m (Location a))

-- | Run a computation in the `LocationT` monad.
runLocationT :: forall m a. LocationT m a -> m (Location a)
runLocationT (LocationT x) = x

derive instance newtypeLocationT :: Newtype (LocationT m a) _

instance functorLocationT :: Functor m => Functor (LocationT m) where
  map f (LocationT ma) = LocationT (map f <$> ma)

instance applyLocationT :: Monad m => Apply (LocationT m) where
  apply = ap

instance applicativeLocationT :: Monad m => Applicative (LocationT m) where
  pure = LocationT <<< pure <<< locate

instance bindLocationT :: Monad m => Bind (LocationT m) where
  bind (LocationT x) f = LocationT (x >>= (\y
    -> let g = map f y
        in case locationValue g of
            (LocationT mb) -> mb))

-- bind1 :: forall m a b. Bind m => Applicative m => LocationT m a -> (a -> LocationT m b) -> LocationT m b
-- bind1 (LocationT (x :: m (Location a))) f = LocationT (x >>= (\(y :: Location a)
--   -> let (g :: Location (LocationT m b)) = map f y
--       in case locationValue g of
--           (LocationT mb) -> mb))

instance monadLocationT :: Monad m => Monad (LocationT m)

instance monadTransLocationT :: MonadTrans LocationT where
  lift = LocationT <<< liftM1 locate

instance monadEffLocation :: MonadEff eff m => MonadEff eff (LocationT m) where
  liftEff = lift <<< liftEff

instance monadAffLocation :: MonadAff eff m => MonadAff eff (LocationT m) where
  liftAff = lift <<< liftAff
