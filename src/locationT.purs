module Perspectives.LocationT where

import Perspectives.Location
import Control.Alt (class Functor)
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind, (>>=))
import Control.Monad (class Monad, ap)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Newtype (class Newtype)
import Prelude (liftM1, map, pure, (<$>), (<<<), bind)

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
  bind (LocationT af) f = LocationT
    (af >>= (\l ->
        let (LocationT af') = f (locationValue l)
        in
          do
          l' <- af'
          _ <- pure (connectLocations l f l')
          af'
          ))

bind2 :: forall a b m. Monad m => LocationT m a
  -> ( a -> LocationT m b)
  -> LocationT m b
bind2 (LocationT af) f = LocationT
  (af >>= (\(l :: Location a) ->
      let (LocationT af') = f (locationValue l)
      in
        do
        (l' :: Location b) <- af'
        _ <- pure (connectLocations l f l')
        af'
        ))

instance monadLocationT :: Monad m => Monad (LocationT m)

instance monadTransLocationT :: MonadTrans LocationT where
  lift = LocationT <<< liftM1 locate

instance monadEffLocation :: MonadEff eff m => MonadEff eff (LocationT m) where
  liftEff = lift <<< liftEff

instance monadAffLocation :: MonadAff eff m => MonadAff eff (LocationT m) where
  liftAff = lift <<< liftAff
