module Perspectives.LocationT where

import Perspectives.Location
import Control.Alt (class Functor)
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind, (>>=))
import Control.Monad (class Monad)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Prelude (bind, map, pure, (<$>), (<*>), (<<<), (<>))

newtype LocationT m a = LocationT (m (Location a))

-- | Run a computation in the `LocationT` monad.
runLocationT :: forall m a. LocationT m a -> m (Location a)
runLocationT (LocationT x) = x

derive instance newtypeLocationT :: Newtype (LocationT m a) _

instance functorLocationT :: Functor m => Functor (LocationT m) where
  map f (LocationT ma) = LocationT (map f <$> ma)

instance applyLocationT :: Monad m => Apply (LocationT m) where
  apply = apply1

apply1 :: forall a b m. Monad m =>
  LocationT m ( a -> b)
  -> LocationT m a
  -> LocationT m b
apply1 (LocationT floc) (LocationT ma) = LocationT do
  (f :: Location (a -> b)) <- floc
  (a :: Location a) <- ma
  pure (f <*> a)

instance applicativeLocationT :: Monad m => Applicative (LocationT m) where
  pure = LocationT <<< pure <<< saveInLocation

instance bindLocationT :: Monad m => Bind (LocationT m) where
  bind = bind1

bind1 :: forall a b m. Monad m => LocationT m a
  -> ( a -> LocationT m b)
  -> LocationT m b
bind1 (LocationT af) f = LocationT
  (af >>= (\(l :: Location a) ->
    case locationDependent (functionName f) l of
      Nothing ->
        let (LocationT af') = f (locationValue l)
        in
          do
            (l' :: Location b) <- af'
            pure (connectLocationsAsInBind l f l')
      (Just resultLoc) -> pure resultLoc))

instance monadLocationT :: Monad m => Monad (LocationT m)

instance monadTransLocationT :: MonadTrans LocationT where
  lift = LocationT <<< map saveInLocation

instance monadEffLocation :: MonadEff eff m => MonadEff eff (LocationT m) where
  liftEff = lift <<< liftEff

instance monadAffLocation :: MonadAff eff m => MonadAff eff (LocationT m) where
  liftAff = lift <<< liftAff

foreign import copyToLocation :: forall a b e. LocationT e b -> LocationT e a -> LocationT e b
