-- | A location for values that is invisible for the type system.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Location
  ( Location
  , locate
  , runLocation
  , setLocationValue
  , THEORYDELTA)
where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data Location :: Type -> Type

-- | Creates a Location with a value.
foreign import locate :: forall a. a -> Location a

-- |Given a Location of effects with no return value, run each effect.
foreign import runLocation :: forall e. Location (Eff e Unit) -> Eff e Unit

-- | TYPE CLASS INSTANCES

-- | The map function of the Functor instance of Location just applies the function to the content of the location.
instance functorLocation :: Functor Location where
  map = mapLoc

foreign import mapLoc :: forall a b. (a -> b) -> Location a -> Location b

-- | The apply function of the Apply instance of Location takes the function out of the first location and
-- | applies it to the content of the second location.
instance applyLocation :: Apply Location where
  apply = applyLoc

foreign import applyLoc :: forall a b. Location (a -> b) -> Location a -> Location b

-- | For the Applicative instance of Location we have pure wrap a value in a Location.
instance applicativeLocation :: Applicative Location where
  pure = locate

-- | The THEORYDELTA Effect labels that the inputs and outputs of the network are not consistent.
foreign import data THEORYDELTA :: Effect

foreign import setLocationValue :: forall a e. (Location a) -> a -> Eff (td :: THEORYDELTA | e) Unit
