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
  , setLocationValue'
  , THEORYDELTA
  , pureTHEORYDELTA)
where

import Prelude
import Control.Monad.Eff (Eff, kind Effect, runPure)

foreign import data Location :: Type -> Type

-- | Creates a Location with a value.
foreign import locate :: forall a. a -> Location a

-- | The location holds a computation with effects (and yields unit).
-- | Returns a computation (yielding value unit in the Eff Monad) that, when run, runs the computation in
-- | the location. Each time a new computation is set into the Location, it will be run again,
-- | because runLocation has subscribed a function to it (the location) that does so (runs the inner computation).
foreign import runLocation :: forall e. Location (Eff e Unit) -> Eff e Unit

-- | The THEORYDELTA Effect labels that the inputs and outputs of the network are not consistent.
-- | This comes about because the axioms of the theory have changed, but the theorems have not yet been recalculated.
foreign import data THEORYDELTA :: Effect

-- | This is an Action.
-- | Returns a computation that has the THEORYDELTA effect, i.e. it will change the theory by changing an axiom when it is run.
foreign import setLocationValue :: forall a e. (Location a) -> a -> Eff (td :: THEORYDELTA | e) Unit

setLocationValue' :: forall a. (Location a) -> a -> Eff (td :: THEORYDELTA) Unit
setLocationValue' l a = setLocationValue l a

-- | This is a handler.
-- | Consumes a computation that has the THEORYDELTA effect. Returns a computation without
-- | that effect.
foreign import runTHEORYDELTA :: forall r. Eff (td :: THEORYDELTA | r) Unit -> Eff r Unit

-- | This is a handler.
-- | A convenience function which combines `runTHEORYDELTA` with `runPure`, which can be
-- | used when the only required effect is `THEORYDELTA`.
pureTHEORYDELTA :: Eff (td :: THEORYDELTA) Unit -> Unit
pureTHEORYDELTA c = runPure (runTHEORYDELTA c)

-----------------------------------------------------------------------------------------------
-- | TYPE CLASS INSTANCES
-----------------------------------------------------------------------------------------------

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
