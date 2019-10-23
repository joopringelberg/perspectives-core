-- | A location for values that is invisible for the type system.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Location
  ( Location
  , saveInLocation
  , saveInNamedLocation
  , saveResource
  , connectLocations
  , addDependent
  , setUpdateFunction
  , connectLocationsAsInBind
  , runLocation
  , setLocationValue
  , setLocationValue'
  , THEORYDELTA
  , runTHEORYDELTA
  , pureTHEORYDELTA
  , locationValue
  , locationName
  , bindLoc
  , traverseLoc
  , nameFunction
  , functionName
  , nestLocationInMonad
  , memorize
  , locationDependent
  , namepreseveringComposeKleisli
  , (>==>))
where

import Prelude
import Control.Monad.Eff (Eff, kind Effect, runPure)
import Data.Foreign (Foreign, isUndefined, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Perspectives.PropertyNames (getInversePropertyName)

foreign import data Location :: Type -> Type

-- | Creates a Location with a value.
foreign import saveInLocation :: forall a. a -> Location a

-- | Creates a Location with the given name, containing the value.
foreign import saveInNamedLocation :: forall a. String -> a -> Location a

-- | Creates a Location with the given name, containing the Maybe Resource. Connects the location to the Resource.
foreign import saveResource :: forall a. String -> a -> Location a

foreign import locationName :: forall a. Location a -> String

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

foreign import connectLocations :: forall a b. Location a -> String -> Location b -> Location b

foreign import connectLocationsAsInBind :: forall a b f. Location a -> f -> Location b -> Location b


-- | This is a handler.
-- | Consumes a computation that has the THEORYDELTA effect. Returns a computation without
-- | that effect.
foreign import runTHEORYDELTA :: forall r. Eff (td :: THEORYDELTA | r) Unit -> Eff r Unit

-- | This is a handler.
-- | A convenience function which combines `runTHEORYDELTA` with `runPure`, which can be
-- | used when the only required effect is `THEORYDELTA`.
pureTHEORYDELTA :: Eff (td :: THEORYDELTA) Unit -> Unit
pureTHEORYDELTA c = runPure (runTHEORYDELTA c)

foreign import locationValue :: forall a. Location a -> a

foreign import locationDependentAux :: forall a. String -> Location a -> Foreign

foreign import addDependent :: forall a b e. Location a -> String -> Location b -> Eff e Unit

foreign import setUpdateFunction :: forall a b e. Location a -> Eff e b -> Eff e Unit

-- | Do not assume anything about the content of the locations and the function.
locationDependent :: forall a b. String -> Location a -> Maybe (Location b)
locationDependent f loc =
  let d = locationDependentAux f loc
  in
    if isUndefined d then Nothing else Just (unsafeFromForeign d)

foreign import nameFunction :: forall a. String -> a -> a

foreign import functionName :: forall a b. (a -> b) -> String

-- | The resulting query function has the same name as f.
nestLocationInMonad :: forall a b m. Monad m => (a -> m b) -> (Location a -> m (Location b))
nestLocationInMonad f = nameFunction (functionName f) query
  where
    query r = case locationDependent (functionName f) r of
      Nothing -> do
        x <- f (locationValue r)
        pure $ connectLocations r (functionName f) (saveInNamedLocation name x)
      (Just result) -> pure result
      where name = functionName f <> " " <> locationName r

memorize :: forall a b m. Monad m => (a -> m (Location b)) -> (Location a -> m (Location b))
memorize f = nameFunction (functionName f) query
  where
    query r = case locationDependent (functionName f) r of
      Nothing -> do
        (x :: Location b) <- f (locationValue r)
        _ <- pure $ connectLocations x (getInversePropertyName (functionName f)) r
        pure $ connectLocations r (functionName f) x
      (Just result) -> pure result

namepreseveringComposeKleisli :: forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c
namepreseveringComposeKleisli f g = nameFunction (functionName f) (\a -> f a >>= g)

infixr 1 namepreseveringComposeKleisli as >==>

-----------------------------------------------------------------------------------------------
-- | TYPE CLASS INSTANCES
-----------------------------------------------------------------------------------------------

instance showLocation :: Show a => Show (Location a) where
  show l = "loc " <> show (locationValue l)

instance eqLocation :: Eq a => Eq (Location a) where
  eq l1 l2 = eq (locationValue l1) (locationValue l2)

-- | The map function of the Functor instance of Location just applies the function to the content of the location. NOTE: the function must have a name!
instance functorLocation :: Functor Location where
  map = mapLoc

foreign import mapLoc :: forall a b. (a -> b) -> Location a -> Location b

-- | The apply function of the Apply instance of Location takes the function out of the first location and
-- | applies it to the content of the second location.  NOTE: the function must have a name!
instance applyLocation :: Apply Location where
  apply = applyLoc

foreign import applyLoc :: forall a b. Location (a -> b) -> Location a -> Location b

-- | For the Applicative instance of Location we have pure wrap a value in a Location.
instance applicativeLocation :: Applicative Location where
  pure = saveInLocation

foreign import bindLoc :: forall a b. Location a -> (a -> Location b) -> Location b

-- |  NOTE: the function must have a name! Bind will make the location returned by f dependent on la, in: bind la f.
instance bindLocation :: Bind Location where
  bind = bindLoc

instance monadLocation :: Monad Location

-- instance traverseLocation :: Traversable Location where
--   traverse = traverseLoc
--   sequence = traverse1 id

traverseLoc :: forall a b m. Applicative m => Monad m => (a -> m b) -> Location a -> m (Location b)
traverseLoc f loc = case locationDependent (functionName f) loc of
  Nothing ->
    do
      b <- f (locationValue loc)
      pure (connectLocations loc (functionName f) (saveInLocation b))
  (Just bLoc) -> pure bLoc
