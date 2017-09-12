module Perspectives.Property where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, toArray, toBoolean, toNumber, toString)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Perspectives.Location (Location, connectLocations, locate, locationValue)
import Perspectives.LocationT (LocationT)
import Perspectives.Resource (getPropDefs, representResource, ResourceIndex)
import Perspectives.ResourceTypes (Resource, PropDefs(..), AsyncDomeinFileM)

{-
Property values are represented by Arrays, or Maybes.
We need functions that give us an array of values or maybe value for a given property for a given resource, depending
on the property being relational or functional.
However, we also want to memoize that and track the dependency and provide a method for updating the values.
Therefore, we lift these functions over Locations. Hence, Resources are put into Locations.
However, a property whose range is Resource, must be represented by a Maybe Resource value - not by a Location.
-}

type PropertyName = String

type AsyncPropDefsM e = AsyncDomeinFileM (st :: ST ResourceIndex | e)

-- | PluralGetter defined in the monad stack of LocationT (Aff e) (through AsyncPropDefsM, an alias giving specific
-- | effects). Accordingly, getPluralGetter lifts its final result (the Array of a in Aff) into Location.
type PluralGetter a = forall e. Resource -> LocationT (AsyncPropDefsM e) (Array a)

-- | PluralGetter defined in the monad stack of LocationT (Aff e) (through AsyncPropDefsM, an alias giving specific
-- | effects). Accordingly, getPluralGetter lifts its final result (the Array of a in Aff) into Location.
type SingleGetter a = forall e. Resource -> LocationT (AsyncPropDefsM e) (Maybe a)

-- Below are the types without LocationT.
-- type AsyncPropDefs e a = AsyncPropDefsM e a
-- type PluralGetter a = forall e. Resource -> AsyncPropDefs e (Array a)
-- type SingleGetter a = forall e. Resource -> AsyncPropDefs e (Maybe a)

-- | Used as a higher order function of a single argument: a function that maps a specific json type to a value
-- | type, e.g. toString.
-- | Returns a function that takes a property name and returns a single getter for that property.
-- | The getter takes a Resource and returns a computation of a Maybe value in a Location. It can throw one of two errors:
-- | - the value is not an Array;
-- | - not all elements in the Array are of the required type.
-- | The computation is effectful according to LocationT (AsyncPropDefsM e) (and extensible).
getSingleGetter :: forall a.
  (Json -> Maybe a)
  -> PropertyName
  -> SingleGetter a
getSingleGetter tofn pn r = lift $ do
  (PropDefs pd) <- getPropDefs r
  case lookup pn pd of
    -- Property is not available. This is not an error.
    Nothing -> pure Nothing
    -- This must be an array filled with a single value that the tofn recognizes.
    (Just json) -> case toArray json of
      Nothing -> throwError $ error ("getSingleGetter: property " <> pn <> " of resource " <> show r <> " is not an array!" )
      (Just arr) -> case traverse tofn arr of
        Nothing -> throwError $ error ("getSingleGetter: property " <> pn <> " of resource " <> show r <> " has an element that is not of the required type" )
        (Just a) -> pure (head a)

getSingleGetter' :: forall a.
  (Json -> Maybe a)
  -> PropertyName
  -> SingleGetter' a
getSingleGetter' tofn pn r = do
  (PropDefs pd) <- getPropDefs r
  case lookup pn pd of
    -- Property is not available. This is not an error.
    Nothing -> pure Nothing
    -- This must be an array filled with a single value that the tofn recognizes.
    (Just json) -> case toArray json of
      Nothing -> throwError $ error ("getSingleGetter: property " <> pn <> " of resource " <> show r <> " is not an array!" )
      (Just arr) -> case traverse tofn arr of
        Nothing -> throwError $ error ("getSingleGetter: property " <> pn <> " of resource " <> show r <> " has an element that is not of the required type" )
        (Just a) -> pure (head a)

type SingleGetter' a = forall e. Resource -> AsyncPropDefsM e (Maybe a)

-- Deze functie is Kleisli-composable.
liftGetter :: forall a e.
  (Resource -> AsyncPropDefsM e (Maybe a))
  -> (Location (Maybe Resource) -> AsyncPropDefsM e (Location (Maybe a)))
liftGetter g aLoc = case locationValue aLoc of
  Nothing -> pure $ locate Nothing
  (Just a) -> do
    maybeB <- g a
    case maybeB of
      Nothing -> pure (locate Nothing)
      justB -> pure (connectLocations aLoc g (locate justB))

-- | Used as a higher order function of a single argument: a function that maps a specific json type to a value
-- | type, e.g. toString.
-- | Returns a function that takes a property name and returns a plural getter for that property.
-- | The getter takes a Resource and returns a computation of an Array of values in a Location. It can throw one of two errors:
-- | - the value is not an Array;
-- | - not all elements in the Array are of the required type.
-- | The computation is effectful according to LocationT (AsyncPropDefsM e) (and extensible).
getPluralGetter :: forall a.
  (Json -> Maybe a)
  -> PropertyName
  -> PluralGetter a
getPluralGetter tofn pn r = lift $ do
  (PropDefs pd) <- getPropDefs r
  case lookup pn pd of
    -- Property is not available. This is not an error.
    Nothing -> pure []
    -- This must be an array filled with values of the type that the tofn recognizes.
    (Just json) -> case toArray json of
      Nothing ->  throwError $ error ("getPluralGetter: property " <> pn <> " of resource " <> show r <> " is not an array!" )
      (Just arr) -> case traverse tofn arr of
        Nothing ->  throwError $ error ("getPluralGetter: property " <> pn <> " of resource " <> show r <> " does not have all elements of the required type!" )
        (Just a) -> pure a

-- | in AsyncDomeinFile, retrieve either a String or an error message.
getString :: PropertyName -> SingleGetter String
getString = getSingleGetter toString

-- | in AsyncDomeinFile, retrieve either an Array of Strings or an error message.
getStrings :: PropertyName -> PluralGetter String
getStrings = getPluralGetter toString

-- | in AsyncDomeinFile, retrieve either a Number or an error message.
getNumber :: PropertyName -> SingleGetter Number
getNumber = getSingleGetter toNumber

-- | in AsyncDomeinFile, retrieve either an Array of Numbers or an error message.
getNumbers :: PropertyName -> PluralGetter Number
getNumbers = getPluralGetter toNumber

-- | in AsyncDomeinFile, retrieve either a Boolean value or an error message.
getBoolean :: PropertyName -> SingleGetter Boolean
getBoolean = getSingleGetter toBoolean

-- | in AsyncDomeinFile, retrieve either a String or an error message.
getResource :: PropertyName -> SingleGetter Resource
getResource pn mr = do
  resIdArray <- (getPluralGetter toString) pn mr
  case head resIdArray of
    Nothing -> pure Nothing
    (Just id) -> liftEff $ (Just <$> (representResource $ id))

-- | in AsyncDomeinFile, retrieve either an Array of Resources or an error message.
getResources :: PropertyName -> PluralGetter Resource
getResources pn mr = do
  resIdArray <- (getPluralGetter toString) pn mr
  (liftEff $ (traverse representResource resIdArray))

-- | in AsyncDomeinFile, retrieve either a Date property or an error message.
--getDate :: PropertyName -> Resource -> AsyncResource () (Either String (Array JSDate))
--getDate = getGetter (\json -> Just (readDate $ toForeign json ))
