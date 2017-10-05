module Perspectives.Property where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Data.Argonaut (Json, toArray, toBoolean, toNumber, toString)
import Data.Array (cons, foldr, head)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Perspectives.Location (Location, nameFunction, saveInLocation)
import Perspectives.LocationT (LocationT)
import Perspectives.Resource (PROPDEFS, ResourceIndex, getPropDefs, representResource, representResourceInLocation)
import Perspectives.ResourceTypes (AsyncDomeinFileM, PropDefs(..), Resource, LocationWithResource)

{-
Property values are represented by Arrays, or Maybes.
We need functions that give us an array of values or maybe value for a given property for a given resource, depending
on the property being relational or functional.
However, we also want to memorize that and track the dependency and provide a method for updating the values.
Therefore, we lift these functions over Locations. Hence, Resources are put into Locations.
However, a property whose range is Resource, must be represented by a Maybe Resource value - not by a Location.
-}

type PropertyName = String

type AsyncPropDefsM e = AsyncDomeinFileM (st :: ST ResourceIndex, prd :: PROPDEFS | e)

type StackedLocation e a = LocationT (AsyncPropDefsM e) a

type NestedLocation e a = AsyncPropDefsM e (Location a)

-- | SingleGetter defined in the monad (Aff e) (through AsyncPropDefsM, an alias giving specific
-- | effects).
type SingleGetter a = forall e. Maybe Resource -> (AsyncPropDefsM e) (Maybe a)

-- type MemorizingSingleGetter a = forall e. Location (Maybe Resource) -> (AsyncPropDefsM e) (Location (Maybe a))
type MemorizingSingleGetter a = forall e. Location (Maybe Resource) -> AsyncPropDefsM e (Location (Maybe a))
type StackedMemorizingSingleGetter a = forall e. Maybe Resource -> StackedLocation e (Maybe a)

-- | PluralGetter defined in the monad (Aff e) (through AsyncPropDefsM, an alias giving specific
-- | effects).
type PluralGetter a = forall e. Maybe Resource -> (AsyncPropDefsM e) (Array a)

-- type MemorizingPluralGetter a = forall e. Location (Maybe Resource) -> (AsyncPropDefsM e) (Location (Array a))
type MemorizingPluralGetter a = forall e. Location (Maybe Resource) -> AsyncPropDefsM e (Location (Array a))
type StackedMemorizingPluralGetter a = forall e. Maybe Resource -> StackedLocation e (Array a)

type StackedMemorizingGetter a = forall e. Maybe Resource -> StackedLocation e a

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
getSingleGetter tofn pn res = case res of
  Nothing -> pure Nothing
  (Just r) -> do
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
getPluralGetter tofn pn res = case res of
  Nothing -> pure []
  (Just r) -> do
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
-- getString :: PropertyName -> SingleGetter String
-- getString = getSingleGetter toString

getString :: PropertyName -> SingleGetter String
getString name = nameFunction name (getSingleGetter toString name)

-- | in AsyncDomeinFile, retrieve either an Array of Strings or an error message.
getStrings :: PropertyName -> PluralGetter String
getStrings name = nameFunction name (getPluralGetter toString name)

-- | in AsyncDomeinFile, retrieve either a Number or an error message.
getNumber :: PropertyName -> SingleGetter Number
getNumber name = nameFunction name (getSingleGetter toNumber name)

-- | in AsyncDomeinFile, retrieve either an Array of Numbers or an error message.
getNumbers :: PropertyName -> PluralGetter Number
getNumbers name = nameFunction name (getPluralGetter toNumber name)

-- | in AsyncDomeinFile, retrieve either a Boolean value or an error message.
getBoolean :: PropertyName -> SingleGetter Boolean
getBoolean name = nameFunction name (getSingleGetter toBoolean name)

-- | in AsyncDomeinFile, retrieve either a String or an error message.
getResource :: forall e. PropertyName -> (Maybe Resource) -> AsyncPropDefsM e LocationWithResource
getResource pn' = nameFunction pn' (f pn') where
  f :: PropertyName -> (Maybe Resource) -> AsyncPropDefsM e LocationWithResource
  f pn mr = do
    (maybeId :: Maybe String) <- getSingleGetter toString pn mr
    case maybeId of
      Nothing -> pure $ saveInLocation Nothing
      (Just id) -> liftEff $ representResourceInLocation id

-- | in AsyncDomeinFile, retrieve either an Array of Resources or an error message.
getResources :: PropertyName -> PluralGetter Resource
getResources pn' = (nameFunction pn' (f pn')) where
  f pn mr = do
    resIdArray <- (getPluralGetter toString) pn mr
    (x :: Array (Maybe Resource)) <- liftEff $ (traverse representResource resIdArray)
    pure $ foldr (maybe id cons) [] x

-- | in AsyncDomeinFile, retrieve either a Date property or an error message.
--getDate :: PropertyName -> Resource -> AsyncResource () (Either String (Array JSDate))
--getDate = getGetter (\json -> Just (readDate $ toForeign json ))
