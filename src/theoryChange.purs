module TheoryChange where


import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.Location (Location, THEORYDELTA, locationValue, setLocationValue)
import Perspectives.Property (AsyncPropDefsM, PropertyName, StackedLocation, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter, StackedMemorizingGetter, getBoolean, getNumber, getResource, getResources, getSingleGetter, getString, getStrings)
import Perspectives.PropertyComposition (memorizeInStackedLocation, memorizeSingleResourceGetter, nestedToStackedLocation, stackedToNestedLocation)
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.SystemQueries (isFunctional, rdfsRange)
import Prelude (Unit, bind, otherwise, pure, ($))

type PropertyAssignment e = AsyncPropDefsM (td :: THEORYDELTA | e) Unit

setSingleProperty :: forall a e. Maybe Resource -> StackedMemorizingSingleGetter a -> Maybe a -> AsyncPropDefsM (td :: THEORYDELTA | e) Unit
setSingleProperty mr propertyGetter value =
  do
    (lma :: Location (Maybe a)) <- stackedToNestedLocation (propertyGetter mr)
    liftEff $ setLocationValue lma value

setPluralProperty :: forall a e. Maybe Resource -> StackedMemorizingPluralGetter a -> Array a -> AsyncPropDefsM (td :: THEORYDELTA | e) Unit
setPluralProperty mr propertyGetter value =
  do
    (lAa :: Location (Array a)) <- stackedToNestedLocation (propertyGetter mr)
    liftEff $ setLocationValue lAa value

setProperty :: forall a e. Maybe Resource -> StackedMemorizingGetter a -> a -> PropertyAssignment e
setProperty mr propertyGetter value =
  do
    (la :: Location a) <- stackedToNestedLocation (propertyGetter mr)
    liftEff $ setLocationValue la value

setPropertyDually :: forall e.
  Maybe Resource
  -> StackedMemorizingGetter (Maybe Resource)
  -> StackedMemorizingGetter (Maybe Resource)
  -> (Maybe Resource)
  -> PropertyAssignment e
setPropertyDually mr propertyGetter inversePropertyGetter value =
  do
    (locSubject :: Location (Maybe Resource)) <- stackedToNestedLocation (propertyGetter mr)
    -- By retrieving the original location of mr from locSubject, we make sure the inverse property
    -- is registered on locSubject.
    _ <- stackedToNestedLocation (inversePropertyGetter (locationValue locSubject))
    liftEff $ setLocationValue locSubject value

-- getPropertyGetterByName :: forall a e. PropertyName -> (Maybe Resource -> StackedLocation e a)
-- getPropertyGetterByName propName = do
--   property <- liftEff $ representResource propName
--   (mustBeSingleGetter :: Maybe Boolean) <- isFunctional property
--   (rangeType :: Maybe String) <- getPropertyJavascriptRangeType property
--   case mustBeSingleGetter of
--     (Just true) ->
--       case rangeType of
--         (Just "String") -> pure $ memorizeInStackedLocation (getString propName)
--         (Just "Boolean") -> pure $ memorizeInStackedLocation (getBoolean propName)
--         (Just "Number") -> pure $ memorizeInStackedLocation $ getNumber propName
--         otherwise -> pure $ memorizeSingleResourceGetter $ getResource propName
--     otherwise ->
--       case rangeType of
--         (Just "String") -> pure $ memorizeInStackedLocation $ getStrings propName
--         otherwise -> pure $ memorizeSingleResourceGetter $ getResources propName
--
-- -- TODO: moet dit in StackedLocation? Misschien niet eens in location?
-- getPropertyJavascriptRangeType :: StackedMemorizingSingleGetter String
-- getPropertyJavascriptRangeType mp = do
--   rangeType <- rdfsRange mp
--   case rangeType of
--     (Just "String") -> pure $ Just "String"
--     (Just "Boolean") -> pure $ Just "Boolean"
--     (Just "Integer") -> pure $ Just "Number"
--     otherwise -> pure $ Just "Resource"
