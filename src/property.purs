module Perspectives.Property where

import Prelude
import Data.Argonaut (Json, toArray, toBoolean, toNumber, toString)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.JSDate (JSDate, readDate)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Perspectives.Resource (Resource, getPropDefs, representResource)
import Perspectives.ResourceRetrieval (PropDefs(..), AsyncResource)

{-
Property values are represented by Arrays, whatever the cardinality of the property.
We need functions that give us an array of values for a given property for a given resource.
However, we also want to memoize that and track the dependency and provide a method for updating the values.
Therefore, we lift these functions over Locations. Hence, Resources are put into Locations.
However, a property whose range is Resource, must be represented by an Array of Resources - not of Locations!
-}

type PropertyName = String

-- | From a Resource's property definitions, that are Foreign - basically unknown types - read an Array of types
-- | that conform to the return type (in the F monad) of the readf reader function.
{-getGetter :: forall a. (Foreign -> F a) -> PropertyName -> Resource -> Array a
getGetter readf pn (Resource{ propDefs: (PropDefs pd)}) = case lookup pn pd of
  Nothing -> []
  (Just f) -> case runExcept $ traverse readf =<< (readArray f) of
    (Left _) -> []
    (Right n) -> n-}

getGetter :: forall a. (Json -> Maybe a) -> PropertyName -> Resource -> AsyncResource () (Either String (Array a))
getGetter tofn pn r = do
  (PropDefs pd) <- getPropDefs r
  case lookup pn pd of
    Nothing -> pure (Right [])
    -- Dit moet een array zijn gevuld met types die de tofn kan herkennen.
    (Just json) -> case toArray json of
      Nothing -> pure (Left "Not an array!")
      (Just arr) -> case traverse tofn arr of
        Nothing -> pure (Left "Not all elements are of the required type")
        (Just a) -> pure (Right a)

-- | in AsyncResource, retrieve either a String property or an error message.
getString :: PropertyName -> Resource -> AsyncResource () (Either String (Array String))
getString = getGetter toString


-- | in AsyncResource, retrieve either a Number property or an error message.
getNumber :: PropertyName -> Resource -> AsyncResource () (Either String (Array Number))
getNumber = getGetter toNumber

-- | in AsyncResource, retrieve either a Boolean property or an error message.
getBoolean :: PropertyName -> Resource -> AsyncResource () (Either String (Array Boolean))
getBoolean = getGetter toBoolean

-- | in AsyncResource, retrieve either a Date property or an error message.
--getDate :: PropertyName -> Resource -> AsyncResource () (Either String (Array JSDate))
--getDate = getGetter (\json -> Just (readDate $ toForeign json ))

-- | in AsyncResource, retrieve either a Resource property or an error message.
getResource :: PropertyName -> Resource -> AsyncResource () (Either String (Array Resource))
--getResource pn r = map (\rs -> Just $ representResource rs) (getString pn r)
getResource pn r = do
  resIdArray <- getString pn r
  case resIdArray of
    (Left err) -> pure $ Left err
    (Right arr) -> pure $ Right $ map representResource arr
