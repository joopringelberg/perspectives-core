module Perspectives.Property where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readArray, readBoolean, readNumber, readString)
import Data.JSDate (JSDate, readDate)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Perspectives.Resource (Resource(..), representResource, PropDefs(..))

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
getGetter :: forall a. (Foreign -> F a) -> PropertyName -> Resource -> Array a
getGetter readf pn (Resource{ propDefs: (PropDefs pd)}) = case lookup pn pd of
  Nothing -> []
  (Just f) -> case runExcept $ traverse readf =<< (readArray f) of
    (Left _) -> []
    (Right n) -> n

-- | Retrieve a string property.
getString :: PropertyName -> Resource -> Array String
getString = getGetter readString

-- | Retrieve a numeric property.
getNumber :: PropertyName -> Resource -> Array Number
getNumber = getGetter readNumber

getBoolean :: PropertyName -> Resource -> Array Boolean
getBoolean = getGetter readBoolean

getDate :: PropertyName -> Resource -> Array JSDate
getDate = getGetter readDate

getResource :: PropertyName -> Resource -> Array Resource
getResource pn r = map (\rs -> representResource rs Nothing) (getString pn r)
