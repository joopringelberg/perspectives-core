module Perspectives.Property where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Data.Argonaut (toArray, toString)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Perspectives.Resource (PROPDEFS, ResourceDefinitions, getPropDefs)
import Perspectives.ResourceTypes (PropDefs(..), Resource, DomeinFileEffects)

{-
Property values are represented by Arrays, or Maybes.
We need functions that give us an array of values or maybe value for a given property for a given resource, depending
on the property being relational or functional.
However, we also want to memorize that and track the dependency and provide a method for updating the values.
Therefore, we lift these functions over Locations. Hence, Resources are put into Locations.
However, a property whose range is Resource, must be represented by a Maybe Resource value - not by a Location.
-}

type PropertyName = String

type PropDefsEffects e = DomeinFileEffects (st :: ST ResourceDefinitions, prd :: PROPDEFS | e)

type Getter = forall e. Resource -> Aff (PropDefsEffects e) (Array String)

-- | Used as a higher order function of a single argument: a function that maps a specific json type to a value
-- | type, e.g. toString.
-- | Returns a function that takes a property name and returns a single getter for that property.
-- | The getter takes a Resource and returns a computation of a Maybe value in a Location. It can throw one of two errors:
-- | - the value is not an Array;
-- | - not all elements in the Array are of the required type.
-- | The computation has the PropDefsEffects in Aff.
getGetter :: PropertyName -> Getter
getGetter pn r = do
  (PropDefs pd) <- getPropDefs r
  case lookup pn pd of
    -- Property is not available. This is not an error.
    Nothing -> pure []
    -- This must be an array filled with zero or more values that toString recognizes.
    (Just json) -> case toArray json of
      Nothing -> throwError $ error ("getSingleGetter: property " <> pn <> " of resource " <> show r <> " is not an array!" )
      (Just arr) -> case traverse toString arr of
        Nothing -> throwError $ error ("getGetter: property " <> pn <> " of resource " <> show r <> " has an element that is not of the required type" )
        (Just a) -> pure a
