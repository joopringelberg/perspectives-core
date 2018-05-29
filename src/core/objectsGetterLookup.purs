module Perspectives.ObjectGetterLookup where

import Data.Array (elemIndex, index)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.CoreTypes (ObjectsGetter)
import Prelude (Unit, unit, ($))

type ObjectsGetterCache e = (Tuple (Array (OG e)) (Array String))

newtype OG e = OG (ObjectsGetter e)

derive instance newtypeOG :: Newtype (OG e) _

instance eqOG :: Eq (OG e) where
  eq = objectsGettersEqual

lookupObjectsGetterName :: forall e. ObjectsGetter e -> Maybe String
lookupObjectsGetterName getter = case elemIndex (OG getter) (fst objectsGetterCache) of
  Nothing -> Nothing
  (Just i) -> index (snd objectsGetterCache) i

lookupObjectsGetterByName :: forall e. String -> Maybe (ObjectsGetter e)
lookupObjectsGetterByName name = case elemIndex name (snd objectsGetterCache) of
  Nothing -> Nothing
  (Just i) -> case (index (fst objectsGetterCache) i :: Maybe (OG e)) of
    Nothing -> (Nothing :: Maybe (ObjectsGetter e))
    (Just og) -> Just $ unwrap og

objectsGetterCacheInsert :: forall e. String -> ObjectsGetter e -> Unit
objectsGetterCacheInsert name getter = case lookupObjectsGetterByName name of
  Nothing -> let
    ignore1 = addToArray name (snd objectsGetterCache)
    ignore2 = addToArray (OG getter) (fst objectsGetterCache)
    in unit
  otherwise -> unit

objectsGetterCache :: forall e. ObjectsGetterCache e
objectsGetterCache = Tuple [][]

foreign import addToArray :: forall a. a -> Array a -> Array a

foreign import objectsGettersEqual :: forall e. OG e -> OG e -> Boolean
