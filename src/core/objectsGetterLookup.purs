module Perspectives.ObjectGetterLookup where

import Data.Array (elemIndex, index)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.CoreTypes (ObjectsGetter)
import Prelude (Unit, unit, ($))

type ObjectsGetterCache = (Tuple (Array OG) (Array String))

newtype OG = OG ObjectsGetter

derive instance newtypeOG :: Newtype OG _

instance eqOG :: Eq OG where
  eq = objectsGettersEqual

lookupObjectsGetterName :: ObjectsGetter -> Maybe String
lookupObjectsGetterName getter = case elemIndex (OG getter) (fst objectsGetterCache) of
  Nothing -> Nothing
  (Just i) -> index (snd objectsGetterCache) i

lookupObjectsGetterByName :: String -> Maybe ObjectsGetter
lookupObjectsGetterByName name = case elemIndex name (snd objectsGetterCache) of
  Nothing -> Nothing
  (Just i) -> case (index (fst objectsGetterCache) i :: Maybe OG) of
    Nothing -> (Nothing :: Maybe ObjectsGetter)
    (Just og) -> Just $ unwrap og

objectsGetterCacheInsert :: String -> ObjectsGetter -> Unit
objectsGetterCacheInsert name getter = case lookupObjectsGetterByName name of
  Nothing -> let
    ignore1 = addToArray name (snd objectsGetterCache)
    ignore2 = addToArray (OG getter) (fst objectsGetterCache)
    in unit
  otherwise -> unit

objectsGetterCache :: ObjectsGetterCache
objectsGetterCache = Tuple [][]

foreign import addToArray :: forall a. a -> Array a -> Array a

foreign import objectsGettersEqual :: OG -> OG -> Boolean
