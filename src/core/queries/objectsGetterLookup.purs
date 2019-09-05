module Perspectives.ObjectGetterLookup where

import Data.Array (elemIndex, index)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.CoreTypes (RoleGetter, PropertyValueGetter)
import Prelude (Unit, unit)

-- TODO: vervang dit door GLStrMap.
-- | A tuple of two arrays. By construction, the items with the same indices belong to each other as name and RoleGetter.
type RoleGetterCache = (Tuple (Array RoleGetter) (Array String))

type PropertyValueGetterCache = (Tuple (Array PropertyValueGetter) (Array String))

lookupRoleGetterByName :: String -> Maybe RoleGetter
lookupRoleGetterByName name = case elemIndex name (snd roleGetterCache) of
  Nothing -> Nothing
  (Just i) -> case (index (fst roleGetterCache) i :: Maybe RoleGetter) of
    Nothing -> (Nothing :: Maybe RoleGetter)
    (Just og) -> Just og

roleGetterCacheInsert :: String -> RoleGetter -> Unit
roleGetterCacheInsert name getter = case lookupRoleGetterByName name of
  Nothing -> let
    ignore1 = addToArray name (snd roleGetterCache)
    ignore2 = addToArray getter (fst roleGetterCache)
    in unit
  otherwise -> unit

-- | This cache is modified destructively out of sight of Purescript.
roleGetterCache :: RoleGetterCache
roleGetterCache = Tuple [][]

lookupPropertyValueGetterByName :: String -> Maybe PropertyValueGetter
lookupPropertyValueGetterByName name = case elemIndex name (snd propertyValueGetterCache) of
  Nothing -> Nothing
  (Just i) -> case (index (fst propertyValueGetterCache) i :: Maybe PropertyValueGetter) of
    Nothing -> (Nothing :: Maybe PropertyValueGetter)
    (Just og) -> Just og

propertyValueGetterCacheInsert :: String -> PropertyValueGetter -> Unit
propertyValueGetterCacheInsert name getter = case lookupPropertyValueGetterByName name of
  Nothing -> let
    ignore1 = addToArray name (snd propertyValueGetterCache)
    ignore2 = addToArray getter (fst propertyValueGetterCache)
    in unit
  otherwise -> unit

-- | This cache is modified destructively out of sight of Purescript.
propertyValueGetterCache :: PropertyValueGetterCache
propertyValueGetterCache = Tuple [][]

foreign import addToArray :: forall a. a -> Array a -> Array a
