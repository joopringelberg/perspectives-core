-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2021 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Data.EncodableMap 
  ( EncodableMap(..)
  , addAll
  , delete
  , empty
  , filterKeys
  , insert
  , keys
  , lookup
  , removeAll
  , union
  , values
  )
  where

import Prelude

import Data.Array (foldr)
import Data.Array.Partial (head, tail)
import Data.List (List)
import Data.Map (Map, fromFoldable, showTree, toUnfoldable, insert, delete, lookup, values, empty, keys, union, filterKeys, unionWith) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign (Foreign, unsafeFromForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (class PrettyPrint)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


newtype EncodableMap k v = EncodableMap (Map.Map k v)

derive instance newtypeEncodableMap :: Newtype (EncodableMap k v) _
instance showEncodableMap :: (Show k, Show v) => Show (EncodableMap k v) where show (EncodableMap m) = show m
instance eqEncodableMap :: (Eq k, Eq v) => Eq (EncodableMap k v) where eq (EncodableMap m1) (EncodableMap m2) = eq m1 m2

instance (WriteForeign k, WriteForeign v) => WriteForeign (EncodableMap k v) where
  writeImpl (EncodableMap m) = writeImpl ((\(Tuple k v) -> [writeImpl k, writeImpl v]) <$> (Map.toUnfoldable m :: Array (Tuple k v)))

instance (Ord k, ReadForeign k, ReadForeign v) => ReadForeign (EncodableMap k v) where
  readImpl f = EncodableMap <<< Map.fromFoldable <$> (traverse
    (\pair -> Tuple <$> (unsafePartial $ readImpl $ head pair) <*> (unsafePartial $ readImpl (head $ tail pair)))
    (unsafeFromForeign f :: Array (Array Foreign)))

instance prettyPrintEncodableMap :: (Show k, Show v) => PrettyPrint (EncodableMap k v) where
  prettyPrint' _ (EncodableMap mp) = Map.showTree mp

instance Functor (EncodableMap k) where
  map f (EncodableMap mp) = EncodableMap $ map f mp

-- instance traversableEncodableMap :: Traversable (EncodableMap k) where
--   traverse f (EncodableMap mp)  = EncodableMap <$> (traverse f mp)
--   sequence (EncodableMap mp) t = EncodableMap <$> (traverse t)
  -- sequence (EncodableMap mp) = EncodableMap <$> traverse identity mp


insert :: forall k v. Ord k => k -> v -> EncodableMap k v -> EncodableMap k v
insert k v mp = EncodableMap $ Map.insert k v (unwrap mp)

delete :: forall k v. Ord k => k -> EncodableMap k v -> EncodableMap k v
delete k mp = EncodableMap $ Map.delete k (unwrap mp)

lookup :: forall k v. Ord k => k -> EncodableMap k v -> Maybe v
lookup k mp = Map.lookup k (unwrap mp)

values :: forall k v. EncodableMap k v -> List v
values (EncodableMap mp) = Map.values mp

empty :: forall k v. EncodableMap k v
empty = EncodableMap Map.empty

keys :: forall k v. EncodableMap k v -> Set k
keys (EncodableMap mp) = Map.keys mp

instance semigroupEncodableMap :: (Ord k, Semigroup v) => Semigroup (EncodableMap k v) where
  append (EncodableMap map1) (EncodableMap map2)= EncodableMap (Map.unionWith append map1 map2)

-- | Add the value to the map for each key.
addAll :: forall key value. Ord key => value -> EncodableMap key value -> Array key -> EncodableMap key value
addAll value = foldr (\key map -> insert key value map)

removeAll :: forall key value. Ord key => value -> EncodableMap key value -> Array key -> EncodableMap key value
removeAll value = foldr (\key map -> delete key map)

union :: forall k v. Ord k => EncodableMap k v -> EncodableMap k v -> EncodableMap k v
union (EncodableMap m1) (EncodableMap m2) = EncodableMap (m1 `Map.union` m2) 

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold.
filterKeys :: forall k. Ord k => (k -> Boolean) -> EncodableMap k ~> EncodableMap k
filterKeys criterium = over EncodableMap (Map.filterKeys criterium)