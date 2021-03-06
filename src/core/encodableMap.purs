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
, module MapExports)
where

import Prelude

import Data.Array.Partial (head, tail)
import Data.Map (Map, fromFoldable, showTree, toUnfoldable)
import Data.Map (empty) as MapExports
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (class PrettyPrint)


newtype EncodableMap k v = EncodableMap (Map k v)

derive instance newtypeEncodableMap :: Newtype (EncodableMap k v) _
instance showEncodableMap :: (Show k, Show v) => Show (EncodableMap k v) where show (EncodableMap m) = show m
instance eqEncodableMap :: (Eq k, Eq v) => Eq (EncodableMap k v) where eq (EncodableMap m1) (EncodableMap m2) = eq m1 m2

instance encodeEncodableMap :: (Encode k, Encode v) => Encode (EncodableMap k v) where
	encode (EncodableMap m) = encode ((\(Tuple k v) -> [encode k, encode v]) <$> (toUnfoldable m :: Array (Tuple k v)))
instance decodeEncodableMap :: (Ord k, Decode k, Decode v) => Decode (EncodableMap k v) where
  decode f = EncodableMap <<< fromFoldable <$> (traverse
		(\pair -> Tuple <$> (unsafePartial $ decode $ head pair) <*> (unsafePartial $ decode (head $ tail pair)))
		(unsafeFromForeign f :: Array (Array Foreign)))

instance prettyPrintEncodableMap :: (Show k, Show v) => PrettyPrint (EncodableMap k v) where
	prettyPrint' t (EncodableMap mp) = showTree mp
