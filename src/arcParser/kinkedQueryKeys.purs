-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.InvertedQuery.Keys

( storeKinkedQuery
, deleteKinkedQuery
, lookupKinkedQueries
, empty
, KinkedQueryKey(..))

where

import Prelude

import Data.Array (cons, delete) as ARR
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Data.EncodableMap (EncodableMap(..), insert, lookup)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType)

-- | A map of complex keys, storing Arrays of integers that are indices
-- | in an Array of QueryWithAKink elements.
-- | RoleKeys are to be used for InvertedQueries (or rather their QueryWithAKink derivates)
-- | for the Value2Role step or the RolGetter step and a few more exotic steps having a
-- | RDOM range.
-- |
-- |
-- |

-----------------------------------------------------------
-- KINKEDQUERYKEY
-----------------------------------------------------------
data KinkedQueryKey =
    RoleKey EnumeratedRoleType
  | ContextKey ContextType
  | FillKey
      { domainContext :: ContextType
      , domainRole :: EnumeratedRoleType
      , rangeContext :: ContextType
      , rangeRole :: EnumeratedRoleType}

derive instance genericKinkedQueryKey :: Generic KinkedQueryKey _
instance eqKinkedQueryKey :: Eq KinkedQueryKey where eq = genericEq
instance ordKinkedQueryKey :: Ord KinkedQueryKey where compare = genericCompare
instance showKinkedQueryKey :: Show KinkedQueryKey where show = genericShow
instance encodeKinkedQueryKey :: Encode KinkedQueryKey where encode = genericEncode defaultOptions
instance decodeKinkedQueryKey :: Decode KinkedQueryKey where decode = genericDecode defaultOptions

-----------------------------------------------------------
-- KINKEDQUERYMAP
-----------------------------------------------------------
type QueryWithAKinkIndex = Int
type KinkedQueryMap =  EncodableMap KinkedQueryKey (Array QueryWithAKinkIndex)

storeKinkedQuery :: KinkedQueryKey -> QueryWithAKinkIndex -> KinkedQueryMap -> KinkedQueryMap
storeKinkedQuery key qwk qwkMap = case lookup key qwkMap of
  Nothing -> insert key [qwk] qwkMap
  Just collection -> insert key (ARR.cons qwk collection) qwkMap

deleteKinkedQuery :: KinkedQueryKey -> QueryWithAKinkIndex -> KinkedQueryMap -> KinkedQueryMap
deleteKinkedQuery key qwk qwkMap = case lookup key qwkMap of
  Nothing -> qwkMap
  Just collection -> insert key (ARR.delete qwk collection) qwkMap

lookupKinkedQueries :: KinkedQueryKey -> KinkedQueryMap -> Array QueryWithAKinkIndex
lookupKinkedQueries key qwkMap = case lookup key qwkMap of
  Nothing -> []
  Just collection -> collection

empty :: KinkedQueryMap
empty = EncodableMap Map.empty
