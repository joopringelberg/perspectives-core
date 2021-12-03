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

module Perspectives.Representation.UserGraph where

import Prelude

import Data.Array (fromFoldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (keys, lookup)
import Data.Maybe (isJust, maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.Representation.TypeIdentifiers (RoleType)

-------------------------------------------------------------------------------
---- USER GRAPH REPRESENTATION
-------------------------------------------------------------------------------
-- | A UserGraph is purely in terms of Enumerated Role types with kind UserRole.
newtype UserGraph = UserGraph (EncodableMap RoleType Edges)

derive instance genericUserGraph :: Generic UserGraph _

derive instance newtypeUserGraph :: Newtype UserGraph _

instance encodeUserGraph :: Encode UserGraph where
  encode = genericEncode defaultOptions --{unwrapSingleConstructors = true}

instance decodeUserGraph :: Decode UserGraph where
  decode = genericDecode defaultOptions

instance showUserGraph :: Show UserGraph where
  show = genericShow

instance eqUserGraph :: Eq UserGraph where
  eq = genericEq

-------------------------------------------------------------------------------
---- EDGES
-------------------------------------------------------------------------------
newtype Edges = Edges (Array RoleType)

derive instance genericEdges :: Generic Edges _

derive instance newtypeEdges :: Newtype Edges _

instance encodeEdges :: Encode Edges where
  encode = genericEncode defaultOptions --{unwrapSingleConstructors = true}

instance decodeEdges :: Decode Edges where
  decode = genericDecode defaultOptions

instance showEdges :: Show Edges where
  show = genericShow

instance eqEdges :: Eq Edges where
  eq = genericEq

-------------------------------------------------------------------------------
---- FUNCTIONS ON USERGRAPH
-------------------------------------------------------------------------------
usersInGraph :: UserGraph -> Array RoleType
usersInGraph (UserGraph (EncodableMap graph)) = fromFoldable (keys graph)

getUserEdges :: UserGraph -> RoleType -> Edges
getUserEdges (UserGraph (EncodableMap graph)) rt = maybe (Edges []) identity (lookup rt graph)

isInGraph :: RoleType -> UserGraph -> Boolean
isInGraph rt (UserGraph (EncodableMap graph)) = isJust $ lookup rt graph
