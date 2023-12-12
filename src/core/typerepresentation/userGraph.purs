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
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (keys, lookup)
import Data.Maybe (isJust, maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.Representation.TypeIdentifiers (RoleType)
import Simple.JSON (class ReadForeign, class WriteForeign)

-------------------------------------------------------------------------------
---- USER GRAPH REPRESENTATION
-------------------------------------------------------------------------------
-- | A UserGraph is purely in terms of Enumerated Role types with kind UserRole.
newtype UserGraph = UserGraph (EncodableMap RoleType Edges)

derive instance genericUserGraph :: Generic UserGraph _

derive instance newtypeUserGraph :: Newtype UserGraph _

derive newtype instance WriteForeign UserGraph
derive newtype instance ReadForeign UserGraph

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

derive newtype instance WriteForeign Edges
derive newtype instance ReadForeign Edges

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
