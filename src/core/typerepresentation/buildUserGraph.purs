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

module Perspectives.Representation.UserGraph.Build where

import Prelude

import Control.Monad.State (gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, filter)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Foreign.Object (Object, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Query.QueryTypes (roleRange)
import Perspectives.Representation.ADT (leavesInADT)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.Role (class RoleClass, perspectives)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleKind(..), RoleType(..))
import Perspectives.Representation.UserGraph (UserGraph(..), UserNode(..))
import Perspectives.Types.ObjectGetters (perspectiveObjectQfd)

-------------------------------------------------------------------------------
---- BUILDING THE USER GRAPH
-------------------------------------------------------------------------------
-- | Build a UserGraph from the DomeinFileRecord kept in PhaseThree State.
buildUserGraph :: PhaseThree UserGraph
buildUserGraph = do
  {enumeratedRoles, calculatedRoles} <- (lift $ gets _.dfr)
  eUserRoles <- pure $ filter ((eq UserRole) <<< _.kindOfRole <<< unwrap) (values enumeratedRoles)
  cUserRoles <- pure $ filter ((eq UserRole) <<< _.kindOfRole <<< unwrap) (values calculatedRoles)
  pure $ UserGraph $ (map (userRoleToUserNode ENR enumeratedRoles) eUserRoles) <>
    (map (userRoleToUserNode CR enumeratedRoles) cUserRoles)
  where
    userRoleToUserNode :: forall r i. RoleClass r i => (i -> RoleType) -> Object EnumeratedRole -> r -> UserNode
    userRoleToUserNode constructor enumeratedRoles r = UserNode
      { userType: constructor $ identifier r
      , edges: filter isUserNode $ concat $ map (leavesInADT <<< unsafePartial roleRange <<< perspectiveObjectQfd) (perspectives r)
      }
      where
      isUserNode :: EnumeratedRoleType -> Boolean
      isUserNode (EnumeratedRoleType rt) = maybe false ((eq UserRole) <<< _.kindOfRole <<< unwrap) $ lookup rt enumeratedRoles
