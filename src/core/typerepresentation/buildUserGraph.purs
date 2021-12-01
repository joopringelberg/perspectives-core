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

import Control.Monad.State (State, execState, get, gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, filter, nub, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Foreign.Object (Object, empty, insert, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Query.QueryTypes (roleRange)
import Perspectives.Representation.ADT (leavesInADT)
import Perspectives.Representation.Class.Role (class RoleClass, Role(..), expansionOfRole, perspectives)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleKind(..))
import Perspectives.Representation.UserGraph (Edges(..), UserGraph(..), UserNode(..))
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
  -- Multiple Calculated user roles may expand to the same Enumerated user role (and the latter may have perspectives
  -- of itself, too). All UserNodes with the same userType should be combined.
  pure $ UserGraph $ combineUserNodes $ concat $ (map (userRoleToUserNode E enumeratedRoles) eUserRoles) <>
    (map (userRoleToUserNode C enumeratedRoles) cUserRoles)
  where
    -- Because we expand the user role having a perspective, multiple UserNodes may result.
    userRoleToUserNode :: forall r i. RoleClass r i => (r -> Role) -> Object EnumeratedRole -> r -> Array UserNode
    userRoleToUserNode constructor enumeratedRoles r = let
        -- retain the user roles in the enumerated role expansion of the perspectives of r.
        edges = nub $ filter isUserNode $ concat $ map (leavesInADT <<< unsafePartial roleRange <<< perspectiveObjectQfd) (perspectives r)
      in
        -- TODO. Voor goede foutmeldingen zouden we de oorspronkelijke calculated rolename ook willen hebben in UserNode.
        (\userType -> UserNode { userType, edges: Edges edges }) <$> (unsafePartial expansionOfRole (constructor r))
      where
      -- We only lookup in the enumerated role types, because we deal with the enumerated role expansion above.
      isUserNode :: EnumeratedRoleType -> Boolean
      isUserNode (EnumeratedRoleType rt) = maybe false ((eq UserRole) <<< _.kindOfRole <<< unwrap) $ lookup rt enumeratedRoles

    combineUserNodes :: Array UserNode -> Object Edges
    combineUserNodes nodes = execState (for_ nodes combine) empty
      where
        combine :: UserNode -> State (Object Edges) Unit
        combine (UserNode{userType, edges}) = do
          (intermediate :: Object Edges) <- get
          case lookup (unwrap userType) intermediate of
            Nothing -> void $ modify (insert (unwrap userType) edges)
            Just (Edges es) -> void $ modify (insert (unwrap userType) (Edges $ union es (unwrap edges)))
