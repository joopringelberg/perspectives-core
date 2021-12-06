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
import Data.Array (concat, cons, filter, filterA, head, nub, union)
import Data.Foldable (for_)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (values) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Representation.ADT (leavesInADT)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (class RoleClass, bindingOfADT, expansionOfRole, getRole, perspectives, roleAndBinding, roleKindOfRoleType, typeOfRole)
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..), RoleType(..))
import Perspectives.Representation.UserGraph (Edges(..), UserGraph(..))

-------------------------------------------------------------------------------
---- BUILDING THE USER GRAPH
-------------------------------------------------------------------------------
type Node = Tuple RoleType Edges
-- | Build a UserGraph from the DomeinFileRecord kept in PhaseThree State.
buildUserGraph :: PhaseThree UserGraph
buildUserGraph = do
  {enumeratedRoles, calculatedRoles} <- (lift $ gets _.dfr)
  eUserRoles <- pure $ filter ((eq UserRole) <<< _.kindOfRole <<< unwrap) (OBJ.values enumeratedRoles)
  cUserRoles <- pure $ filter ((eq UserRole) <<< _.kindOfRole <<< unwrap) (OBJ.values calculatedRoles)
  (fromEroles :: Array Node) <- lift $ lift
    (traverse userRoleToUserNode eUserRoles >>=
    -- Apply Calculated User Rule.
      traverse (unsafePartial expandSourceToExtension) >>=
        pure <<< concat >>=
          -- Apply Filler Rule.
          traverse expandSourceToBindings >>=
            pure <<< concat)
  (fromCroles :: Array Node) <- lift $ lift
    (traverse userRoleToUserNode cUserRoles >>=
    -- Apply Calculated User Rule.
      traverse (unsafePartial expandSourceToExtension) >>=
        pure <<< concat)
  -- All UserNodes with the same userType should be combined.
  pure $ UserGraph $ EncodableMap $ combineUserNodes (fromEroles <> fromEroles)
  where
    -- Apply the Calculated User Rule: when CU1 has a perspective on U2, we connect
    -- CU1 to U2 in the User Graph. This rule says we should connect extension U1 of CU1
    -- to U2 as well.
    expandSourceToExtension :: Partial => Node -> MonadPerspectives (Array Node)
    expandSourceToExtension t@(Tuple source edges) = case source of
      ENR _ -> pure [t]
      CR croleType -> do
        expansion <- getRole source >>= pure <<< map ENR <<< expansionOfRole
        pure $ flip Tuple edges <$> (cons source expansion)

    -- Apply the Filler Rule: When U1 has a perspective on U2, we connect U1 to U2 in the
    -- User Graph. The Filler Rule says we should connect filler F of U1 to U2 as well
    -- (and apply this rule recursively).
    expandSourceToBindings :: Tuple RoleType Edges -> MonadPerspectives (Array Node)
    expandSourceToBindings t@(Tuple source edges) = case source of
      ENR eroleType -> do
        rAndb <- getEnumeratedRole eroleType >>= roleAndBinding
        recursiveFillers <- bindingOfADT rAndb >>= pure <<< leavesInADT
        pure $ flip Tuple edges <$> (ENR <$> (recursiveFillers <> leavesInADT rAndb))
      CR _ -> pure [t]

    -- Because we expand the user role having a perspective, multiple UserNodes may result.
    userRoleToUserNode :: forall r i. RoleClass r i => r -> MonadPerspectives Node
    userRoleToUserNode r = do
        -- retain the user roles in the enumerated role expansion of the perspectives of r.
        -- edges = nub $ filter isUserNode $ concat $ map (leavesInADT <<< unsafePartial roleRange <<< perspectiveObjectQfd) (perspectives r)
        edges <- nub <$> filterA isUserNode (concat (proximalObject <$> perspectives r))
        pure $ Tuple (typeOfRole r) (Edges edges)
      where
      -- We only lookup in the enumerated role types, because we deal with the enumerated role expansion above.
      isUserNode :: RoleType -> MonadPerspectives Boolean
      isUserNode rt = eq UserRole <$> roleKindOfRoleType rt

      proximalObject :: Perspective -> Array RoleType
      proximalObject (Perspective{roleTypes, object}) = case head roleTypes of
        Just cr@(CR _) -> [cr]
        otherwise -> roleTypes

    combineUserNodes :: Array (Tuple RoleType Edges) -> Map RoleType Edges
    combineUserNodes nodes = execState (for_ nodes combine) empty
      where
        combine :: (Tuple RoleType Edges) -> State (Map RoleType Edges) Unit
        combine (Tuple userType edges) = do
          (intermediate :: Map RoleType Edges) <- get
          case lookup userType intermediate of
            Nothing -> void $ modify (insert userType edges)
            Just (Edges es) -> void $ modify (insert userType (Edges $ union es (unwrap edges)))
