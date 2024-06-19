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
import Data.Array (concat, cons, filter, filterA, foldM, head, nub, union)
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
import Perspectives.Query.QueryTypes (Calculation(..), domain2roleType, range, roleInContext2Role)
import Perspectives.Representation.ADT (commonLeavesInADT)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Role (class RoleClass, Role(..), getRole, perspectives, roleKindOfRoleType, typeOfRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, RoleKind(..), RoleType(..))
import Perspectives.Representation.UserGraph (Edges(..), UserGraph(..))

-- | In this module we don't treat (user) roles as RoleInContexts. This means that Aspect user roles
-- | are not handled correctly; perspectives on Aspect User roles are treated as if they hold for any
-- | occurrence of the User role that is used as Aspect.

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
            pure <<< concat >>=
              -- Apply Inverted Calculated User Rule
              traverse (unsafePartial expandDestinationToExtension))
  (fromCroles :: Array Node) <- lift $ lift
    (traverse userRoleToUserNode cUserRoles >>=
    -- Apply Calculated User Rule.
      traverse (unsafePartial expandSourceToExtension) >>=
        pure <<< concat >>=
          -- Apply Inverted Calculated User Rule
          traverse (unsafePartial expandDestinationToExtension))
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

    -- Partial, because of the embedded case and because domain2roleType is Partial because it just handles
    -- RDOM cases.
    -- | The same result as roleAspects, but not in MonadPerspectives.
    expansionOfRole :: Partial => Role -> Array EnumeratedRoleType
    expansionOfRole (E (EnumeratedRole {id:i})) = [i]
    expansionOfRole (C role@(CalculatedRole {calculation})) = roleInContext2Role <$> (commonLeavesInADT $ domain2roleType $ range $ (case calculation of Q qd -> qd))
    -- calculation role

    -- Apply the Filler Rule: When U1 has a perspective on U2, we connect U1 to U2 in the
    -- User Graph. The Filler Rule says we should connect filler F of U1 to U2 as well
    -- (and apply this rule recursively).
    expandSourceToBindings :: Tuple RoleType Edges -> MonadPerspectives (Array Node)
    expandSourceToBindings t = pure [t]
    -- TODO. Dit is tijdelijk uitgeschakeld bij wijze van experiment. Schakel opnieuw in of verwijder.
    -- expandSourceToBindings t@(Tuple source edges) = case source of
    --   ENR eroleType -> do
    --     rAndb <- getEnumeratedRole eroleType >>= roleAndBinding
    --     recursiveFillers <- bindingOfADT rAndb >>= pure <<< commonLeavesInADT
    --     pure $ flip Tuple edges <$> (ENR <$> (recursiveFillers <> commonLeavesInADT rAndb))
    --   CR _ -> pure [t]

    -- Apply the Inverted Calculated User Rule: When U1 has a perspective on calculated user role C2,
    -- we connect U1 to C2 in the User Graph. The Inverted Calculated User Rule says we should connect
    -- U1 to extension U2 as well.
    expandDestinationToExtension :: Partial => Node -> MonadPerspectives Node
    expandDestinationToExtension t@(Tuple source (Edges edges)) = do
      -- close the Array of RoleTypes in edges under expansionOfRole, while preserving the originals.
      expansion <- foldM
        (\destinations nextDestination ->
          case nextDestination of
            -- Each Enumerated role is in the result on the outset.
            ENR _ -> pure destinations
            CR _ -> getRole nextDestination >>= pure <<< map ENR <<< expansionOfRole
            )
        edges
        edges
      pure $ Tuple source (Edges expansion)

    -- Because we expand the user role having a perspective, multiple UserNodes may result.
    userRoleToUserNode :: forall r i. RoleClass r i => r -> MonadPerspectives Node
    userRoleToUserNode r = do
        -- retain the user roles in the enumerated role expansion of the perspectives of r.
        -- edges = nub $ filter isUserNode $ concat $ map (commonLeavesInADT <<< unsafePartial roleRange <<< perspectiveObjectQfd) (perspectives r)
        edges <- nub <$> filterA isUserNode (concat (proximalObject <$> perspectives r))
        pure $ Tuple (typeOfRole r) (Edges edges)
      where
      -- We only lookup in the enumerated role types, because we deal with the enumerated role expansion above.
      isUserNode :: RoleType -> MonadPerspectives Boolean
      isUserNode rt = eq UserRole <$> roleKindOfRoleType rt

      proximalObject :: Perspective -> Array RoleType
      proximalObject (Perspective{roleTypes}) = case head roleTypes of
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
