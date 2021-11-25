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

module Perspectives.Parsing.Arc.CheckSynchronization where

import Control.Monad.State (State, evalState, get, gets, lift, modify)
import Data.Array (cons, delete, elemIndex, filter, foldl, head, length, null)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for, for_)
import Effect.Class.Console (log)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, RoleType)
import Perspectives.Representation.UserGraph (UserGraph(..), UserNode(..), getUserEdges, usersInGraph)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<<<), (<>), (>), (>>=))

-----------------------------------------------------------
-- PROJECT THE USER ROLE GRAPH
-----------------------------------------------------------
-- | For an EnumeratedRoleType, return a UserGraph that is restricted to users with a perspective on
-- | that role type that allows them to see the instances.
projectForRoleInstanceDeltas :: Partial => EnumeratedRoleType -> PhaseThree UserGraph
projectForRoleInstanceDeltas etype = do
  UserGraph nodes <- (lift $ gets (_.userGraph <<< _.dfr))
  EnumeratedRole{onContextDelta_context, onContextDelta_role} <- (lift $ gets (_.enumeratedRoles <<< _.dfr)) >>= pure <<< fromJust <<< lookup (unwrap etype)
  usersWithAPerspective <- pure $ foldl
    (\collectedUsers (InvertedQuery {users}) -> collectedUsers <> users)
    []
    (onContextDelta_context <> onContextDelta_role)
  pure $ UserGraph $ filter (criterium usersWithAPerspective) nodes
  where
    criterium :: Array RoleType -> UserNode -> Boolean
    criterium usersWithAPerspective (UserNode{ userType }) = isJust $ elemIndex userType usersWithAPerspective

-- | For an EnumeratedRoleType, return a UserGraph that is restricted to users with a perspective on
-- | that role type that allows them to see the binding.
projectForRoleBindingDeltas :: Partial => EnumeratedRoleType -> PhaseThree UserGraph
projectForRoleBindingDeltas etype = do
  UserGraph nodes <- (lift $ gets (_.userGraph <<< _.dfr))
  EnumeratedRole{onRoleDelta_binding, onRoleDelta_binder} <- (lift $ gets (_.enumeratedRoles <<< _.dfr)) >>= pure <<< fromJust <<< lookup (unwrap etype)
  usersWithAPerspective <- pure $ foldl
    (\collectedUsers (InvertedQuery {users}) -> collectedUsers <> users)
    []
    (onRoleDelta_binding <> onRoleDelta_binder)
  pure $ UserGraph $ filter (criterium usersWithAPerspective) nodes
  where
    criterium :: Array RoleType -> UserNode -> Boolean
    criterium usersWithAPerspective (UserNode{ userType }) = isJust $ elemIndex userType usersWithAPerspective

-- | For an EnumeratedRoleType, return a UserGraph that is restricted to users with a perspective on
-- | that role type that allows them to see the binding.
projectForPropertyDeltas :: Partial => EnumeratedPropertyType -> PhaseThree UserGraph
projectForPropertyDeltas etype = do
  UserGraph nodes <- (lift $ gets (_.userGraph <<< _.dfr))
  EnumeratedProperty{onPropertyDelta} <- (lift $ gets (_.enumeratedProperties <<< _.dfr)) >>= pure <<< fromJust <<< lookup (unwrap etype)
  usersWithAPerspective <- pure $ foldl
    (\collectedUsers (InvertedQuery {users}) -> collectedUsers <> users)
    []
    onPropertyDelta
  pure $ UserGraph $ filter (criterium usersWithAPerspective) nodes
  where
    criterium :: Array RoleType -> UserNode -> Boolean
    criterium usersWithAPerspective (UserNode{ userType }) = isJust $ elemIndex userType usersWithAPerspective

-----------------------------------------------------------
-- COMPUTE DISCONNECTED SUBGRAPHS
-----------------------------------------------------------
type UserType = RoleType

type SubGraphState =
	{ currentSubGraph :: Array UserType
	, subGraphs :: Array (Array UserType)
	, visitedNodes :: Array UserType
	, unvisitedNodes :: Array UserType
	}

type UserGroup = Array RoleType

-- | Construct all groups of nodes in the graph that form a subgraph that is not connected to any of the
-- | other subgraphs. At least one subgraph is expected.
findAllSubGraphs :: UserGraph -> (Array UserGroup)
findAllSubGraphs graph = evalState findAllSubGraphs'
  { currentSubGraph: []
  , subGraphs: []
  , visitedNodes: []
  , unvisitedNodes: usersInGraph graph}

  where
  findAllSubGraphs' :: State SubGraphState (Array UserGroup)
  findAllSubGraphs' = do
    {unvisitedNodes, currentSubGraph, subGraphs} <- get
    case head unvisitedNodes of
      Nothing -> pure $ addSubGraph currentSubGraph subGraphs
      Just nextNode -> do
        void $ modify (\s -> s {currentSubGraph = [], subGraphs = addSubGraph currentSubGraph subGraphs})
        buildCurrentSubGraph nextNode
        findAllSubGraphs'
    where
      addSubGraph :: UserGroup -> Array UserGroup -> Array UserGroup
      addSubGraph ug groups = if null ug
        then groups
        else cons ug groups

  buildCurrentSubGraph :: UserType -> State SubGraphState Unit
  buildCurrentSubGraph userType = do
    void $ modify (\s@{currentSubGraph, visitedNodes, unvisitedNodes} -> s
      { currentSubGraph = cons userType currentSubGraph
      , visitedNodes = cons userType visitedNodes
      , unvisitedNodes = delete userType unvisitedNodes})
    unvisitedNodes <- gets _.unvisitedNodes
    void $ for (filter
        (\edge -> isJust $ elemIndex edge unvisitedNodes)
        (getUserEdges graph userType))
      buildCurrentSubGraph

-----------------------------------------------------------
-- CHECKSYNCHRONIZATION
-----------------------------------------------------------
checkSynchronization :: PhaseThree Unit
checkSynchronization = do
  -- For all EnumeratedProperties, project the UserGraph and construct its subGraphs.
  -- Warn the modeller if there are more than one.
  eprops <- (lift $ gets (_.enumeratedProperties <<< _.dfr))
  for_ eprops \prop -> do
    projectedGraph <- unsafePartial projectForPropertyDeltas (identifier prop)
    subGraphs <- pure $ findAllSubGraphs projectedGraph
    if length subGraphs > 1
      then log ("Found unconnected subgraphs for property " <> show (identifier prop) <> ".\n" <> show subGraphs)
      else pure unit
