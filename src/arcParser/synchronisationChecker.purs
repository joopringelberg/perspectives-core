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

import Control.Monad.State (State, evalState, execState, get, gets, lift, modify)
import Data.Array (cons, delete, difference, elemIndex, filter, foldM, concat, head, null, union, fromFoldable)
import Data.Map (filterKeys, values)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.ErrorLogging (warnModeller)
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Context (contextADT, externalRole)
import Perspectives.Representation.Class.PersistentType (getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Class.Role (Role(..), allLocallyRepresentedProperties, allRoles, kindOfRole) 
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleKind(..), RoleType(..), roletype2string)
import Perspectives.Representation.UserGraph (UserGraph(..), getUserEdges, isInGraph, usersInGraph)
import Perspectives.Warning (PerspectivesWarning(..))
import Prelude (Unit, bind, discard, identity, pure, unit, void, ($), (<<<), (<>), (==))

-----------------------------------------------------------
-- PROJECT THE USER ROLE GRAPH
-----------------------------------------------------------
-- | The startpoints of the projection are the user role types with a modifying perspective.
newtype UserGraphProjection = UserGraphProjection
  { startpoints :: Array RoleType
  , graph :: UserGraph
  , modifiedUserRole :: Maybe RoleType
  }

-- | For an EnumeratedRoleType, return a UserGraph that is restricted to users with a perspective on
-- | that role type that allows them to see the instances.
projectForRoleInstanceDeltas :: Partial => EnumeratedRoleType -> Object (Array InvertedQuery) -> PhaseThree UserGraphProjection
projectForRoleInstanceDeltas etype onContextDelta_role = do
  UserGraph (EncodableMap edgesObject) <- (lift $ gets (_.userGraph <<< _.dfr))
  EnumeratedRole{contextInvertedQueries} <- lift $ lift $ getEnumeratedRole etype
  invertedQueries <- pure $ (maybe [] identity (lookup (unwrap etype) contextInvertedQueries) <> maybe [] identity (lookup (unwrap etype) onContextDelta_role))
  -- Expand to EnumeratedRoleTypes, because the UserGraph is in terms of EnumeratedRoleTypes, too.
  users <- usersWithAPerspective invertedQueries
  startpoints <- usersWithAModifyingPerspective invertedQueries
  pure $ UserGraphProjection
    { startpoints
    , graph: UserGraph $ EncodableMap $ filterKeys (criterium users) edgesObject
    , modifiedUserRole: Nothing
    }
  where
    criterium :: Array RoleType -> RoleType -> Boolean
    criterium usersWithAPerspective' userType = isJust $ elemIndex userType usersWithAPerspective'

-- | For an EnumeratedRoleType, return a UserGraph that is restricted to users with a perspective on
-- | that role type that allows them to see the binding.
-- | Because we include Aspects, roles may be in another namespace than the model.
projectForRoleBindingDeltas :: Partial => EnumeratedRoleType -> ContextType -> PhaseThree UserGraphProjection
projectForRoleBindingDeltas etype ctype = do
  UserGraph (EncodableMap edgesObject) <- (lift $ gets (_.userGraph <<< _.dfr))
  EnumeratedRole{fillerInvertedQueries, filledInvertedQueries} <- lift $ lift $ getEnumeratedRole etype
  -- We start from RoleInContext{context: ctype, role: etype}. We want all InvertedQueries that go to any other
  -- RoleInContext.
  invertedQueries1 <- pure $ concat $ fromFoldable $ values $ filterKeys (\(InvertedQueryKey startctxt _ _) -> startctxt == ctype) (unwrap fillerInvertedQueries)
  invertedQueries2 <- pure $ concat $ fromFoldable $ values $ filterKeys (\(InvertedQueryKey startctxt _ _) -> startctxt == ctype) (unwrap filledInvertedQueries)
  invertedQueries <- pure $ invertedQueries1 <> invertedQueries2
  users <- usersWithAPerspective invertedQueries
  startpoints <- usersWithAModifyingPerspective invertedQueries
  pure $ UserGraphProjection
    { startpoints
    , graph: UserGraph $ EncodableMap $ filterKeys (criterium users) edgesObject
    , modifiedUserRole: Nothing
    }
  where
    criterium :: Array RoleType -> RoleType -> Boolean
    criterium usersWithAPerspective' userType = isJust $ elemIndex userType usersWithAPerspective'

-- | For an EnumeratedProperty§Type, return a UserGraph that is restricted to users with a perspective on
-- | that property type.
projectForPropertyDeltas :: Partial => EnumeratedPropertyType -> EnumeratedRoleType -> PhaseThree UserGraphProjection
projectForPropertyDeltas ePropType erole = do
  UserGraph (EncodableMap edgesObject) <- (lift $ gets (_.userGraph <<< _.dfr))
  -- We cannot rely on looking up in the model in state, because Aspect properties
  -- are included. But the entire synchronization check (including this function) is run
  -- in the context of a withDomeinFile call, hence we can just retrieve the property
  -- using getProperty
  EnumeratedProperty{onPropertyDelta} <- lift $ lift $ getEnumeratedProperty ePropType
  users <- usersWithAPerspective (maybe [] identity (lookup (unwrap erole) onPropertyDelta))
  startpoints <- usersWithAModifyingPerspective (maybe [] identity (lookup (unwrap erole) onPropertyDelta))
  eroleRep <- lift $ lift $ getEnumeratedRole erole
  pure $ UserGraphProjection
    { startpoints
    , graph: UserGraph $ EncodableMap $ filterKeys (criterium users) edgesObject
    , modifiedUserRole: if UserRole == kindOfRole eroleRep
      then Just $ ENR erole
      else Nothing
    }
  where
    criterium :: Array RoleType -> RoleType -> Boolean
    criterium usersWithAPerspective' userType = isJust $ elemIndex userType usersWithAPerspective'


usersWithAPerspective :: Partial => Array InvertedQuery -> PhaseThree (Array RoleType)
usersWithAPerspective invertedQueries = foldM
  (\collectedUsers (InvertedQuery {users}) -> pure $ users `union` collectedUsers
  )
  []
  invertedQueries

usersWithAModifyingPerspective :: Partial => Array InvertedQuery -> PhaseThree (Array RoleType)
usersWithAModifyingPerspective invertedQueries = foldM
  (\collectedUsers (InvertedQuery {users, modifies}) -> if modifies
    then pure $ users `union` collectedUsers
    else pure collectedUsers
  )
  []
  invertedQueries

getRoleFromState :: Partial => RoleType -> PhaseThree Role
getRoleFromState rt = do
  {enumeratedRoles, calculatedRoles} <- (lift $ gets _.dfr)
  case lookup (roletype2string rt) enumeratedRoles, lookup (roletype2string rt) calculatedRoles of
    Just er, Nothing -> pure $ E er
    Nothing, Just cr -> pure $ C cr


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
        (unwrap $ getUserEdges graph userType))
      buildCurrentSubGraph

-----------------------------------------------------------
-- CHECKALLSTARTPOINTS
-----------------------------------------------------------
-- From each node with a modifiying perspective, walk the graph in a depth-first
-- manner and afterwards check whether all nodes have been visited.
-- Collect visited nodes in state to prevent looping.
-- Collect starting nodes that connect completely for optimalization.
-- Collect failed nodes with the nodes that cannot be reached.

type ConnectionState =
  { visitedNodes :: UserGroup
  , completelyConnectedNodes :: UserGroup
  , failures :: Array MissingConnections}

type MissingConnections = Tuple UserType UserGroup

checkAllStartpoints :: UserGraphProjection -> Array MissingConnections
checkAllStartpoints (UserGraphProjection{startpoints, graph, modifiedUserRole}) = _.failures $ execState (for_ startpoints checkStartPoint)
  { visitedNodes: []
  , completelyConnectedNodes: []
  , failures: []}
  where
    checkStartPoint :: UserType -> State ConnectionState Unit
    checkStartPoint startpoint = do
      void $ modify \s -> s {visitedNodes = case modifiedUserRole of
        Nothing -> []
        Just ur -> if ur == startpoint
          then []
          else [ur]}
      walkTheTree startpoint
      {visitedNodes} <- get
      -- Comparing lengths will not do as the modifiedUserRole may or may not occur in the graph, but will occurr in
      -- the visited nodes.
      unvisitedNodes <- pure (difference (usersInGraph graph) visitedNodes)
      if null unvisitedNodes
        then void $ modify \s@{completelyConnectedNodes} -> s {completelyConnectedNodes = cons startpoint completelyConnectedNodes}
        else void $ modify \s@{failures} -> s { failures = cons (Tuple startpoint (difference (usersInGraph graph) visitedNodes)) failures}
      where
      walkTheTree :: UserType -> State ConnectionState Unit
      walkTheTree currentNode = if isInGraph currentNode graph
        then do
          {visitedNodes, completelyConnectedNodes} <- get
          if isJust $ elemIndex currentNode visitedNodes
            then pure unit
            else if isJust $ elemIndex currentNode completelyConnectedNodes
              then void $ modify \s -> s {visitedNodes = usersInGraph graph}
              else do
                void $ modify \s -> s {visitedNodes = visitedNodes `union` [currentNode]}
                for_ (unwrap $ getUserEdges graph currentNode) walkTheTree
        else pure unit

-----------------------------------------------------------
-- CHECKSYNCHRONIZATION
-----------------------------------------------------------
checkSynchronization :: PhaseThree Unit
checkSynchronization = do
  -- For all EnumeratedProperties, project the UserGraph and construct its subGraphs.
  -- Warn the modeller if there are more than one.
  {contexts, enumeratedRoles} <- lift $ gets _.dfr
  for_ enumeratedRoles \r@(EnumeratedRole{id:roleId, properties}) -> do
    allLocalProps <- lift $ lift $ allLocallyRepresentedProperties (ST roleId)
    for_ allLocalProps \propType -> case propType of
      ENP propId -> do
        projectedGraph <- unsafePartial projectForPropertyDeltas propId roleId
        case checkAllStartpoints projectedGraph of
          none | null none -> pure unit
          failures -> lift $ lift $ for_ failures \(Tuple source destinations) -> warnModeller (PropertySynchronizationIncomplete propId source destinations)
      otherwise -> pure unit
    pure unit
  for_ contexts \c@(Context{id, roleInvertedQueries}) -> do
    -- All roles, including Aspect roles, but without the External Role.
    allLocalRoles <- lift $ lift $ allRoles (contextADT c)
    for_ (cons (ENR $ externalRole c) allLocalRoles) \roleType -> case roleType of
      ENR roleId -> do
        -- CHECK INVERTED QUERIES FOR 'CONTEXT' AND '<ROLE>' OPERATORS
        projectedGraph <- unsafePartial projectForRoleInstanceDeltas roleId roleInvertedQueries
        case checkAllStartpoints projectedGraph of
          none | null none -> pure unit
          failures -> lift $ lift $ for_ failures \(Tuple source destinations) -> warnModeller (RoleSynchronizationIncomplete roleId source destinations)
        -- CHECK INVERTED QUERIES FOR 'BINDING' AND 'BINDER' OPERATORS
        projectedGraph' <- unsafePartial projectForRoleBindingDeltas roleId id
        case checkAllStartpoints projectedGraph' of
          none | null none -> pure unit
          failures -> lift $ lift $ for_ failures \(Tuple source destinations) -> warnModeller (RoleBindingSynchronizationIncomplete roleId source destinations)
      otherwise -> pure unit
 