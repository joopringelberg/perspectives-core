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

-- | We store InvertedQueries in four different sets on EnumeratedRoles, and another set in EnumeratedProperty.
-- | Roles can have an __Aspect Context__ that is different from their __Lexical Context__.
-- | We have to take into account this fact when, in runtime, we need to apply an InvertedQuery to a given Delta.
-- | Simply put, a Role in one context is a waystation in different paths than that same role in another Context.
-- |
-- | In this module we analyse the situation for a given pair of role types, and a given set of InvertedQueries.
-- | The result of this analysis is an index that we use to select the right InvertedQueries from a set.

module Perspective.InvertedQuery.Indices where

import Control.Monad.Trans.Class (lift)
import Data.Array (concat)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectives, liftToInstanceLevel, (##=), (###=))
import Perspectives.Instances.ObjectGetters (contextType, roleType)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo')
import Perspectives.Query.QueryTypes (QueryFunctionDescription, RoleInContext(..), domain, isRoleDomain, queryFunction, roleDomain, roleInContext2Role, roleRange)
import Perspectives.Representation.ADT (ADT(..), leavesInADT)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (binding)
import Perspectives.Representation.EnumeratedRole (InvertedQueryKey(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType)
import Perspectives.Types.ObjectGetters (aspectsOfContext, aspectsOfRole, enumeratedRoleContextType)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..))
import Prelude (bind, map, pure, ($), (/=), (<#>), (<$>), (<<<), (>=>), (>>=))

-- | An InvertedQueryKey is a triplet of types. From a RoleBindingDelta we take the role instances and their
-- | context instances.
-- | But an instance can have many types: its base type and all its Aspects. So from a RoleBindingDelta we can
-- | derive three sets of types.
-- | A InvertedQueryKeyCollection represents all seperate triplets that can be formed from these three type sets.
-- | It is, in fact, a triplet of PRODUCTs of types.
-- | When we look for InvertedQueries on an InvertedQueryMap, we have to traverse (in principle) all its keys
-- | and check whether it is a member of the InvertedQueryKeyCollection, collecting InvertedQueries from matched
-- | keys as we go.
-- | Or, equivalently, we generate all keys in the collection and index the map with each of them, joining the results.
-- |
-- |
-- |
-- |
-- |
-- data RuntimeInvertedQueryKey = InvertedQueryKey (Array ContextType) (Array ContextType) (Array EnumeratedRoleType)


-----------------------------------------------------------
-- COMPUTING KEYS IN RUN TIME
-----------------------------------------------------------
-- | Index member `fillsInvertedQueries` of EnumeratedRoleType with this key computed from a RoleBindingDelta
-- | with type SetFirstBinding or ReplaceBinding.
runtimeIndexForFillsQueries :: Partial => RoleBindingDelta -> MonadPerspectives (Array InvertedQueryKey)
runtimeIndexForFillsQueries (RoleBindingDelta{filled, filler, deltaType}) | deltaType /= RemoveBinding = runtimeIndexForFillsQueries' filled

runtimeIndexForFillsQueries' :: RoleInstance -> MonadPerspectives (Array InvertedQueryKey)
runtimeIndexForFillsQueries' filled = do
  filledTypes <- filled ##= roleType >=> liftToInstanceLevel aspectsOfRole
  concat <$> for filledTypes \(filledType :: EnumeratedRoleType) -> do
    fillerTypes <- (getEnumeratedRole >=> pure <<< map roleInContext2Role <<< leavesInADT <<< _.binding <<< unwrap) filledType
    for fillerTypes \fillerType -> do
      filledContextType <- enumeratedRoleContextType filledType
      fillerContextType <- enumeratedRoleContextType fillerType
      pure $ InvertedQueryKey fillerContextType filledContextType filledType

-- | Index member `filledByInvertedQueries` of EnumeratedRoleType with this key computed from a RoleBindingDelta
-- | with type SetFirstBinding or ReplaceBinding.
runtimeIndexForFilledByQueries :: Partial => RoleBindingDelta -> MonadPerspectives (Array InvertedQueryKey)
runtimeIndexForFilledByQueries (RoleBindingDelta{filled, filler, deltaType}) | deltaType /= RemoveBinding = do
  filledTypes <- filled ##= roleType >=> liftToInstanceLevel aspectsOfRole
  concat <$> for filledTypes \(filledType :: EnumeratedRoleType) -> do
    fillerTypes <- (getEnumeratedRole >=> pure <<< map roleInContext2Role <<< leavesInADT <<< _.binding <<< unwrap) filledType
    for fillerTypes \fillerType -> do
      filledContextType <- enumeratedRoleContextType filledType
      fillerContextType <- enumeratedRoleContextType fillerType
      pure $ InvertedQueryKey filledContextType fillerContextType fillerType

-- | Index member `contextInvertedQueries` of EnumeratedRoleType with this key computed from a ContextDelta.
runtimeIndexForContextQueries :: ContextInstance -> MonadPerspectives (Array ContextType)
runtimeIndexForContextQueries contextInstance = contextInstance ##= (contextType >=> liftToInstanceLevel aspectsOfContext)

-- | Index member `invertedQueries` of ContextType with this key computed from an EnumeratedRoleType.
runTimeIndexForRoleQueries :: EnumeratedRoleType -> MonadPerspectives (Array EnumeratedRoleType)
runTimeIndexForRoleQueries roleType = roleType ###= aspectsOfRole

-- | Index member 'onPropertyDelta' of EnumeratedPropertyType with this key computed from a RolePropertyDelta.
-- | Notice that there is no need to scour Aspects of the EnumeratedRoleType. We will find the Aspect that
-- | provided the Property, if indeed the Property is an Aspect Property of the EnumeratedRoleType; but that
-- | will only give us __the same__ inverted queries. Other Aspects of the EnumeratedRoleType will return nothing
-- | we look up inverted queries on the Property. when we
runtimeIndexForPropertyQueries :: EnumeratedRoleType -> String
runtimeIndexForPropertyQueries = unwrap

-----------------------------------------------------------
-- COMPUTING KEYS IN COMPILE TIME
-----------------------------------------------------------
-- | Compute the keys for the filledBy (Binding) step.
-- | Returns a map whose keys identify EnumeratedRoles and whose values are Arrays of keys that the given
-- | (inverted) query should be stored under.
compiletimeIndexForFilledByQueries :: Partial => QueryFunctionDescription -> (PhaseTwo' MonadPerspectives) (Array (Tuple EnumeratedRoleType (Array InvertedQueryKey)))
compiletimeIndexForFilledByQueries qfd | isRoleDomain $ domain qfd = for (leavesInADT $ roleDomain qfd) keysForRoleInContext
  where
    keysForRoleInContext :: RoleInContext -> (PhaseTwo' MonadPerspectives) (Tuple EnumeratedRoleType (Array InvertedQueryKey))
    keysForRoleInContext (RoleInContext{context:startContext, role:startRole}) = do
      (adtBinding :: ADT RoleInContext) <- lift $ lift $ getEnumeratedRole startRole >>= binding
      case queryFunction qfd of
        (DataTypeGetterWithParameter BindingF mContextType) -> case mContextType of
          "" -> pure $ Tuple startRole ((leavesInADT adtBinding) <#> \(RoleInContext{context:endContext, role:endRole}) -> (InvertedQueryKey startContext endContext endRole))
          ctype -> pure $ Tuple startRole ((leavesInADT adtBinding) <#> \(RoleInContext{context:endContext, role:endRole}) -> (InvertedQueryKey startContext (ContextType ctype) endRole))

-- | Compute the keys for the fills (Binder) step.
-- | Returns a map whose keys identify EnumeratedRoles and whose values are Arrays of keys that the given
-- | (inverted) query should be stored under.
compiletimeIndexForFillsQueries :: Partial => QueryFunctionDescription -> (Array (Tuple EnumeratedRoleType (Array InvertedQueryKey)))
compiletimeIndexForFillsQueries qfd | isRoleDomain $ domain qfd = (leavesInADT $ roleDomain qfd) <#>
  keysForRoleInContext
  where
    keysForRoleInContext :: RoleInContext -> (Tuple EnumeratedRoleType (Array InvertedQueryKey))
    keysForRoleInContext (RoleInContext{context:startContext, role:startRole}) = case roleRange qfd of
      ST (RoleInContext{context:endContext, role:endRole}) -> case queryFunction qfd of
        (DataTypeGetterWithTwoParameters GetRoleBindersF _ mContextType) -> case mContextType of
          "" -> Tuple startRole [InvertedQueryKey startContext endContext endRole]
          ctype -> Tuple startRole [InvertedQueryKey startContext (ContextType ctype) endRole]
