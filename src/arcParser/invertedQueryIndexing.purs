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
import Perspectives.CoreTypes (MonadPerspectives, liftToInstanceLevel, (###=), (##=))
import Perspectives.Instances.ObjectGetters (contextType, roleType)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo')
import Perspectives.Query.QueryTypes (QueryFunctionDescription, RoleInContext(..), domain, isRoleDomain, queryFunction, roleDomain, roleInContext2Role, roleRange)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (binding)
import Perspectives.Representation.EnumeratedRole (InvertedQueryKey(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType)
import Perspectives.Types.ObjectGetters (contextAspectsClosure, enumeratedRoleContextType, roleAspectsClosure)
import Perspectives.TypesForDeltas (RoleBindingDelta(..))
import Prelude (bind, map, pure, ($), (<#>), (<$>), (<<<), (>=>), (>>=))

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

-----------------------------------------------------------
-- COMPUTING KEYS IN RUN TIME
-----------------------------------------------------------
-- | Index member `filledInvertedQueries` of EnumeratedRoleType with this key computed from a RoleBindingDelta
-- | with type SetFirstBinding or ReplaceBinding. We look in the **filled** EnumeratedRoleType!
runtimeIndexForFillsQueries :: Partial => RoleBindingDelta -> MonadPerspectives (Array InvertedQueryKey)
runtimeIndexForFillsQueries (RoleBindingDelta{filled, filler, deltaType}) {-| deltaType /= RemoveBinding-} = runtimeIndexForFillsQueries' filled

runtimeIndexForFillsQueries' :: RoleInstance -> MonadPerspectives (Array InvertedQueryKey)
runtimeIndexForFillsQueries' filled = do
  filledTypes <- filled ##= roleType >=> liftToInstanceLevel roleAspectsClosure
  concat <$> for filledTypes \(filledType :: EnumeratedRoleType) -> do
    fillerTypes <- (getEnumeratedRole >=> pure <<< map roleInContext2Role <<< allLeavesInADT <<< _.binding <<< unwrap) filledType
    for fillerTypes \fillerType -> do
      filledContextType <- enumeratedRoleContextType filledType
      fillerContextType <- enumeratedRoleContextType fillerType
      pure $ InvertedQueryKey fillerContextType filledContextType filledType

-- | Index member `fillerInvertedQueries` of EnumeratedRoleType with this key computed from a RoleBindingDelta
-- | with type SetFirstBinding or ReplaceBinding.
runtimeIndexForFilledByQueries :: Partial => RoleBindingDelta -> MonadPerspectives (Array InvertedQueryKey)
runtimeIndexForFilledByQueries (RoleBindingDelta{filled, filler, deltaType}) {-| deltaType /= RemoveBinding-} = do
  filledTypes <- filled ##= roleType >=> liftToInstanceLevel roleAspectsClosure
  concat <$> for filledTypes \(filledType :: EnumeratedRoleType) -> do
    fillerTypes <- (getEnumeratedRole >=> pure <<< map roleInContext2Role <<< allLeavesInADT <<< _.binding <<< unwrap) filledType
    for fillerTypes \fillerType -> do 
      filledContextType <- enumeratedRoleContextType filledType
      fillerContextType <- enumeratedRoleContextType fillerType
      pure $ InvertedQueryKey filledContextType fillerContextType fillerType

-- | Index member `contextInvertedQueries` of EnumeratedRoleType with this key computed from a ContextDelta.
runtimeIndexForContextQueries :: ContextInstance -> MonadPerspectives (Array ContextType)
runtimeIndexForContextQueries contextInstance = contextInstance ##= (contextType >=> liftToInstanceLevel contextAspectsClosure)

-- | Index member `roleInvertedQueries` of ContextType with this key computed from an EnumeratedRoleType.
runTimeIndexForRoleQueries :: EnumeratedRoleType -> MonadPerspectives (Array EnumeratedRoleType)
runTimeIndexForRoleQueries roleType = roleType ###= roleAspectsClosure

-- | Index member 'onPropertyDelta' of EnumeratedPropertyType with these keys.
-- |
runtimeIndexForPropertyQueries :: EnumeratedRoleType -> MonadPerspectives (Array String)
runtimeIndexForPropertyQueries eroleType = map unwrap <$> (eroleType ###= roleAspectsClosure)

-----------------------------------------------------------
-- COMPUTING KEYS IN COMPILE TIME
-----------------------------------------------------------
-- | Compute the keys for the filledBy (Binding) step.
-- | Returns a map whose keys identify EnumeratedRoles and whose values are Arrays of keys that the given
-- | (inverted) query should be stored under.
compiletimeIndexForFilledByQueries :: Partial => QueryFunctionDescription -> (PhaseTwo' MonadPerspectives) (Array (Tuple EnumeratedRoleType (Array InvertedQueryKey)))
compiletimeIndexForFilledByQueries qfd | isRoleDomain $ domain qfd = for (allLeavesInADT $ roleDomain qfd) keysForRoleInContext
  where
    -- start is filled, because filledBy goes from filled to filler.
    keysForRoleInContext :: RoleInContext -> (PhaseTwo' MonadPerspectives) (Tuple EnumeratedRoleType (Array InvertedQueryKey))
    keysForRoleInContext (RoleInContext{context:filledContext, role:filledRole}) = do
      (adtBinding :: ADT RoleInContext) <- lift $ lift $ getEnumeratedRole filledRole >>= binding
      case queryFunction qfd of
        -- end is the filler role.
        (DataTypeGetter FillerF) -> pure $ Tuple filledRole ((allLeavesInADT adtBinding) <#> \(RoleInContext{context:fillerContext, role:fillerRole}) ->
          (InvertedQueryKey filledContext fillerContext fillerRole))
        (DataTypeGetterWithParameter FillerF requiredFillerContext) -> pure $ Tuple filledRole ((allLeavesInADT adtBinding) <#> \(RoleInContext{context:fillerContext, role:fillerRole}) ->
          (InvertedQueryKey filledContext (ContextType requiredFillerContext) fillerRole))

-- | Compute the keys for the fills (Binder) step.
-- | Returns a map whose keys identify EnumeratedRoles and whose values are Arrays of keys that the given
-- | (inverted) query should be stored under.
compiletimeIndexForFillsQueries :: Partial => QueryFunctionDescription -> (Array (Tuple EnumeratedRoleType (Array InvertedQueryKey)))
compiletimeIndexForFillsQueries qfd | isRoleDomain $ domain qfd = (allLeavesInADT $ roleDomain qfd) <#>
  keysForRoleInContext
  where
    -- start is the filler, because fills goes from filler to filled.
    keysForRoleInContext :: RoleInContext -> (Tuple EnumeratedRoleType (Array InvertedQueryKey))
    keysForRoleInContext (RoleInContext{context:fillerContext, role:fillerRole}) = case roleRange qfd of
      -- The filled role is always an ST, because it derives directly from the filledBy clause of an
      -- EnumeratedRole - by construction a single type.
      ST (RoleInContext{context:filledContext, role:filledRole}) -> case queryFunction qfd of
        (FilledF _ (ContextType mrequiredFilledContext)) -> case mrequiredFilledContext of
          "" -> Tuple filledRole [InvertedQueryKey fillerContext filledContext filledRole] -- FillerContext FilledContext FilledRole
          requiredFilledContext -> Tuple filledRole
            [InvertedQueryKey fillerContext (ContextType requiredFilledContext) filledRole]
