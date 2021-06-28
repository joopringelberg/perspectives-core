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

module Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries where

import Control.Monad.Except (throwError)
import Data.Array (nub, union)
import Data.Map (Map, lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (domain2RoleType)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, domain, functional, mandatory, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and, or)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier)
import Prelude (Unit, flip, identity, pure, unit, ($), (<$>))

setPathForStep :: Partial =>
  QueryFunctionDescription ->
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  PhaseThree Unit
setPathForStep (SQD dom qf ran fun man) qWithAK users states statesPerProperty = case qf of
  QF.Value2Role pt -> case pt of
    ENP p -> modifyDF \dfr@{enumeratedProperties} -> case lookup (unwrap p) enumeratedProperties of
      Nothing -> addInvertedQueryForDomain (unwrap p)
        (InvertedQuery
          { description: qWithAK
          , backwardsCompiled: Nothing
          , forwardsCompiled: Nothing
          , users
          , statesPerProperty: EncodableMap statesPerProperty
          -- NOTE. Mag het voorkomen dat de property niet in statesPerProperty zit?
          -- en wat is de betekenis van een InvertedQuery zonder states?
          , states: maybe [] identity (Map.lookup (ENP p) statesPerProperty)
          })
        OnPropertyDelta
        dfr
      Just ep -> dfr {enumeratedProperties = insert (unwrap p) (addPathToProperty ep qWithAK) enumeratedProperties}
    -- CP _ -> throwError $ Custom "Implement the handling of Calculated Properties in setPathForStep."
    CP _ -> pure unit

  -- add to onRoleDelta_binder of the role that we apply `binder enr` to (the domain of the step; the role that is bound).
  QF.DataTypeGetterWithParameter QF.GetRoleBindersF enr -> modifyDF \dfr@{enumeratedRoles} -> let
    -- We remove the first step of the backwards path, because we apply it (runtime) not to the binding, but to
    -- the binder. We skip the binding step because its cardinality is larger than one. It would cause a fan-out
    -- while we know, when applying the inverted query when handling a RoleBindingDelta, the exact path to follow.
    -- The function `aisInRoleDelta` applies the forward part to the binding (not the binder).
    -- Hence we don't have to adapt the forward part.
    oneStepLess = removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
    roleName = unwrap $ unsafePartial $ domain2RoleType dom
    in case oneStepLess of
        -- WARNING. This may not be correct
        ZQ Nothing _ -> dfr
        _ -> case lookup roleName enumeratedRoles of
          Nothing -> addInvertedQueryForDomain roleName
            (InvertedQuery {description: oneStepLess, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})
            OnRoleDelta_binder
            dfr
          Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binder en oneStepLess) enumeratedRoles}

  -- add to onRoleDelta_binding of the role that we apply `binding` to (the domain of the step; the role that binds).
  QF.DataTypeGetter QF.BindingF -> modifyDF \dfr@{enumeratedRoles} -> case dom of
    (RDOM EMPTY) -> dfr
    otherwise -> let
      roleName = unwrap $ unsafePartial $ domain2RoleType dom
      in case lookup roleName  enumeratedRoles of
        Nothing -> addInvertedQueryForDomain roleName
          (InvertedQuery {description: qWithAK, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})
          OnRoleDelta_binding
          dfr
        Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binding en qWithAK) enumeratedRoles}

  QF.RolGetter roleType -> case roleType of
    ENR (EnumeratedRoleType roleName) -> modifyDF \dfr@{enumeratedRoles} -> let
      -- We remove the first step of the backwards path, because we apply it runtime not to the context, but to
      -- the new role instance. We skip the RolGetter step because its cardinality is larger than one.
      -- Because the forward part will be applied to that same role (instead of the context), we have to compensate
      -- for that by prepending it with the inversal of the first backward step - which is, by construction, a
      -- `context` step.
      contextStep = SQD ran (QF.DataTypeGetter ContextF) dom True man
      oneStepLess = removeFirstBackwardsStep qWithAK
        (\dom' ran' man' -> Just $ SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
      in case oneStepLess of
        ZQ Nothing _ -> dfr
        _ -> case lookup roleName enumeratedRoles of
          Nothing -> addInvertedQueryForDomain roleName
            (InvertedQuery {description: oneStepLess, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})
            OnContextDelta_role
            dfr
          Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_role en oneStepLess) enumeratedRoles}
    CR _ -> throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

  QF.DataTypeGetter QF.ContextF -> modifyDF \dfr@{enumeratedRoles} -> let
    roleName = unwrap $ unsafePartial $ domain2RoleType dom
    in case lookup roleName  enumeratedRoles of
      Nothing -> addInvertedQueryForDomain roleName
        (InvertedQuery {description: qWithAK, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})
        OnContextDelta_context
        dfr
      Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_context en qWithAK) enumeratedRoles}

  -- As there is, by construction, no link from the range (an external role type)
  -- to the domain (a context type), we can not attach an inverted query anywhere
  QF.ExternalCoreContextGetter f -> pure unit

  -- The query would be added to onContextDelta_role of the external role. Such inverse queries are run when a new
  -- instance of the role type is added to the context (or when it is removed). But the external role never changes,
  -- so this is superfluous.
  QF.DataTypeGetter QF.ExternalRoleF -> pure unit

  -- Ignore ExternalRoleF and IdentityF in an inverse path. We do not
  -- establish queries to gather affected contexts on them. For IdentityF this is
  -- because we will establish a query on the next step (or have done so at the
  -- previous step). For ExternalRoleF this is because there cannot be ContextDeltas
  -- for the External role.
  QF.DataTypeGetter QF.IdentityF -> pure unit

  _ -> throwError $ Custom "setPathForStep: there should be no other cases. This is a system programming error."

  where
    addPathToProperty :: EnumeratedProperty -> QueryWithAKink -> EnumeratedProperty
    addPathToProperty (EnumeratedProperty propRecord@{_id, onPropertyDelta}) inverseQuery = EnumeratedProperty propRecord {onPropertyDelta = union onPropertyDelta [(InvertedQuery
      { description: inverseQuery
      , backwardsCompiled: Nothing
      , forwardsCompiled: Nothing
      , users
      , statesPerProperty: EncodableMap statesPerProperty
      , states: maybe [] identity (Map.lookup (ENP _id) statesPerProperty)})]}

    addPathToOnRoleDelta_binder :: EnumeratedRole -> QueryWithAKink -> EnumeratedRole
    addPathToOnRoleDelta_binder (EnumeratedRole rolRecord@{onRoleDelta_binder}) inverseQuery = EnumeratedRole rolRecord {onRoleDelta_binder = union onRoleDelta_binder [(InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})] }

    addPathToOnRoleDelta_binding :: EnumeratedRole -> QueryWithAKink -> EnumeratedRole
    addPathToOnRoleDelta_binding (EnumeratedRole rolRecord@{onRoleDelta_binding}) inverseQuery = EnumeratedRole rolRecord {onRoleDelta_binding = union onRoleDelta_binding [(InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})]}

    addPathToOnContextDelta_context :: EnumeratedRole -> QueryWithAKink -> EnumeratedRole
    addPathToOnContextDelta_context (EnumeratedRole rolRecord@{onContextDelta_context}) inverseQuery = EnumeratedRole rolRecord {onContextDelta_context = union onContextDelta_context [(InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})]}

    addPathToOnContextDelta_role :: EnumeratedRole -> QueryWithAKink -> EnumeratedRole
    addPathToOnContextDelta_role (EnumeratedRole rolRecord@{onContextDelta_role}) inverseQuery = EnumeratedRole rolRecord {onContextDelta_role = union onContextDelta_role [(InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, statesPerProperty: EncodableMap statesPerProperty, states})]}

------------------------------------------------------------------------------------------
---- REMOVE FIRST BACKWARDS STEP
------------------------------------------------------------------------------------------
removeFirstBackwardsStep :: QueryWithAKink ->
  -- A function from domain and range of the step to prepend, and whether it is mandatory.
  (Domain -> Range -> ThreeValuedLogic -> Maybe QueryFunctionDescription) ->
  QueryWithAKink
removeFirstBackwardsStep q@(ZQ backward forward) originalStepF = case backward, forward of
  (Just (BQD _ (BinaryCombinator ComposeF) firstBackwardStep qfd2 _ _ _)), (Just forward') -> ZQ
      (Just qfd2)
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (Just $ maybe forward' (flip makeComposition forward') (originalStepF (range firstBackwardStep) (domain firstBackwardStep) (mandatory firstBackwardStep)))
  (Just (BQD _ (BinaryCombinator ComposeF) firstBackwardStep qfd2 _ _ _)), Nothing -> ZQ
      (Just qfd2)
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (originalStepF (range firstBackwardStep) (domain firstBackwardStep) (mandatory firstBackwardStep))
  (Just i@(SQD dom _ ran fun man)), (Just forward') -> (ZQ
      Nothing
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (Just $ maybe forward' (flip makeComposition forward') (originalStepF ran dom man)))
  (Just i@(SQD dom _ ran fun man)), Nothing -> (ZQ
      Nothing
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (originalStepF ran dom man))
  _, _ -> q

makeComposition :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
makeComposition left right = BQD
  (domain left)
  (BinaryCombinator ComposeF)
  left
  right
  (range right)
  (and (functional left) (functional right))
  (or (functional left) (functional right))
