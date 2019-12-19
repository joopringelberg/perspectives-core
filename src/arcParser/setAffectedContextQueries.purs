-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Parsing.Arc.PhaseThree.SetAffectedContextCalculations where

import Control.Monad.Except (throwError)
import Data.Array (cons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.AffectedContextCalculation (AffectedContextCalculation(..))
import Perspectives.Parsing.Arc.PhaseTwo (PhaseThree, modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (domain2RoleType, invertFunctionDescription)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Prelude (Unit, discard, pure, unit, ($))

-- Compute the inverse paths for the condition. Save each path with the type that is the origin
-- (Domain) of the inverse path. These paths are used to detect contexts whose rules need to be (re)run
-- when a Delta is processed for the head of a path.
setAffectedContextCalculations :: ActionType -> QueryFunctionDescription -> PhaseThree Unit
setAffectedContextCalculations action qfd = unsafePartial $ for_ (invertFunctionDescription qfd)
  \path -> do
    -- log $ prettyPrint path
    setPathForEachSubPath path

  where
    setPathForEachSubPath :: Partial => QueryFunctionDescription -> PhaseThree Unit
    -- Terminating step.
    setPathForEachSubPath path@(BQD _ (QF.BinaryCombinator QF.ComposeF) sub1@(SQD _ _ _ _ _) sub2@(SQD _ _ _ _ _) _ _ _) = do
      setPathForStep sub1 path

    -- Recursing step.
    setPathForEachSubPath path@(BQD _ (QF.BinaryCombinator QF.ComposeF) sub1@(SQD _ _ _ _ _) sub2@(BQD _ (QF.BinaryCombinator QF.ComposeF) _ _ _ _ _) _ _ _) = do
      setPathForStep sub1 path
      setPathForEachSubPath sub2

    setPathForEachSubPath path@(SQD _ _ _ _ _) = setPathForStep path path

    setPathForStep :: Partial => QueryFunctionDescription -> QueryFunctionDescription -> PhaseThree Unit
    setPathForStep (SQD dom qf _ _ _) path = case qf of

      QF.Value2Role pt -> case pt of
        ENP p -> modifyDF \dfr@{enumeratedProperties} -> case lookup (unwrap p) enumeratedProperties of
          Nothing -> dfr
          Just ep -> dfr {enumeratedProperties = insert (unwrap p) (addPathToProperty ep path) enumeratedProperties}
        CP _ -> throwError $ Custom "Implement the handling of Calculated Properties in invertFunctionDescription."

      QF.DataTypeGetterWithParameter QF.GetRoleBindersF enr -> modifyDF \dfr@{enumeratedRoles} -> let
        roleName = unwrap $ unsafePartial $ domain2RoleType dom
        in case lookup roleName  enumeratedRoles of
          Nothing -> dfr
          Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binder en path) enumeratedRoles}

      QF.DataTypeGetter QF.BindingF -> modifyDF \dfr@{enumeratedRoles} -> let
        roleName = unwrap $ unsafePartial $ domain2RoleType dom
        in case lookup roleName  enumeratedRoles of
          Nothing -> dfr
          Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binding en path) enumeratedRoles}

      -- Ignore ExternalRoleF and IdentityF in an inverse path. We do not
      -- establish queries to gather affected contexts on them. For IdentityF this is
      -- because we will establish a query on the next step (or have done so at the
      -- previous step). For ExternalRoleF this is because there cannot be ContextDeltas
      -- for the External role.
      QF.DataTypeGetter QF.ExternalRoleF -> pure unit
      QF.DataTypeGetter QF.IdentityF -> pure unit

      QF.RolGetter roleType -> case roleType of
        ENR (EnumeratedRoleType roleName) -> modifyDF \dfr@{enumeratedRoles} -> case lookup roleName enumeratedRoles of
          Nothing -> dfr
          Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_role en path) enumeratedRoles}
        CR _ -> throwError $ Custom "Implement the handling of Calculated Roles in invertFunctionDescription."

      QF.DataTypeGetter QF.ContextF->  modifyDF \dfr@{enumeratedRoles} -> let
        roleName = unwrap $ unsafePartial $ domain2RoleType dom
        in case lookup roleName  enumeratedRoles of
          Nothing -> dfr
          Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_context en path) enumeratedRoles}

      _ -> throwError $ Custom "setAffectedContextCalculations: there should be no other cases. This is a system programming error."

      where
        addPathToProperty :: EnumeratedProperty -> QueryFunctionDescription -> EnumeratedProperty
        addPathToProperty (EnumeratedProperty propRecord@{onPropertyDelta}) inverseQuery = EnumeratedProperty propRecord {onPropertyDelta = cons (AffectedContextCalculation {description: inverseQuery, compilation: Nothing, action: action}) onPropertyDelta}

        addPathToOnRoleDelta_binder :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
        addPathToOnRoleDelta_binder (EnumeratedRole rolRecord@{onRoleDelta_binder}) inverseQuery = EnumeratedRole rolRecord {onRoleDelta_binder = cons (AffectedContextCalculation {description: inverseQuery, compilation: Nothing, action: action}) onRoleDelta_binder}

        addPathToOnRoleDelta_binding :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
        addPathToOnRoleDelta_binding (EnumeratedRole rolRecord@{onRoleDelta_binding}) inverseQuery = EnumeratedRole rolRecord {onRoleDelta_binding = cons (AffectedContextCalculation {description: inverseQuery, compilation: Nothing, action: action}) onRoleDelta_binding}

        addPathToOnContextDelta_context :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
        addPathToOnContextDelta_context (EnumeratedRole rolRecord@{onContextDelta_context}) inverseQuery = EnumeratedRole rolRecord {onContextDelta_context = cons (AffectedContextCalculation {description: inverseQuery, compilation: Nothing, action: action}) onContextDelta_context}

        addPathToOnContextDelta_role :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
        addPathToOnContextDelta_role (EnumeratedRole rolRecord@{onContextDelta_role}) inverseQuery = EnumeratedRole rolRecord {onContextDelta_role = cons (AffectedContextCalculation {description: inverseQuery, compilation: Nothing, action: action}) onContextDelta_role}
