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

module Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries where

import Control.Monad.Except (throwError)
import Data.Array (union)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (domain2RoleType)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Prelude (Unit, pure, unit, ($))

setPathForStep :: Partial => QueryFunctionDescription -> QueryFunctionDescription -> Array RoleType -> PhaseThree Unit
setPathForStep (SQD dom qf ran _ _) path userTypes = case qf of
  QF.Value2Role pt -> case pt of
    ENP p -> modifyDF \dfr@{enumeratedProperties} -> case lookup (unwrap p) enumeratedProperties of
      Nothing -> addInvertedQueryForDomain (unwrap p)
        (InvertedQuery {description: path, compilation: Nothing, userTypes})
        OnPropertyDelta
        dfr
      Just ep -> dfr {enumeratedProperties = insert (unwrap p) (addPathToProperty ep path) enumeratedProperties}
    -- CP _ -> throwError $ Custom "Implement the handling of Calculated Properties in setPathForStep."
    CP _ -> pure unit

  QF.DataTypeGetterWithParameter QF.GetRoleBindersF enr -> modifyDF \dfr@{enumeratedRoles} -> let
    roleName = unwrap $ unsafePartial $ domain2RoleType dom
    in case lookup roleName  enumeratedRoles of
      Nothing -> addInvertedQueryForDomain roleName
        (InvertedQuery {description: path, compilation: Nothing, userTypes})
        OnRoleDelta_binder
        dfr
      Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binder en path) enumeratedRoles}

  QF.DataTypeGetter QF.BindingF -> modifyDF \dfr@{enumeratedRoles} -> case dom of
    (RDOM EMPTY) -> dfr
    otherwise -> let
      roleName = unwrap $ unsafePartial $ domain2RoleType dom
      in case lookup roleName  enumeratedRoles of
        Nothing -> addInvertedQueryForDomain roleName
          (InvertedQuery {description: path, compilation: Nothing, userTypes})
          OnRoleDelta_binding
          dfr
        Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binding en path) enumeratedRoles}

  QF.RolGetter roleType -> case roleType of
    ENR (EnumeratedRoleType roleName) -> modifyDF \dfr@{enumeratedRoles} -> case lookup roleName enumeratedRoles of
      Nothing -> addInvertedQueryForDomain roleName
        (InvertedQuery {description: path, compilation: Nothing, userTypes})
        OnContextDelta_role
        dfr
      Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_role en path) enumeratedRoles}
    CR _ -> throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

  QF.DataTypeGetter QF.ContextF-> modifyDF \dfr@{enumeratedRoles} -> let
    roleName = unwrap $ unsafePartial $ domain2RoleType dom
    in case lookup roleName  enumeratedRoles of
      Nothing -> addInvertedQueryForDomain roleName
        (InvertedQuery {description: path, compilation: Nothing, userTypes})
        OnContextDelta_context
        dfr
      Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_context en path) enumeratedRoles}

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

  _ -> throwError $ Custom "setInvertedQueries: there should be no other cases. This is a system programming error."

  where
    addPathToProperty :: EnumeratedProperty -> QueryFunctionDescription -> EnumeratedProperty
    addPathToProperty (EnumeratedProperty propRecord@{onPropertyDelta}) inverseQuery = EnumeratedProperty propRecord {onPropertyDelta = union onPropertyDelta [(InvertedQuery {description: inverseQuery, compilation: Nothing, userTypes})]}

    addPathToOnRoleDelta_binder :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
    addPathToOnRoleDelta_binder (EnumeratedRole rolRecord@{onRoleDelta_binder}) inverseQuery = EnumeratedRole rolRecord {onRoleDelta_binder = union onRoleDelta_binder [(InvertedQuery {description: inverseQuery, compilation: Nothing, userTypes})] }

    addPathToOnRoleDelta_binding :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
    addPathToOnRoleDelta_binding (EnumeratedRole rolRecord@{onRoleDelta_binding}) inverseQuery = EnumeratedRole rolRecord {onRoleDelta_binding = union onRoleDelta_binding [(InvertedQuery {description: inverseQuery, compilation: Nothing, userTypes})]}

    addPathToOnContextDelta_context :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
    addPathToOnContextDelta_context (EnumeratedRole rolRecord@{onContextDelta_context}) inverseQuery = EnumeratedRole rolRecord {onContextDelta_context = union onContextDelta_context [(InvertedQuery {description: inverseQuery, compilation: Nothing, userTypes})]}

    addPathToOnContextDelta_role :: EnumeratedRole -> QueryFunctionDescription -> EnumeratedRole
    addPathToOnContextDelta_role (EnumeratedRole rolRecord@{onContextDelta_role}) inverseQuery = EnumeratedRole rolRecord {onContextDelta_role = union onContextDelta_role [(InvertedQuery {description: inverseQuery, compilation: Nothing, userTypes})]}
