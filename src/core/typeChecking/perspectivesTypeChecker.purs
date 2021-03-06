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

module Perspectives.Checking.PerspectivesTypeChecker where

import Control.Monad.Error.Class (try)
import Control.Monad.Except (catchError, lift, throwError)
import Data.Array (cons, elemIndex)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Error.Boundaries (handlePerspectContextError)
import Perspectives.InstanceRepresentation (PerspectContext, pspType)
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.Parsing.Messages (PerspectivesError(..), PF, fail)
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Context (contextAspects, contextRole, externalRole, roleInContext, userRole, position, defaultPrototype)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (ContextType, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (bindingOfRole, kindOfRole, lessThanOrEqualTo, roleAspectsBindingADT)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, RoleKind(..), RoleType(..))
import Prelude (Unit, bind, discard, pure, unit, ($), (<<<), (==), (>=>), (>>=), (&&), not)

checkDomeinFile :: DomeinFile -> MonadPerspectives (Array PerspectivesError)
checkDomeinFile df = pure []

checkDomeinFile_ :: DomeinFile -> PF Unit
checkDomeinFile_ df = pure unit

checkContext :: Context -> PF Unit
checkContext c = do

  -- 1. The default prototype should have the type that we are checking.
  case defaultPrototype c of
    Nothing -> pure unit
    (Just pt) -> do
      (lift $ try $ getPerspectContext pt) >>=
        handlePerspectContextError "checkContext"
        \(p :: PerspectContext) -> if (pspType p == identifier c)
          then pure unit
          else fail $ DefaultPrototype (identifier c :: ContextType) (pspType p)

  -- 2. The graph formed by the contextAspects may not be cyclic.
  -- I.e. when traversing the graph, the next node to be visited may not be in the path
  -- starting with c.
  catchError (lift $ throwOnCycle [] c)
    \e -> fail $ CyclicAspects (position c) (identifier c)

  -- 3. The RoleKind of each RoleType must equal the position of the RoleType in the context.
  -- E.g.: all EnumeratedRoles and CalculatedRoles in rolInContext must have RoleKind RolInContext.
  for_ (roleInContext c) (checkRoleKind RoleInContext)
  for_ (contextRole c) (checkRoleKind ContextRole)
  for_ (userRole c) (checkRoleKind UserRole)
  checkEnumeratedRole ExternalRole (externalRole c)

  where
    checkRoleKind :: RoleKind -> RoleType -> PF Unit
    checkRoleKind kind (r :: RoleType) = do
      k <- lift $ rolekind r
      if (k == kind)
        then pure unit
        else fail $ WrongRoleKind r kind k

    checkEnumeratedRole :: RoleKind -> EnumeratedRoleType -> PF Unit
    checkEnumeratedRole kind r = do
      (rr :: EnumeratedRole) <- lift $ getPerspectType r
      if (kindOfRole rr == kind)
        then pure unit
        else fail $ WrongRoleKind (ENR r) kind (kindOfRole rr)

    throwOnCycle :: Array ContextType -> Context -> MP Unit
    throwOnCycle path next = if (isJust $ elemIndex (identifier next) path)
      then (throwError (error "cyclic"))
      else for_
        (contextAspects next)
        (getPerspectType >=> throwOnCycle (cons (identifier c) path))

    rolekind :: RoleType -> MP RoleKind
    rolekind (ENR r) = (getPerspectType r :: MP EnumeratedRole) >>= pure <<< kindOfRole
    rolekind (CR r) = (getPerspectType r :: MP CalculatedRole) >>= pure <<< kindOfRole

-----------------------------------------------------------
-- CHECKBINDING
-----------------------------------------------------------
-- | The allowed binding of the given role type must be equal to or more general than the type of the proposed binding.
-- | The type of the proposed binding may not be equal to the given role type (we disallow a role binding to itself).
-- | Retrieves from the repository the model that holds the RoleType, if necessary.
checkBinding :: RoleType -> RoleInstance -> MP Boolean
checkBinding roletype instanceToBind = do
  -- If the model is not available locally, try to get it from the repository.
  (instanceType :: EnumeratedRoleType) <- roleType_ instanceToBind
  (instanceType' :: ADT EnumeratedRoleType) <- (getEnumeratedRole >=> roleAspectsBindingADT) instanceType
  -- TODO. Voor de rol moet ik alleen de binding ophalen.
  -- roleType' <- (getEnumeratedRoleInstances roletype) >>= adtOfRoleAspectsBinding
  (roleType' :: ADT EnumeratedRoleType) <- bindingOfRole roletype
  b1 <- roleType' `lessThanOrEqualTo` instanceType'
  pure $ b1 && not (roletype == ENR instanceType)
