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
import Data.Traversable (traverse)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Error.Boundaries (handlePerspectContextError)
import Perspectives.InstanceRepresentation (PerspectContext, pspType)
import Perspectives.Instances.ObjectGetters (completeRuntimeType)
import Perspectives.Parsing.Messages (PerspectivesError(..), PF, fail)
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Representation.ADT (equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Context (contextAspects, contextRole, externalRole, roleInContext, userRole, position, defaultPrototype)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (ContextType, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (completeDeclaredFillerRestriction, kindOfRole, toConjunctiveNormalForm_)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, RoleKind(..), RoleType(..))
import Perspectives.Types.ObjectGetters (equalsOrGeneralisesRoleInContext)
import Prelude (Unit, bind, discard, pure, unit, ($), (<<<), (==), (>=>), (>>=))

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

  -- TODO. #13 Check Aspect restrictions

  where
    checkRoleKind :: RoleKind -> RoleType -> PF Unit
    checkRoleKind kind (r :: RoleType) = do
      k <- lift $ rolekind r
      if (k == kind)
        then pure unit
        else fail $ WrongRoleKind r kind k

    checkEnumeratedRole :: RoleKind -> EnumeratedRoleType -> PF Unit
    checkEnumeratedRole kind r = do
      rr@(EnumeratedRole{roleAspects, binding, pos, displayName}) <- lift $ getPerspectType r
      if (kindOfRole rr == kind)
        then pure unit
        else fail $ WrongRoleKind (ENR r) kind (kindOfRole rr)
      -- The restrictions on filling this role must be equal to or a specialisation of 
      -- that of its aspects.
      for_ roleAspects \(QT.RoleInContext{role}) -> do
        EnumeratedRole{binding:maspectBinding, pos:aspectPos, displayName:aspectDisplayName} <- lift $ getEnumeratedRole role
        case maspectBinding, binding of
          Just aspectBinding, Just bnd -> 
            -- aspectBinding -> bnd
            (lift $ aspectBinding `equalsOrGeneralisesRoleInContext` bnd) >>= if _
              then pure unit
              else fail $ FillerRestrictionNotAnAspectSubtype pos aspectPos displayName aspectDisplayName
          _, _ -> pure unit

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
-- | The allowed filler of the given role type to be filled (including those of its Aspects!) must be equal to or more general than the type of the proposed filler.
-- | The type of the proposed filler may not be equal to the given role type (we disallow a role filling itself).
-- | Retrieves from the repository the model that holds the RoleType, if necessary.
checkBinding :: EnumeratedRoleType -> RoleInstance -> MP Boolean
checkBinding filledType filler = do
  -- (mrestriction :: Maybe (ExpandedADT QT.RoleInContext)) <- getEnumeratedRole filledType >>= completeExpandedFillerRestriction
  (mrestriction :: Maybe (CNF QT.RoleInContext)) <- getEnumeratedRole filledType >>= completeDeclaredFillerRestriction >>= traverse toConjunctiveNormalForm_
  (fillerType :: CNF QT.RoleInContext) <- completeRuntimeType filler >>= toConjunctiveNormalForm_
  case mrestriction of 
    -- restriction -> fillerType
    Just restriction -> pure (fillerType `equalsOrSpecialises_` restriction)
    Nothing -> pure true