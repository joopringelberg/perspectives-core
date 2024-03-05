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

-- | The functions exported by this module claim to result in a resource (Context or Role), but actually
-- | never do so. Instead, they throw an exception that will have to be handled elsewhere.
-- | They do, however, take away the **cause** of that exception. 
-- | In essence that means that the end user can retry her action and the problem will have gone away.
-- | We fix by removing 'broken links'; that is, references to resources that cannot be found, will be removed.
-- | As a consequence, of course, the end user may not be able to re-execute her action. 
-- | The good thing is that the referential integrity of her representation of the part of the 
-- | Perspectives Universe that is visible to her, has been restored.
-- | Each installation must fix its own referential integrity problems. Results of this fixing
-- | will not be synchronized.
-- | This again is a good thing, as it may (but need not) happen that a peer will later send a transaction
-- | that restores the resource that went missing.


-- END LICENSE
module Perspectives.ReferentialIntegrity  
  (fixReferences)
where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array (delete)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Foreign.Object (mapWithKey)
import Perspectives.Assignment.Update (cacheAndSave, deleteProperty)
import Perspectives.ContextAndRole (changeContext_me, context_me, removeRol_gevuldeRollen)
import Perspectives.CoreTypes (MonadPerspectives, ResourceToBeStored(..))
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol)
import Perspectives.Instances.ObjectGetters (getContextFromDatabase_, getContextRolesFromDatabase_, getFilledRolesFromDatabase_, getFillerFromDatabase_, getRoleOnClipboard)
import Perspectives.ModelDependencies (cardClipBoard, sysUser)
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, removeEntiteit, saveMarkedResources)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleAssignment (filledNoLongerPointsTo)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runEmbeddedIfNecessary)
import Perspectives.SaveUserData (severeBindingLinks)

fixReferences :: ResourceToBeStored -> MonadPerspectives Unit
fixReferences (Ctxt cid) = fixContextReferences cid
fixReferences (Rle rid) = fixRoleReferences rid
fixReferences (Dfile did) = pure unit

-- | Apply this function when a reference to a context has been found that cannot be retrieved.
-- | We want all references to this context to be removed.
-- | Only roles refer to contexts.
-- | All these roles must be removed, as a role must have a context!
-- | Notice that we do not synchronize the changes.
fixContextReferences :: ContextInstance -> MonadPerspectives Unit
fixContextReferences cid = do
  -- As we look for dangling references in the database only, we should be sure that
  -- there is no difference between cache and database.
  saveMarkedResources
  referringRoles <- getContextRolesFromDatabase_ cid
  for_ referringRoles \roleId -> (try $ (getPerspectRol roleId)) >>= handlePerspectRolError "fixContextReferences"
    \role -> do
      severeBindingLinks role
      -- PERSISTENCE (finally remove the role instance from cache and database).
      removeEntiteit roleId
  saveMarkedResources

-- | Apply this function when a reference to a role has been found that cannot be retrieved.
-- | All references from other roles and from its context must be removed.
fixRoleReferences :: RoleInstance -> MonadPerspectives Unit
fixRoleReferences roleId = do
  -- As we look for dangling references in the database only, we should be sure that
  -- there is no difference between cache and database.
  saveMarkedResources
  ctxts <- getContextFromDatabase_ roleId
  -- If the role type is unlinked, we will find no contexts.
  for_ ctxts removeRoleInstanceFromContext
  -- Retrieve all roles that still refer to roleId as their filler.
  filledRoles <- getFilledRolesFromDatabase_ roleId
  -- Remove this role from all other roles that point to it as their filler.
  for_ filledRoles (flip filledNoLongerPointsTo roleId)
  --- Retrieve the filler that still refers to roleId.
  fillerRoles <- getFillerFromDatabase_ roleId
  for_ fillerRoles 
    \({filler, filledContextType, filledRoleType}) ->  (fillerNoLongerPointsTo filler roleId filledContextType filledRoleType)
  -- Now check the clipboard. It may hold a reference to the role that can no longer be found.
  clearClipboard
  saveMarkedResources

  where
  removeRoleInstanceFromContext :: ContextInstance -> MonadPerspectives Unit
  removeRoleInstanceFromContext contextId = do
    (try $ getPerspectContext contextId) >>=
      handlePerspectContextError "removeRoleInstance"
        \(pe :: PerspectContext) -> do
          -- Modify the context: remove the role instances from those recorded with the role type.
          changedContext <- pure $ removeFromContext pe
          -- CURRENTUSER.
          case context_me pe of
            Just m | m == roleId -> do
              cacheAndSave contextId (changeContext_me changedContext Nothing)
            _ -> cacheAndSave contextId changedContext

  removeFromContext :: PerspectContext -> PerspectContext
  removeFromContext (PerspectContext ct@{rolInContext}) = PerspectContext ct {rolInContext = 
    mapWithKey (\rtype roles -> delete roleId roles) rolInContext}
  
  clearClipboard :: MonadPerspectives Unit
  clearClipboard = do
    clipboard <- getRoleOnClipboard
    case clipboard of 
      Nothing -> pure unit
      Just r -> if r == roleId
        then do 
          s <- getMySystem
          runEmbeddedIfNecessary doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser)
            (deleteProperty [RoleInstance $ buitenRol s] (EnumeratedPropertyType cardClipBoard))
        else pure unit

  -- This version only changes the administration in the filler; the full version in Perspectives.RoleAssignment changes both sides.
  fillerNoLongerPointsTo :: RoleInstance -> RoleInstance -> ContextType -> EnumeratedRoleType -> MonadPerspectives Unit
  fillerNoLongerPointsTo fillerId filledId filledContextType filledRoleType = (try $ getPerspectRol fillerId) >>=
    handlePerspectRolError' "fillerNoLongerPointsTo" unit
      \(filler :: PerspectRol) -> do
        filler' <- pure $ (removeRol_gevuldeRollen filler filledContextType filledRoleType filledId)
        cacheAndSave fillerId filler'


