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
module Perspectives.RoleAssignment where

import Control.Monad.Error.Class (try)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Perspectives.Assignment.Update (cacheAndSave)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_binding, changeRol_isMe, removeRol_binding, removeRol_gevuldeRollen, rol_context, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives, (##>), (##>>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie)
import Perspectives.DependencyTracking.Dependency (findMeRequests)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Instances.Me (getMyType)
import Perspectives.Instances.ObjectGetters (contextType)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (kindOfRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (Unit, bind, discard, pure, unit, ($), (>>=), (==), (||))

-- | In the context of the roleInstance, find the role that is me and set its isMe to true.
-- | Set the me of the context to that role.
-- | If not found, set me of the context to Nothing.
-- | Invariant: roleId was the previous value of me of contextId at the time of calling.
lookForAlternativeMe :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
lookForAlternativeMe roleId contextId = (lift $ try $ getPerspectContext contextId) >>=
  handlePerspectContextError "lookForAlternativeMe"
    \ctxt -> do
      mmyType <- lift (contextId ##> getMyType)
      case mmyType of
        Nothing -> lift $ cacheAndSave contextId (changeContext_me ctxt Nothing)
        Just myType -> do
          mme <- lift (contextId ##> getRoleInstances myType)
          case mme of
            Nothing -> lift $ cacheAndSave contextId (changeContext_me ctxt Nothing)
            Just me -> roleIsMe me contextId

-- | Set isMe of the roleInstance to false.
roleIsNotMe :: RoleInstance -> MonadPerspectivesTransaction Unit
roleIsNotMe roleId = (lift $ try $ getPerspectRol roleId) >>=
  handlePerspectRolError "roleIsNotMe"
    \role -> do
      lift $ cacheAndSave roleId (changeRol_isMe role false)

-- | Set isMe of the roleInstance to true, but only if the role is a user role.
-- | Set me of the context to the roleInstance.
roleIsMe :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
roleIsMe roleId contextId = (lift $ try $ getPerspectContext contextId) >>=
  handlePerspectContextError "roleIsMe"
    \ctxt -> (lift $ try $ getPerspectRol roleId) >>=
      handlePerspectRolError "roleIsMe"
        \role -> do
          rt <- lift $ getEnumeratedRole (rol_pspType role)
          if kindOfRole rt == UserRole || kindOfRole rt == Public || kindOfRole rt == PublicProxy
            then do
              (lift $ findMeRequests contextId)  >>= addCorrelationIdentifiersToTransactie
              lift $ cacheAndSave roleId (changeRol_isMe role true)
              lift $ cacheAndSave contextId (changeContext_me ctxt (Just roleId))
            else pure unit

-- | <fillerId> `fillerNoLongerPointsTo` <filledId>
-- | Break the link from filler to filled (FILLS link)
-- | (Remove from the filledRoles, in other words: change filler)
-- | Not the other way round!
-- | Removes filled from filledRoles of filler (because filler no longer fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectives Unit
fillerNoLongerPointsTo fillerId filledId = (try $ getPerspectRol fillerId) >>=
  handlePerspectRolError' "fillerNoLongerPointsTo" unit
    \(filler :: PerspectRol) -> (try $ getPerspectRol filledId) >>=
      handlePerspectRolError "fillerNoLongerPointsTo"
      \(filled :: PerspectRol) -> do
        filledContextType <- rol_context filled ##>> contextType
        filler' <- pure $ (removeRol_gevuldeRollen filler filledContextType (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <fillerId> `fillerPointsTo` <filledId>
-- | Add the link from filler to filled (FILLS link)
-- | (Add to the filledRoles, in other words: change filler)
-- | Not the other way round!
-- | Adds filled from filledRoles of filler (because filler now fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectives Unit
fillerPointsTo fillerId filledId = (try $ getPerspectRol fillerId) >>=
  handlePerspectRolError' "fillerPointsTo" unit
    \(filler :: PerspectRol) -> (try $ getPerspectRol filledId) >>=
      handlePerspectRolError' "fillerPointsTo" unit
      \(filled :: PerspectRol) -> do
        filledContextType <- rol_context filled ##>> contextType
        filler' <- addRol_gevuldeRollen filler filledContextType (rol_pspType filled) filledId
        cacheAndSave fillerId filler'

-- | <filledId> `filledNoLongerPointsTo` <fillerId>
-- | Break the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Remove the binding, in other words: change filled. Also removes the bindingDelta.)
-- | NOTE: the second argument is currently useless, but we anticipate with it on multiple fillers.
filledNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectives Unit
filledNoLongerPointsTo filledId fillerId = (try $ getPerspectRol filledId) >>=
  handlePerspectRolError' "filledNoLongerPointsTo" unit
    \(filled :: PerspectRol) -> cacheAndSave filledId (removeRol_binding filled)

-- | <filledId> `filledPointsTo` <fillerId>
-- | Add the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Insert the binding, in other words: change filled)
filledPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectives Unit
filledPointsTo filledId fillerId = (try $ getPerspectRol filledId) >>=
  handlePerspectRolError' "filledPointsTo" unit
    \(filled :: PerspectRol) -> cacheAndSave filledId (changeRol_binding fillerId filled)
