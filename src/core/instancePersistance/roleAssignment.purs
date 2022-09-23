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
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##>), (##>>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie)
import Perspectives.DependencyTracking.Dependency (findMeRequests)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Instances.ObjectGetters (contextType)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol)
import Perspectives.Query.UnsafeCompiler (getMyType, getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Prelude (Unit, bind, discard, pure, unit, ($), (>>=))

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
        Nothing -> cacheAndSave contextId (changeContext_me ctxt Nothing)
        Just myType -> do
          mme <- lift (contextId ##> getRoleInstances myType)
          case mme of
            Nothing -> cacheAndSave contextId (changeContext_me ctxt Nothing)
            Just me -> roleIsMe me contextId

-- | Set isMe of the roleInstance to false.
roleIsNotMe :: RoleInstance -> MonadPerspectivesTransaction Unit
roleIsNotMe roleId = (lift $ try $ getPerspectRol roleId) >>=
  handlePerspectRolError "roleIsNotMe"
    \role -> do
      cacheAndSave roleId (changeRol_isMe role false)

-- | Set isMe of the roleInstance to true.
-- | Set me of the context to the roleInstance.
roleIsMe :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
roleIsMe roleId contextId = (lift $ try $ getPerspectContext contextId) >>=
  handlePerspectContextError "roleIsMe"
    \ctxt -> (lift $ try $ getPerspectRol roleId) >>=
      handlePerspectRolError "roleIsMe"
        \role -> do
          (lift $ findMeRequests contextId)  >>= addCorrelationIdentifiersToTransactie
          cacheAndSave roleId (changeRol_isMe role true)
          cacheAndSave contextId (changeContext_me ctxt (Just roleId))

-- | <fillerId> `fillerNoLongerPointsTo` <filledId>
-- | Break the link from filler to filled (FILLS link)
-- | (Remove from the filledRoles, in other words: change filler)
-- | Not the other way round!
-- | Removes filled from filledRoles of filler (because filler no longer fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
fillerNoLongerPointsTo fillerId filledId = (lift $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError' "fillerNoLongerPointsTo" unit
    \(filler :: PerspectRol) -> (lift $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError "fillerNoLongerPointsTo"
      \(filled :: PerspectRol) -> do
        filledContextType <- lift (rol_context filled ##>> contextType)
        filler' <- pure $ (removeRol_gevuldeRollen filler filledContextType (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <fillerId> `fillerPointsTo` <filledId>
-- | Add the link from filler to filled (FILLS link)
-- | (Add to the filledRoles, in other words: change filler)
-- | Not the other way round!
-- | Adds filled from filledRoles of filler (because filler now fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
fillerPointsTo fillerId filledId = (lift $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError' "fillerNoLongerPointsTo" unit
    \(filler :: PerspectRol) -> (lift $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError' "fillerNoLongerPointsTo" unit
      \(filled :: PerspectRol) -> do
        filledContextType <- lift (rol_context filled ##>> contextType)
        filler' <- lift (addRol_gevuldeRollen filler filledContextType (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <filledId> `filledNoLongerPointsTo` <fillerId>
-- | Break the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Remove the binding, in other words: change filled. Also removes the bindingDelta.)
-- | NOTE: the second argument is currently useless, but we anticipate with it on multiple fillers.
filledNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
filledNoLongerPointsTo filledId fillerId = (lift $ try $ getPerspectEntiteit filledId) >>=
  handlePerspectRolError' "filledNoLongerPointsTo" unit
    \(filled :: PerspectRol) -> cacheAndSave filledId (removeRol_binding filled)

-- | <filledId> `filledPointsTo` <fillerId>
-- | Add the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Insert the binding, in other words: change filled)
filledPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
filledPointsTo filledId fillerId = (lift $ try $ getPerspectEntiteit filledId) >>=
  handlePerspectRolError' "filledPointsTo" unit
    \(filled :: PerspectRol) -> cacheAndSave filledId (changeRol_binding fillerId filled)