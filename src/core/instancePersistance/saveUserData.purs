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

module Perspectives.SaveUserData
  ( saveUserContext
  , removeUserContext
  , removeUserRol)

  where

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.State (StateT, lift)
import Data.Array (cons, nub)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Foreign.Object (values)
import Perspectives.Actions (tearDownBotActions)
import Perspectives.Assignment.Update (saveEntiteit) as Update
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, removeContext_rolInContext, removeRol_gevuldeRollen, rol_pspType)
import Perspectives.CoreTypes (MP, MonadPerspectives, Updater, MonadPerspectivesTransaction)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances (getPerspectEntiteit, removeEntiteit, saveEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Sync.Transaction (_createdContexts, _createdRoles, _deletedContexts)
import Prelude (Unit, bind, discard, join, pure, unit, void, ($), (>>>))

-- | These functions are Updaters, too. They do not push deltas like the Updaters that change PerspectEntities, but
-- | they modify other members of Transaction (createdContexts, deletedContexts, createdRoles, deletedRoles).

-- This function saves a previously cached ContextInstance and all its Roles and adds them to the Transaction
saveUserContext :: Updater ContextInstance
saveUserContext id = do
  (ctxt :: PerspectContext) <- lift $ lift $ saveEntiteit id
  lift $ modify (over _createdContexts (cons ctxt))
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> lift $ lift $ saveEntiteit rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- lift $ lift $ saveEntiteit (context_buitenRol ctxt)
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Remove the ContextInstance both from the cache and from the database and adds it to the Transaction.
removeUserContext :: Updater ContextInstance
removeUserContext id = do
  lift $ modify (over _deletedContexts (cons id))
  lift $ lift $ tearDownBotActions id
  (ctxt :: PerspectContext) <- lift $ lift $ getPerspectEntiteit id
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> removeUserRol_ rol
  void $ removeUserRol_ (context_buitenRol ctxt)
  (_ :: PerspectContext) <- lift $ lift $ removeEntiteit id
  pure unit


-- Removes the rol from the cache and from the database.
-- Removes the rol from the inverse administration of its binding.
-- Removes the rol as binding from all its binders.
-- We do not need to push Delta's, as receiving cores will recompute the effect of the removed Role.
removeUserRol_ :: RoleInstance -> MonadPerspectivesTransaction PerspectRol
removeUserRol_ roleId = do
  -- Remove from couchdb, remove from the cache.
  originalRole@(PerspectRol{context, gevuldeRollen, binding, pspType}) <- lift $ lift $ removeEntiteit roleId :: MonadPerspectives PerspectRol
  case binding of
    Nothing -> pure unit
    (Just oldBindingId) -> do
      -- Remove this roleinstance as a binding role from its binding.
      (oldBinding :: PerspectRol) <- lift $ lift $ getPerspectEntiteit oldBindingId
      Update.saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)

  -- Now handle all Roles that have this Role as binding.
  forWithIndex_ gevuldeRollen \rol filledRollen ->
    -- for each filledRol instance...
    for_ filledRollen \filledRolId -> do
      -- ... remove the removed Role from that instance.
      (filledRol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit filledRolId
      Update.saveEntiteit filledRolId (removeRol_gevuldeRollen filledRol (rol_pspType originalRole) roleId)
  pure originalRole

-- | Adds the Role to the Transaction.
removeUserRol :: Updater RoleInstance
removeUserRol pr = do
  r@(PerspectRol{pspType, context}) <- removeUserRol_ pr
  lift $ modify (over _createdRoles (cons r))
  (pe :: PerspectContext) <- lift $ lift $ getPerspectEntiteit context
  Update.saveEntiteit context (removeContext_rolInContext pe pspType pr)
