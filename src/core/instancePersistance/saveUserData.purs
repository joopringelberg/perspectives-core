{-
  TODO. Restore the functionality to remove data once Actions is back on line.
-}
module Perspectives.SaveUserData where

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.State (StateT, lift)
import Data.Array (cons, nub)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Foreign.Object (values)
import Perspectives.Assignment.Update (saveEntiteit)
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, removeContext_rolInContext, removeRol_gevuldeRollen, rol_pspType)
import Perspectives.CoreTypes (MP, MonadPerspectives, Updater, MonadPerspectivesTransaction)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances (getPerspectEntiteit, removeEntiteit, saveEntiteitPreservingVersion)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Sync.Transactie (_createdContexts, _createdRoles, _deletedContexts)
import Prelude (Unit, bind, discard, join, pure, unit, void, ($), (>>>))

-- | These functions are Updaters, too. They do not push deltas like the Updaters that change PerspectEntities, but
-- | they modify other members of Transactie (createdContexts, deletedContexts, createdRoles, deletedRoles).

type UserDataState = Array ID

type MonadSaveUserData = StateT UserDataState MonadPerspectives

saveDomeinFileAsUserData :: Array String -> MonadPerspectives Unit
saveDomeinFileAsUserData ids = do
  for_ ids (deconstructBuitenRol >>> ContextInstance >>> saveEntiteitPreservingVersion :: ContextInstance -> MP PerspectContext)

-- This function saves a previously cached ContextInstance and adds it to the Transactie
saveUserContext :: Updater ContextInstance
saveUserContext id = do
  (ctxt :: PerspectContext) <- lift $ lift $ saveEntiteitPreservingVersion id
  lift $ modify (over _createdContexts (cons ctxt))
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> lift $ lift $ saveEntiteitPreservingVersion rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- lift $ lift $ saveEntiteitPreservingVersion (context_buitenRol ctxt)
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Remove the ContextInstance both from the cache and from the database and adds it to the Transactie.
removeUserContext :: Updater ContextInstance
removeUserContext id = do
  lift $ modify (over _deletedContexts (cons id))
  -- TODO: tearDownBotActions id
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
      saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)

  -- Now handle all Roles that have this Role as binding.
  forWithIndex_ gevuldeRollen \rol filledRollen ->
    -- for each filledRol instance...
    for_ filledRollen \filledRolId -> do
      -- ... remove the removed Role from that instance.
      (filledRol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit filledRolId
      saveEntiteit filledRolId (removeRol_gevuldeRollen filledRol (rol_pspType originalRole) roleId)
  pure originalRole

-- | Adds the Role to the Transactie.
removeUserRol :: Updater RoleInstance
removeUserRol pr = do
  r@(PerspectRol{pspType, context}) <- removeUserRol_ pr
  lift $ modify (over _createdRoles (cons r))
  (pe :: PerspectContext) <- lift $ lift $ getPerspectEntiteit context
  saveEntiteit context (removeContext_rolInContext pe pspType pr)
