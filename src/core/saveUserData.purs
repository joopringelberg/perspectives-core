module Perspectives.SaveUserData where

import Control.Monad.State (StateT)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Perspectives.Actions (tearDownBotActions, updatePerspectEntiteit', updatePerspectEntiteitMember')
import Perspectives.ContextAndRole (context_id, removeRol_binding, removeRol_gevuldeRollen, rol_id)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DataTypeObjectGetters (iedereRolInContext)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (binnenRol, buitenRol) as ID
import Perspectives.Resource (getPerspectEntiteit, removeEntiteit)
import Perspectives.ResourceRetrieval (saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol(..))
import Prelude (Unit, pure, unit, discard, bind, (>>>))

type UserDataState = Array ID

type MonadSaveUserData = StateT UserDataState MonadPerspectives

saveDomeinFileAsUserData :: DomeinFile -> MonadPerspectives Unit
saveDomeinFileAsUserData (DomeinFile{contexts, roles}) = do
  for_ contexts (context_id >>> saveEntiteitPreservingVersion :: ID -> MP PerspectContext)
  for_ roles (rol_id >>> saveEntiteitPreservingVersion :: ID -> MP PerspectRol)

saveUserContext :: ID -> MonadPerspectives Unit
saveUserContext id = do
  (_ :: PerspectContext) <- saveEntiteitPreservingVersion id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> saveEntiteitPreservingVersion rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.buitenRol id)
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.binnenRol id)
  pure unit

-- TODO:
-- * remove tripleAdministration.
removeUserContext :: ID -> MonadPerspectives Unit
removeUserContext id = do
  tearDownBotActions id
  (_ :: PerspectContext) <- getPerspectEntiteit id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> removeRol rol
  removeRol (ID.buitenRol id)
  (_ :: PerspectRol) <- removeEntiteit (ID.binnenRol id)
  (_ :: PerspectContext) <- removeEntiteit id
  pure unit
  where
    -- Removes the rol from the cache and from the database.
    -- Removes the rol from the inverse administration of its binding.
    -- Removes the rol as binding from all its binders.
    removeRol :: String -> MonadPerspectives Unit
    removeRol pr = do
      (PerspectRol{gevuldeRollen, binding, pspType}) <- removeEntiteit pr :: MonadPerspectives PerspectRol
      case binding of
        Nothing -> pure unit
        (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob pspType pr
      forWithIndex_ gevuldeRollen \rol filledRollen ->
        for_ filledRollen \filledRol ->
          updatePerspectEntiteit'
            ((\_ (r :: PerspectRol) -> removeRol_binding r) )
            filledRol
            ""
