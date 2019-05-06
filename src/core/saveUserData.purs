module Perspectives.SaveUserData where

import Control.Monad.State (StateT)
import Data.Traversable (for_)
import Perspectives.ContextAndRole (context_id, rol_id)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DataTypeObjectGetters (iedereRolInContext)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (binnenRol, buitenRol) as ID
import Perspectives.Resource (getPerspectEntiteit, removeEntiteit)
import Perspectives.ResourceRetrieval (saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol)
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

removeUserContext :: ID -> MonadPerspectives Unit
removeUserContext id = do
  (_ :: PerspectContext) <- getPerspectEntiteit id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> removeEntiteit rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- removeEntiteit (ID.buitenRol id)
  (_ :: PerspectRol) <- removeEntiteit (ID.binnenRol id)
  (_ :: PerspectContext) <- removeEntiteit id
  pure unit
