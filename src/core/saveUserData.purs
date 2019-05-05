module Perspectives.SaveUserData where

import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Array (elemIndex, cons)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, for_)
import Perspectives.ContextAndRole (context_id, rol_id)
import Perspectives.CoreTypes (MonadPerspectives, (%%>>), (##>>), MP)
import Perspectives.DataTypeObjectGetters (buitenRol, context, genericContext, iedereRolInContext)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (binnenRol, buitenRol, buitenToBinnenRol) as ID
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectEntiteit (class PerspectEntiteit)
import Perspectives.PerspectivesTypes (BuitenRol)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (removeEntiteit, saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, ifM, pure, unit, (==), discard, (>>=), bind, ($), const, void, (>=>), (<<<), map, (>>>))

type UserDataState = Array ID

type MonadSaveUserData = StateT UserDataState MonadPerspectives

saveDomeinFileAsUserData :: DomeinFile -> MonadPerspectives Unit
saveDomeinFileAsUserData (DomeinFile{contexts, roles}) = do
  for_ contexts (context_id >>> saveEntiteitPreservingVersion :: ID -> MP PerspectContext)
  for_ roles (rol_id >>> saveEntiteitPreservingVersion :: ID -> MP PerspectRol)

saveUserContext :: ID -> MonadPerspectives Unit
saveUserContext id = do
  (_ :: PerspectContext) <- saveEntiteitPreservingVersion id
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.buitenRol id)
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.binnenRol id)
  pure unit

saveUserData :: Array BuitenRol -> MonadPerspectives Unit
saveUserData buitenRollen = evalStateT (saveUserData' buitenRollen) []

saveUserData' :: Array BuitenRol -> MonadSaveUserData Unit
saveUserData' buitenRollen = void $ for buitenRollen saveBuitenRol
  where
    saveEntiteit :: forall a. PerspectEntiteit a => String -> MonadSaveUserData a
    saveEntiteit id = lift $ saveEntiteitPreservingVersion id

    saveBuitenRol :: BuitenRol -> MonadSaveUserData Unit
    saveBuitenRol rolId = ifM (seenBefore $ unwrap rolId)
      (pure unit)
      (do
        haveSeen $ unwrap rolId
        void (saveEntiteit (unwrap rolId) :: MonadSaveUserData PerspectRol)
        lift (rolId ##>> context) >>= saveContext
        void (saveEntiteit (ID.buitenToBinnenRol $ unwrap rolId) :: MonadSaveUserData PerspectRol)
      )

    saveContext :: ID -> MonadSaveUserData Unit
    saveContext contextId = ifM (seenBefore contextId)
      (pure unit)
      do
        haveSeen contextId
        void (saveEntiteit contextId :: MonadSaveUserData PerspectContext)
        rollen <- lift $ iedereRolInContext contextId
        void $ for rollen \(rol :: String) -> do
          haveSeen rol
          void (saveEntiteit rol :: MonadSaveUserData PerspectRol)
        pure unit

    seenBefore :: ID -> MonadSaveUserData Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex id ids)

    haveSeen :: ID -> MonadSaveUserData Unit
    haveSeen id = void $ modify (cons id)

    isBuitenRol :: ID -> MonadSaveUserData Boolean
    isBuitenRol id = do
      r <- lift $ (id %%>> genericContext /-/ buitenRol >=> pure <<< map unwrap)
      pure $ id == r

removeUserData :: Array BuitenRol -> MonadPerspectives Unit
removeUserData buitenRollen = evalStateT (removeUserData' buitenRollen) []

removeUserData' :: Array BuitenRol -> MonadSaveUserData Unit
removeUserData' buitenRollen = void $ for buitenRollen removeBuitenRol
  where
    removeEntiteit' :: forall a. PerspectEntiteit a => String -> MonadSaveUserData a
    removeEntiteit' id = do
      ent <- lift $ getPerspectEntiteit id
      lift $ removeEntiteit id ent

    removeBuitenRol :: BuitenRol -> MonadSaveUserData Unit
    removeBuitenRol rolId = ifM (seenBefore $ unwrap rolId)
      (pure unit)
      (do
        haveSeen $ unwrap rolId
        -- remove the binnenRol.
        void (removeEntiteit' (ID.buitenToBinnenRol (unwrap rolId)) :: MonadSaveUserData PerspectRol)
        lift (rolId ##>> context) >>= removeContext
        void (removeEntiteit' (unwrap rolId) :: MonadSaveUserData PerspectRol)
      )

    removeContext :: ID -> MonadSaveUserData Unit
    removeContext contextId = ifM (seenBefore contextId)
      (pure unit)
      do
        haveSeen contextId
        rollen <- lift $ iedereRolInContext contextId
        void (removeEntiteit' contextId :: MonadSaveUserData PerspectContext)
        void $ for rollen \rol -> do
          haveSeen rol
          (removeEntiteit' rol :: MonadSaveUserData PerspectRol)

    seenBefore :: ID -> MonadSaveUserData Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex id ids)

    haveSeen :: ID -> MonadSaveUserData Unit
    haveSeen id = void $ modify (cons id)

    isBuitenRol :: ID -> MonadSaveUserData Boolean
    isBuitenRol id = do
      r <- lift $ (id %%>> genericContext /-/ buitenRol >=> pure <<< map unwrap)
      pure $ id == r
