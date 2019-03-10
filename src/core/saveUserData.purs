module Perspectives.SaveUserData where

import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Array (elemIndex, cons)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Perspectives.CoreTypes (MonadPerspectives, (%%>>), (##>>))
import Perspectives.DataTypeObjectGetters (buitenRol, context, genericContext, iedereRolInContext)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectEntiteit (class PerspectEntiteit)
import Perspectives.PerspectivesTypes (BuitenRol)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (removeEntiteit, saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, ifM, pure, unit, (==), discard, (>>=), bind, ($), const, void, (>=>), (<<<), map, (>>>))

type UserDataState = Array ID

type MonadSaveUserData e = StateT UserDataState (MonadPerspectives e)

saveUserData :: forall e. Array BuitenRol -> MonadPerspectives (AjaxAvarCache e) Unit
saveUserData buitenRollen = evalStateT (saveUserData' buitenRollen) []

saveUserData' :: forall e. Array BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
saveUserData' buitenRollen = void $ for buitenRollen saveBuitenRol
  where
    saveEntiteit :: forall a. PerspectEntiteit a => String -> MonadSaveUserData (AjaxAvarCache e) a
    saveEntiteit id = lift $ saveEntiteitPreservingVersion id

    saveBuitenRol :: BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
    saveBuitenRol rolId = ifM (seenBefore $ unwrap rolId)
      (pure unit)
      (do
        haveSeen $ unwrap rolId
        void (saveEntiteit (unwrap rolId) :: MonadSaveUserData (AjaxAvarCache e) PerspectRol)
        lift (rolId ##>> context) >>= saveContext
      )

    saveContext :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    saveContext contextId = ifM (seenBefore contextId)
      (pure unit)
      do
        haveSeen contextId
        void (saveEntiteit contextId :: MonadSaveUserData (AjaxAvarCache e) PerspectContext)
        rollen <- lift $ iedereRolInContext contextId
        void $ for rollen \rol -> do
          haveSeen rol
          void (saveEntiteit rol :: MonadSaveUserData (AjaxAvarCache e) PerspectRol)

    seenBefore :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex id ids)

    haveSeen :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    haveSeen id = modify (cons id)

    isBuitenRol :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    isBuitenRol id = do
      r <- lift $ (id %%>> genericContext /-/ buitenRol >=> pure <<< map unwrap)
      pure $ id == r

removeUserData :: forall e. Array BuitenRol -> MonadPerspectives (AjaxAvarCache e) Unit
removeUserData buitenRollen = evalStateT (removeUserData' buitenRollen) []

removeUserData' :: forall e. Array BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
removeUserData' buitenRollen = void $ for buitenRollen removeBuitenRol
  where
    removeEntiteit' :: forall a. PerspectEntiteit a => String -> MonadSaveUserData (AjaxAvarCache e) a
    removeEntiteit' id = do
      ent <- lift $ getPerspectEntiteit id
      lift $ removeEntiteit id ent

    removeBuitenRol :: BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
    removeBuitenRol rolId = ifM (seenBefore $ unwrap rolId)
      (pure unit)
      (do
        haveSeen $ unwrap rolId
        lift (rolId ##>> context) >>= removeContext
        void (removeEntiteit' (unwrap rolId) :: MonadSaveUserData (AjaxAvarCache e) PerspectRol)
      )

    removeContext :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    removeContext contextId = ifM (seenBefore contextId)
      (pure unit)
      do
        haveSeen contextId
        rollen <- lift $ iedereRolInContext contextId
        void (removeEntiteit' contextId :: MonadSaveUserData (AjaxAvarCache e) PerspectContext)
        void $ for rollen \rol -> do
          haveSeen rol
          (removeEntiteit' rol :: MonadSaveUserData (AjaxAvarCache e) PerspectRol)

    seenBefore :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex id ids)

    haveSeen :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    haveSeen id = modify (cons id)

    isBuitenRol :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    isBuitenRol id = do
      r <- lift $ (id %%>> genericContext /-/ buitenRol >=> pure <<< map unwrap)
      pure $ id == r
