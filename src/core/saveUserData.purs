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
import Perspectives.Syntax (PerspectRol)
import Prelude (Unit, ifM, pure, unit, (==), discard, (>>=), bind, ($), const, void, (>=>), (<<<), map)

type UserDataState = Array ID

type MonadSaveUserData e = StateT UserDataState (MonadPerspectives e)

saveUserData :: forall e. Array BuitenRol -> MonadPerspectives (AjaxAvarCache e) Unit
saveUserData buitenRollen = evalStateT (handleUserData' saveEntiteit buitenRollen) []
  where
    saveEntiteit :: String -> MonadSaveUserData (AjaxAvarCache e) PerspectRol
    saveEntiteit id = lift $ saveEntiteitPreservingVersion id

removeUserData :: forall e. Array BuitenRol -> MonadPerspectives (AjaxAvarCache e) Unit
removeUserData buitenRollen = evalStateT (handleUserData' removeEntiteit' buitenRollen) []
  where
    removeEntiteit' :: String -> MonadSaveUserData (AjaxAvarCache e) PerspectRol
    removeEntiteit' id = do
      ent <- lift $ getPerspectEntiteit id
      lift $ removeEntiteit id ent

handleUserData' :: forall e a. PerspectEntiteit a =>
  (String -> MonadSaveUserData (AjaxAvarCache e) a) ->
  Array BuitenRol ->
  MonadSaveUserData (AjaxAvarCache e) Unit
handleUserData' handleEntiteit buitenRollen = void $ for buitenRollen handleBuitenRol
  where
    handleBuitenRol :: BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
    handleBuitenRol rolId = ifM (seenBefore $ unwrap rolId)
      (pure unit)
      (do
        haveSeen $ unwrap rolId
        void $ handleEntiteit $ unwrap rolId
        lift (rolId ##>> context) >>= handleContext
      )

    handleContext :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    handleContext contextId = ifM (seenBefore contextId)
      (pure unit)
      do
        haveSeen contextId
        void $ handleEntiteit contextId
        rollen <- lift $ iedereRolInContext contextId
        void $ for rollen \rol -> do
          haveSeen rol
          void $ handleEntiteit rol

    seenBefore :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex id ids)

    haveSeen :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    haveSeen id = modify (cons id)

    isBuitenRol :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    isBuitenRol id = do
      r <- lift $ (id %%>> genericContext /-/ buitenRol >=> pure <<< map unwrap)
      pure $ id == r
