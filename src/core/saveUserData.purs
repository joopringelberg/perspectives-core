module Perspectives.SaveUserData where

import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Array (elemIndex, cons)
import Data.Maybe (maybe, Maybe(..))
import Data.Traversable (for)
import Perspectives.CoreTypes (MonadPerspectives, (%%>>), (%%>))
import Perspectives.DataTypeObjectGetters (binding, buitenRol, context, iedereRolInContext)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.ResourceRetrieval (saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, ifM, pure, unit, (==), discard, (>>=), bind, ($), const, void)

type UserDataState = Array ID

type MonadSaveUserData e = StateT UserDataState (MonadPerspectives e)

saveUserData :: forall e. Array ID -> MonadPerspectives (AjaxAvarCache e) Unit
saveUserData buitenRollen = evalStateT (saveUserData' buitenRollen) []

saveUserData' :: forall e. Array ID -> MonadSaveUserData (AjaxAvarCache e) Unit
saveUserData' buitenRollen = void $ for buitenRollen saveBuitenRol
  where
    saveBuitenRol :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    saveBuitenRol rolId = ifM (seenBefore rolId)
      (pure unit)
      (do
        haveSeen rolId
        lift $ void (saveEntiteitPreservingVersion rolId :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
        lift (rolId %%>> context) >>= saveContext
      )

    saveContext :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    saveContext contextId = ifM (seenBefore contextId)
      (pure unit)
      (do
        haveSeen contextId
        lift $ void (saveEntiteitPreservingVersion contextId :: MonadPerspectives (AjaxAvarCache e) PerspectContext)
        rollen <- lift $ iedereRolInContext contextId
        void $ for rollen \rol -> do
          haveSeen rol
          lift $ void (saveEntiteitPreservingVersion rol :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
          mbnd <- lift (rol %%> binding)
          case mbnd of
            Nothing -> pure unit
            (Just bnd) -> ifM (isBuitenRol bnd)
              (saveBuitenRol bnd)
              (pure unit))

    seenBefore :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex id ids)

    haveSeen :: ID -> MonadSaveUserData (AjaxAvarCache e) Unit
    haveSeen id = modify (cons id)

    isBuitenRol :: ID -> MonadSaveUserData (AjaxAvarCache e) Boolean
    isBuitenRol id = do
      r <- lift $ (id %%>> context /-/ buitenRol)
      pure $ id == r

    -- isBuitenRol id = do
    --   beschrevenBuitenRolType <- lift $ (id %%>> context /-/ contextType /-/ buitenRolBeschrijvingDef) -- "model:Perspectives$ContextPrototype$buitenRolBeschrijving"
    --   buitenRolType <- lift $ (id %%>> rolType) -- TODO "model:Systeem$TrustedCluster$buitenRolBeschrijving"
    --   pure $ beschrevenBuitenRolType == buitenRolType
