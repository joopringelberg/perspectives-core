module Perspectives.SaveUserData where

import Control.Monad.State (StateT, evalStateT, get, lift, modify)
import Data.Array (cons, elemIndex)
import Data.Maybe (maybe, Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for)
import Perspectives.CoreTypes (MonadPerspectives, (##>), (##>>), MP)
import Perspectives.DataTypeObjectGetters (binding, context, iedereRolInContext)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectivesTypesInPurescript (class ContextType, BuitenRol(..), ContextDef, RolInContext)
import Perspectives.ResourceRetrieval (saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, ifM, pure, unit, discard, (>>=), bind, ($), const, void)

type UserDataState = Array ID

type MonadSaveUserData e = StateT UserDataState (MonadPerspectives e)

saveUserData :: forall e. Array BuitenRol -> MonadPerspectives (AjaxAvarCache e) Unit
saveUserData buitenRollen = evalStateT (saveUserData' buitenRollen) []

saveUserData' :: forall e. Array BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
saveUserData' buitenRollen = void $ for buitenRollen saveBuitenRol
  where
    saveBuitenRol :: BuitenRol -> MonadSaveUserData (AjaxAvarCache e) Unit
    saveBuitenRol rolId = ifM (seenBefore rolId)
      (pure unit)
      (do
        haveSeen rolId
        lift $ void (saveEntiteitPreservingVersion (unwrap rolId) :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
        -- In the next statement, notice the arbitrary choice for ContextDef. Any instance of ContextType satisfies the purescript compiler.
        lift ((rolId ##>> context) :: MP e ContextDef) >>= saveContext
      )

    saveContext :: forall c. ContextType c => c -> MonadSaveUserData (AjaxAvarCache e) Unit
    saveContext contextId = ifM (seenBefore contextId)
      (pure unit)
      (do
        haveSeen contextId
        lift $ void (saveEntiteitPreservingVersion (unwrap contextId) :: MonadPerspectives (AjaxAvarCache e) PerspectContext)
        (rollen :: Array RolInContext) <- lift $ iedereRolInContext contextId
        void $ for rollen \rol -> do
          haveSeen rol
          lift $ void (saveEntiteitPreservingVersion (unwrap rol) :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
          mbnd <- lift (rol ##> binding)
          case mbnd of
            (Just bnd@(BuitenRol _)) -> saveBuitenRol bnd
            otherwise -> pure unit)

    seenBefore :: forall t. Newtype t String => t -> MonadSaveUserData (AjaxAvarCache e) Boolean
    seenBefore id = get >>= \ids -> pure $ maybe false (const true) (elemIndex (unwrap id) ids)

    haveSeen :: forall t. Newtype t String => t -> MonadSaveUserData (AjaxAvarCache e) Unit
    haveSeen id = modify (cons (unwrap id))
