module Perspectives.CollectDomeinFile where

import Prelude
import Control.Monad.Eff (kind Effect)
import Control.Monad.State (StateT, execStateT, lift, modify)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap (insert)
import Perspectives.ContextAndRole (context_buitenRol, context_id, context_rev, rol_id)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFile)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (saveEntiteit)
import Perspectives.Syntax (PerspectContext, PerspectRol, revision')
import Perspectives.SystemQueries (boundContexts, iedereRolInContext)
import Perspectives.TripleAdministration (tripleObjects)
import Perspectives.TripleGetter ((##))

-- | From a context, create a DomeinFile (a record that holds an id, maybe a revision and a StrMap of CouchdbResources).
domeinFileFromContext :: forall e. PerspectContext -> MonadPerspectives (AjaxAvarCache e) DomeinFile
domeinFileFromContext enclosingContext = do
  (DomeinFile df) <- execStateT (collect enclosingContext false) defaultDomeinFile
  pure $ DomeinFile $ df {_rev = revision' (context_rev enclosingContext), _id = (context_id enclosingContext)}
  where
    collect :: PerspectContext -> Boolean -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Unit
    collect c definedAtToplevel =
      ifM (saveContext c definedAtToplevel)
        do
          boundContexts <- lift $ ((context_id c) ## boundContexts)
          -- boundContexts <- lift $ ((context_id c) ## (iedereRolInContext >-> binding))
          for_ (tripleObjects boundContexts) recursiveCollect
        (pure unit)
      where
        recursiveCollect :: ContextID -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Unit
        recursiveCollect subContextId = do
          (mSubContext :: (Maybe PerspectContext)) <- lift $ getPerspectEntiteit subContextId
          case mSubContext of
            (Just subContext) -> collect subContext ((context_id c) == (context_id enclosingContext))
            Nothing -> pure unit
        saveContext :: PerspectContext -> Boolean -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Boolean
        saveContext ctxt definedAtToplevel' = if isInNamespace (context_id ctxt) (context_id enclosingContext)
          then
            do
              modify $ insertContextInDomeinFile ctxt
              mBuitenRol <- lift $ getPerspectEntiteit (context_buitenRol ctxt)
              case mBuitenRol of
                (Just buitenRol) -> modify $ insertRolInDomeinFile buitenRol
                Nothing -> pure unit
              rollen <- lift $ ((context_id ctxt) ## iedereRolInContext)
              for_ (tripleObjects rollen)
                \rolID -> do
                  (mRol :: Maybe PerspectRol) <- lift $ getPerspectEntiteit rolID
                  case mRol of
                    Nothing -> pure unit
                    (Just rol) -> (modify $ insertRolInDomeinFile rol)
              pure true
          else if definedAtToplevel'
            then do
              lift $ void ((saveEntiteit (context_buitenRol ctxt)) :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
              rollen <- lift $ ((context_id ctxt) ## iedereRolInContext)
              for_ (tripleObjects rollen)
                \rolID -> lift ((saveEntiteit rolID)  :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
              lift $ void ((saveEntiteit (context_id ctxt)) :: MonadPerspectives (AjaxAvarCache e) PerspectContext)
              pure true
            else pure false

        -- The same context may be inserted multiple times without consequence; it is an idempotent operation.
        insertContextInDomeinFile :: PerspectContext -> DomeinFile -> DomeinFile
        insertContextInDomeinFile ctxt (DomeinFile dfc@{contexts}) = DomeinFile $ dfc {contexts = insert (context_id ctxt) ctxt contexts}

        insertRolInDomeinFile :: PerspectRol -> DomeinFile -> DomeinFile
        insertRolInDomeinFile r (DomeinFile dfc@{roles}) = DomeinFile $ dfc {roles = insert (rol_id r) r roles}
