module Perspectives.CollectDomeinFile where

import Control.Monad.Eff (kind Effect)
import Control.Monad.State (StateT, execStateT, lift, modify, get)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (lookup)
import Perspectives.ContextAndRole (context_buitenRol, context_id, context_rev)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DataTypeTripleGetters (iedereRolInContextM)
import Perspectives.DomeinFile (DomeinFile(..), addContextToDomeinFile, addRolToDomeinFile, defaultDomeinFile)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.ModelBasedTripleGetters (boundContextsM)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (saveEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.Syntax (PerspectContext, PerspectRol, revision')
import Prelude (Unit, flip, ifM, pure, unit, ($), (==), discard, (<<<), bind, (&&), void, const, (>>=))

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
          boundContexts <- lift $ ((context_id c) ##= boundContextsM)
          for_ boundContexts recursiveCollect
        (pure unit)
      where
        recursiveCollect :: ContextID -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Unit
        recursiveCollect subContextId = (lift $ getPerspectEntiteit subContextId) >>= (flip collect ((context_id c) == (context_id enclosingContext)))
        saveContext :: PerspectContext -> Boolean -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Boolean
        saveContext ctxt definedAtToplevel' = if (context_id ctxt) `isInNamespace` (context_id enclosingContext)
          then
            ifM (inDomeinFile (context_id ctxt))
              (pure false)
              do
                modify $ addContextToDomeinFile ctxt
                (lift $ getPerspectEntiteit (context_buitenRol ctxt)) >>= (modify <<< addRolToDomeinFile)
                rollen <- lift $ ((context_id ctxt) ##= iedereRolInContextM)
                for_ rollen
                  \rolID ->
                    (lift $ getPerspectEntiteit rolID) >>= (modify <<< addRolToDomeinFile)
                pure true
          else if definedAtToplevel' && (context_id ctxt) `isInNamespace` "model:User"
            then if (savedToCouchdb ctxt)
              then pure false
              else
                do
                lift $ void ((saveEntiteit (context_buitenRol ctxt)) :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
                rollen <- lift $ ((context_id ctxt) ##= iedereRolInContextM)
                for_ rollen
                  \rolID -> lift ((saveEntiteit rolID)  :: MonadPerspectives (AjaxAvarCache e) PerspectRol)
                lift $ void ((saveEntiteit (context_id ctxt)) :: MonadPerspectives (AjaxAvarCache e) PerspectContext)
                pure true
            else pure false

        inDomeinFile :: ID -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Boolean
        inDomeinFile id = do
          (DomeinFile {contexts, roles}) <- get
          case lookup id contexts of
            (Just _) -> pure true
            Nothing -> case lookup id roles of
              (Just _) -> pure true
              otherwise -> pure false

        savedToCouchdb :: PerspectContext -> Boolean
        savedToCouchdb ctxt = maybe false (const true) (context_rev ctxt)
