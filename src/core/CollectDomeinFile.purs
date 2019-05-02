module Perspectives.CollectDomeinFile where

import Effect (kind Effect)
import Control.Monad.State (StateT, execStateT, lift, modify, get)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Foreign.Object (lookup)
import Perspectives.ContextAndRole (context_binnenRol, context_buitenRol, context_id, context_rev)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DataTypeTripleGetters (iedereRolInContext) as DTG
import Perspectives.DomeinFile (DomeinFile(..), addContextToDomeinFile, addRolToDomeinFile, defaultDomeinFile)

import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.ModelBasedTripleGetters (boundContexts)
import Perspectives.PerspectivesTypes (ContextDef)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.Syntax (PerspectContext)
import Prelude (Unit, flip, ifM, pure, unit, ($), (==), discard, (<<<), bind, const, (>>=))

-- | From a context, create a DomeinFile (a record that holds an id, maybe a revision and a StrMap of CouchdbResources).
domeinFileFromContext :: forall e. PerspectContext -> MonadPerspectives (AjaxAvarCache e) DomeinFile
domeinFileFromContext enclosingContext = do
  (DomeinFile df) <- execStateT (collect enclosingContext false) defaultDomeinFile
  pure $ DomeinFile $ df {_rev = (context_rev enclosingContext), _id = (context_id enclosingContext)}
  where
    collect :: PerspectContext -> Boolean -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Unit
    collect c definedAtToplevel =
      ifM (saveContext c definedAtToplevel)
        do
          boundContexts <- lift $ ((context_id c) ##= boundContexts)
          for_ boundContexts recursiveCollect
        (pure unit)
      where
        -- On recursively collecting a bound context, it may happen that the binding is not available in cache
        -- and neither is a DomeinFile available that holds a definition. We then accept an error
        -- thrown by getPerspectEntiteit.
        recursiveCollect :: ContextDef -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Unit
        recursiveCollect subContextId = (lift $ getPerspectEntiteit $ unwrap subContextId) >>= (flip collect ((context_id c) == (context_id enclosingContext)))
        saveContext :: PerspectContext -> Boolean -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Boolean
        saveContext ctxt definedAtToplevel' = if (context_id ctxt) `isInNamespace` (context_id enclosingContext)
          then
            ifM (inDomeinFile (context_id ctxt))
              (pure false)
              do
                modify $ addContextToDomeinFile ctxt
                -- As a context and its buitenRol are always created together, we can safely assume the latter exists.
                (lift $ getPerspectEntiteit (context_buitenRol ctxt)) >>= (modify <<< addRolToDomeinFile)
                (lift $ getPerspectEntiteit (context_binnenRol ctxt)) >>= (modify <<< addRolToDomeinFile)
                rollen <- lift $ ((context_id ctxt) ##= DTG.iedereRolInContext)
                for_ rollen
                  \rolID ->
                    -- A context is constructed together with its roles. It seems reasonable to assume the rol will exist.
                    (lift $ getPerspectEntiteit rolID) >>= (modify <<< addRolToDomeinFile)
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
