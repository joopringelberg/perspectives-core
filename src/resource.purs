module Perspectives.Resource where

import Prelude
import Control.Monad.Aff.AVar (AVar, readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (kind Effect)
import Control.Monad.State (StateT, execStateT, lift, modify)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap (insert)
import Perspectives.ContextAndRole (context_id, context_rev, context_rolInContext, rol_binding, rol_context, rol_id, rol_pspType)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFile)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (isInNamespace, isUserURI)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, retrieveInternally)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb, saveEntiteit)
import Perspectives.Syntax (PerspectContext, PerspectRol, revision')

-- TODO: DE MAYBE KAN ERAF
getPerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) (Maybe a)
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ readVar avar
        pure $ Just pe
      Nothing -> do
        (ent :: a) <- fetchPerspectEntiteitFromCouchdb id
        pure $ Just ent

-- | From a context, create a DomeinFile (a record that holds an id, maybe a revision and a StrMap of CouchdbResources).
domeinFileFromContext :: forall e. PerspectContext -> MonadPerspectives (AjaxAvarCache e) DomeinFile
domeinFileFromContext c' = do
  (DomeinFile df) <- execStateT (collect c') defaultDomeinFile
  -- pure { _id : context_id c'
  --   , _rev : maybe "" id (context_rev c')
  --   , contexts: contexts
  --   }
  pure $ DomeinFile $ df {_rev = revision' (context_rev c'), _id = (context_id c')}
  where
    -- TODO. Je kunt hier niet het type synoniem MonadPerspectives gebruiken, want de compiler klaagt dan over
    -- PartiallyAppliedSynonym: Type synonym Perspectives.PerspectivesState.MonadPerspectives is partially applied.
    -- Type synonyms must be applied to all of their type arguments.
    collect :: PerspectContext -> StateT DomeinFile (MonadPerspectives (AjaxAvarCache e)) Unit
    collect c = do
      modify $ insertContext  c
      for_ (context_rolInContext c)
        \(ids :: Array ID) -> -- These are IDs of role instances!
          for_ ids
            \roleId -> do
              (mRole :: Maybe PerspectRol) <- lift $ getPerspectEntiteit roleId
              case mRole of
                Nothing -> pure unit
                (Just (rolInContext :: PerspectRol)) -> do
                  modify $ insertRol rolInContext
                  mBinding <- pure (rol_binding rolInContext)
                  case mBinding of
                    Nothing -> pure unit
                    (Just (binding :: ID)) ->
                      if isInNamespace (context_id c) binding
                        then do
                          (mBuitenRol :: Maybe PerspectRol) <- lift $ getPerspectEntiteit binding
                          case mBuitenRol of
                            (Just (buitenRol :: PerspectRol)) -> if rol_pspType buitenRol == "model:Perspectives$BuitenRol"
                              then do
                                modify $ insertRol buitenRol
                                (mContext :: Maybe PerspectContext) <- lift $ getPerspectEntiteit (rol_context buitenRol)
                                case mContext of
                                  Nothing -> pure unit
                                  (Just context) -> collect context
                              else pure unit
                            Nothing -> pure unit
                        else if isUserURI binding
                          -- Save the resource by itself, iff it is in the user domein.
                        then do
                          (mBuitenRol :: Maybe PerspectRol) <- lift $ getPerspectEntiteit binding
                          case mBuitenRol of
                            (Just (buitenRol :: PerspectRol)) -> if rol_pspType buitenRol == "model:Perspectives$BuitenRol"
                              then do
                                (_ :: PerspectRol) <- lift (saveEntiteit binding)
                                (_ :: PerspectContext) <- lift $ saveEntiteit (rol_context buitenRol)
                                pure unit
                              else pure unit
                            Nothing -> pure unit
                          else pure unit

    insertContext :: PerspectContext -> DomeinFile -> DomeinFile
    insertContext c (DomeinFile dfc@{contexts}) = DomeinFile $ dfc {contexts = insert (context_id c) c contexts}

    insertRol :: PerspectRol -> DomeinFile -> DomeinFile
    insertRol r (DomeinFile dfc@{roles}) = DomeinFile $ dfc {roles = insert (rol_id r) r roles}
