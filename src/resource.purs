module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff, catchError)
import Control.Monad.Aff.AVar (AVar, isEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, lift, modify)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap (insert)
import Perspectives.ContextAndRole (context_id, context_rev, context_rolInContext, rol_binding, rol_context, rol_id, rol_pspType)
import Perspectives.DomeinCache (DomeinFile(..), defaultDomeinFile)
import Perspectives.Effects (AjaxAvarCache, AvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (isInNamespace, isUserURI)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, encode, getId, representInternally, retrieveInternally, setRevision)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb, saveEntiteit, saveUnversionedEntiteit)
import Perspectives.Syntax (PerspectContext, PerspectRol, revision')

-- TODO: moeten we hier wel fouten afhandelen? En zeker niet stilletjes!
getPerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> Aff (AjaxAvarCache e) (Maybe a)
getPerspectEntiteit id =
  catchError
    do
      (av :: Maybe (AVar a)) <- retrieveInternally id
      case av of
        (Just avar) -> do
          pe <- readVar avar
          pure $ Just pe
        Nothing -> do
          (avar :: (AVar a)) <- representInternally id
          pe <- fetchPerspectEntiteitFromCouchdb id
          putVar pe avar
          pure $ Just pe
    -- ignore errors.
    \_ -> pure Nothing

-- | From a context, create a DomeinFile (a record that holds an id, maybe a revision and a StrMap of CouchdbResources).
domeinFileFromContext :: forall e. PerspectContext -> Aff (AjaxAvarCache e) DomeinFile
domeinFileFromContext c' = do
  (DomeinFile df) <- execStateT (collect c') defaultDomeinFile
  -- pure { _id : context_id c'
  --   , _rev : maybe "" id (context_rev c')
  --   , contexts: contexts
  --   }
  pure $ DomeinFile $ df {_rev = revision' (context_rev c'), _id = (context_id c')}
  where
    collect :: PerspectContext -> StateT DomeinFile (Aff (AjaxAvarCache e)) Unit
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
