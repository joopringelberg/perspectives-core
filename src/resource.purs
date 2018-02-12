module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff, catchError)
import Control.Monad.Aff.AVar (
  AVar, isEmptyVar, makeEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, lift, modify)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap (insert)
import Perspectives.ContextAndRole (context_id, context_rev, context_rolInContext, rol_binding, rol_context, rol_id, rol_pspType)
import Perspectives.DomeinCache (DomeinFile(..), defaultDomeinFile)
import Perspectives.Effects (AjaxAvarCache, AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, encode, getId, representInternally, retrieveInternally, setRevision)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb, createResourceInCouchdb)
import Perspectives.Syntax (PerspectContext, PerspectRol, revision')
import Perspectives.EntiteitAndRDFAliases (ID)

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
    \_ -> pure Nothing

-- | Store an internally created PerspectEntiteit for the first time in the local store.
storePerspectEntiteitInResourceDefinitions :: forall e a. PerspectEntiteit a => ID -> a -> Aff (AvarCache e) Unit
storePerspectEntiteitInResourceDefinitions id e = do
  (av :: AVar a) <- representInternally id
  putVar e av
  pure unit

-- | Modify a PerspectEntiteit in the cache.
changePerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> a -> Aff (AvarCache e) Unit
changePerspectEntiteit id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error $ "changePerspectEntiteit: cannot change an entiteit that is not cached: " <> id
    (Just avar) -> putVar e avar

{-
	- haal AVar op
	- indien aanwezig: breek af
	- indien afwezig:
		- maak AVar
		- sla op in couchdb
		- na afloop: vul AVar met couchdbresource inclusief revision.
-}
-- | Store a freshly created and not yet internally stored resource both internally and in couchdb.
createPerspectEntiteitInCouchdb :: forall e a. PerspectEntiteit a => a -> Aff (AjaxAvarCache e) Unit
createPerspectEntiteitInCouchdb pe = do
    (mAvar :: Maybe (AVar a)) <- retrieveInternally (getId pe)
    case mAvar of
      Nothing -> do
        (avar :: AVar a) <- representInternally (getId pe)
        (rev :: String) <- createResourceInCouchdb (getId pe) (encode pe)
        putVar (setRevision rev pe) avar
      (Just avar) -> pure unit

{-
	- haal AVar op
	- indien leeg, breek af (want de operatie is kennelijk al in uitvoering)
	- anders: lees uit met takeVar
	- sla op in couchdb, ontvang revision
	- na afloop: vul AVar met coudhbresource met revision
-}
-- | A Resource may be created and stored locally, but not sent to the couchdb. Send such resources to
-- | couchdb with this function.
storeExistingCouchdbResourceInCouchdb :: forall e a. PerspectEntiteit a => ID -> Aff (AjaxAvarCache e) Unit
storeExistingCouchdbResourceInCouchdb id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("storeExistingCouchdbResourceInCouchdb needs a locally stored resource for " <> id)
    (Just avar) -> do
      empty <- isEmptyVar avar
      if empty
        then pure unit
        else do
          pe <- takeVar avar
          (rev :: String) <- createResourceInCouchdb id (encode pe)
          putVar (setRevision rev pe) avar

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
                        else pure unit

    insertContext :: PerspectContext -> DomeinFile -> DomeinFile
    insertContext c (DomeinFile dfc@{contexts}) = DomeinFile $ dfc {contexts = insert (context_id c) c contexts}

    insertRol :: PerspectRol -> DomeinFile -> DomeinFile
    insertRol r (DomeinFile dfc@{roles}) = DomeinFile $ dfc {roles = insert (rol_id r) r roles}
