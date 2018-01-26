module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff, catchError)
import Control.Monad.Aff.AVar (AVAR, AVar, isEmptyVar, makeEmptyVar, makeVar, putVar, readVar, takeVar)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, lift, modify)
import Data.Argonaut (fromString)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (empty, insert)
import Network.HTTP.Affjax (AJAX)
import Perspectives.ContextAndRole (context_id, context_rev, context_rolInContext, rol_binding, rol_context, rol_pspType)
import Perspectives.DomeinCache (DomeinFile, DomeinFileContexts)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.ResourceRetrieval (fetchCouchdbResource, createResourceInCouchdb)
import Perspectives.ResourceTypes (DomeinFileEffects, PropDefs(..), Resource, CouchdbResource)
import Perspectives.Syntax (PerspectContext, PerspectRol, ID)

-- | The global index of definitions of all resources, indexed by Resource.
type ResourceDefinitions = GLStrMap (AVar CouchdbResource)

resourceDefinitions :: ResourceDefinitions
resourceDefinitions = new unit

foreign import data PROPDEFS :: Effect

-- | Get the property definitions of a Resource.
getPropDefs :: forall e. Resource -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) PropDefs
getPropDefs id = do
  cdbr <- getCouchdbResource id
  pure $ PropDefs cdbr

-- | Get the property definitions of a Resource.
getRole :: forall e. Resource -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) (Maybe PerspectRol)
getRole id = catchError
  do
    cdbr <- getCouchdbResource id
    pure $ Just $ castPerspectRol cdbr
  \_ -> pure Nothing

-- | Get the property definitions of a Resource.
getContext :: forall e. Resource -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) (Maybe PerspectContext)
getContext id = catchError
  do
    cdbr <- getCouchdbResource id
    pure $ Just $ castPerspectContext cdbr
  \_ -> pure Nothing

foreign import castPerspectRol :: CouchdbResource -> PerspectRol

foreign import castPerspectContext :: CouchdbResource -> PerspectContext

foreign import unwrapPerspectRol :: PerspectRol -> CouchdbResource

foreign import unwrapPerspectContext :: PerspectContext -> CouchdbResource

-- | Get the property definitions of a Resource.
getCouchdbResource :: forall e. Resource -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) CouchdbResource
getCouchdbResource id = do
  av <- liftEff $ peek resourceDefinitions id
  case av of
    (Just avar) -> readVar avar
    Nothing -> do
      avar <- makeEmptyVar
      pd <- fetchCouchdbResource id
      putVar pd avar
      pure pd

getResourceAVar :: forall e. Resource -> Aff (avar :: AVAR, gm :: GLOBALMAP | e) (AVar CouchdbResource)
getResourceAVar id = do
  propDefs <- liftEff $ peek resourceDefinitions id
  case propDefs of
    Nothing -> do
      ev <- makeEmptyVar
      _ <- liftEff $ poke resourceDefinitions id ev
      pure ev
    (Just avar) -> pure avar

storeContextInResourceDefinitions :: forall e. String -> PerspectContext -> Aff (DomeinFileEffects e) Unit
storeContextInResourceDefinitions key c = do
  av <- makeVar (unwrapPerspectContext c)
  _ <- liftEff $ poke resourceDefinitions key av
  pure unit

storeRoleInResourceDefinitions :: forall e. String -> PerspectRol -> Aff (DomeinFileEffects e) Unit
storeRoleInResourceDefinitions key r = do
  av <- makeVar (unwrapPerspectRol r)
  _ <- liftEff $ poke resourceDefinitions key av
  pure unit

-- | Save the context in couchdb. Uses AVar as semaphore to order overlapping writes.
-- | Can be used to create a document in couchdb and to update it.
storeCouchdbResourceInCouchdb :: forall e. Resource -> Aff (ajax :: AJAX, avar :: AVAR, gm :: GLOBALMAP | e) Unit
storeCouchdbResourceInCouchdb id = do
  av <- getResourceAVar id
  cdbr <- takeVar av
  (rev :: String) <- createResourceInCouchdb (context_id (castPerspectContext cdbr)) cdbr
  putVar (insert "_rev" (fromString rev) cdbr) av

{-
	- haal AVar op
	- indien aanwezig: breek af
	- indien afwezig:
		- maak AVar
		- sla op in couchdb
		- na afloop: vul AVar met couchdbresource inclusief revision.
-}
createCouchdbResourceInCouchdb :: forall e. CouchdbResource -> Aff (ajax :: AJAX, avar :: AVAR, gm :: GLOBALMAP | e) Unit
createCouchdbResourceInCouchdb cdbr = do
    id <- pure (context_id (castPerspectContext cdbr))
    mAvar <- liftEff $ peek resourceDefinitions id
    case mAvar of
      Nothing -> do
        avar <- makeEmptyVar
        _ <- liftEff $ poke resourceDefinitions id avar
        (rev :: String) <- createResourceInCouchdb id cdbr
        putVar (insert "_rev" (fromString rev) cdbr) avar
      (Just avar) -> pure unit

{-
	- haal AVar op
	- indien leeg, breek af (want de operatie is kennelijk al in uitvoering)
	- anders: lees uit met takeVar
	- sla op in couchdb, ontvang revision
	- na afloop: vul AVar met coudhbresource met revision
-}
storeExistingCouchdbResourceInCouchdb :: forall e. Resource -> Aff (ajax :: AJAX, avar :: AVAR, gm :: GLOBALMAP | e) Unit
storeExistingCouchdbResourceInCouchdb id = do
  mAvar <- liftEff $ peek resourceDefinitions id
  case mAvar of
    Nothing -> throwError $ error ("storeExistingCouchdbResourceInCouchdb needs a locally stored resource for " <> id)
    (Just avar) -> do
      empty <- isEmptyVar avar
      if empty
        then pure unit
        else do
          cdbr <- takeVar avar
          (rev :: String) <- createResourceInCouchdb id cdbr
          putVar (insert "_rev" (fromString rev) cdbr) avar

-- | From a context, create a DomeinFile (a record that holds an id, maybe a revision and a StrMap of CouchdbResources).
domeinFileFromContext :: forall e. PerspectContext -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) DomeinFile
domeinFileFromContext c' = do
  contexts <- execStateT (collect c') empty
  pure { _id : context_id c'
    , _rev : maybe "" id (context_rev c')
    , contexts: contexts
    }
  where
    collect :: PerspectContext -> StateT DomeinFileContexts (Aff (DomeinFileEffects (prd :: PROPDEFS | e))) Unit
    collect c = do
      modify \dfc -> insert (context_id c) (unwrapPerspectContext c) dfc
      for_ (context_rolInContext c)
        \(ids :: Array ID) -> -- These are IDs of role instances!
          for_ ids
            \roleId -> do
              mRole <- lift $ getRole roleId
              case mRole of
                Nothing -> pure unit
                (Just (rolInContext :: PerspectRol)) -> do
                  modify \dfc -> insert roleId (unwrapPerspectRol rolInContext) dfc
                  mBinding <- pure (rol_binding rolInContext)
                  case mBinding of
                    Nothing -> pure unit
                    (Just (binding :: ID)) ->
                      if isInNamespace binding roleId
                        then do
                          mBuitenRol <- lift $ getRole binding
                          case mBuitenRol of
                            (Just (buitenRol :: PerspectRol)) -> if rol_pspType buitenRol == "model:Perspectives$BuitenRol"
                              then do
                                modify \dfc -> insert binding (unwrapPerspectRol buitenRol) dfc
                                mContext <- lift $ getContext (rol_context buitenRol)
                                case mContext of
                                  Nothing -> pure unit
                                  (Just context) -> collect context
                              else pure unit
                            Nothing -> pure unit
                        else pure unit

foreign import saveRevision :: String -> CouchdbResource -> Unit
