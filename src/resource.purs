module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff, catchError)
import Control.Monad.Aff.AVar (AVAR, AVar, isEmptyVar, makeEmptyVar, makeVar, putVar, readVar, takeVar)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.ContextAndRole (context_id)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.ResourceRetrieval (fetchCouchdbResource, storeCouchdbResource)
import Perspectives.ResourceTypes (DomeinFileEffects, PropDefs(..), Resource, CouchdbResource)
import Perspectives.Syntax (PerspectContext, PerspectRol)

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
  av <- getResourceAVar id
  emp <- isEmptyVar av
  if emp
    then do
      pd <- fetchCouchdbResource id
      putVar pd av
      pure pd
    else readVar av

getResourceAVar :: forall e. Resource -> Aff (avar :: AVAR, gm :: GLOBALMAP | e) (AVar CouchdbResource)
getResourceAVar id = do
  propDefs <- liftEff $ peek resourceDefinitions id
  case propDefs of
    Nothing -> makeEmptyVar
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
  (mrev :: Maybe String) <- storeCouchdbResource (context_id (castPerspectContext cdbr)) cdbr
  case mrev of
    Nothing -> putVar cdbr av
    (Just rev) -> do
      _ <- pure $ saveRevision rev cdbr
      putVar cdbr av

foreign import saveRevision :: String -> CouchdbResource -> Unit
