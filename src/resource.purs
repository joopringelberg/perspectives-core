module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff, catchError)
import Control.Monad.Aff.AVar (AVar, makeVar, readVar)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, poke, peek)
import Perspectives.ResourceRetrieval (fetchCouchdbResource)
import Perspectives.ResourceTypes (DomeinFileEffects, PropDefs(..), Resource, CouchdbResource)
import Perspectives.Syntax (PerspectContext, PerspectRol)

type PerspectEffects e = (DomeinFileEffects (prd :: PROPDEFS | e))

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
  propDefs <- liftEff $ peek resourceDefinitions id
  case propDefs of
    Nothing -> do
                pd <- fetchCouchdbResource id
                av <- makeVar pd
                -- set av as the value of propDefs in the resource!
                _ <- liftEff $ poke resourceDefinitions id av
                pure pd
    (Just avar) -> do
                    pd <- readVar avar
                    pure pd

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
