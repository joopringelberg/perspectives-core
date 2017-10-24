module Perspectives.Resource where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeVar', peekVar, AVAR)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Perspectives.ResourceRetrieval (fetchPropDefs)
import Perspectives.ResourceTypes (PropDefs, Resource)

-- | The global index of definitions of all resources, indexed by Resource.
type ResourceDefinitions = GLStrMap (AVar PropDefs)
resourceDefinitions :: ResourceDefinitions
resourceDefinitions = new unit

foreign import data PROPDEFS :: Effect

-- | Get the property definitions of a Resource.
getPropDefs :: forall e. Resource -> Aff (gm :: GLOBALMAP, avar :: AVAR, ajax :: AJAX, prd :: PROPDEFS | e) PropDefs
getPropDefs id = do
  propDefs <- liftEff $ peek resourceDefinitions id
  case propDefs of
    Nothing -> do
                pd <- fetchPropDefs id
                av <- makeVar' pd
                -- set av as the value of propDefs in the resource!
                _ <- liftEff $ poke resourceDefinitions id av
                pure pd
    (Just avar) -> do
                    pd <- peekVar avar
                    pure pd
