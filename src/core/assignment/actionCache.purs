module Perspectives.Assignment.ActionCache where

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Perspectives.CoreTypes (Updater)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType)
import Prelude (unit)

type ActionCache = GLStrMap (Updater ContextInstance)

-- | A global store of SupportedEffect-s
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
actionCache :: ActionCache
actionCache = new unit

cacheAction :: ActionType -> (Updater ContextInstance) -> Effect ActionCache
cacheAction a u = poke actionCache (unwrap a) u

retrieveAction :: ActionType -> Effect (Maybe (Updater ContextInstance))
retrieveAction a = peek actionCache (unwrap a)
