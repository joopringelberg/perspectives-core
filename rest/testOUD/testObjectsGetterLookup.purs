module Test.ObjectsGetterLookup where

import Prelude

import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.ObjectGetterLookup (lookupObjectsGetterByName, lookupObjectsGetterName, objectsGetterCacheInsert)
import Perspectives.DataTypeObjectGetters (contextType)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  lift $ log "=========================Test.ObjectsGetterLookup================================"
  _ <-  pure $ objectsGetterCacheInsert "model:Perspectives$ContextType" contextType
  naam <- pure $ lookupObjectsGetterName contextType
  case naam of
    Nothing -> lift $ log "Niet gevonden!"
    (Just n) -> do
      lift $ log n
      case lookupObjectsGetterByName n of
        Nothing -> lift $ log "Kan de getter niet terugvinden"
        (Just g) -> do
          lift $ log "Iets teruggevonden!"
          tp <- g "model:Perspectives$Rol"
          lift $ logShow tp
  lift $ log "Finished"
