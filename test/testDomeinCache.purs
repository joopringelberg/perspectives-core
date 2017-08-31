module Test.DomeinCache where

import Prelude
import Control.Monad.Aff (Aff, Canceler, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Affjax (AJAX)
import Perspectives.DomeinCache (retrieveDomeinResourceDefinition)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Location (Location, locationValue)
-- import Perspectives.LocationT (LocationT, runLocationT)
import Perspectives.ResourceTypes (PropDefs)

-- test = launchAff $ runLocationT do
--   lift $ log "========================================================="
--   lift $ log ("namespaceToDomeinFileName 'model:ExecutieKetenDomein#'= " <> show (namespaceToDomeinFileName "model:ExecutieKetenDomein#"))
--   lift $ log "========================================================="
--   (pd :: PropDefs) <- retrieveDomeinResourceDefinition "model:ExecutieKetenDomein#" "model:ExecutieKetenDomein#"
--   lift $ log ("retrieveDomeinResourceDefinition 'model:ExecutieKetenDomein#' 'model:ExecutieKetenDomein#' = " <> show pd)


test = do
  (pd :: PropDefs) <- retrieveDomeinResourceDefinition "model:ExecutieKetenDomein#" "model:ExecutieKetenDomein#"
  log (show pd)

c = retrieveDomeinResourceDefinition "model:ExecutieKetenDomein#" "model:ExecutieKetenDomein#"
