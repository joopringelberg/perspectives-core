module Test.PropertiesInLocation where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Perspectives.Location (Location)
import Perspectives.LocationT (LocationT(..), runLocationT)
import Perspectives.Property (SingleGetter, SingleGetterL, getResourceL, getStringL)
import Perspectives.QueryCombinators (filter, hasValue) as QC
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource, ResourceLocation(..))

test = launchAff $ runLocationT do
  lift $ log "=========================Test.Properties================================"
  (gb :: Resource) <- liftEff $ representResource "user:xGebruiker"
  (l :: Maybe String) <- (rol_RolBinding >-> label) gb
  lift $ log ( "label user:xGebruiker = " <> (show l))


label :: SingleGetterL String
label = getStringL "rdfs:label"

rol_RolBinding :: SingleGetterL Resource
rol_RolBinding = getResourceL "model:SysteemDomein#rol_RolBinding"
