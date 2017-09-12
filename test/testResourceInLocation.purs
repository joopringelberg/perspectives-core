module Test.ResourceInLocation where

import Prelude
import Control.Monad.Aff (Aff, Canceler(..), launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log) as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap)
import Perspectives.Location (Location, locationValue)
import Perspectives.LocationT (runLocationT)
import Perspectives.Property (getString)
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource(..), ResourceLocation(..))
import Perspectives.SystemQueries (label)

test :: forall t36.
  Eff
    ( exception :: EXCEPTION
    , console :: Aff.CONSOLE
    , ajax :: AJAX
    , avar :: AVAR
    , gm :: GLOBALMAP
    , st :: ST (GLStrMap ResourceLocation)
    | t36
    )
    (Canceler
       ( console :: Aff.CONSOLE
       , ajax :: AJAX
       , avar :: AVAR
       , gm :: GLOBALMAP
       , st :: ST (GLStrMap ResourceLocation)
       | t36
       )
    )
test = launchAff $ do
  log "========================================================="
  (gb :: Resource) <- liftEff $ representResource "user:xGebruiker"
  log $ show gb

  log "========================================================="
  (l :: (Maybe String)) <- (getString "rdfs:label" gb)
  case l of
    Nothing -> log "Niets gevonden"
    _ -> log "Wel iets gevonden"

log = Aff.log
