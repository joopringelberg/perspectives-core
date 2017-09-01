module Main where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Argonaut.Core (foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (ResponseType(..))

import Test.Properties


-- main :: forall r. Eff (ref :: REF, console :: CONSOLE | r ) Unit
main = test
