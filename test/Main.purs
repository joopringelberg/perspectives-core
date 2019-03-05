module Test.Main where

import Prelude

import Control.Monad.Aff (Error, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Data.Either (Either(..))
import Perspectives.Effects (AjaxAvarCache)
import Test.BasicConstructors (test)
import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO
import Test.Perspectives.ObjectGetterConstructors (theSuite) as OGC
import Test.Perspectives.Utils (runP)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

-- main = runAff handleError (runP test)
main :: forall e. Eff (AjaxAvarCache (console :: CONSOLE, now :: NOW, testOutput :: TESTOUTPUT | e)) Unit
main = runTest do
  DTO.theSuite
  OGC.theSuite

-- handleError :: forall e a. (Either Error a -> Eff (console :: CONSOLE | e) Unit)
-- handleError (Left e) = log $ "An error condition: " <> (show e)
-- handleError (Right a) = log $ "Perspectives-core has started!"
