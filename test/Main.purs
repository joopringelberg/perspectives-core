module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Perspectives.Effects (AjaxAvarCache)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO

main :: forall e. Eff (AjaxAvarCache (console :: CONSOLE, now :: NOW, testOutput :: TESTOUTPUT | e)) Unit
main = runTest do
  DTO.theSuite
