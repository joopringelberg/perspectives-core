module Test.Main where

import Prelude

import Effect (Effect)
-- import Perspectives.SetupUser (setupUser)
-- import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO
import Test.Parsing.Arc.Simple (theSuite) as TPA
import Test.Parsing.Arc.PhaseTwo (theSuite) as TPAP
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  TPA.theSuite
  TPAP.theSuite
