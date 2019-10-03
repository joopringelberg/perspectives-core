module Test.Main where

import Prelude

import Effect (Effect)
-- import Perspectives.SetupUser (setupUser)
-- import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO
import Test.Parsing.Arc.Simple (theSuite) as TPA
import Test.Parsing.Arc.PhaseTwo (theSuite) as TPA2
import Test.Parsing.Arc.PhaseThree as TPA3
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  TPA.theSuite
  TPA2.theSuite
  TPA3.theSuite
