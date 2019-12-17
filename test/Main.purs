module Test.Main where

import Prelude

import Effect (Effect)
-- import Perspectives.SetupUser (setupUser)
-- import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO
import Test.Parsing.Arc.TransferFile (theSuite) as TF
import Test.Parsing.Arc as TPA
import Test.Parsing.Arc.PhaseTwo (theSuite) as TPA2
import Test.Parsing.Arc.PhaseThree as TPA3
import Test.Parsing.Arc.Expression as TPAE
import Test.Query.DescriptionCompiler as QDC
import Test.Representation.ADT as ADT
import Test.LoadCRL as LCRL
import Test.LoadArc as LARC
import Test.ContextAndRole as CAR
import Test.ContextRoleParser as CRP
import Test.Actions as ACT
import Test.Query.Inversion as INV
import Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations as SACC
import Test.Unit.Main (runTest)


main :: Effect Unit
main = runTest do
  TF.theSuite
  TPA.theSuite
  TPA2.theSuite
  TPA3.theSuite
  TPAE.theSuite
  QDC.theSuite
  ADT.theSuite
  LCRL.theSuite
  LARC.theSuite
  CAR.theSuite
  CRP.theSuite
  ACT.theSuite
  INV.theSuite
  SACC.theSuite
