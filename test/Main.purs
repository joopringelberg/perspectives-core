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
import Test.RunMonadPerspectivesTransaction as RMPT
import Test.Combinators as COMB
import Test.Extern.Couchdb as CDB
import Test.Model.System as TMS
import Test.Types.ObjectGetters as TTO
import Test.Queries as QR
import Test.SetupCouchdb as SCDB
import Test.LocalAuthentication as LA
import Test.SerialisedAsDeltas as SAD
import Test.Sync.Channel as CHA
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
  RMPT.theSuite
  COMB.theSuite
  CDB.theSuite
  TMS.theSuite
  TTO.theSuite
  QR.theSuite
  SCDB.theSuite
  LA.theSuite
  SAD.theSuite
  CHA.theSuite
