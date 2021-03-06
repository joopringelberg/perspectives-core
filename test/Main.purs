module Test.Main where

import Prelude

import Test.Unit.Main (runTest)
import Effect (Effect)
-- import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO
-- import Test.Parsing.Arc.TransferFile (theSuite) as TF
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
import Test.SerialisedAsDeltas as SAD
import Test.Sync.Channel as CHA
import Test.ArrayT as ARRT
import Test.Sync.HandleTransaction as HTA
import Test.Assignment.SerialiseAsJson as SAJ
import Test.Utilities as UT
import Test.Invitation as IT
import Test.TypeLevelQueries as TLQ
import Test.Instances.ObjectGetters as IOG
import Test.DomeinFile.Encoding as DFE
import Test.Guid as GUID


main :: Effect Unit
main = runTest do
  -- TF.theSuite
  TPA.theSuite
  TPA2.theSuite
  TPA3.theSuite
  TPAE.theSuite
  QDC.theSuite
  QR.theSuite
  ADT.theSuite
  CDB.theSuite
  LCRL.theSuite
  LARC.theSuite
  CAR.theSuite
  CRP.theSuite
  ACT.theSuite
  INV.theSuite
  SACC.theSuite
  RMPT.theSuite
  COMB.theSuite
  TMS.theSuite
  TTO.theSuite
  SAD.theSuite
  CHA.theSuite
  ARRT.theSuite
  HTA.theSuite
  SAJ.theSuite
  UT.theSuite
  IT.theSuite
  TLQ.theSuite
  IOG.theSuite
  GUID.theSuite
  DFE.theSuite
