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
import Test.LoadArc as LARC
import Test.ContextAndRole as CAR
import Test.Query.Inversion as INV
import Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations as SACC
import Test.Combinators as COMB
import Test.Extern.Couchdb as CDB
import Test.Types.ObjectGetters as TTO
import Test.Queries as QR
import Test.Sync.Channel as CHA
import Test.ArrayT as ARRT
import Test.Sync.HandleTransaction as HTA
import Test.Utilities as UT
import Test.DomeinFile.Encoding as DFE
import Test.RabbitMQ as RB
import Test.Class.Role as ROLE
import Test.Perspectives.Representation.ADT.DisjunctiveNormalForm as DNF
import Test.Perspectives.Representation.ADT2 as ADT2
import Test.Perspectives.Representation.ADT.SpecialisesADT as SPECADT


main :: Effect Unit
main = runTest do
  -- TF.theSuite
  -- TPA.theSuite
  -- TPA2.theSuite
  -- TPA3.theSuite
  -- TPAE.theSuite
  -- QDC.theSuite
  -- QR.theSuite
  -- ADT.theSuite
  -- CDB.theSuite
  -- LARC.theSuite
  -- CAR.theSuite
  -- INV.theSuite
  -- SACC.theSuite
  -- COMB.theSuite
  -- TMS.theSuite
  -- TTO.theSuite
  -- CHA.theSuite
  -- ARRT.theSuite
  -- HTA.theSuite
  -- UT.theSuite
  -- GUID.theSuite
  -- DFE.theSuite
  -- RB.theSuite
  ROLE.theSuite
  -- DNF.theSuite
  -- ADT2.theSuite
  -- SPECADT.theSuite