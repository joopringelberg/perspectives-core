module Test.Main where

import Prelude

import Effect.Aff (runAff)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Data.Either (Either(..))
import Perspectives.SetupUser (setupUser)
import Test.Perspectives.DataTypeObjectGetters (theSuite) as DTO
import Test.Perspectives.DataTypeTripleGetters as DTG
import Test.Perspectives.LoadAModel (theSuite) as LAM
import Test.Perspectives.ModelBasedObjectGetters (theSuite) as MBOG
import Test.Perspectives.ModelBasedStringTripleGetters (theSuite) as MBSTG
import Test.Perspectives.ModelBasedTripleGetters (theSuite) as MBTG
import Test.Perspectives.ObjectGetterConstructors (theSuite) as OGC
import Test.Perspectives.QueryCombinators (theSuite) as QC
import Test.Perspectives.QueryCompiler (theSuite) as CQR
import Test.Perspectives.TestBotActie as TBA
import Test.Perspectives.TripleGetterComposition (theSuite) as TGCO
import Test.Perspectives.TripleGetterConstructors (theSuite) as TGC
import Test.Perspectives.TypeDefChecker (theSuite) as TDC
import Test.Perspectives.Utils (runP)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DTO.theSuite
  OGC.theSuite
  MBTG.theSuite
  MBSTG.theSuite
  TGC.theSuite
  MBOG.theSuite
  TDC.theSuite
  DTG.theSuite
  TGCO.theSuite
  LAM.theSuite
  QC.theSuite
  TBA.theSuite
  CQR.theSuite

-- Running other tests:
-- main = runAff handleError (runP setupUser)
--
-- handleError :: forall e a. (Either Error a -> Effect (console :: CONSOLE | e) Unit)
-- handleError (Left e) = log $ "An error condition: " <> (show e)
-- handleError (Right a) = log $ "Perspectives-core has started!"
