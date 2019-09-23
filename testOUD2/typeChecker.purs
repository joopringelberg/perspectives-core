module Test.Perspectives.TypeChecker (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.CoreTypes (UserMessage(..))
import Perspectives.PerspectivesTypes (ContextDef(..))
import Perspectives.TypeChecker (contextHasType, isOrHasAspect)
import Test.Perspectives.Utils (assertEqual, p, typeDefCheckerNotifies)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly)

t :: String -> String
t s = "model:TestTDC$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "TypeChecker" do
  test "isOrHasAspect" do
    assertEqual "psp:Context isOrHasAspect psp:Systeem should be false."
      ((ContextDef $ p "Context") `isOrHasAspect` (ContextDef $ p "Systeem"))
      false
    assertEqual "psp:Systeem isOrHasAspect psp:Context should be true."
      ((ContextDef $ p "Systeem") `isOrHasAspect` (ContextDef $ p "Context"))
      true

  test "contextHasType" do
    assertEqual "Context `contextHasType` PerspectivesSysteem should be false"
      ((p "Context") `contextHasType` (ContextDef $ p "PerspectivesSysteem"))
      false
    assertEqual "PerspectivesSysteem `contextHasType` Context should be true"
      ((p "PerspectivesSysteem") `contextHasType` (ContextDef $ p "Context"))
      true

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"
