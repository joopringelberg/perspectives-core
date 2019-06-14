module Test.Perspectives.LoadAModel (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Perspectives.ContextAndRole (context_id)
import Perspectives.DataTypeTripleGetters (binding, rolBindingDef)
import Perspectives.LoadCRL (loadCRLFile, withSemanticChecks, withoutSemanticChecks)
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextRol(..), RolDef(..))
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.Syntax (PerspectContext(..))
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (getContextRol)
import Test.Perspectives.Utils (assertEqual, runP, p)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

theSuite :: Free TestF Unit
theSuite = suiteSkip "Loading the model:" do
  test "loadCRLFile" do
    void $ runP $ loadCRLFile withoutSemanticChecks "perspectives.crl"
    -- void $ runP $ loadCRLFile withSemanticChecks "query.crl"
  -- test "Unloading the model" do
    -- unLoadTestModel "model:Perspectives"

  testSkip "Retrieve the model context from Perspectives." do
    assertEqual "It should be possible to get the enclosing context from the model."
      ((getPerspectEntiteit "model:Perspectives") >>= \(pe :: PerspectContext) -> pure $ context_id pe)
      "model:Perspectives"

  testSkip "Retrieve indexed context type from model." do
    assertEqual ""
      ("model:Perspectives" ##= (getContextRol $ RolDef $ p "Model$indexedContextTypes") >-> rolBindingDef)
      [p "PerspectivesSysteem"]
