module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.ModelBasedTripleGetters (rollenDef)
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextRol(..), RolDef(..), RolInContext(..), binding)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (TestEffects, addTestContext, assertEqual, p, u)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suite "ModelBasedTripleGetters" do
  test "rollenDef" do
    assertEqual "The Context 'psp:Property' defines three roles."
      ((p "Property") ##= rollenDef)
      -- []
      [ RolDef (p "Property$range")
      , RolDef (p "Property$aspectProperty")
      , RolDef (p "Property$bindingProperty")]
