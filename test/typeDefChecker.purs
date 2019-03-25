module Test.Perspectives.TypeDefChecker (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Perspectives.ModelBasedTripleGetters (nonQueryRollen, rollenDef)
import Perspectives.PerspectivesTypes (RolDef(..))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, typeDefCheckerNotifies, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly)
import Test.Unit.Assert (assert)

t :: String -> String
t s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "TypeDefChecker" do
  test "All error situations that the TypeDefChecker can find, except for MissingType, MissingAspect and RolWithoutContext (we cannot upload a CRL file with those error situations)." do
    typeDefCheckerNotifies "testTypeDefChecker.crl"
      [ "MissingMogelijkeBinding"
      , "MissingRolInstance"
      , "IncorrectBinding"
      , "RolNotDefined"
      , "MissingPropertyValue"
      , "MissingExternalPropertyValue"
      , "MissingInternalPropertyValue"
      , "IncorrectPropertyValue"
      , "TooManyPropertyValues"
      , "PropertyNotDefined"
      , "AspectRolNotFromAspect"
      , "CycleInAspects"
      , "CycleInAspectRoles"
      , "CycleInAspectProperties"
      , "CannotOverrideBooleanAspectProperty"
      , "MissingRange"
      , "RangeNotSubsumed"
      , "MogelijkeBindingNotSubsumed"
      , "RangeNotSubsumedByBindingProperty"
      , "MissingAspectPropertyForBindingProperty"
        --  Not testable because the parser fails on the testfile:
        -- "MissingType"
        -- "MissingAspect"
        -- "RolWithoutContext"
      ]
    unLoadTestModel "model:TestTDC"
