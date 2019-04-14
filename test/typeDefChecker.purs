module Test.Perspectives.TypeDefChecker (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.CoreTypes (UserMessage(..))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, typeDefCheckerNotifies)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly)

t :: String -> String
t s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "TypeDefChecker" do
  test "All error situations that the TypeDefChecker can find, except for MissingType, MissingAspect, RolWithoutContext and PropertyWithoutRol (we cannot upload a CRL file with those error situations)." do
    typeDefCheckerNotifies "testTypeDefChecker.crl"
      [ "MissingMogelijkeBinding"
      , "MissingRolInstance"
      , "IncorrectRolinContextBinding"
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
      , "AspectPropertyNotFromAspectRol"
      , "BindingPropertyNotAvailable"
      , "IncompatiblePrototype"
      , "CannotOverideBooleanRolProperty"
        --  Not testable because the parser fails on the testfile:
        -- "MissingType"
        -- "MissingAspect"
        -- "RolWithoutContext"
        -- "PropertyWithoutRol"
      ]
