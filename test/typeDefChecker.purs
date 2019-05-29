module Test.Perspectives.TypeDefChecker (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.CoreTypes (UserMessage(..))
import Test.Perspectives.Utils (typeDefCheckerNotifies)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly)

t :: String -> String
t s = "model:TestTDC$" <> s

theSuite :: Free TestF Unit
theSuite = suite "TypeDefChecker" do
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
      , "BindingPropertyCannotOverrideBooleanAspectProperty"
      , "IncompatiblePrototype"
      -- , "CannotOverideBooleanRolProperty"
        --  Not testable because the parser fails on the testfile:
        -- "MissingType"
        -- "MissingAspect"
        -- "RolWithoutContext"
        -- "PropertyWithoutRol"
      ]
