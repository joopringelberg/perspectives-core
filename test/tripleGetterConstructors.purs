module Test.Perspectives.TripleGetterConstructors (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.ModelBasedTripleGetters (rollenDef)
import Perspectives.PerspectivesTypes (BuitenRol(..), Context(..), ContextRol(..), RolDef(..), RolInContext(..), binding)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.StringTripleGetterConstructors (getContextRol, searchInAspectsAndPrototypes, searchLocallyAndInPrototypeHierarchy)
import Perspectives.TripleGetterComposition (before, followedBy)
import Perspectives.TripleGetterConstructors ()
import Test.Perspectives.Utils (TestEffects, addTestContext, assertEqual, p, u)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suite "TripleGetterConstructors" do
  test "getContextRol" do
    assertEqual "The Context 'psp:Property' has three rolInContext roles."
      (((p "Property") ##= (getContextRol ( "model:Perspectives$Context$rolInContext"))) >>= (pure <<< length))
      3
  test "searchInAspectsAndPrototypes" do
    assertEqual "searchInAspectsAndPrototypes applied to getContextRol should yield at least as many results"
      (((p "Property") ##= searchInAspectsAndPrototypes (getContextRol ( "model:Perspectives$Context$rolInContext"))) >>= (pure <<< length))
      3
  test "searchLocallyAndInPrototypeHierarchy" do
    assertEqual "searchLocallyAndInPrototypeHierarchy applied to getContextRol should yield at least as many results"
      (((p "Property") ##= searchLocallyAndInPrototypeHierarchy (getContextRol ( "model:Perspectives$Context$rolInContext"))) >>= (pure <<< length))
      3
