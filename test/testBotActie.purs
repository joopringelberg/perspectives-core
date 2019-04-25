module Test.Perspectives.TestBotActie (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.Actions (constructActionFunction)
import Perspectives.CoreTypes (type (~~>), (##>))
import Perspectives.DataTypeObjectGetters (rolBindingDef)
import Perspectives.DataTypeTripleGetters (binnenRol, buitenRol, identity)
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef, propertyIsFunctioneel, propertyIsVerplicht, rolDef, rolIsVerplicht)
import Perspectives.ObjectGetterConstructors (directAspectProperties, getContextRol, getInternalProperty, getRoleBinders)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (BuitenRol, ContextRol(..), PBool(..), PString(..), PropertyDef(..), RolDef(..), Value(..))
import Perspectives.TripleGetterComposition (followedBy)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, unLoadTestModel, p, u)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "TestBotActie" do
  test "Setting up" do
    loadTestModel "testBotActie.crl"

  test "constructActionFunction" do
    assertEqual "The action function copies the value of $v1 into $v2"
      do
        meffect <- (tba "Test$botCopiesV1ToV2") ##> getContextRol (RolDef $ p "Actie$effect") /-/ rolBindingDef
        case meffect of
          Nothing -> pure []
          (Just effect) -> do
            f <- (constructActionFunction effect (binnenRol `followedBy` unwrap))
            void $ f (u "test1") (PBool "true")
            getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2") (u "test1")
      [Value "aap"]

  -- testOnly "Add a test!" do
  --   loadTestModel "testBotActie.crl"
  --
  --   unLoadTestModel "model:TestBotActie"

  -- test "Tearing down" do
  --   unLoadTestModel "model:TestBotActie"
  pure unit
