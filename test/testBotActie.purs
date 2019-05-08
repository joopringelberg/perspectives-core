module Test.Perspectives.TestBotActie (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Perspectives.Actions (compileBotAction, constructActionFunction, getBindingOfRol, setProperty')
import Perspectives.CoreTypes (type (~~>), NamedFunction(..), (##>))
import Perspectives.DataTypeObjectGetters (rolBindingDef)
import Perspectives.DataTypeTripleGetters (binnenRol, buitenRol, identity)
import Perspectives.Identifiers (psp)
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef, propertyIsFunctioneel, propertyIsVerplicht, rolDef, rolIsVerplicht)
import Perspectives.ObjectGetterConstructors (directAspectProperties, getContextRol, getInternalProperty, getRoleBinders)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesState (tripleQueue)
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextDef(..), ContextRol(..), PBool(..), PString(..), PropertyDef(..), RolDef(..), Value(..))
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryEffect ((~>))
import Perspectives.RunMonadPerspectivesQuery ((##), (##=))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter)
import Perspectives.TheoryChange (propagate)
import Perspectives.TripleGetterComposition (followedBy)
import Perspectives.Utilities (onNothing)
import Test.Perspectives.Utils (assertEqual, loadTestModel, unLoadTestModel, p, u, assertEqualWithPropagation)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "TestBotActie" do
  test "Setting up" do
    loadTestModel "testBotActie.crl"
    loadTestModel "testbotInstantie.crl"

  test "constructActionFunction" do
    assertEqual "The action function copies the value of $v1 into $v2"
      do
        meffect <- (tba "Test$botCopiesV1ToV2") ##> getContextRol (RolDef $ p "Actie$effect") /-/ rolBindingDef
        case meffect of
          Nothing -> pure []
          (Just effect) -> do
            f <- (constructActionFunction effect (binnenRol `followedBy` unwrap))
            void $ f (u "test1") ["true"]
            getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2") (u "test1")
      [Value "aap"]

  test "nAryOperator-defined property $propsEqual of usr:test1 is false" do
    assertEqual "$propsEqual"
      do
        propsEqual <- constructQueryFunction (tba "Test$binnenRolBeschrijving$propsEqual")
        (u "test1") ##= propsEqual
      ["true"]

  test "testing the action condition" do
    assertEqual "The condition of botCopiesV1ToV2 evalates to true for test1."
      do
        condition <- onNothing
          (error "Cannot find condition")
          (tba "Test$botCopiesV1ToV2" ##> getBindingOfRol (psp "Actie$condition"))
        conditionQuery <- constructQueryFunction condition
        (u "test1") ##= conditionQuery
      ["false"]

  testOnly "compileBotAction" do
    loadTestModel "testBotActie.crl"
    loadTestModel "testbotInstantie.crl"
    assertEqualWithPropagation "Apply the botAction to the context usr:test1 to copy the value of $v1 to $v2"
      do
        botaction <- (compileBotAction (ContextDef $ tba "Test$botCopiesV1ToV2") (u "test1"))
        propsEqual <- constructQueryFunction (tba "Test$binnenRolBeschrijving$propsEqual")
        propsEqualWithEffect <- pure $ propsEqual ~> NamedFunction "propsEqualWithEffect" \vals -> liftEffect $ log ("propsEqual is now: " <>  show vals)
        void ((u "test1") ## propsEqualWithEffect)
        void ((u "test1") ## botaction)
        getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2") (u "test1")
      [Value "aap"]
      1000.0
    assertEqual "$propsEqual should now be true"
      do
        propsEqual <- constructQueryFunction (tba "Test$binnenRolBeschrijving$propsEqual")
        (u "test1") ##= propsEqual
      ["true"]
    assertEqualWithPropagation "Setting $v1 to a new value should cause $propsEqual to be false, but the bot immediately copies the new value to $v2 so it is true again."
      do
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v1") "noot" (u "test1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        propsEqual <- constructQueryFunction (tba "Test$binnenRolBeschrijving$propsEqual")
        (u "test1") ##= propsEqual
        -- pure ["false"]
      ["true"]
      1000.0
    assertEqual "Setting $v1 to a new value caused the condition to become true, but the bot copied $v1 to $v2 so it is false again."
      do
        condition <- onNothing
          (error "Cannot find condition")
          (tba "Test$botCopiesV1ToV2" ##> getBindingOfRol (psp "Actie$condition"))
        conditionQuery <- constructQueryFunction condition
        (u "test1") ##= conditionQuery
      ["false"]

    assertEqual "Setting $v1 to a new value caused $v2 to assume the same value"
      do
        r <- getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2") (u "test1")
        -- removeUserData [BuitenRol "model:User$test1_buitenRol"]
        pure r
      [Value "noot"]

  -- testOnly "Add a test!" do
  --   loadTestModel "testBotActie.crl"
  --
  --   unLoadTestModel "model:TestBotActie"

  -- test "Tearing down" do
  --   unLoadTestModel "model:TestBotActie"
  pure unit
