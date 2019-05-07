module Test.TheoryChange (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, length)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Perspectives.Actions (setBinding, setProperty')
import Perspectives.CoreTypes (Triple(..), TripleRef(..))
import Perspectives.DataTypeTripleGetters (binnenRol, buitenRol)
import Perspectives.ObjectGetterConstructors (getContextRol)
import Perspectives.PerspectivesTypes (BuitenRol(..), PBool(..), PropertyDef(..), RolDef(..), typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (equal, not)
import Perspectives.RunMonadPerspectivesQuery ((##), (##=), (##>>))
import Perspectives.TripleAdministration (getTriple, lookupSubject)
import Perspectives.TripleGetterComposition (followedBy)
import Perspectives.TripleGetterConstructors (directAspectRoles, directAspects, getInternalProperty)
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Test.Perspectives.Utils (assertEqual, assertEqualWithPropagation, loadTestModel, p, u)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, test)


t :: String -> String
t s = "model:TestOGC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: Free TestF Unit
theSuite = suite "TheoryChange" do
  test "Setting up" do
    loadTestModel "testBotActie.crl"

  test "Change a value" do
    loadTestModel "testbotInstantie.crl"
    assertEqualWithPropagation "Setting the value of $trigger changes the triple created by retrieving it before."
      do
        f <- pure $ (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$trigger") `followedBy` (PBool <<< unwrap))
        (Triple{object}) <- ((u "test1") ## f)
        void $ setProperty' (tba "Test$binnenRolBeschrijving$trigger") "false" (u "test1_binnenRol")
        lift $ delay (Milliseconds 500.0)
        ((u "test1") ##= f)
        -- pure $ typeWithPerspectivesTypes object
        -- Als we een `followedBy` toevoegen aan f, verandert het object niet. Dat komt omdat het daarmee
        -- geconstrueerde triple niet herberekend wordt, omdat het niet wordt toegevoegd aan de TripleAdministration.
      [PBool "false"]
      1000.0

  test "Propagating over `not`" do
    loadTestModel "testbotInstantie.crl"
    -- $trigger == "true"
    assertEqualWithPropagation "Changing the support of a query constructed with `not` should propagate: \
    \the object of the triple is changed destructively."
      do
        f <- pure $ not (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$trigger") `followedBy` (unwrap >>> PBool))
        (Triple{object}) <- ((u "test1") ## f)
        void $ setProperty' (tba "Test$binnenRolBeschrijving$trigger") "false" (u "test1_binnenRol")
        -- $trigger == "false"
        lift $ delay (Milliseconds 100.0)
        -- pure object
        ((u "test1") ##= f)
      [PBool "true"]
      1000.0

  test "Propagating over double `not`" do
    loadTestModel "testbotInstantie.crl"
    -- $trigger == "true"
    assertEqualWithPropagation "Changing the support of a query constructed with `not` should propagate: \
    \the object of the triple is changed destructively."
      do
        f <- pure $ not (not (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$trigger") `followedBy` (unwrap >>> PBool)))
        (Triple{object}) <- ((u "test1") ## f)
        void $ setProperty' (tba "Test$binnenRolBeschrijving$trigger") "false" (u "test1_binnenRol")
        -- $trigger == "false"
        lift $ delay (Milliseconds 100.0)
        -- pure object
        ((u "test1") ##= f)
      [PBool "false"]
      1000.0

  test "Propagating over `equal`" do
    loadTestModel "testbotInstantie.crl"
    assertEqualWithPropagation "Changing the support of a query constructed with `equal` should propagate: \
    \the object of the triple is changed destructively."
      do
        f <- pure $ equal (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v1"))
          (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2"))
        (Triple{object}) <- ((u "test1") ## f)
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v2") "aap" (u "test1_binnenRol")
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v1") "noot" (u "test1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        -- ((u "test1") ##= f)
        pure object
      [PBool "false"]
      1000.0

  test "Propagating over `not equal`" do
    loadTestModel "testbotInstantie.crl"
    assertEqualWithPropagation "Changing the support of a query constructed with `equal` should propagate: \
    \the object of the triple is changed destructively."
      do
        f <- pure $ not (equal (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v1"))
          (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2")))
        (Triple{object}) <- ((u "test1") ## f)
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v2") "aap" (u "test1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        -- s <- liftEffect $ lookupSubject (u "test1")
        -- liftEffect $ log (show s)
        ((u "test1") ##= f)
        -- pure object
      [PBool "true"]
      1000.0

  -- test "Change of the binding of a rol" do
  --   loadTestModel "TestOGC.crl"
  --   assertEqualWithPropagation "Set the binding of t:myContextDef$aspect to its buitenrol."
  --     do
  --       f <- pure (getContextRol (RolDef "model:Perspectives$Context$aspect") `trackedAs` "myAspectRol")
  --       rol <- (t "myContextDef") ##>> f
  --       newBinding <- (t "myContextDef") ##>> buitenRol
  --       setBinding (unwrap rol) (unwrap newBinding)
  --       (t "myContextDef") ##= f
  --     []
  --     1000.0
