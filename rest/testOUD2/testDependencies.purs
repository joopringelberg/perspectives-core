module Test.Dependencies (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Perspectives.CoreTypes (Triple(..), TripleRef(..))
import Perspectives.DataTypeTripleGetters (binnenRol)
import Perspectives.PerspectivesTypes (PropertyDef(..), PBool(..))
import Perspectives.QueryCombinators (not)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.TripleAdministration (clearTripleIndex, getTriple, lookupSubject)
import Perspectives.TripleGetterComposition (followedBy)
import Perspectives.TripleGetterConstructors (getInternalProperty)
import Test.Perspectives.Utils (assertEqual, loadTestModel, p, u)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "Dependencies" do
  test "Setting up" do
    loadTestModel "testBotActie.crl"
    loadTestModel "testbotInstantie.crl"
    liftEffect clearTripleIndex

  test "Support construction for `trackedAs`" do
    assertEqual "A triple obtained with a getter constructed with `trackedAs` has no supports."
      ((u "test1" ## binnenRol) >>= \(Triple{supports}) -> pure $ length supports)
      0
    assertEqual "A triple obtained with a getter constructed with `trackedAs` has no dependencies."
      ((u "test1" ## binnenRol) >>= \(Triple{dependencies}) -> pure $ length dependencies)
      0

  test "Support construction for `>->`" do
    assertEqual "A triple obtained with a getter constructed with >-> has two supports."
      (((u "test1") ## getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v1")) >>= \(Triple{supports}) -> pure $ map show supports)
      [
        "<model:User$test1 - model:Perspectives$binnenRol>",
        "<model:User$test1_binnenRol - model:TestBotActie$Test$binnenRolBeschrijving$v1>"
      ]
    assertEqual "<usr:test1 - psp:binnenRol> now has a dependency"
      do
        tr <- liftEffect $ getTriple (TripleRef{subject: u "test1", predicate: p "binnenRol"})
        case tr of
          (Just (Triple{dependencies})) -> pure $ map show dependencies
          _ -> pure []
      ["<model:User$test1 - model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$v1>"]

  test "Support construction for `not`" do
    assertEqual "A Triple obtained with a query beginning with `not` should have a single support."
      (((u "test1") ## not (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$trigger") `followedBy` (unwrap >>> PBool))) >>= \(Triple{supports}) -> pure $ map show supports)
      ["<model:User$test1 - model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$trigger>"]
    assertEqual "A Triple obtained with a query with double `not` should have a single support."
      (((u "test1") ## not (not (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$trigger") `followedBy` (unwrap >>> PBool)))) >>= \(Triple{supports}) -> pure $ map show supports)
      ["<model:User$test1 - not(model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$trigger)>"]
    assertEqual "<usr:test1 - psp:binnenRol> now has two dependencies"
      do
        tr <- liftEffect $ getTriple (TripleRef{subject: u "test1", predicate: p "binnenRol"})
        case tr of
          (Just (Triple{dependencies})) -> pure $ map show dependencies
          _ -> pure []
      ["<model:User$test1 - model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$v1>", "<model:User$test1 - model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$trigger>"]
    assertEqual "<test1 - (binnenRol >-> trigger)> should have a single dependency."
      do
        tr <- liftEffect $ getTriple (TripleRef{subject: u "test1", predicate: "model:Perspectives$binnenRol >->\
        \ model:TestBotActie$Test$binnenRolBeschrijving$trigger"})
        case tr of
          (Just (Triple{dependencies})) -> pure $ map show dependencies
          _ -> pure []
      ["<model:User$test1 - not(model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$trigger)>"]
    assertEqual "<test1 - not(binnenRol >-> trigger)> should have a single dependency: the double negated trigger."
      do
        tr <- liftEffect $ getTriple (TripleRef{subject: u "test1", predicate: "not(model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$trigger)"})
        -- s <- liftEffect $ lookupSubject (u "test1")
        -- liftEffect $ log (show s)
        case tr of
          (Just (Triple{dependencies})) -> pure $ map show dependencies
          _ -> pure ["failure"] -- Het triple wordt niet gevonden.
      ["<model:User$test1 - not(not(model:Perspectives$binnenRol >-> model:TestBotActie$Test$binnenRolBeschrijving$trigger))>"]
