module Test.Query.Inversion where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (runWriterT)
import Data.Array (intercalate, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (evalPhaseTwo', traverseDomain)
import Perspectives.Query.Inversion (invertFunctionDescription)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), prettyPrint)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), PropertyType(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

theSuite :: Free TestF Unit
theSuite = suite "Test.Query.Inversion" do

  test "Constant" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = true" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedProperties}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                  Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                  (Just (CalculatedProperty{calculation})) -> case calculation of
                    S _ -> assert "The calculation should have been compiled!" false
                    Q c -> do
                      (Tuple inv _) <- runWriterT (unsafePartial $ invertFunctionDescription c [] true)
                      assert "The inversion of a Constant should be Nothing" (isNothing inv)

  test "Property of another role in the same context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = context >> AnotherRole >> Prop2\n    thing: AnotherRole\n      property: Prop2 (mandatory, functional, Boolean)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedProperties}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                  Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                  (Just (CalculatedProperty{calculation})) -> case calculation of
                    S _ -> assert "The calculation should have been compiled!" false
                    Q c -> do
                      (Tuple inv _) <- runWriterT (unsafePartial $ invertFunctionDescription c [] true)
                      -- logShow inv
                      assert "The inversion of a a property of another role in the same context should run \
                      \from that property to the role to the context" (isJust inv)
                      assert "The domain of the inversion should be Prop2." (case inv of
                        Just (BQD (VDOM PBool (Just (ENP (EnumeratedPropertyType "model:Test$TestCase1$AnotherRole$Prop2")))) _ _ _ _ _ _) -> true
                        otherwise -> false)
                      assert "The first term of the composition should be Value2Role" (case inv of
                        Just (BQD _ (BinaryCombinator ComposeF) (SQD _ Value2Role _ _ _) _ _ _ _) -> true
                        otherwise -> false)

  test "Property of another role in the same context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = context >> NestedContext >> binding >> context >> AnotherRole >> Prop2\n    context: NestedContext filledBy: SubCase\n    case: SubCase\n      thing: AnotherRole\n        property: Prop2 (mandatory, functional, Boolean)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedProperties}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                  Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                  (Just (CalculatedProperty{calculation})) -> case calculation of
                    S _ -> assert "The calculation should have been compiled!" false
                    Q c -> do
                      (Tuple inv _) <- runWriterT (unsafePartial $ invertFunctionDescription c [] true)
                      -- log (maybe "Nothing" prettyPrint inv)
                      assert "The inversion of a a property of another role in the same context should run \
                      \from that property to the role to the context" (isJust inv)
                      assert "The domain of the inversion should be Prop2." (case inv of
                        Just (BQD (VDOM PBool (Just (ENP (EnumeratedPropertyType "model:Test$TestCase1$SubCase$AnotherRole$Prop2")))) _ _ _ _ _ _) -> true
                        otherwise -> false)
                      assert "The first term of the composition should be Value2Role" (case inv of
                        Just (BQD _ (BinaryCombinator ComposeF) (SQD _ Value2Role _ _ _) _ _ _ _) -> true
                        otherwise -> false)
                      assert "The third term of the composition should be external" (case inv of
                        Just (BQD _ _ _ (BQD _ _ _ (BQD _ _ (SQD _ (DataTypeGetter ExternalRoleF) _ _ _ ) _ _ _ _ ) _ _ _) _ _ _) -> true
                        otherwise -> false)

  test "A constant on a subpath nullifies the inversion on that path." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = context >> NestedContext >> binding >> context >> AnotherRole >> Prop2 and true\n    context: NestedContext filledBy: SubCase\n    case: SubCase\n      thing: AnotherRole\n        property: Prop2 (mandatory, functional, Boolean)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedProperties}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                  Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                  (Just (CalculatedProperty{calculation})) -> case calculation of
                    S _ -> assert "The calculation should have been compiled!" false
                    Q c -> do
                      (Tuple inv _) <- runWriterT (unsafePartial $ invertFunctionDescription c [] true)
                      log (maybe "\nNothing" prettyPrint inv)
                      assert "The inversion of a property with a constant is nothing" (isNothing inv)

  test "A constant on a subpath nullifies the inversion on that path." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = context >> NestedContext >> binding >> context >> AnotherRole >> true\n    context: NestedContext filledBy: SubCase\n    case: SubCase\n      thing: AnotherRole\n        property: Prop2 (mandatory, functional, Boolean)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedProperties}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                  Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                  (Just (CalculatedProperty{calculation})) -> case calculation of
                    S _ -> assert "The calculation should have been compiled!" false
                    Q c -> do
                      (Tuple inv _) <- runWriterT (unsafePartial $ invertFunctionDescription c [] true)
                      log (maybe "\nNothing" prettyPrint inv)
                      assert "The inversion of a property with a constant is nothing" (isNothing inv)

  testOnly "A filter exprssion should yield two inverse queries." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = filter context >> AnotherRole with Prop2 >> Prop2\n    thing: AnotherRole\n      property: Prop2 (mandatory, functional, Boolean)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedProperties}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                  Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                  (Just (CalculatedProperty{calculation})) -> case calculation of
                    S _ -> assert "The calculation should have been compiled!" false
                    Q c -> do
                      log (prettyPrint c)
                      (Tuple _ paths) <- runWriterT (unsafePartial $ invertFunctionDescription c [] true)
                      log $ intercalate "\n" (prettyPrint <$> paths)
                      assert "The inversion of an expression with a filter should yield two inverse queries" (length paths == 2)
