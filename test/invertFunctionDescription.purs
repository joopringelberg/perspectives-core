module Test.Query.Inversion where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (runWriterT)
import Data.Array (cons, head, intercalate, length, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((##>))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (evalPhaseTwo', traverseDomain)
import Perspectives.Query.Inversion (invertFunctionDescription)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), prettyPrint, queryFunction, domain)
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.Class.PersistentType (getAction)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), EnumeratedPropertyType(..), PropertyType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCacheArcFile, loadAndSaveArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

testDirectory :: String
testDirectory = "test"

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
                      inv <- pure $ invertFunctionDescription c
                      assert "The inversion of a Constant should be Nothing" (null inv)

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
                      paths <- pure $ invertFunctionDescription c
                      -- log $ intercalate "\n" (prettyPrint <$> paths)
                      assert "The inversion of a a property of another role in the same context should run \
                      \from that property to the role to the context" (not $null paths)
                      assert "The domain of the inversion should be Prop2." (case head paths of
                        Just (BQD (VDOM PBool (Just (ENP (EnumeratedPropertyType "model:Test$TestCase1$AnotherRole$Prop2")))) _ _ _ _ _ _) -> true
                        otherwise -> false)
                      assert "The first term of the composition should be Value2Role" (case head paths of
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
                      paths <- pure $ invertFunctionDescription c
                      -- log $ intercalate "\n" (prettyPrint <$> paths)
                      assert "The inversion of a a property of another role in the same context should run \
                      \from that property to the role to the context" (not $ null paths)
                      assert "The domain of the inversion should be Prop2." (case head paths of
                        Just (BQD (VDOM PBool (Just (ENP (EnumeratedPropertyType "model:Test$TestCase1$SubCase$AnotherRole$Prop2")))) _ _ _ _ _ _) -> true
                        otherwise -> false)
                      assert "The first term of the composition should be Value2Role" (case head paths of
                        Just (BQD _ (BinaryCombinator ComposeF) (SQD _ Value2Role _ _ _) _ _ _ _) -> true
                        otherwise -> false)
                      assert "The third term of the composition should be external" (case head paths of
                        Just (BQD _ _ _ (BQD _ _ _ (BQD _ _ (SQD _ (DataTypeGetter ExternalRoleF) _ _ _ ) _ _ _ _ ) _ _ _) _ _ _) -> true
                        otherwise -> false)

  test "A filter exprssion should yield two inverse queries." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  case: TestCase1\n    thing: ARole\n      property: Prop1 = filter context >> AnotherRole with Prop2 >> Prop3\n    thing: AnotherRole\n      property: Prop2 (mandatory, functional, Boolean)\n      property: Prop3 (mandatory, functional, Boolean)\n" ARC.domain
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
                      -- log (prettyPrint c)
                      paths <- pure $ invertFunctionDescription c
                      -- log $ intercalate "\n" (prettyPrint <$> paths)
                      assert "The inversion of an expression with a filter should yield two inverse queries" (length paths == 2)

  test "Invert a rule condition" (runP do
      modelErrors <- loadAndCacheArcFile "inversion.arc" testDirectory
      if null modelErrors
        then do
          (Action{condition}) <- getAction (ActionType "model:Test$TestCase1$Self_bot$ChangeARole")
          -- logShow condition
          case condition of
            S _ -> liftAff $ assert "Condition should have been compiled." false
            Q qfd -> do
              affectedContextQueries <- pure $ invertFunctionDescription qfd
              -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
              -- logShow $ paths2functions <$> affectedContextQueries
              liftAff $ assert "There should be two AffectedContextQueries." ((paths2functions <$> affectedContextQueries) == [[Value2Role,(DataTypeGetter ContextF)],[Value2Role,(DataTypeGetter ContextF)]])
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "Invert a rule condition that reaches into a subcontext" (runP do
      modelErrors <- loadAndCacheArcFile "inversion.arc" testDirectory
      if null modelErrors
        then do
          (Action{condition}) <- getAction (ActionType "model:Test$TestCase2$Self_bot$ChangeARole")
          -- logShow condition
          case condition of
            S _ -> liftAff $ assert "Condition should have been compiled." false
            Q qfd -> do
              affectedContextQueries <- pure $ invertFunctionDescription qfd
              -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
              -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> affectedContextQueries)
              liftAff $ assert "There should be two AffectedContextQueries." ((paths2functions <$> affectedContextQueries) == [
                [ Value2Role
                , (DataTypeGetter ContextF)
                , (DataTypeGetter ExternalRoleF)
                , (DataTypeGetterWithParameter GetRoleBindersF "model:Test$TestCase2$ARole")
                , (DataTypeGetter ContextF)],
                [ Value2Role
                , (DataTypeGetter ContextF)]])
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "Invert a rule condition that reaches into a subcontext" (runP do
      modelErrors <- loadAndCacheArcFile "inversion.arc" testDirectory
      if null modelErrors
        then do
          (Action{condition}) <- getAction (ActionType "model:Test$TestCase3$Self_bot$ChangeARole")
          -- logShow condition
          case condition of
            S _ -> liftAff $ assert "Condition should have been compiled." false
            Q qfd -> do
              affectedContextQueries <- pure $ invertFunctionDescription qfd
              -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
              -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> affectedContextQueries)
              liftAff $ assert "There should be three AffectedContextQueries." (length affectedContextQueries == 3)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "Invert a rule condition with a filter criterium that reaches into a subcontext" (runP do
      modelErrors <- loadAndCacheArcFile "inversion.arc" testDirectory
      if null modelErrors
        then do
          (Action{condition}) <- getAction (ActionType "model:Test$TestCase4$Self_bot$ChangeARole")
          -- logShow condition
          case condition of
            S _ -> liftAff $ assert "Condition should have been compiled." false
            Q qfd -> do
              affectedContextQueries <- pure $ invertFunctionDescription qfd
              -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
              -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> affectedContextQueries)
              -- logShow (map domain affectedContextQueries)
              liftAff $ assert "There should be three AffectedContextQueries." (length affectedContextQueries == 3)
              liftAff $ assert "The three AffectedContextQueries should start in the properties Prop3, Prop5 and Prop2"
                ((map domain affectedContextQueries) ==
                  [ (VDOM PBool (Just $ ENP $ EnumeratedPropertyType "model:Test$TestCase4$NestedCase3$AnotherRole$Prop3"))
                  , (VDOM PBool (Just $ ENP $ EnumeratedPropertyType "model:Test$TestCase4$NestedCase3$YetAnotherRole$Prop5"))
                  , (VDOM PBool (Just $ ENP $ EnumeratedPropertyType "model:Test$TestCase4$NestedCase3$AnotherRole$Prop2"))])
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

composition2path :: QueryFunctionDescription -> Array QueryFunctionDescription
composition2path (BQD _ (BinaryCombinator ComposeF) left right _ _ _) = cons left (composition2path right)
composition2path qfd = [qfd]

paths2functions :: QueryFunctionDescription -> Array QueryFunction
paths2functions = (map queryFunction <<< composition2path)
