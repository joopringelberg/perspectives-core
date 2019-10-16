module Test.Parsing.Arc.Expression where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Class.Console (logShow)
import Perspectives.Parsing.Arc.Expression (binaryStep, compoundStep, filterStep, simpleStep, step, unaryStep)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), Operator(..), SimpleStep(..), Step(..), UnaryStep(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), runIndentParser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError(..))

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.Expression" do
  test "SimpleStep: ArcIdentifier" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyRole" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'MyRole' should be parsed as a the simple step ArcIdentifier" case id of
          (Simple (ArcIdentifier (ArcPosition{column: 1, line: 1}) "MyRole")) -> true
          otherwise -> false

  test "SimpleStep: Binding" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "binding" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'MyRole' should be parsed as a the simple step Binding" case id of
          (Simple (Binding (ArcPosition{column: 1, line: 1}))) -> true
          otherwise -> false

  test "SimpleStep: when it fails" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "notmatched" simpleStep
    case r of
      (Left (ParseError m _)) -> assert "Should fail with: 'Expected binding, binder, context, extern or a valid identifier'" (m == "Expected binding, binder, context, extern or a valid identifier")
      (Right id) -> assert "Should fail with: 'Expected binding, binder, context, extern or a valid identifier'" false

  test "UnaryStep: LogicalNot" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "not MyProperty" unaryStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'not MyProperty' should be parsed as a the unary step LogicalNot" case id of
          (Unary (LogicalNot (ArcPosition{column: 1, line: 1}) _)) -> true
          otherwise -> false

  test "UnaryStep: create" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "create MyRole" unaryStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'create MyRole' should be parsed as a the unary step Create" case id of
          (Unary (Create (ArcPosition{column: 1, line: 1}) "MyRole")) -> true
          otherwise -> false

  test "FilterStep" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "filter MyRole with ItsBooleanProp" filterStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'filter MyRole with ItsBooleanProp' should be parsed as a a binary step with operator 'Filter'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Filter _) -> true
              otherwise -> false
            otherwise -> false

  test "CompoundStep on filter with parens" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "(filter MyRole with ItsBooleanProp)" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'(filter MyRole with ItsBooleanProp)' should be parsed as a a binary step with operator 'Filter'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Filter _) -> true
              otherwise -> false
            otherwise -> false

  test "BinaryStep with ==" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "Prop1 == Prop2" binaryStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'Prop1 == Prop2' should be parsed as a a binary step with operator 'Equals'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Equals _) -> true
              otherwise -> false
            otherwise -> false
        assert "'Prop1 == Prop2' should have the operator starting at position (1, 6)"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Equals (ArcPosition{line: 1, column: 7})) -> true
              otherwise -> false
            otherwise -> false

  test "BinaryStep that fails on operator" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "Prop1 ? Prop2" binaryStep
    case r of
      (Left (ParseError m _)) -> do
        logShow m
        assert "In 'Prop1 ? Prop2', '?' is an invalid operator and that should be detected."
          (m == "Expected >>, ==, /=, <, <=, >, >=, and, or, +, -, /, *")
      (Right id) -> do
        logShow id
        assert "In 'Prop1 ? Prop2', '?' is an invalid operator and that should be detected." false

  test "Step on == with nested filter expression left" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "(filter MyRole with ItsBooleanProp) == MyOtherRole" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'(filter MyRole with ItsBooleanProp) == MyOtherRole' should be parsed as a a binary step with operator 'Equals'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Equals _) -> true
              otherwise -> false
            otherwise -> false

  test "Step on recursive binaryStep" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyRole >> MyProp == MyOtherRole >> MyProp" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'MyRole >> MyProp == MyOtherRole >> MyProp' should be parsed as a a binary step with operator 'Equals'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Equals _) -> true
              otherwise -> false
            otherwise -> false

  test "Step on recursive binaryStep with last subexpression as filter" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyRole >> MyProp == filter MyRole with MyProp" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'MyRole >> MyProp == filter MyRole with MyProp' should be parsed as a a binary step with operator 'Equals'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Equals _) -> true
              otherwise -> false
            otherwise -> false

  test "Step on unneccasary parens" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "((MyRole) >> (MyProp))" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'(MyRole) >> (MyProp)' should be parsed as a a binary step with operator 'Compose'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Compose _) -> true
              otherwise -> false
            otherwise -> false

  testOnly "Operator precedence on 'MyProp1 + MyProp2 * MyProp3'" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyProp1 + MyProp2 * MyProp3" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'(MyRole) >> (MyProp)' should be parsed as a a binary step with operator 'Add'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Add _) -> true
              otherwise -> false
            otherwise -> false
