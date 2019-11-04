module Test.Parsing.Arc.Expression where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Class.Console (logShow)
import Perspectives.Parsing.Arc.Expression (assignment, operator, simpleStep, step, unaryStep)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), AssignmentOperator(..), BinaryStep(..), Operator(..), SimpleStep(..), Step(..), UnaryStep(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), runIndentParser)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError(..))

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.Parsing.Arc.Expression" do
  test "SimpleStep: ArcIdentifier" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyRole" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'MyRole' should be parsed as a the simple step ArcIdentifier" case id of
          (Simple (ArcIdentifier (ArcPosition{column: 1, line: 1}) "MyRole")) -> true
          otherwise -> false

  test "SimpleStep: Binding" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "binding" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'MyRole' should be parsed as a the simple step Binding" case id of
          (Simple (Binding (ArcPosition{column: 1, line: 1}))) -> true
          otherwise -> false

  test "SimpleStep: when it fails" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "notmatched" simpleStep
    case r of
      (Left (ParseError m _)) -> assert "Should fail with: 'binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or count'" (m == "Expected binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or count")
      (Right id) -> assert "Should fail with: 'Expectedbinding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or count'" false

  test "UnaryStep: LogicalNot" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "not MyProperty" unaryStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'not MyProperty' should be parsed as a the unary step LogicalNot" case id of
          (Unary (LogicalNot (ArcPosition{column: 1, line: 1}) _)) -> true
          otherwise -> false

  test "UnaryStep: create" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "createRole MyRole" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'create MyRole' should be parsed as a the unary step Create" case id of
          (Simple (CreateEnumeratedRole (ArcPosition{column: 1, line: 1}) "MyRole")) -> true
          otherwise -> false

  test "FilterStep" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "filter MyRole with ItsBooleanProp" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
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
        -- logShow id
        assert "'(filter MyRole with ItsBooleanProp)' should be parsed as a a binary step with operator 'Filter'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Filter _) -> true
              otherwise -> false
            otherwise -> false

  test "BinaryStep with ==" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "Prop1 == Prop2" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
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


  test "BinaryStep with different SimpleSteps" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "AnotherRole >> binding" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'AnotherRole >> binding' should be parsed as a a binary step with operator 'Compose'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Compose _) -> true
              otherwise -> false
            otherwise -> false

  test "BinaryStep that fails on operator" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "Prop1 ? Prop2" step
    case r of
      (Left (ParseError m _)) -> do
        -- logShow m
        assert "In 'Prop1 ? Prop2', '?' is an invalid operator and that should be detected."
          (m == "with, or an operator: >>, ==, /=, <, <=, >, >=, and, or, +, -, /, *, >>=")
      (Right id) -> do
        -- logShow id
        assert "Of 'Prop1 ? Prop2', just the first term should be parsed." (id == (Simple (ArcIdentifier (ArcPosition { column: 1, line: 1 }) "Prop1")))

  test "Step on == with nested filter expression left" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "filter MyRole with ItsBooleanProp == MyOtherRole" step
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
        -- logShow id
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
        -- logShow id
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
        -- logShow id
        assert "'(MyRole) >> (MyProp)' should be parsed as a a binary step with operator 'Compose'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Compose _) -> true
              otherwise -> false
            otherwise -> false

  test "Operator precedence on 'MyProp1 + MyProp2 * MyProp3'" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyProp1 + MyProp2 * MyProp3" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'MyProp1 + MyProp2 * MyProp3' should be parsed as a a binary step with operator 'Add'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Add _) -> true
              otherwise -> false
            otherwise -> false

  test "Operator precedence on '(MyProp1 + MyProp2) * MyProp3'" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "(MyProp1 + MyProp2) * MyProp3" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        logShow id
        assert "'(MyRole) >> (MyProp)' should be parsed as a a binary step with operator 'Multiply'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Multiply _) -> true
              otherwise -> false
            otherwise -> false

  test "Assignment: MyRole = AnotherRole" do
    (r :: Either ParseError Assignment) <- pure $ unwrap $ runIndentParser "MyRole = AnotherRole" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Assignment{operator})) -> do
        -- logShow a
        assert "'MyRole = AnotherRole' should be parsed as a an Assignment with operator Set"
          case operator of
            (Set _) -> true
            otherwise -> false

  test "MyRole =+ AnotherRole >> binding" do
    (r :: Either ParseError Assignment) <- pure $ unwrap $ runIndentParser "MyRole =+ AnotherRole >> binding" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Assignment{operator})) -> do
        -- logShow a
        assert "'MyRole =+ AnotherRole >> binding' should be parsed as a an Assignment with operator AddTo"
          case operator of
            (AddTo _) -> true
            otherwise -> false

  test "delete MyProp" do
    (r :: Either ParseError Assignment) <- pure $ unwrap $ runIndentParser "delete Myprop" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Assignment{operator})) -> do
        -- logShow a
        assert "'delete Myprop' should be parsed as a an Assignment with operator Delete"
          case operator of
            (Delete _) -> true
            otherwise -> false

  test "number in equation" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyProp > 10" step
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Binary (BinaryStep{operator, right}))) -> do
        -- logShow a
        assert "'MyProp > 10' should be parsed as a a GreaterThen with left operand the number 10"
          case operator of
            (GreaterThan _) -> true
            otherwise -> false
        assert "The right term should be '(Simple (Value _ PNumber \"10\"))'"
          case right of
            (Simple (Value _ PNumber "10")) -> true
            otherwise -> false
      otherwise -> assert "'MyProp > 10' should be parsed as a a GreaterThen" false

  test "boolean" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "false" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PBool "false"))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "'false' should be parsed as a (Value _ PBoolean \"false\")" false

  test "string" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "\"aap\"" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PString "aap"))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "'\"aap\"' should be parsed as a (Value _ PBoolean \"aap\")" false

  test "date" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "'1995-12-17T03:24:00'" simpleStep
    -- logShow r -- (Right (Simple (Value (ArcPosition { column: 1, line: 1 }) PNumber "1995")))
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PDate _))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "\"1995-12-17T03:24:00\" should be parsed as a (Date _ PDate ...)" false

  test "date" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "'1995-12-17'" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PDate _))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "'1995-12-17' should be parsed as a (Date _ PDate ...)" false

  test "date in comparison" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyProp > '1995-12-17'" step
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Binary (BinaryStep{operator, right}))) -> do
        -- logShow a
        assert "'MyProp > 10' should be parsed as a a GreaterThan with right operand the DateTime '1995-12-17'"
          case operator of
            (GreaterThan _) -> true
            otherwise -> false
        assert "The right term should be '(Simple (Value _ PDate \"1995-12-17\"))'"
          case right of
            (Simple (Value _ PDate _)) -> true
            otherwise -> false
      otherwise -> assert "'MyProp > '1995-12-17'' should be parsed as a a GreaterThan" false

  test "SimpleStep: sum" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "sum" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'sum' should be parsed as a the simple step SequenceFunction" case id of
          (Simple (SequenceFunction (ArcPosition{column: 1, line: 1}) "sum")) -> true
          otherwise -> false

  test "Operator: >>=" do
    (r :: Either ParseError Operator) <- pure $ unwrap $ runIndentParser ">>=" operator
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'>>=' should be parsed as a the operator Sequence" case id of
          (Sequence (ArcPosition{column: 1, line: 1})) -> true
          otherwise -> false

  test "sequenceStep" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyProp >>= sum" step
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Binary (BinaryStep {operator}))) -> do
        -- logShow a
        assert "'MyProp >>= sum' should be parsed as a a BinaryStep with operator equal to 'Sequence'"
          case operator of
            Sequence _ -> true
            otherwise -> false
      otherwise -> do
        -- logShow otherwise
        assert "'MyProp >>= sum' should be parsed as a a SequenceFunction" false

  test "sequenceStep with non-existing sequenceFunction" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser " this >>= fantasy" step
    case r of
      (Left (ParseError m _)) -> do
        -- logShow m
        assert "Should fail with: 'Expected binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or coun'" (m == "Expected binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or count")
      (Right id) -> do
        -- logShow id
        assert "Should fail with: 'Expected binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or coun'" false

  test "sequenceStep with complex left side" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyRole >> binding >> MyProp >>= sum" step
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Binary (BinaryStep {right}))) -> do
        -- logShow a
        case right of
          (Binary (BinaryStep{right:right'})) -> do
            case right' of
              (Binary (BinaryStep{operator})) -> assert "'MyRole >> MyProp >>= sum' should be parsed as a a BinaryStep with operator equal to 'Sequence'"
                case operator of
                  Sequence _ -> true
                  otherwise -> false
              otherwise -> assert "'MyRole >> binding >> MyProp >>= sum' should be parsed as a a Sequence" false
          otherwise -> assert "'MyRole >> binding >> MyProp >>= sum' should be parsed as a a Sequence" false
      x -> do
        -- logShow x
        assert "'MyRole >> binding >> MyProp >>= sum' should be parsed as a a Sequence" false

  test "sequenceStep with complex left side" do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "MyRole >> binding >> MyProp >>= sum + 1" step
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Binary (BinaryStep {operator}))) -> do
        assert "'MyRole >> binding >> MyProp >>= sum + 1' should be parsed as a a BinaryStep with operator equal to 'add'"
          case operator of
            Add _ -> true
            otherwise -> false
      x -> do
        -- logShow x
        assert "'MyRole >> binding >> MyProp >>= sum + 1' should be parsed as a BinaryStep with operator equal to add" false
