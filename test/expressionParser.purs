module Test.Parsing.Arc.Expression where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (isJust, isNothing)
import Effect.Class.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.Expression (computationStep, operator, simpleStep, step, unaryStep)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), ComputedType(..), Operator(..), SimpleStep(..), Step(..), UnaryStep(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Arc.Statement (assignment, roleAssignment)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..)) as PAS
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), AssignmentOperator(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Utilities (prettyPrint)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Parsing (ParseError(..))

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.Expression" do
  test "SimpleStep: ArcIdentifier" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyRole" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do 
        -- logShow id
        assert "'MyRole' should be parsed as a the simple step ArcIdentifier" case id of
          (Simple (ArcIdentifier (ArcPosition{column: 1, line: 1}) "MyRole")) -> true 
          otherwise -> false

  test "SimpleStep: Binding" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "binding" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'MyRole' should be parsed as a the simple step Binding" case id of
          (Simple (Filler (ArcPosition{column: 1, line: 1}) _)) -> true
          otherwise -> false

  test "SimpleStep: Variable" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "someName" simpleStep
    case r of
      (Left (ParseError m _)) -> assert "A lowercase name should be parsed as a variable" false
      (Right v@(Simple (Variable _ _))) -> do
        -- logShow v
        assert "A lowercase name should be parsed as a variable" true
      otherwise -> do
        -- logShow otherwise
        assert "A lowercase name should be parsed as a variable" false

  test "UnaryStep: LogicalNot" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "not MyProperty" unaryStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'not MyProperty' should be parsed as a the unary step LogicalNot" case id of
          (Unary (LogicalNot (ArcPosition{column: 1, line: 1}) _)) -> true
          otherwise -> false

  test "FilterStep" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "filter MyRole with ItsBooleanProp" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "(filter MyRole with ItsBooleanProp)" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "Prop1 == Prop2" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "AnotherRole >> binding" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "Prop1 ? Prop2" step
    case r of
      (Left (ParseError m _)) -> do
        -- logShow m
        assert "In 'Prop1 ? Prop2', '?' is an invalid operator and that should be detected."
          (m == "with, or an operator: >>, ==, /=, <, <=, >, >=, and, or, +, -, /, *, >>=")
      (Right id) -> do
        -- logShow id
        assert "Of 'Prop1 ? Prop2', just the first term should be parsed." (id == (Simple (ArcIdentifier (ArcPosition { column: 1, line: 1 }) "Prop1")))

  test "Step on == with nested filter expression left" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "filter MyRole with ItsBooleanProp == MyOtherRole" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'(filter MyRole with ItsBooleanProp) == MyOtherRole' should be parsed as a a binary step with operator 'Equals'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Equals _) -> true
              otherwise -> false
            otherwise -> false

  test "Filter with compound criterium" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "filter MyRole with (MyOtherRole >> Criterium)" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        -- log $ prettyPrint id
        assert "'filter MyRole with (MyOtherRole >> Criterium)' should be parsed as a a binary step with operator 'Filter'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Filter _) -> true
              otherwise -> false
            otherwise -> false

  test "Step on recursive binaryStep" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyRole >> MyProp == MyOtherRole >> MyProp" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyRole >> MyProp == filter MyRole with MyProp" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "((MyRole) >> (MyProp))" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyProp1 + MyProp2 * MyProp3" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "(MyProp1 + MyProp2) * MyProp3" step
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'(MyRole) >> (MyProp)' should be parsed as a a binary step with operator 'Multiply'"
          case id of
            (Binary (BinaryStep {operator})) -> case operator of
              (Multiply _) -> true
              otherwise -> false
            otherwise -> false

  test "number in equation" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyProp > 10" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "false" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PBool "false"))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "'false' should be parsed as a (Value _ PBoolean \"false\")" false

  test "string" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "\"aap\"" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PString "aap"))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "'\"aap\"' should be parsed as a (Value _ PBoolean \"aap\")" false

  test "date" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "'1995-12-17T03:24:00'" simpleStep
    -- logShow r -- (Right (Simple (Value (ArcPosition { column: 1, line: 1 }) PNumber "1995")))
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PDate _))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "\"1995-12-17T03:24:00\" should be parsed as a (Date _ PDate ...)" false

  test "date" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "'1995-12-17'" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Simple (Value _ PDate _))) -> do
        -- logShow a
        assert "bla" true
      otherwise -> assert "'1995-12-17' should be parsed as a (Date _ PDate ...)" false

  test "date in comparison" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyProp > '1995-12-17'" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "sum" simpleStep
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'sum' should be parsed as a the simple step SequenceFunction" case id of
          (Simple (SequenceFunction (ArcPosition{column: 1, line: 1}) AddF)) -> true
          otherwise -> false

  test "Operator: >>=" do
    (r :: Either ParseError Operator) <- {-pure $ unwrap $-} runIndentParser ">>=" (unsafePartial operator)
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        -- logShow id
        assert "'>>=' should be parsed as a the operator Sequence" case id of
          (Sequence (ArcPosition{column: 1, line: 1})) -> true
          otherwise -> false

  test "sequenceStep" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyProp >>= sum" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser " this >>= fantasy" step
    case r of
      (Left (ParseError m _)) -> do
        -- logShow m
        assert "bla" true
      (Right id) -> do
        -- logShow id
        assert "Should fail." false

  test "sequenceStep with complex left side" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyRole >> binding >> MyProp >>= sum" step
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
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "MyRole >> binding >> MyProp >>= sum + 1" step
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Binary (BinaryStep {operator}))) -> do
        assert "'MyRole >> binding >> MyProp >>= sum + 1' should be parsed as a a BinaryStep with operator equal to 'add'"
          case operator of
            Add _ -> true
            otherwise -> false
      x -> do
        -- logShow x
        assert "'MyRole >> binding >> MyProp >>= sum + 1' should be parsed \
        \as a BinaryStep with operator equal to add" false

  -- test "LetStep" do
  --   (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "let*\n  a <- MyProp\n  b <- SecondProp\nin\n  AnotherProp = a\n  SomeProp = 1" letStep
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right a@(Let (LetStep {bindings, assignments}))) -> do
  --       -- logShow a
  --       assert "There should be two bindings" (length bindings == 2)
  --       assert "There should be two statements" (length assignments == 2)
  --     x -> do
  --       -- logShow x
  --       assert "'let*\n  a <- MyProp\n  b <- SecondProp\nin\n  AnotherProp = a' should be parsed as a LetStep" false
-----------------------------------------------------------------------------------
---- ASSIGNMENT
-----------------------------------------------------------------------------------
  test "Assignment: remove" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "remove MyRole" roleAssignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.RemoveRole _)) -> do
        -- logShow a
        assert "'remove MyRole' should be parsed as a Remove assignment" true
      otherwise -> assert ("'remove MyRole' should be parsed as a Remove assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: createRole" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "createRole MyRole" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.CreateRole {contextExpression})) -> do
        -- logShow a
        assert "There should be no contextExpression" (isNothing contextExpression)
      otherwise -> assert ("'createRole MyRole' should be parsed as a CreateRole assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: createRole in an embedded context" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "createRole MyRole in SomeContextRole >> binding >> context" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.CreateRole {contextExpression})) -> do
        -- logShow a
        assert "There should be a contextExpression" (isJust contextExpression)
      otherwise -> assert ("'createRole MyRole in SomeContextRole >> binding >> context' should be parsed as a CreateRole assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: move" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "move MyRole" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.Move {roleExpression: Simple (ArcIdentifier _ "MyRole"), contextExpression})) -> do
        -- logShow a
        assert "test ok" (isNothing contextExpression)
      otherwise -> assert ("'move MyRole' should be parsed as a Move assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: move to an embedded context" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "move MyRole to SomeContextRole >> binding >> context" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.Move {contextExpression})) -> do
        -- logShow a
        assert "There should be a contextExpression" (isJust contextExpression)
      otherwise -> assert ("'move MyRole to SomeContextRole >> binding >> context' should be parsed as a Move assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: bind" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "bind MyRole to AnotherRole" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.Bind {bindingExpression: Simple (ArcIdentifier _ "MyRole"), roleIdentifier})) -> do
        -- logShow a
        assert "roleIdentifier should be 'AnotherRole'" (roleIdentifier == "AnotherRole")
      otherwise -> assert ("'bind MyRole to AnotherRole' should be parsed as a Bind assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: bind in an embedded context" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "bind MyRole to AnotherRole in SomeContextRole >> binding >> context" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PAS.Bind {bindingExpression: Simple (ArcIdentifier _ "MyRole"), roleIdentifier, contextExpression})) -> do
        -- logShow a
        assert "roleIdentifier should be 'AnotherRole'" (isJust contextExpression)
      otherwise -> assert ("'bind MyRole to AnotherRole in SomeContextRole >> binding >> context' should be parsed as a Bind assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: bind with path as second term should fail" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "bind MyRole to (SomeContextRole >> binding >> context >> AnotherRole)" assignment
    case r of
      (Left e) -> assert "failure expected" true
      otherwise -> assert "'bind MyRole to SomeContextRole >> binding >> context >> AnotherRole' should fail because the second term must be an identifier" false

  test "Assignment: propertyAssignment" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "MyProp = 10" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PropertyAssignment {propertyIdentifier, operator, valueExpression, roleExpression})) -> do
        -- logShow a
        assert "propertyIdentifier should be 'MyProp'" (propertyIdentifier == "MyProp")
        assert "operator should be Set" (case operator of
          (Set _) -> true
          otherwise -> false)
        assert "valueExpression should be Simple" (valueExpression == (Simple (Value (ArcPosition { column: 10, line: 1 }) PNumber "10")))
        assert "There should be no roleExpression" (isNothing roleExpression)
      otherwise -> assert ("'MyProp = 10' should be parsed as a PropertyAssignment assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: propertyAssignment for another role" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "MyProp = 10 for AnotherRole" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(PropertyAssignment {roleExpression})) -> do
        assert "There should be a roleExpression" (isJust roleExpression)
      otherwise -> assert ("'MyProp = 10 for AnotherRole' should be parsed as a PropertyAssignment assignment, instead this was returned: " <> show otherwise) false

  test "Assignment: callEffect" do
    (r :: Either ParseError Assignment) <- {-pure $ unwrap $-} runIndentParser "callEffect cdb:LoadModel()" assignment
    case r of
      (Left e) -> assert (show e) false
      (Right a@(ExternalEffect {effectName, arguments})) -> do
        -- logShow a
        assert "functionName should be 'cbd:LoadModel'" (effectName == "cdb:LoadModel")
        assert "no arguments" (arguments == [])
      otherwise -> assert ("'callEffect cdb:LoadModel()' should be parsed as an ExternalEffect assignment, instead this was returned: " <> show otherwise) false

  test "callExternal" do
    (r :: Either ParseError Step) <- {-pure $ unwrap $-} runIndentParser "callExternal ser:SerialiseFor( \"model:System$Invitation$Invitee\", context ) returns String" computationStep
    case r of
      (Left e) -> assert (show e) false
      (Right a@(Computation (ComputationStep {functionName, arguments, computedType}))) -> do
        -- logShow a
        assert "functionName should be 'cbd:LoadModel'" (functionName == "ser:SerialiseFor")
        assert "two arguments" (length arguments == 2)
        assert "ComputedType should be 'String'" (computedType == ComputedRange PString)
      otherwise -> assert ("'callExternal ser:SerialiseFor( \"model:System$Invitation$Invitee\", context ) returns String' should be parsed as an ComputationStep, instead this was returned: " <> show otherwise) false
