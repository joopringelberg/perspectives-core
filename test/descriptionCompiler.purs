module Test.Query.DescriptionCompiler where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (Namespace)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), DomeinFileId(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Parsing (ParseError)

withDomeinFile :: forall a. Namespace -> DomeinFile -> MonadPerspectives a -> MonadPerspectives a
withDomeinFile ns df mpa = do
  void $ storeDomeinFileInCache (DomeinFileId ns) df
  r <- mpa 
  removeDomeinFileFromCache (DomeinFileId ns)
  pure r

makeTest_ :: (String -> Aff Unit -> Free TestF Unit) ->
  String ->
  String ->
  (PerspectivesError -> Aff Unit) ->
  (DomeinFileRecord -> Aff Unit) ->
  Free TestF Unit
makeTest_ test title source errorHandler theTest = test title do
  (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser source ARC.domain
  case r of
    (Left e) -> assert (show e) false
    (Right ctxt@(ContextE{id})) -> do
      -- logShow ctxt
      runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
            case x of
              (Left e) -> for_ e errorHandler
              (Right (Tuple correctedDFR _)) -> theTest correctedDFR

makeTest :: String -> String -> (PerspectivesError -> Aff Unit) -> (DomeinFileRecord -> Aff Unit) -> Free TestF Unit
makeTest = makeTest_ test

makeTestOnly :: String -> String -> (PerspectivesError -> Aff Unit) -> (DomeinFileRecord -> Aff Unit) -> Free TestF Unit
makeTestOnly = makeTest_ testOnly

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.Query.ExpressionCompiler" do

  makeTest "compileSimpleStep: ArcIdentifier, Role."
    "domain Test\n  thing Role = AnotherRole\n  thing AnotherRole (mandatory)"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedRoles}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role" calculatedRoles of
        Nothing -> assert "There should be a role 'Role'" false
        Just (CalculatedRole{calculation}) -> do
          assert "The calculation should have '(RDOM (ST EnumeratedRoleType model:Test$AnotherRole))' as its Range"
            case calculation of
              (Q (SQD _ _ (RDOM (ST (RoleInContext{context: (ContextType "model:Test"), role: (EnumeratedRoleType "model:Test$AnotherRole")}))) _ _)) -> true
              otherwise -> false
          assert "The queryfunction of the calculation should be '(RolGetter \"model:Test$AnotherRole\")'"
            case calculation of
              (Q (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$AnotherRole"))) _ _ _)) -> true
              otherwise -> false)

  makeTest "compileSimpleStep: ArcIdentifier, missing Role."
    "domain Test\n  thing Role = AnotherRole"
    (\e -> case e of
      (ContextHasNoRole _ _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that there is no role AnotherRole" false)

  makeTest "compileSimpleStep: ArcIdentifier, Property."
    "domain Test\n  thing Role (mandatory)\n    property Prop1 = Prop2\n    property Prop2 (mandatory, Boolean)\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role$Prop1" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop1'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should have '(VDOM PBool)' as its Range"
            case calculation of
              (Q (SQD _ _ (VDOM PBool _) _ _)) -> true
              otherwise -> false
          assert "The queryfunction of the calculation should be '(PropertyGetter \"model:Test$AnotherRole\")'"
            case calculation of
              (Q (SQD _ (PropertyGetter (ENP (EnumeratedPropertyType "model:Test$Role$Prop2"))) _ _ _)) -> true
              otherwise -> false)

  makeTest "compileSimpleStep: ArcIdentifier, missing Property."
    "domain Test\n  thing Role (mandatory)\n    property Prop1 = Prop2"
    (\e -> case e of
      (RoleHasNoProperty _ _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that there is no property Prop2" false)

  makeTest "compileSimpleStep: Value."
    "domain Test\n  thing Role (mandatory)\n    property Prop1 = 1"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role$Prop1" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop1'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should have '(VDOM PNumber)' as its Range"
            case calculation of
              (Q (SQD _ _ (VDOM PNumber _) _ _)) -> true
              otherwise -> false
          assert "The queryfunction of the calculation should be '(Constant PNumber \"1\")'"
            case calculation of
              (Q (SQD _ (Constant PNumber "1") _ _ _)) -> true
              otherwise -> false)

  makeTest "compileSimpleStep: Binding."
    "domain Test\n  thing Role1 (mandatory) filledBy Role2\n    property Prop1 = binding >> Prop2\n  thing Role2 (mandatory)\n    property Prop2 (mandatory, Boolean)"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1$Prop1" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop1'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be a composition, the first step of which is 'binding'"
            case calculation of
              (Q (BQD _ _ (SQD _ (DataTypeGetter FillerF) _ _ _) _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileSimpleStep: Binding, missing binding."
    "domain Test\n  thing Role1 (mandatory) filledBy None\n    property Prop1 = binding >> Prop2\n"
    (\e -> case e of
      (RoleHasNoBinding _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that there can be no binding for Role1" false)

  makeTest "compileSimpleStep: Binder."
    "domain Test\n  thing Role1 (mandatory) filledBy Role2\n  thing Role2 (mandatory)\n  thing Role3 = Role2 >> binder Role1\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedRoles}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role3" calculatedRoles of
        Nothing -> assert "There should be a role 'Role3'" false
        Just (CalculatedRole{calculation}) -> do
          assert "The calculation should be a composition, the second step of which is 'getRoleBinders'"
            case calculation of
              (Q (BQD _ _ _ (SQD _ (FilledF (EnumeratedRoleType "model:Test$Role1") (ContextType "model:Test")) _ _ _) _ _ _)) -> true
              otherwise -> false
              )

  -- TODO. It is not clear how None should be compiled or what its effect should be.
  -- makeTestOnly "compileSimpleStep: Binder, missing binding."
  --   "domain Test\n  thing Role1 (mandatory) filledBy None\n  thing Role2 (mandatory)\n  thing Role3 = Role2 >> binder Role1\n"
  --   (\e -> case e of
  --     (RoleDoesNotBind _ _ _) -> pure unit
  --     e' -> assert (show e') false)
  --   (\(correctedDFR@{calculatedRoles}) -> do
  --     logShow correctedDFR
  --     assert "It should be detected that there is no binding for Role1" false)

  makeTest "compileSimpleStep: Context."
    "domain Test\n  thing Role1 (mandatory)\n  thing Role2 = Role1 >> context >> Role1\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedRoles}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role2" calculatedRoles of
        Nothing -> assert "There should be a role 'Role2'" false
        Just (CalculatedRole{calculation}) -> do
          assert "The calculation should be a composition, the second step of which is a composition the first of which 'context'"
            case calculation of
              (Q (BQD _ _ _ (BQD _ _ (SQD _ (DataTypeGetter ContextF) _ _ _) _ _ _ _) _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileSimpleStep: Context, wrong argument type."
    "domain Test\n  thing Role1 (mandatory)\n  thing Role2 (mandatory)\n  thing Role3 = binder Role1\n"
    (\e -> case e of
      (IncompatibleQueryArgument _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that context cannot be applied to a Context" false)

  makeTest "compileSimpleStep: Extern."
    "domain Test\n  thing Role1 = extern\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedRoles}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1" calculatedRoles of
        Nothing -> assert "There should be a role 'Role1'" false
        Just (CalculatedRole{calculation}) -> do
          assert "The calculation should be a simple calculation,of which the queryfunction is '(DataTypeGetter \"externalRole\")'"
            case calculation of
              (Q (SQD _ (DataTypeGetter ExternalRoleF) _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileSimpleStep: Extern, wrong argument type."
    "domain Test\n  thing Role1 = Role2 >> extern\n  thing Role2 (mandatory)\n"
    (\e -> case e of
      (IncompatibleQueryArgument _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that extern cannot be applied to a Role" false)

  makeTest "compileSimpleStep: Identity."
    "domain Test\n  thing Guest (mandatory)\n  thing GuestToo = Guest >> this"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedRoles}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$GuestToo" calculatedRoles of
        Nothing -> assert "There should be a role 'GuestToo'" false
        Just (CalculatedRole{calculation}) -> do
          assert "The calculation should be a composition,of which the second operand is the Identity function."
            case calculation of
              (Q (BQD _ _ _ (SQD _ (DataTypeGetter IdentityF) _ _ _) _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileUnaryStep: LogicalNot."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 = not Prop1\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1$Prop2" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop2'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be a Unary combination, the queryFunction of which should be '(UnaryCombinator \"not\")'"
            case calculation of
              (Q (UQD _ (UnaryCombinator NotF) _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileUnaryStep: LogicalNot, wrong argument type"
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, String)\n    property Prop2 = not Prop1\n"
    (\e -> case e of
      (IncompatibleQueryArgument _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that 'not' must be applied to a Boolean value" false)

  makeTest "compileUnaryStep: exists."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 = exists Prop1\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1$Prop2" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop2'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be a Unary combination, the queryFunction of which should be '(UnaryCombinator \"exists\")'"
            case calculation of
              (Q (UQD _ (UnaryCombinator ExistsF) _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileUnaryStep: exists, wrong argument type"
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, String)\n    property Prop2 = exists context\n"
    (\e -> case e of
      (IncompatibleQueryArgument _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that exists cannot be usefully applied to a Context" false)

  makeTest "compileUnaryStep: available."
    "domain Test\n  thing SomeOtherRole\n  thing Role1 (mandatory) filledBy SomeOtherRole\n    property HasBinding = available binding\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1$HasBinding" calculatedProperties of
        Nothing -> assert "There should be a property 'HasBinding'" false
        Just (CalculatedProperty{calculation}) -> do
          case calculation of
            -- (Q c) -> log $ prettyPrint c
            (Q c) -> pure unit
            otherwise -> pure unit
          assert "The calculation should be a Unary combination, the queryFunction of which should be '(UnaryCombinator \"available\")'"
            case calculation of
              (Q c@(UQD _ (UnaryCombinator AvailableF) _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileUnaryStep: available, wrong argument type"
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, String)\n    property Prop2 = available Prop1\n"
    (\e -> case e of
      (IncompatibleQueryArgument _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that available cannot be applied to a Value" false)

  makeTest "compileBinaryStep: compose."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n  thing Role3 (mandatory)\n    property Prop2 = context >> Role1 >> Prop1\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role3$Prop2" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop2'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be a composition"
            case calculation of
              (Q (BQD _ (BinaryCombinator ComposeF) _ _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileBinaryStep: compose with incompatible types"
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n  thing Role3 (mandatory)\n    property Prop2 = context >> binding >> Prop1\n"
    (\e -> case e of
      (IncompatibleQueryArgument _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that the terms have differnet types" false)

  makeTest "compileBinaryStep: make a comparison."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 (mandatory, Boolean)\n    property Prop3 = Prop1 == Prop2\n    \n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1$Prop3" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop3'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be (BinaryCombinator \"equals\")"
            case calculation of
              (Q (BQD _ (BinaryCombinator EqualsF) _ _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileBinaryStep: make a comparison, terms have different result types."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 (mandatory, String)\n    property Prop3 = Prop1 == Prop2\n    \n"
    (\e -> case e of
      (TypesCannotBeCompared _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that the terms have differnet types" false)

  makeTest "compileBinaryStep: make a binary operation with 'and'."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 (mandatory, Boolean)\n    property Prop3 = Prop1 and Prop2\n    \n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role1$Prop3" calculatedProperties of
        Nothing -> assert "There should be a property 'Prop3'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be (BinaryCombinator \"and\")"
            case calculation of
              (Q (BQD _ (BinaryCombinator AndF) _ _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileBinaryStep: make a binary operation with `and` on Number."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Number)\n    property Prop2 (mandatory, Number)\n    property Prop3 = Prop1 and Prop2\n    \n"
    (\e -> case e of
      (WrongTypeForOperator _ _ _) -> pure unit
      e' -> assert (show e') false)
    (\(correctedDFR@{calculatedRoles}) -> assert "It should be detected that the terms have differnet types" false)

  makeTest "compileBinaryStep: make a binary operation with 'filter'."
    "domain Test\n  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n  thing Role2 = filter Role1 with Prop1"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedRoles}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Role2" calculatedRoles of
        Nothing -> assert "There should be a role 'Role2'" false
        Just (CalculatedRole{calculation}) -> do
          assert "The calculation should be (BinaryCombinator \"filter\")"
            case calculation of
              (Q (UQD _ FilterF  _ _ _ _)) -> true
              otherwise -> false
              )

  makeTest "compileBinaryStep: make a binary operation with '>>='."
    "domain Test\n  thing Guest (mandatory)\n    property NumberOfGuests = this >>= count"
    (\e -> assert (show e) false)
    (\(correctedDFR@{calculatedProperties}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Guest$NumberOfGuests" calculatedProperties of
        Nothing -> assert "There should be a property 'NumberOfGuests'" false
        Just (CalculatedProperty{calculation}) -> do
          assert "The calculation should be (DataTypeGetter \"count\")"
            case calculation of
              (Q (SQD _ (DataTypeGetter CountF) (VDOM PNumber _) _ _)) -> true
              otherwise -> false
              )
{-
  makeTest "compileLetStep."
    "domain Test\n  user: Self\n    property Prop1 (mandatory, Number)\n    property AnotherProp (mandatory, Number)\n  bot: for Self\n    perspective on: Self\n      if Self >> Prop1 > 10 then\n        let*\n          a <- 20\n        in\n          AnotherProp = a\n          Prop1 = a\n"
    (\e -> assert (show e) false)
    (\(correctedDFR@{enumeratedRoles, actions}) -> do
      -- logShow correctedDFR
      case lookup "model:Test$Self" enumeratedRoles of
        Nothing -> assert "There should be a role 'Self'" false
        Just (EnumeratedRole{perspectives}) -> do
          -- logShow perspectives
          assert "bla" true
      case lookup "model:Test$Self_bot$ChangeSelf" actions of
        Nothing -> assert "There should be an action 'model:Test$Self_bot$ChangeSelf'" false
        (Just (Action{effect})) -> do
          -- logShow effect
          case effect of
            Nothing -> assert "There should be an effect in the action 'model:Test$Self_bot$ChangeSelf'" false
            (Just qfd@(EF (BQD _ (BinaryCombinator SequenceF) (UQD _ (BindVariable "a") _ _) (BQD _ (BinaryCombinator SequenceF) (BQD _ (AssignmentOperator "SetProperty") _ _ _) (BQD _ (AssignmentOperator "SetProperty") _ _ _) _) _ ))) -> do
              logShow qfd
              assert "Tests ok" true
            otherwise -> do
              logShow otherwise
              assert "There should be a LetStep in the effect of the action." false
          )

  test "compileLetStep. Rule with PureLetStep" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user: Self\n    property Prop1 (mandatory, Number)\n    property AnotherProp (mandatory, Number)\n  bot: for Self\n    perspective on: Self\n      if Self >> Prop1 > 10 then\n        let*\n          a <- 20\n        in\n          a\n" ARC.domain
    case r of
      (Left (ParseError m _)) -> do
        assert "bla" true
      otherwise -> do
        logShow otherwise
        assert "It should have been detected that the let* doesn't have an assignment in its body." false
-}
