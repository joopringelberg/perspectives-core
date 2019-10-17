module Test.Parsing.Arc where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.List (filter, findIndex, head, length)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Perspectives.Parsing.Arc (actionE, domain, perspectiveE, propertyE, roleE, viewE)
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), runIndentParser)
import Perspectives.Representation.Action (Verb(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError(..))
import Unsafe.Coerce (unsafeCoerce)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.Parsing.Arc" do
  test "arcIdentifier on simple name" do
    (r :: Either ParseError String) <- pure $ unwrap $ runIndentParser "MyTestDomain" arcIdentifier
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        assert "'MyTestDomain' should be parsed as a valid identifier" (id == "MyTestDomain")

  test "arcIdentifier on uncapitalised name" do
    (r :: Either ParseError String) <- pure $ unwrap $ runIndentParser "myTestDomain" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        assert "'MyTestDomain' should be parsed as a valid identifier" (id == "MyTestDomain")

  test "arcIdentifier on segmented name" do
    (r :: Either ParseError String) <- pure $ unwrap $ runIndentParser "MyTestDomain$Context" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        logShow id
        assert "'MyTestDomain$Context' should be parsed as a valid identifier" (id == "MyTestDomain$Context")

  test "arcIdentifier on prefixed name" do
    (r :: Either ParseError String) <- pure $ unwrap $ runIndentParser "pre:MyTestDomain" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        assert "'pre:MyTestDomain' should be parsed as a valid identifier" (id == "pre:MyTestDomain")

  test "arcIdentifier on prefixed segmented name" do
    (r :: Either ParseError String) <- pure $ unwrap $ runIndentParser "pre:MyTestDomain$Context" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        logShow id
        assert "'pre:MyTestDomain$Context' should be parsed as a valid identifier" (id == "pre:MyTestDomain$Context")

  test "arcIdentifier on fully qualified name" do
    (r :: Either ParseError String) <- pure $ unwrap $ runIndentParser "model:MyTestDomain$Context" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        logShow id
        assert "'model:MyTestDomain$Context' should be parsed as a valid identifier" (id == "model:MyTestDomain$Context")

  test "Representing the Domain" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        assert "The Domain should have the id 'MyTestDomain'" (id == "MyTestDomain")

  test "Role with attributes" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "thing : MyRole (mandatory, functional)\n" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{id}))) -> do
        assert "The Role should have the id 'MyRole'" (id == "MyRole")
      otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  thing : MyRole (not mandatory, not functional)" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a computed role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  use: sys for model:MyTestDomain\n  thing : MyRole = apicall \"ModellenM\" returns : sys:Modellen" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        logShow ctxt
        case head (filter (case _ of
            (RE _) -> true
            otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{roleParts}))) -> do
            case (head (filter (case _ of
                (Computation "ModellenM" "sys:Modellen") -> true
                otherwise -> false) roleParts)) of
              Nothing -> assert "There should be a computation RolePart" false
              otherwise -> assert "" true

          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a thing role and a user role and a context role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  thing : MyRole (not mandatory, not functional)\n  user: Gast (mandatory, functional)\n  context: Celebration (mandatory, functional)" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head (filter (case _ of
          (RE (RoleE{id, kindOfRole})) -> id == "MyRole" && kindOfRole == RoleInContext
          otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a thing role 'MyRole'." false
          otherwise -> pure unit
        case head (filter (case _ of
          (RE (RoleE{id, kindOfRole})) -> id == "Gast" && kindOfRole == UserRole
          otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a user role 'Gast'." false
          otherwise -> pure unit
        case head (filter (case _ of
          (RE (RoleE{id, kindOfRole})) -> id == "Celebration" && kindOfRole == ContextRole
          otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a context role 'Celebration'." false
          otherwise -> pure unit

  test "Domain with a thing, user and context role and an embedded case" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  thing : MyRole (not mandatory, not functional)\n  user: Gast (mandatory, functional)\n  context: Celebration (mandatory, functional)\n  case: Celebration" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head (filter (case _ of
          (CE (ContextE{id, kindOfContext})) -> id == "Celebration" && kindOfContext == Case
          otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a nested Case 'Celebration'." false
          otherwise -> pure unit

  test "A role commented out with '--' should be ignored" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  --thing : MyRole (not mandatory, not functional)\n  user: Gast (mandatory, functional)\n  context: Celebration (mandatory, functional)\n  case: Celebration" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head (filter (case _ of
          (RE (RoleE{id, kindOfRole})) -> id == "MyRole" && kindOfRole == RoleInContext
          otherwise -> false) contextParts) of
          Nothing -> pure unit
          otherwise -> assert "The Domain should NOT have a thing role 'MyRole'." false
        case head (filter (case _ of
          (CE (ContextE{id, kindOfContext})) -> id == "Celebration" && kindOfContext == Case
          otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a nested Case 'Celebration'." false
          otherwise -> pure unit

  test "Role that is mandatory" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "thing : MyRole (mandatory, not functional)\n" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{roleParts}))) -> case head roleParts of
        Nothing -> assert "The Role should have parts" false
        (Just (MandatoryAttribute b)) -> assert "The Role should have the attribute 'Mandatory' with value 'True'" b
        otherwise -> assert "The Role should have the attribute 'Mandatory'" false
      otherwise -> assert "Parsed an unexpected type" false

  test "Role that has a mandatory and a functional attribute" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "thing : MyRole (mandatory, not functional)\n" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        (assert "Role should have a Mandatory attribute"
          (isJust (findIndex (case _ of
            (MandatoryAttribute true) -> true
            otherwise -> false) roleParts)))
        (assert "Role should have a Functional attribute"
          (isJust (findIndex (case _ of
            (FunctionalAttribute false) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Role with a calculation" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "thing: MyRole = context >> Role\n  property: Prop (mandatory, functional, String)" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        (assert "Role should have a Calculation"
          (isJust (findIndex (case _ of
            (Calculation _) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Role that has two FilledBy attributes" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "thing : MyRole (mandatory, not functional) filledBy : Host, Guest\n" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        assert "Role should have two FilledBy attributes"
          (2 == (length (filter (case _ of
            (FilledByAttribute _) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "BotRole" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "bot : for MySelf" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> case (head (filter (case _ of
            (ForUser _) -> true
            otherwise -> false) roleParts)) of
          Nothing -> assert "BotRole should have a ForUser part." false
          Just (ForUser u) -> assert "BotRole should have ForUser part with value 'MySelf'" (u == "MySelf")
          otherwise -> assert "BotRole should have ForUser part" false
      otherwise -> assert "BotRole should have a ForUser part." false

  test "Property with functional and mandatory properties" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "property: MyProperty (mandatory, not functional, Number)" propertyE
    case r of
      (Left e) -> assert (show e) false
      (Right pr@(PE (PropertyE{propertyParts}))) -> do
        (assert "Property should have a Mandatory attribute"
          (isJust (findIndex (case _ of
            (MandatoryAttribute' true) -> true
            otherwise -> false) propertyParts)))
        (assert "Property should have a Functional attribute"
          (isJust (findIndex (case _ of
            (FunctionalAttribute' false) -> true
            otherwise -> false) propertyParts)))
      otherwise -> assert "Property should have parts" false

  test "Calculated property" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "property: MyProperty = context >> Role >> Prop" propertyE
    case r of
      (Left e) -> assert (show e) false
      (Right pr@(PE (PropertyE{propertyParts}))) -> do
        (assert "Property should have a calculation"
          (isJust (findIndex (case _ of
            (Calculation' _) -> true
            otherwise -> false) propertyParts)))
      otherwise -> assert "Property should have parts" false

  test "View with property references" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "view : MyView(P1, P2)" viewE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(VE (ViewE{id, viewParts}))) -> do
        assert "View should have name 'MyView'" (id == "MyView")
        (assert "View should have a Property 'P1'"
          (isJust (findIndex (case _ of
            "P1" -> true
            otherwise -> false) viewParts)))
        (assert "View should have a Property 'P2'"
          (isJust (findIndex (case _ of
            "P2" -> true
            otherwise -> false) viewParts)))
      otherwise -> assert "Property should have parts" false

  test "Perspective with ObjectRef, DefaultObjectView and two Actions" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "perspective on: MyObject (MyView): Consult, Change" perspectiveE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(PRE (PerspectiveE{id, perspectiveParts}))) -> do
        -- logShow pre
        (assert "The Perspective should have the object 'MyObject'"
          (isJust (findIndex (case _ of
            (Object _) -> true
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have the default object view 'MyView'"
          (isJust (findIndex (case _ of
            (DefaultView _) -> true
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Consult'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Consult
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Change'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Change
            otherwise -> false) perspectiveParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Perspective with just an ObjectRef" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "perspective on: MyObject" perspectiveE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(PRE (PerspectiveE{id, perspectiveParts}))) -> do
        -- logShow pre
        (assert "The Perspective should have the object 'MyObject'"
          (isJust (findIndex (case _ of
            (Object _) -> true
            otherwise -> false) perspectiveParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Perspective with just an ObjectRef and a defaultView" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "perspective on: MyObject (MyView)" perspectiveE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(PRE (PerspectiveE{id, perspectiveParts}))) -> do
        -- logShow pre
        (assert "The Perspective should have the object 'MyObject'"
          (isJust (findIndex (case _ of
            (Object _) -> true
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have the default object view 'MyView'"
          (isJust (findIndex (case _ of
            (DefaultView _) -> true
            otherwise -> false) perspectiveParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Perspective with just an ObjectRef and some otherwise unspecified Actions" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "perspective on: MyObject: Consult, Change" perspectiveE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(PRE (PerspectiveE{id, perspectiveParts}))) -> do
        -- logShow pre
        (assert "The Perspective should have the object 'MyObject'"
          (isJust (findIndex (case _ of
            (Object _) -> true
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Consult'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Consult
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Change'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Change
            otherwise -> false) perspectiveParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Action with its own View" do
    (r :: Either ParseError PerspectivePart) <- pure $ unwrap $ runIndentParser "Consult with ViewOnGuest" actionE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(Act (ActionE{verb, actionParts}))) -> do
        -- logShow pre
        assert "The Action should have the Verb 'Consult'" (verb == Consult)
        (assert "The Action should have a view with name 'ViewOnGuest'"
          (isJust (findIndex (case _ of
            (ObjectView v) -> v == "ViewOnGuest"
            otherwise -> false) actionParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Action with its own View and with a subjectView" do
    (r :: Either ParseError PerspectivePart) <- pure $ unwrap $ runIndentParser "Consult with ViewOnGuest\n  subjectView: AnotherView" actionE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(Act (ActionE{verb, actionParts}))) -> do
        -- logShow pre
        assert "The Action should have the Verb 'Consult'" (verb == Consult)
        (assert "The Action should have a view with name 'ViewOnGuest'"
          (isJust (findIndex (case _ of
            (ObjectView v) -> v == "ViewOnGuest"
            otherwise -> false) actionParts)))
        (assert "The Action should have a subjectView with name 'AnotherView'"
          (isJust (findIndex (case _ of
            (SubjectView v) -> v == "AnotherView"
            otherwise -> false) actionParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Action with its own View, with a subjectView and an indirectObject with a view." do
    (r :: Either ParseError PerspectivePart) <- pure $ unwrap $ runIndentParser "Consult with ViewOnGuest\n  subjectView: AnotherView\n  indirectObject: AnotherRole (ViewOnAnotherRole)" actionE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(Act (ActionE{verb, actionParts}))) -> do
        -- logShow pre
        assert "The Action should have the Verb 'Consult'" (verb == Consult)
        (assert "The Action should have a view with name 'ViewOnGuest'"
          (isJust (findIndex (case _ of
            (ObjectView v) -> v == "ViewOnGuest"
            otherwise -> false) actionParts)))
        (assert "The Action should have a subjectView with name 'AnotherView'"
          (isJust (findIndex (case _ of
            (SubjectView v) -> v == "AnotherView"
            otherwise -> false) actionParts)))
        (assert "The Action should have an indirectObject with name 'AnotherRole'"
          (isJust (findIndex (case _ of
            (IndirectObject v) -> v == "AnotherRole"
            otherwise -> false) actionParts)))
        (assert "The Action should have an IndirectObjectView with name 'ViewOnAnotherRole'"
          (isJust (findIndex (case _ of
            (IndirectObjectView v) -> v == "ViewOnAnotherRole"
            otherwise -> false) actionParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Action without a view and an indirectObject" do
    (r :: Either ParseError PerspectivePart) <- pure $ unwrap $ runIndentParser "Consult\n  indirectObject: AnotherRole" actionE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(Act (ActionE{verb, actionParts}))) -> do
        assert "The Action should have the Verb 'Consult'" (verb == Consult)
        (assert "The Action should have an indirectObject with name 'AnotherRole'"
          (isJust (findIndex (case _ of
            (IndirectObject v) -> v == "AnotherRole"
            otherwise -> false) actionParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Perspective with an ObjectRef, some otherwise unspecified Actions and a Delete with a subjectView and an indirect object with view" do
    (r :: Either ParseError RolePart) <- pure $ unwrap $ runIndentParser "perspective on: MyObject: Consult, Change\n  Delete with SomeView\n    subjectView: AnotherView\n    indirectObject: AnotherRole (ViewOnAnotherRole)" perspectiveE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(PRE (PerspectiveE{id, perspectiveParts}))) -> do
        -- logShow pre
        (assert "The Perspective should have the object 'MyObject'"
          (isJust (findIndex (case _ of
            (Object _) -> true
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Consult'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Consult
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Change'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Change
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have an action with Verb 'Delete'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb: v})) -> v == Delete
            otherwise -> false) perspectiveParts)))
        (assert "The action with Verb 'Delete' should have ObjectView 'SomeView'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb, actionParts})) -> verb == Delete && (isJust (findIndex (case _ of
              (ObjectView "SomeView") -> true
              otherwise -> false) actionParts))
            otherwise -> false) perspectiveParts)))
        (assert "The action with Verb 'Delete' should have IndirectObject 'AnotherRole'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb, actionParts})) -> verb == Delete && (isJust (findIndex (case _ of
              (IndirectObject "AnotherRole") -> true
              otherwise -> false) actionParts))
            otherwise -> false) perspectiveParts)))
        (assert "The action with Verb 'Delete' should have IndirectObjectView 'ViewOnAnotherRole'"
          (isJust (findIndex (case _ of
            (Act (ActionE {verb, actionParts})) -> verb == Delete && (isJust (findIndex (case _ of
              (IndirectObjectView "ViewOnAnotherRole") -> true
              otherwise -> false) actionParts))
            otherwise -> false) perspectiveParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Parse a file" do
    fileName <- pure "arcsyntax.arc"
    text <- liftEffect (readTextFile UTF8 (Path.concat [testDirectory, fileName]))
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser text domain
    case r of
      (Left e) -> assert (show e) false
      (Right dom) -> do
        logShow dom
        assert ("The file '" <> fileName <> "' does not parse") true

  test "Context with an aspect" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  aspect : pre:MyAspectRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Context should have an aspect 'pre:MyAspectRole'." false
          (Just (ContextAspect "pre:MyAspectRole" _)) -> pure unit
          otherwise -> assert "The Context should have an aspect 'pre:MyAspectRole'." false

  test "Role with an aspect" do
    (r :: Either ParseError ContextPart) <- pure $ unwrap $ runIndentParser "thing : MyRole (mandatory, functional)\n  aspect: pre:MyAspectRole" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> case (head (filter (case _ of
            (RoleAspect _ _) -> true
            otherwise -> false) roleParts)) of
          Nothing -> assert "Role should have a RoleAspect part." false
          Just (RoleAspect u _) -> assert "Role should have a RoleAspect part with value 'pre:MyAspectRole'" (u == "pre:MyAspectRole")
          otherwise -> assert "Role should have a RoleAspect part" false
      otherwise -> assert "Role should have a RoleAspect part" false

  test "Context with a use clause" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  use: sys for model:System$System" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Context should have a user clause." false
          (Just (PREFIX "sys" "model:System$System")) -> pure unit
          otherwise -> assert "The Context should have a use clause: 'use: sys model:System$System'." false

  test "Well-formedness of a use clause" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  use: sys for System$System" domain
    case r of
      (Left (ParseError _ pos)) -> assert "The error should be situated at (2, 16)" (unsafeCoerce pos == ArcPosition {line: 2, column: 16})
      otherwise -> assert "The modelname is not well-formed and that should be detected" false
