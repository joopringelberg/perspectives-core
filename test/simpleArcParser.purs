module Test.Parsing.Arc.Simple where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.List (findIndex, head, filter, length)
import Data.Maybe (Maybe(..), isJust)
import Effect.Class.Console (logShow)
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Simple (actionE, domain, perspectiveE, propertyE, roleE, viewE)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.Simple" do
  test "Representing the Domain" do
    (r :: Either ParseError ContextE) <- runP $ runIndentParser "Context : Domain : MyTestDomain\n" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        assert "The Domain should have the id 'MyTestDomain'" (id == "MyTestDomain")

  test "Role without parts" do
    (r :: Either ParseError ContextPart) <- runP $ runIndentParser "Role : Role : MyRole\n" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{id}))) -> do
        assert "The Role should have the id 'MyRole'" (id == "MyRole")
      otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a role" do
    (r :: Either ParseError ContextE) <- runP $ runIndentParser "Context : Domain : MyTestDomain\n  Role : Role : MyRole\n" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false

  test "Role that is mandatory" do
    (r :: Either ParseError ContextPart) <- runP $ runIndentParser "Role : Role : MyRole\n  Mandatory : True" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{roleParts}))) -> case head roleParts of
        Nothing -> assert "The Role should have parts" false
        (Just (MandatoryAttribute b)) -> assert "The Role should have the attribute 'Mandatory' with value 'True'" b
        otherwise -> assert "The Role should have the attribute 'Mandatory'" false
      otherwise -> assert "Parsed an unexpected type" false

  test "Role that has a mandatory and a functional attribute" do
    (r :: Either ParseError ContextPart) <- runP $ runIndentParser "Role : Role : MyRole\n  Mandatory : True\n  NonFunctional : False" roleE
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

  test "Role that has two FilledBy attributes" do
    (r :: Either ParseError ContextPart) <- runP $ runIndentParser "Role : Role : MyRole\n  Mandatory : True\n  NonFunctional : False\n  FilledBy : Host\n  FilledBy : Guest" roleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        assert "Role should have two FilledBy attributes"
          (2 == (length (filter (case _ of
            (FilledByAttribute _) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Property without parts" do
    (r :: Either ParseError RolePart) <- runP $ runIndentParser "Property : NumberProperty : MyProperty\n" propertyE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(PE (PropertyE{id, range}))) -> do
        assert "The Property should have the id 'MyProperty'" (id == "MyProperty")
        assert "The Property should have the range PNumber'" (range == PNumber)
      otherwise -> assert "Parsed an unexpected type" false

  test "Property with functional and mandatory properties" do
    (r :: Either ParseError RolePart) <- runP $ runIndentParser "Property : NumberProperty : MyProperty\n  Mandatory : True\n  NonFunctional : False" propertyE
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

  test "View with property references" do
    (r :: Either ParseError RolePart) <- runP $ runIndentParser "View : View : MyView\n  Property : PropertyRef : P1\n  Property : PropertyRef : P2" viewE
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

  test "Perspective without actions" do
    (r :: Either ParseError RolePart) <- runP $ runIndentParser "Perspective : Perspective : MyPerspective\n  Object : MyObject\n  View : DefaultObjectViewRef : MyView" perspectiveE
    case r of
      (Left e) -> assert (show e) false
      (Right pre@(PRE (PerspectiveE{id, perspectiveParts}))) -> do
        assert "The Perspective should have the id 'MyPerspective'" (id == "MyPerspective")
        (assert "The Perspective should have the object 'MyObject'"
          (isJust (findIndex (case _ of
            (Object _) -> true
            otherwise -> false) perspectiveParts)))
        (assert "The Perspective should have the default object view 'MyView'"
          (isJust (findIndex (case _ of
            (DefaultView _) -> true
            otherwise -> false) perspectiveParts)))
      otherwise -> assert "Parsed an unexpected type" false

  test "Action without parts" do
    (r :: Either ParseError PerspectivePart) <- runP $ runIndentParser "Action : Consults : MyAction\n" actionE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(Act (ActionE{id, verb}))) -> do
        assert "The Action should have the id 'MyAction'" (id == "MyAction")
        assert "The Action should have the verb 'Consults''" (verb == "Consults")
      otherwise -> assert "Parsed an unexpected type" false

  test "Action with indirect object and objectview" do
    (r :: Either ParseError PerspectivePart) <- runP $ runIndentParser "Action : Consults : MyAction\n  IndirectObjectRef : SomeObjectRef\n  View : ObjectViewRef : SomeObjectViewRef" actionE
    case r of
      (Left e) -> assert (show e) false
      (Right act@(Act (ActionE{id, actionParts}))) -> do
        (assert "The Action should have the indirect object view 'SomeObjectRef'"
          (isJust (findIndex (case _ of
            (IndirectObject "SomeObjectRef") -> true
            otherwise -> false) actionParts)))
        (assert "The Action should have the object view 'SomeObjectViewRef'"
          (isJust (findIndex (case _ of
            (ObjectView "SomeObjectViewRef") -> true
            otherwise -> false) actionParts)))
      otherwise -> assert "Parsed an unexpected type" false
