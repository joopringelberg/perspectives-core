module Test.Parsing.Arc where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Error.Class (try)
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.List (List(..), filter, findIndex, head, index, length, (:))
import Data.List.NonEmpty (length) as LNE
import Data.Maybe (Maybe(..), isJust)
import Effect.Class.Console (log, logShow)
import Effect.Exception (message)
import Node.Encoding as ENC
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Perspectives.Parsing.Arc (automaticEffectE, contextE, domain, propertyE, thingRoleE, userRoleE, viewE)
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..), FilledBySpecification(..), PropertyE(..), PropertyPart(..), PropsOrView(..), RoleE(..), RoleIdentification(..), RolePart(..), StateQualifiedPart, StateSpecification(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier)
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), RoleKind(..))
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerbList(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..)) as RV
import Test.Parsing.ArcAstSelectors (actionExists, allPropertyVerbs, ensureAction, ensureContext, ensureOnEntry, ensureOnExit, ensurePerspectiveOf, ensurePerspectiveOn, ensurePropertyVerbsForPropsOrView, ensureRoleVerbs, ensureStateInContext, ensureStateInRole, ensureSubState, ensureUserRole, failure, hasAutomaticAction, isImplicitRoleOnIdentifier, isIndexed, isNotified, isStateWithContext, isStateWithExplicitRole, isStateWithExplicitRole_, perspectiveExists, stateExists, stateParts)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Parsing (ParseError(..))
import Unsafe.Coerce (unsafeCoerce)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test" 

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc" do

  --------------------------------------------------------------------------------
  ---- ARCIDENTIFIER
  --------------------------------------------------------------------------------

  test "arcIdentifier on simple name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "MyTestDomain" arcIdentifier
    case r of
      (Left e) -> assert (show e) false 
      (Right id) -> do
        assert "'MyTestDomain' should be parsed as a valid identifier" (id == "MyTestDomain")

  test "arcIdentifier on uncapitalised name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "myTestDomain" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        assert "'MyTestDomain' should be parsed as a valid identifier" (id == "MyTestDomain")

  test "arcIdentifier on segmented name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "MyTestDomain$Context" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        -- logShow id
        assert "'MyTestDomain$Context' should be parsed as a valid identifier" (id == "MyTestDomain$Context")

  test "arcIdentifier on prefixed name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "pre:MyTestDomain" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        assert "'pre:MyTestDomain' should be parsed as a valid identifier" (id == "pre:MyTestDomain")

  test "arcIdentifier on prefixed segmented name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "pre:MyTestDomain$Context" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        -- logShow id
        assert "'pre:MyTestDomain$Context' should be parsed as a valid identifier" (id == "pre:MyTestDomain$Context")

  test "arcIdentifier on prefixed double segmented name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "pre:MyTestDomain$Context$Role" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        -- logShow id
        assert "'pre:MyTestDomain$Context$Role' should be parsed as a valid identifier" (id == "pre:MyTestDomain$Context$Role")

  test "arcIdentifier on fully qualified name" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "model:MyTestDomain$Context" arcIdentifier
    case r of
      (Left e) -> assert (show e) true
      (Right id) -> do
        -- logShow id
        assert "'model:MyTestDomain$Context' should be parsed as a valid identifier" (id == "model:MyTestDomain$Context")


  test "reservedIdentifier, succeeding" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "entry" reservedIdentifier
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        assert "'entry' should be parsed as a valid reserved identifier" (id == "entry")

  test "reservedIdentifier, failing" do
    (r :: Either ParseError String) <- {-pure $ unwrap $-} runIndentParser "cheese" reservedIdentifier
    case r of
      (Left e@(ParseError m _)) -> assert (show e) (m == "not a reserved word \"cheese\"(or unexpected end of input)")
      (Right id) -> do
        assert "'cheese' should not be parsed as a valid reserved identifier" true

  --------------------------------------------------------------------------------
  ---- ROLE
  --------------------------------------------------------------------------------
  test "Role withOUT attributes" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole\n" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{id}))) -> do
        assert "The Role should have the id 'MyRole'" (id == "MyRole")
      otherwise -> assert "Parsed an unexpected type" false

  test "roleE on an empty string" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "" thingRoleE
    case r of
      (Left e) -> pure unit
      otherwise -> assert "roleE on an empty string should fail" false

  test "roleE on a role with attributes" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole (mandatory)\n" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{id}))) -> do
        assert "The Role should have the id 'MyRole'" (id == "MyRole")
      otherwise -> assert "Parsed an unexpected type" false

  test "Role that is mandatory" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole (mandatory)\n" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(RE (RoleE{roleParts}))) -> case head roleParts of
        Nothing -> assert "The Role should have parts" false
        (Just (MandatoryAttribute b)) -> assert "The Role should have the attribute 'Mandatory' with value 'True'" b
        otherwise -> assert "The Role should have the attribute 'Mandatory'" false
      otherwise -> assert "Parsed an unexpected type" false

  test "Role that has a mandatory and a relational attribute" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole (mandatory, relational)\n" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        (assert "Role should have a Mandatory attribute"
          (isJust (findIndex (case _ of
            (MandatoryAttribute true) -> true
            otherwise -> false) roleParts)))
        (assert "Role should have a Relational attribute"
          (isJust (findIndex (case _ of
            (FunctionalAttribute false) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Role with a calculation" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole = context >> Role\n  property Prop (mandatory, String)" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        (assert "Role should have a Calculation"
          (isJust (findIndex (case _ of
            (Calculation _ _) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Role that has two FilledBy attributes" do 
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole (mandatory) filledBy Host, Guest\n" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
        assert "Role should have two FilledBy attributes"
          (1 == (length (filter (case _ of 
            FilledBySpecifications spec -> (case spec of
              (Alternatives atts) -> LNE.length atts == 2
              otherwise -> false)
            _ -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Role with two properties" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole (mandatory) filledBy Host, Guest\n  property Prop1 (mandatory, Number)\n  property Prop2" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> do
          -- logShow rl
        assert "Role should have two properties"
          (2 == (length (filter (case _ of
            (PE _) -> true
            otherwise -> false) roleParts)))
      otherwise -> assert "Role should have parts" false

  test "Perspective with ObjectRef, DefaultObjectView and two Actions" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "user MyRole\n  perspective on MyObject\n      only (Remove, Create)\n      view MyView (Consult)" userRoleE
    case r of
      Left e -> assert (show e) false
      Right (RE rl@(RoleE rr)) -> do
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "MyObject" >>=
            ensureRoleVerbs (Including [RV.Remove, RV.Create])
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "MyObject" >>=
            ensurePropertyVerbsForPropsOrView [Consult] (View "MyView")
      _ -> assert "There should be a PRE RoleE" false

  test "Perspective on External" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "user MyRole\n  perspective on External\n    all roleverbs" userRoleE
    case r of
      (Left e) -> assert (show e) false
      Right (RE rl) ->
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "External" >>= perspectiveExists
      otherwise -> assert "Parsed an unexpected type" false

  test "Perspective with just an ObjectRef and a defaultView" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "user MyRole\n  perspective on MyObject\n    view MyView" userRoleE
    case r of
      (Left e) -> assert (show e) false
      Right (RE rl) ->
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "MyObject" >>=
            ensurePropertyVerbsForPropsOrView allPropertyVerbs (View "MyView")
      otherwise -> assert "Parsed an unexpected type" false

  test "Perspective with just an ObjectRef and some otherwise unspecified Actions" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "user MyRole\n  perspective on MyObject\n    only (Remove, Create)" userRoleE
    case r of
      (Left e) -> assert (show e) false
      Right (RE rl) ->
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "MyObject" >>=
            ensureRoleVerbs (Including [RV.Remove, RV.Create])
      otherwise -> assert "Parsed an unexpected type" false

  test "Two properties, each with its own verbs" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "user MyRole\n  perspective on MyObject\n    props (MyFirstProp) (AddPropertyValue)\n    props (MySecondProp) (RemovePropertyValue)" userRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right (RE rl)) -> do
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "MyObject" >>=
            ensurePropertyVerbsForPropsOrView [RV.AddPropertyValue] (Properties ("MyFirstProp" : Nil))
        ensureStateInRole (isStateWithContext "model:") rl >>=
          ensurePerspectiveOn "MyObject" >>=
            ensurePropertyVerbsForPropsOrView [RV.RemovePropertyValue] (Properties ("MySecondProp" : Nil))
      otherwise -> assert "Parsed an unexpected type" false

  test "Role with an aspect" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "thing MyRole (mandatory)\n  aspect pre:MyAspectRole" thingRoleE
    case r of
      (Left e) -> assert (show e) false
      (Right rl@(RE (RoleE{roleParts}))) -> case (head (filter (case _ of
            (RoleAspect _ _ _) -> true
            otherwise -> false) roleParts)) of
          Nothing -> assert "Role should have a RoleAspect part." false
          Just (RoleAspect u _ _) -> assert "Role should have a RoleAspect part with value 'pre:MyAspectRole'" (u == "pre:MyAspectRole")
          otherwise -> assert "Role should have a RoleAspect part" false
      otherwise -> assert "Role should have a RoleAspect part" false

  --------------------------------------------------------------------------------
  ---- PERSPECTIVE
  --------------------------------------------------------------------------------
  test "Perspective with roleverbs" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Feest\n  user GoedeGast = filter Gast with WellBehaved\n    perspective on Gast >> binding\n      only (Create)" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom -> do
        ensureUserRole "GoedeGast" dom >>=
          ensureStateInRole (isStateWithExplicitRole "model:Feest$GoedeGast") >>=
            perspectiveExists

  --------------------------------------------------------------------------------
  ---- DOMAIN
  --------------------------------------------------------------------------------
  test "Representing the Domain" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        assert "The Domain should have the id 'MyTestDomain'" (id == "MyTestDomain")

  test "domain on empty string" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "" domain
    case r of
      (Left e) -> pure unit
      otherwise -> assert "contextE on an empty string should fail" false

  test "Domain with a mandatory and functional role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole (mandatory)" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a mandatory role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a role without attributes" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with two thing roles" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole\n  thing AnotherRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a user role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  user MyRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'UserRole'" (kindOfRole == UserRole)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a thing role and context role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole\n  context MyContext" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyRole'" (id == "MyRole")
            assert "The role should have kind 'RoleInContext'" (kindOfRole == RoleInContext)
          otherwise -> assert "Parsed an unexpected type" false
        case index contextParts 1 of
          Nothing -> assert "The Domain should have two roles." false
          (Just (RE (RoleE{id, kindOfRole}))) -> do
            assert "The role should have the id 'MyContext'" (id == "MyContext")
            assert "The role should have kind 'ContextRole'" (kindOfRole == ContextRole)
          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a computed role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use sys for model:MyTestDomain\n  use cdb for model:Couchdb\n  thing MyRole = callExternal cdb:Models() returns Model$External" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        -- logShow ctxt
        case head (filter (case _ of
            (RE _) -> true
            otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a role." false
          (Just (RE (RoleE{roleParts}))) -> do
            case (head (filter (case _ of
                (Calculation _ _) -> true
                otherwise -> false) roleParts)) of
              Nothing -> assert "There should be a computation RolePart" false
              otherwise -> assert "" true

          otherwise -> assert "Parsed an unexpected type" false

  test "Domain with a thing role and a user role and a context role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole\n  user Gast (mandatory)\n  context Celebration (mandatory)" domain
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
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole\n  user Gast (mandatory)\n  context Celebration (mandatory)\n  case Celebration" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head (filter (case _ of
          (CE (ContextE{id, kindOfContext})) -> id == "Celebration" && kindOfContext == Case
          otherwise -> false) contextParts) of
          Nothing -> assert "The Domain should have a nested Case 'Celebration'." false
          otherwise -> pure unit

  test "A role commented out with '--' should be ignored" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  --thing MyRole\n  user Gast (mandatory)\n  context Celebration (mandatory)\n  case Celebration" domain
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

  test "Context with an aspect" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  aspect pre:MyAspectRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Context should have an aspect 'pre:MyAspectRole'." false
          (Just (ContextAspect "pre:MyAspectRole" _)) -> pure unit
          otherwise -> assert "The Context should have an aspect 'pre:MyAspectRole'." false

  test "Context with a use clause" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use sys for model:System$System" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{contextParts})) -> do
        case head contextParts of
          Nothing -> assert "The Context should have a user clause." false
          (Just (PREFIX "sys" "model:System$System")) -> pure unit
          otherwise -> assert "The Context should have a use clause 'use sys model:System$System'." false

  test "Well-formedness of a use clause" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use sys for System$System" domain
    case r of
      (Left (ParseError _ pos)) -> assert "The error should be situated at (2, 16)" (unsafeCoerce pos == ArcPosition {line: 2, column: 16})
      otherwise -> assert "The modelname is not well-formed and that should be detected" false

  --------------------------------------------------------------------------------
  ---- CONTEXT
  --------------------------------------------------------------------------------
  test "Context with indexed clause" do
    (r :: Either ParseError ContextPart) <- {-pure $ unwrap $-} runIndentParser "domain MyContext\n  indexed IndexedName" contextE
    case r of
      (Left e) -> assert (show e) false
      Right (CE ctxt) -> isIndexed "IndexedName" ctxt
      _ -> failure "Expected a ContextPart that is a ContextE"

  --------------------------------------------------------------------------------
  ---- STATE
  --------------------------------------------------------------------------------
  test "Domain with a context with a state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = SomeRole >> SomeProp > 10\n        perspective on SomeRole\n            perspective of SomeUser\n                all roleverbs" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>= stateExists

  test "Domain with a Context with a state with a substate" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = SomeRole >> SomeProp > 10\n      state NestedState = true\n        perspective on SomeRole\n          perspective of SomeUser\n            all roleverbs" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>=
            ensureSubState (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState$NestedState")) >>= stateExists

  test "Domain with a context with a state with perspective on in perspective of" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = SomeRole >> SomeProp > 10\n      perspective of SomeUser\n        perspective on SomeRole\n          all roleverbs" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>=
            stateParts >>=
              ensurePerspectiveOf (isImplicitRoleOnIdentifier "SomeUser") >>=
                ensurePerspectiveOn "SomeRole" >>=
                  perspectiveExists

  test "Domain with a context with a state with perspective of in perspective on" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = SomeRole >> SomeProp > 10\n      perspective on SomeRole\n        perspective of SomeUser\n          all roleverbs" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>=
            stateParts >>=
              ensurePerspectiveOf (isImplicitRoleOnIdentifier "SomeUser") >>=
                ensurePerspectiveOn "SomeRole" >>=
                  perspectiveExists

  test "Fail roleVerbs without subject" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = SomeRole >> SomeProp > 10\n      perspective on SomeRole\n        perspective on AnotherRole\n          all roleverbs" domain
    case r of
      (Left e@(ParseError m _)) -> assert (show e) (m == "User role must be given")
      -- Right dom -> assert ("A subject should have been given.") false
      Right dom -> do
        logShow dom
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>=
            stateExists

  test "A state with an on entry and notification" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = true\n      on entry\n        notify SomeUser \"Hello {SomeUser >> FirstName}!\"\n  case AnotherCase" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>=
            stateParts >>=
              ensureOnEntry >>=
                isNotified "SomeUser"

  test "on entry directly nested in role" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    user Self\n      on entry\n        notify SomeUser \"Hello {SomeUser >> FirstName}!\"\n  case AnotherCase" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureUserRole "Self" >>=
            ensureStateInRole (isStateWithExplicitRole_ "model:MyTestDomain$MyCase$Self" Nothing) >>=
              ensureOnEntry >>=
                isNotified "SomeUser"

  test "on entry directly nested in role with assignment" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    user Self\n      on entry\n        do\n          Prop1 = false for ARole\n  case AnotherCase" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureUserRole "Self" >>=
            ensureStateInRole (isStateWithExplicitRole_ "model:MyTestDomain$MyCase$Self" Nothing) >>=
              ensureOnEntry >>=
                hasAutomaticAction

  test "Failing: A state with an on exit and automatic action, no user" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = true\n      on exit\n        do\n          remove SomeRole" domain
    case r of
      (Left e@(ParseError m _)) -> assert ("Should fail with 'A subject is required', but fails with " <> show e) (m == "A subject is required")
      Right dom -> assert "automatic action should fail because subject is missing." false

  test "A state with an on exit and automatic action" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    state SomeState = true\n      on exit\n        do for SomeUser\n          remove SomeRole" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom ->
        ensureContext "MyCase" dom >>=
          ensureStateInContext (ContextState (ContextType "model:MyTestDomain$MyCase") (Just "SomeState")) >>=
            stateParts >>=
              ensureOnExit >>=
                hasAutomaticAction

  test "automaticEffectE" do
    (r :: Either ParseError (List StateQualifiedPart)) <- {-pure $ unwrap $-} runIndentParser "do\n  remove SomeRole" automaticEffectE
    case r of
      (Left e) -> pure unit
      Right dom -> assert "Should fail because no subject (user role) is provided." false

  --------------------------------------------------------------------------------
  ---- IN STATE
  --------------------------------------------------------------------------------

  test "Role with perspective in a state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    user SomeUser\n      in state SomeState\n        perspective on SomeRole\n          all roleverbs" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom -> do
        ensureContext "MyCase" dom >>=
          ensureUserRole "SomeUser" >>=
            ensureStateInRole (isStateWithExplicitRole_ "model:MyTestDomain$MyCase$SomeUser" (Just "SomeState")) >>=
              ensurePerspectiveOn "SomeRole" >>=
                perspectiveExists
        try (ensureContext "MyCase" dom >>=
          ensureUserRole "SomeUser" >>=
            ensureStateInRole (isStateWithExplicitRole_ "model:MyTestDomain$MyCase$SomeUser" Nothing) >>=
              ensurePerspectiveOn "SomeRole" >>=
                perspectiveExists) >>= case _ of
                  Left e -> assert ("Expected another error than: " <> message e) (message e == "No perspective on '(Simple (ArcIdentifier (ArcPosition { column: 0, line: 0 }) \"SomeRole\"))'.")
                  Right _ -> failure "There should be no perspective on SomeRole in the root state of MyCase"

  --------------------------------------------------------------------------------
  ---- IN STATE
  --------------------------------------------------------------------------------
  test "Role with action in a state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    user SomeUser\n      in state SomeState\n        perspective on SomeRole\n          action MyAction\n            remove MyRole" domain
    case r of
      (Left e) -> assert (show e) false
      Right dom -> do
        ensureContext "MyCase" dom >>=
          ensureUserRole "SomeUser" >>=
            ensureStateInRole (isStateWithExplicitRole_ "model:MyTestDomain$MyCase$SomeUser" (Just "SomeState")) >>=
              ensurePerspectiveOn "SomeRole" >>=
                ensureAction "MyAction" >>=
                  actionExists

  --------------------------------------------------------------------------------
  ---- PROPERTY
  --------------------------------------------------------------------------------
  test "Property with functional and mandatory attributes" do
      (r :: Either ParseError RolePart) <- {-pure $ unwrap $-} runIndentParser "property MyProperty (mandatory, Number)" propertyE
      case r of
        (Left e) -> assert (show e) false
        (Right pr@(PE (PropertyE{propertyParts}))) -> do
          (assert "Property should have a Mandatory attribute"
            (isJust (findIndex (case _ of
              (MandatoryAttribute' true) -> true
              otherwise -> false) propertyParts)))
          -- (assert "Property should have a Functional attribute"
          --   (isJust (findIndex (case _ of
          --     (FunctionalAttribute' false) -> true
          --     otherwise -> false) propertyParts)))
        otherwise -> assert "Property should have parts" false

  test "Calculated property" do
    (r :: Either ParseError RolePart) <- {-pure $ unwrap $-} runIndentParser "property MyProperty = context >> Role >> Prop" propertyE
    case r of
      (Left e) -> assert (show e) false
      (Right pr@(PE (PropertyE{propertyParts}))) -> do
        (assert "Property should have a calculation"
          (isJust (findIndex (case _ of
            (Calculation' _ _) -> true
            otherwise -> false) propertyParts)))
      otherwise -> assert "Property should have parts" false

  test "Calculated property with DateTime" do
    (r :: Either ParseError RolePart) <- {-pure $ unwrap $-} runIndentParser "property MyProperty = Datum > '1995-12-17'\n-- commentaar" propertyE
    case r of
      (Left e) -> assert (show e) false
      (Right pr@(PE (PropertyE{propertyParts}))) -> do
        (assert "Property should have a calculation"
          (isJust (findIndex (case _ of
            (Calculation' _ _) -> true
            otherwise -> false) propertyParts)))
      otherwise -> assert "Property should have parts" false

--------------------------------------------------------------------------------
---- VIEW
--------------------------------------------------------------------------------
  test "View with property references" do
    (r :: Either ParseError RolePart) <- {-pure $ unwrap $-} runIndentParser "view MyView(P1, P2)" viewE
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

  test "Parse a file" do
    fileName <- pure "arcsyntax.arc"
    text <- (readTextFile ENC.UTF8 (Path.concat [testDirectory, fileName]))
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser text domain
    case r of
      (Left e) -> assert (show e) false
      (Right dom) -> do
        -- logShow dom
        assert ("The file '" <> fileName <> "' does not parse") true
