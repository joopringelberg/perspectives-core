module Test.Parsing.Arc.PhaseTwo where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Lens (_Just, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import Foreign.Object (lookup)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwo (evalPhaseTwo, traverseDomain)
import Perspectives.Parsing.Arc.Simple (domain)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Verb(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..), Range(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType(..))
import Perspectives.Representation.View (View(..))
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.PhaseTwo" do
  test "Representing the Domain and a context with subcontext and role." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- (DomeinFile dr) <- pure defaultDomeinFile
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            -- logShow context
            assert "The DomeinFile should have context 'model:MyTestDomain'."
              (isJust (lookup "model:MyTestDomain" dr'.contexts))
            assert "The DomeinFile should have context 'model:MyTestDomain$MyCase'."
              (isJust (lookup "model:MyTestDomain$MyCase" dr'.contexts))
            assert "The DomeinFile should have role 'model:MyTestDomain$MyCase$MyRoleInContext'."
              (isJust (lookup "model:MyTestDomain$MyCase$MyRoleInContext" dr'.enumeratedRoles))
            assert ("The Model should have MyCase as one of its sub-contexts")
              ((Just $ ContextType "model:MyTestDomain$MyCase") == (preview (prop (SProxy :: SProxy "contexts") <<< at "model:MyTestDomain" <<< traversed <<< _Newtype <<< (prop (SProxy :: SProxy "nestedContexts")) <<< ix 0) dr'))
            assert "'MyCase' should have 'MyRoleInContext' as a role in context"
              ((Just $ ENR $ EnumeratedRoleType "model:MyTestDomain$MyCase$MyRoleInContext") == (preview (prop (SProxy :: SProxy "contexts") <<< at "model:MyTestDomain$MyCase" <<< traversed <<< _Newtype <<< (prop (SProxy :: SProxy "rolInContext")) <<< ix 0) dr'))
            assert "The Domain should have the id 'MyTestDomain'" (id == "MyTestDomain")

  test "A Context with a CalculatedRole and a position." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      Calculation : some calculation" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The role MyRoleInContext should be a calculated role."
              (isJust (lookup "model:MyTestDomain$MyCase$MyRoleInContext" dr'.calculatedRoles))

  test "A role with a view" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      View : View : MyView\n        PropertyRef : MyProp" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the view 'MyView'" (isJust (lookup "model:MyTestDomain$MyCase$MyRoleInContext$MyView" dr'.views))
            assert "MyRoleInContext should have the view MyView"
              case (lookup "model:MyTestDomain$MyCase$MyRoleInContext" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {views})) -> case (head views) of
                  Nothing -> false
                  (Just (ViewType v)) -> v == "model:MyTestDomain$MyCase$MyRoleInContext$MyView"

  test "A role with attributes" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Functional : False\n    Mandatory : True" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the role 'model:MyTestDomain$MyRoleInContext' with\
            \ attribute 'functional' equal to false"
              case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {functional, mandatory})) -> functional == false && mandatory == true

  test "A role with binding" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    FilledBy : model:MyTestDomain$MyOtherRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the role 'model:MyTestDomain$MyRoleInContext' that is\
            \ filled with 'ST EnumeratedRoleType model:MyTestDomain$MyOtherRole'"
              case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {binding})) -> binding == (ST $ EnumeratedRoleType "model:MyTestDomain$MyOtherRole")

  test "Role has context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    FilledBy : model:MyTestDomain$MyOtherRole" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The role 'model:MyTestDomain$MyRoleInContext' should have context 'model:MyTestDomain'"
              case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {context: cont})) -> cont == ContextType "model:MyTestDomain"

  test "Domain has no context, a context does" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The domain 'model:MyTestDomain' should have no context."
              case (lookup "model:MyTestDomain" dr'.contexts) of
                Nothing -> false
                (Just (Context {context: cont})) -> cont == Nothing
            assert "The context 'model:MyTestDomain$MyCase' should have context 'model:MyTestDomain'"
              case (lookup "model:MyTestDomain$MyCase" dr'.contexts) of
                Nothing -> false
                (Just (Context {context: cont})) -> cont == Just (ContextType "model:MyTestDomain")

  test "A role with a Number property with attributes 'Functional' and 'Mandatory'" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Property : StringProperty : MyProp\n      Functional : False\n      Mandatory: False" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the property 'MyProp'" (isJust (lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties))
            assert "MyRoleInContext should have the property MyProp"
              case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {properties})) -> case (head properties) of
                  Nothing -> false
                  (Just (ENP (EnumeratedPropertyType v))) -> v == "model:MyTestDomain$MyRoleInContext$MyProp"
                  otherwise -> false
            assert "The property 'MyProp' should have attribute 'Functional' False and 'Mandatory' False."
              case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties of
                Nothing -> false
                (Just (EnumeratedProperty {functional, mandatory})) -> functional == false && mandatory == false
            assert "The property 'MyProp' should have 'PString' as 'range'."
              case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties of
                Nothing -> false
                (Just (EnumeratedProperty {range})) -> range == PString

  test "A role with a CalculatedProperty" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Property : StringProperty : MyProp\n      Calculation : prop1 prop2" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the property 'MyProp'" (isJust (lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.calculatedProperties))
            assert "MyRoleInContext should have the property MyProp"
              case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {properties})) -> case (head properties) of
                  Nothing -> false
                  (Just (CP (CalculatedPropertyType v))) -> v == "model:MyTestDomain$MyRoleInContext$MyProp"
                  otherwise -> false
            assert "The property 'MyProp' should have a calculation"
              case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.calculatedProperties of
                Nothing -> false
                -- TODO: check the actual calculation.
                (Just (CalculatedProperty {calculation})) -> true

  test "Parse a file and pass phase two over it" do
    fileName <- pure "parsetest1.arc"
    text <- liftEffect (readTextFile UTF8 (Path.concat [testDirectory, fileName]))
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser text domain
    case r of
      (Left e) -> assert (show e) false
      (Right dom) -> do
        -- logShow dom

        case evalPhaseTwo (traverseDomain dom "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert ("The file '" <> fileName <> "' does not parse") true

  test "An Agent role for a Bot without a ForUser clause" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left (e :: PerspectivesError)) -> do
            -- logShow e
            assert (show e) true
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "A BotRole should specify the user it represents with a 'ForUser' clause." false

  test "An Agent role for a Bot adds clauses to its represented user" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) true
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The BotRole for user 'MySelf' should give rise to an enumeratedRole 'model:MyTestDomain$MySelf'"
              case lookup "model:MyTestDomain$MySelf" dr'.enumeratedRoles of
                Nothing -> false
                (Just (EnumeratedRole{_id})) -> _id == EnumeratedRoleType "model:MyTestDomain$MySelf"

  test "Properties of a UserRole and its BotRole are integrated into the UserRole" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Property : StringProperty : NickName\n  Agent : UserRole : MySelf\n    Property : BooleanProperty : Happy" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) true
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The enumeratedRole 'model:MyTestDomain$MySelf' should have the properties 'NickName' and 'Happy'"
              case lookup "model:MyTestDomain$MySelf" dr'.enumeratedRoles of
                Nothing -> false
                (Just (EnumeratedRole{properties})) -> length properties == 2

  test "Types should have positions in their definining texts." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Property : StringProperty : NickName\n    Property : BooleanProperty : Happy\n      Calculation : prop1\n    View : View : MyView\n  Role : RoleInContext : MyRoleInContext\n    Calculation : prop1 prop2" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) true
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            context <- pure $ unsafePartial $ fromJust $ lookup "model:MyTestDomain" dr'.contexts
            assert "The Domain should be on position (1, 20)"
              (case context of (Context{pos}) -> pos == ArcPosition {line: 1, column: 20})
            assert "The role MySelf should have position (2, 21)"
              case (lookup "model:MyTestDomain$MySelf" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole{pos})) -> pos == ArcPosition({line: 2, column: 21})
            assert "The role MyRoleInContext should have position (8,26)"
              case (lookup "model:MyTestDomain$MyRoleInContext" dr'.calculatedRoles) of
                Nothing -> false
                (Just (CalculatedRole{pos})) -> pos == ArcPosition({line: 8, column: 26})
            assert "'MyView' should have position (7, 19)"
              case (lookup "model:MyTestDomain$MySelf$MyView" dr'.views) of
                Nothing -> false
                (Just (View{pos})) -> pos == ArcPosition{line: 7, column: 19}
            assert "Property 'Nickname' should have position (4, 33)"
              case (lookup "model:MyTestDomain$MySelf$NickName" dr'.enumeratedProperties) of
                Nothing -> false
                (Just (EnumeratedProperty{pos})) -> pos == ArcPosition{line: 4, column: 33}
            assert "Property 'Happy' should have position (5, 34)"
              case (lookup "model:MyTestDomain$MySelf$Happy" dr'.calculatedProperties) of
                Nothing -> false
                (Just (CalculatedProperty{pos})) -> pos == ArcPosition{line: 5, column: 34}

  testOnly "Actions" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultsAnotherRole\n      View : DefaultObjectViewRef : ViewOpWens\n      Action : Change : ChangeAnotherRole\n        View : ObjectViewRef : AnotherView" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) true
          (Right (DomeinFile dr')) -> do
            logShow dr'
            assert "An Action in a Perspective should end up in the DomeinFile."
              (isJust (lookup "model:MyTestDomain$MySelf$ConsultsAnotherRole" dr'.actions))
            assert "The Role MySelf should have a Perspective on 'model:MyTestDomain$AnotherRole' that includes the Action 'model:MyTestDomain$MySelf$ConsultsAnotherRole'."
              (let
                -- _p :: Traversal' DomeinFileRecord ActionType)
                -- NOTE: the index at the end of the Traversal' is very dependent on the order of the Actions!
                _p = prop (SProxy :: (SProxy "enumeratedRoles")) <<< at "model:MyTestDomain$MySelf" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "perspectives")) <<< at "model:MyTestDomain$AnotherRole" <<< traversed <<< ix 1
                in case (preview _p dr') of
                  (Just (ActionType "model:MyTestDomain$MySelf$ConsultsAnotherRole")) -> true
                  otherwise -> false)
            assert "The Action 'model:MyTestDomain$MySelf$ConsultsAnotherRole' should have 'model:MyTestDomain$AnotherRole' as Object"
              (let
                _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "object"))
                in case (preview _o dr') of
                  (Just (ENR (EnumeratedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf$ConsultsAnotherRole' should be executed by the bot."
              (let
                _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "executedByBot"))
                in case (preview _o dr') of
                  (Just true) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf$ConsultsAnotherRole' should have as Subject 'model:MyTestDomain$MySelf'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "subject")) <<< ix 0
                in case (preview _s dr') of
                  (Just (EnumeratedRoleType "model:MyTestDomain$MySelf")) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf$ConsultsAnotherRole' should have as Verb 'Consult'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "verb"))
                in case (preview _s dr') of
                  (Just Consult) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf$ConsultsAnotherRole' should have as requiredObjectProperties 'ViewOpWens'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "requiredObjectProperties")) <<< _Just
                in case (preview _s dr') of
                  (Just (ViewType "ViewOpWens")) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf$ChangeAnotherRole' should have as requiredObjectProperties the custom (non-default) View 'AnotherView'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ChangeAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "requiredObjectProperties")) <<< _Just
                in case (preview _s dr') of
                  (Just (ViewType "AnotherView")) -> true
                  otherwise -> false
                )
