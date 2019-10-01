module Test.Parsing.Arc.PhaseTwo where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Lens (preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import Foreign.Object (lookup)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFile)
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwo (traverseContextE)
import Perspectives.Parsing.Arc.Simple (domain)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..), Range(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType(..))
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.Parsing.Arc.PhaseTwo" do
  test "Representing the Domain and a context with subcontext and role." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
            -- logShow dr'
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

  test "A Context with a CalculatedRole." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      Calculation : some calculation" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
            -- logShow dr'
            assert "bla bla" true

  test "A role with a view" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      View : View : MyView\n        PropertyRef : MyProp" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE dom dr "model:") of
          (Left e) -> assert e false
          (Right (Tuple dr' context)) -> do
            -- logShow dr'
            assert ("The file '" <> fileName <> "' does not parse") true

  test "An Agent role for a Bot without a ForUser clause" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> do
            log e
            assert e true
          (Right (Tuple dr' context)) -> do
            -- logShow dr'
            assert "A BotRole should specify the user it represents with a 'ForUser' clause." false

  test "An Agent role for a Bot adds clauses to its represented user" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e true
          (Right (Tuple dr' context)) -> do
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
        (DomeinFile dr) <- pure defaultDomeinFile
        case runExcept (traverseContextE ctxt dr "model:") of
          (Left e) -> assert e true
          (Right (Tuple dr' context)) -> do
            -- logShow dr'
            assert "The enumeratedRole 'model:MyTestDomain$MySelf' should have the properties 'NickName' and 'Happy'"
              case lookup "model:MyTestDomain$MySelf" dr'.enumeratedRoles of
                Nothing -> false
                (Just (EnumeratedRole{properties})) -> length properties == 2