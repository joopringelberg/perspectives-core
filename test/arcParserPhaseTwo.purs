module Test.Parsing.Arc.PhaseTwo where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (elemIndex, head, length)
import Data.Either (Either(..))
import Data.Lens (_Just, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import Foreign.Object (lookup)
import Node.Encoding (Encoding(..)) as ENC
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..))
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), Operator(..), Step(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo, evalPhaseTwo', expandNamespace, withNamespaces)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Parsing.TransferFile (domain)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Calculation(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action(..), Verb(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType(..))
import Perspectives.Representation.View (View(..))
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

evalPhaseTwo :: forall a. PhaseTwo a -> (Either PerspectivesError a)
evalPhaseTwo = unwrap <<< evalPhaseTwo'

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.Parsing.Arc.PhaseTwo" do
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

  test "A domain should have an external role, too." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain" ARC.domain
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
            assert "The DomeinFile should have role 'model:MyTestDomain$External'."
              (isJust (lookup "model:MyTestDomain$External" dr'.enumeratedRoles))

  test "A Context with a CalculatedRole and a position." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      Calculation : context >> Role" domain
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

  test "A Context with a Computed Role." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  use: sys for model:MyTestDomain\n  thing : MyRole = callExternal cdb:Models() returns : sys:Modellen" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo do
          addAllExternalFunctions
          (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr'@{calculatedRoles})) -> do
            -- logShow dr'
            case lookup "model:MyTestDomain$MyRole" calculatedRoles of
              Nothing -> assert "There should be a role 'MyRole'" false
              Just (CalculatedRole{calculation}) -> do
                assert "The calculation should have '(RDOM (ST EnumeratedRoleType model:MyTestDomain$Modellen))' as its Range"
                  case calculation of
                    (Q (MQD _ _ _ (RDOM (ST (EnumeratedRoleType "model:MyTestDomain$Modellen"))) _ _)) -> true
                    otherwise -> false
                -- logShow calculation
                assert "The queryfunction of the calculation should be '(ExternalCoreRoleGetter \"cbd:Models\")'"
                  case calculation of
                    (Q (MQD _ (ExternalCoreRoleGetter "couchdb_Models") _ _ _ _)) -> true
                    otherwise -> false

  test "A Context with an external property and role." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  case: MyCase\n    external:\n      property: MyProp (mandatory, functional, String) " ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "There should be an external role."
              (isJust (lookup "model:MyTestDomain$MyCase$External" dr'.enumeratedRoles))
            assert "The External Role of MyCase should have a property MyProp"
              case (lookup "model:MyTestDomain$MyCase$External" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole{properties})) -> isJust (elemIndex (ENP $ EnumeratedPropertyType "model:MyTestDomain$MyCase$External$MyProp") properties)

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

  test "A role withOUT attributes" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  thing: MyRole\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the role 'model:Test$MyRole' with\
            \ attribute 'functional' equal to true and 'mandatory' false"
              case (lookup "model:Test$MyRole" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {functional, mandatory})) -> functional == true && mandatory == false

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

  test "A role with binding on a double segmented name" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  use: sys for model:System\n  user: MyUser filledBy: sys:PerspectivesSystem$User" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The DomeinFile should have the role 'model:MyTestDomain$MyUser' that is\
            \ filled with 'ST EnumeratedRoleType model:System$PerspectivesSystem$User'"
              case (lookup "model:MyTestDomain$MyUser" dr'.enumeratedRoles) of
                Nothing -> false
                (Just (EnumeratedRole {binding})) -> binding == (ST $ EnumeratedRoleType "model:System$PerspectivesSystem$User")

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

  test "A role with a Number property withOUT attributes" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: MyRoleInContext\n    property: MyProp" ARC.domain
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
                (Just (EnumeratedProperty {functional, mandatory})) -> functional == true && mandatory == false
            assert "The property 'MyProp' should have 'PString' as 'range'."
              case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties of
                Nothing -> false
                (Just (EnumeratedProperty {range})) -> range == PString

  test "A role with a CalculatedProperty" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Property : StringProperty : MyProp\n      Calculation : Prop1 + Prop2" domain
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
    fileName <- pure "arcsyntax.arc"
    text <- liftEffect (readTextFile ENC.UTF8 (Path.concat [testDirectory, fileName]))
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser text ARC.domain
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Property : StringProperty : NickName\n    Property : BooleanProperty : Happy\n      Calculation : Prop1 and Prop2\n    View : View : MyView\n  Role : RoleInContext : MyRoleInContext\n    Calculation : context >> Role" domain
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

  test "Actions" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultsAnotherRole\n      View : DefaultObjectViewRef : ViewOpWens\n      Action : Change : ChangeAnotherRole\n        View : ObjectViewRef : AnotherView" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) true
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "An Action in a Perspective should end up in the DomeinFile."
              (isJust (lookup "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" dr'.actions))
            assert "The Role MySelf should have a Perspective on 'model:MyTestDomain$AnotherRole' that includes the Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole'."
              (let
                -- _p :: Traversal' DomeinFileRecord ActionType)
                -- NOTE: the index at the end of the Traversal' is very dependent on the order of the Actions!
                _p = prop (SProxy :: (SProxy "enumeratedRoles")) <<< at "model:MyTestDomain$MySelf" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "perspectives")) <<< at "AnotherRole" <<< traversed <<< ix 1
                in case (preview _p dr') of
                  (Just (ActionType "model:MyTestDomain$MySelf_bot$ConsultAnotherRole")) -> true
                  otherwise -> false)
            assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have 'model:MyTestDomain$AnotherRole' as Object"
              (let
                _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "object"))
                in case (preview _o dr') of
                  (Just (ENR (EnumeratedRoleType "AnotherRole"))) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should be executed by the bot."
              (let
                _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "executedByBot"))
                in case (preview _o dr') of
                  (Just true) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have as Subject 'model:MyTestDomain$MySelf'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "subject"))
                in case (preview _s dr') of
                  (Just (EnumeratedRoleType "model:MyTestDomain$MySelf")) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have as Verb 'Consult'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "verb"))
                in case (preview _s dr') of
                  (Just Consult) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have as requiredObjectProperties 'ViewOpWens'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "requiredObjectProperties")) <<< _Just
                in case (preview _s dr') of
                  (Just (ViewType "ViewOpWens")) -> true
                  otherwise -> false
                )
            assert "The Action 'model:MyTestDomain$MySelf_bot$ChangeAnotherRole' should have as requiredObjectProperties the custom (non-default) View 'AnotherView'."
              (let
                _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ChangeAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "requiredObjectProperties")) <<< _Just
                in case (preview _s dr') of
                  (Just (ViewType "AnotherView")) -> true
                  otherwise -> false
                )
  test "withNamespaces" do
    case evalPhaseTwo (unsafePartial $ withNamespaces (Cons (PREFIX "sys" "model:System$System") Nil)
        (expandNamespace "sys:User")) of
      (Right eu) -> assert "The expansion of 'sys:User' should be 'model:System$System$User'" (eu == "model:System$System$User")
      (Left e) -> assert (show e) false

  test "Context with Aspect" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Feest\n  aspect: model:MyAspectModel$MyAspect" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The domain 'model:Feest' should have an aspect 'model:MyAspectModel$MyAspect'."
              (isJust (lookup "model:Feest" dr'.contexts))
            case (lookup "model:Feest" dr'.contexts) of
              Nothing -> assert "Cannot find the domain" false
              (Just (Context{contextAspects})) -> assert "The domain 'model:Feest' should have an aspect 'model:MyAspectModel$MyAspect'."
                (isJust (elemIndex (ContextType "model:MyAspectModel$MyAspect") contextAspects))

  test "Test well-formedness of a ContextAspect name" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Feest\n  aspect: MyAspectModel$MyAspect" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left (NotWellFormedName pos _)) -> assert "The position in the error message should be" (pos == ArcPosition {line: 2, column: 11})
          otherwise -> assert "The name of the aspect is not well-formed and that should be detected." false

  test "Role with Aspect" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Feest\n  thing: Wens (mandatory, functional)\n    aspect: model:MyAspectModel$MyAspect$MyAspectRole" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            case lookup "model:Feest$Wens" dr'.enumeratedRoles of
              Nothing -> assert "Cannot find the role 'model:Feest$Wens'" false
              (Just (EnumeratedRole{roleAspects})) -> assert "The role 'model:Feest$Wens' should have an aspect 'model:MyAspectModel$MyAspect$MyAspectRole'."
                (isJust (elemIndex (EnumeratedRoleType "model:MyAspectModel$MyAspect$MyAspectRole") roleAspects))

  test "Role with binding to Context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Feest\n  context: Uitje (mandatory, functional) filledBy: Speeltuin" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr'@{enumeratedRoles})) -> do
            -- logShow dr'
            case lookup "model:Feest$Uitje" enumeratedRoles of
              (Just (EnumeratedRole{binding})) -> assert "The binding of Uitje should be an External Role"
                (binding == (ST $ EnumeratedRoleType "Speeltuin$External"))
              otherwise -> assert "The binding of Uitje should be an External Role" false

  test "Action with Condition" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    perspective on: Party\n      Consult with ViewOnGuest\n        subjectView: AnotherView\n        if Prop1 > 10" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr'@{actions})) -> do
            -- logShow dr'
            case lookup "model:Test$Gast$ConsultParty" actions of
              (Just (Action{condition})) -> assert "The condition should have operator '>'"
                (case condition of
                  S (Binary (BinaryStep{operator})) -> case operator of
                    (GreaterThan _) -> true
                    otherwise -> false
                  otherwise -> false)
              otherwise -> assert "There should be an action Consult Party" false

  test "Action with a DateTime Condition" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    perspective on: Party\n      Consult with ViewOnGuest\n        subjectView: AnotherView\n        if MyProp > '1995-12-17'" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr'@{actions})) -> do
            -- logShow dr'
            case lookup "model:Test$Gast$ConsultParty" actions of
              (Just (Action{condition})) -> assert "The condition should have operator '>'"
                (case condition of
                  S (Binary (BinaryStep{operator})) -> case operator of
                    (GreaterThan _) -> true
                    otherwise -> false
                  otherwise -> false)
              otherwise -> assert "There should be an action Consult Party" false

  test "Bot Action with if-then rule" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  bot: for Gast\n    perspective on: Party\n      if Prop1 > 10 then Prop2 = 20\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr'@{actions})) -> do
            -- logShow dr'
            case lookup "model:Test$Gast_bot$ChangeParty" actions of
              (Just (Action{condition, effect})) -> do
                assert "The condition should have operator '>'"
                  (case condition of
                    S (Binary (BinaryStep{operator})) -> case operator of
                      (GreaterThan _) -> true
                      otherwise -> false
                    otherwise -> false)
                assert "There should be an effect"
                  (case effect of
                    Just (A _) -> true
                    otherwise -> false)
              otherwise -> assert "There should be an action Change Party" false

  test "A role with a CalculatedProperty" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  thing: Guest (mandatory, functional)\n    property: NumberOfGuests = this >>= sum" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt

        case evalPhaseTwo (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            assert "The property 'NumberOfGuests' should have a calculation"
              case lookup "model:Test$Guest$NumberOfGuests" dr'.calculatedProperties of
                (Just (CalculatedProperty {calculation})) -> case calculation of
                  (S (Binary (BinaryStep{operator}))) -> case operator of
                    Sequence _ -> true
                    _ -> false
                  _ -> false
                _ -> false
