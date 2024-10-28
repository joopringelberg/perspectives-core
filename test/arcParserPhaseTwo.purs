module Test.Parsing.Arc.PhaseTwo where 

import Prelude

import Control.Monad.Free (Free)
import Data.Array (elemIndex, head)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (lookup)
import Node.Encoding (Encoding(..)) as ENC
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..))
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), ComputedType(..), Operator(..), Step(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo, evalPhaseTwo', expandNamespace, withNamespaces)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..))
import Test.Parsing.DomeinFileSelectors (all, ensureERole, ensureEnumeratedProperty, ensureEnumeratedRoleHasProperty, ensureState, enumeratedPropertyHasRange, enumeratedPropertyIsFunctional, enumeratedPropertyIsMandatory, exists)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)
import Parsing (ParseError)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

evalPhaseTwo :: forall a. PhaseTwo a -> Aff (Either PerspectivesError a)
-- evalPhaseTwo = unwrap <<< evalPhaseTwo'
evalPhaseTwo = evalPhaseTwo' >=> case _ of
  Left errs -> pure $ Left (unsafePartial fromJust $ head errs)
  Right r -> pure $ Right r

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.PhaseTwo" do
  -- testOnly "Representing the Domain and a context with subcontext and role." do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- (DomeinFile dr) <- pure defaultDomeinFile
  --       evalPhaseTwo (traverseDomain ctxt) >>=
  --       case _ of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           -- logShow context
  --           assert "The DomeinFile should have context 'model:MyTestDomain'."
  --             (isJust (lookup "model:MyTestDomain" dr'.contexts))
  --           assert "The DomeinFile should have context 'model:MyTestDomain$MyCase'."
  --             (isJust (lookup "model:MyTestDomain$MyCase" dr'.contexts))
  --           assert "The DomeinFile should have role 'model:MyTestDomain$MyCase$MyRoleInContext'."
  --             (isJust (lookup "model:MyTestDomain$MyCase$MyRoleInContext" dr'.enumeratedRoles))
  --           assert ("The Model should have MyCase as one of its sub-contexts")
  --             ((Just $ ContextType "model:MyTestDomain$MyCase") == (preview (prop (SProxy :: SProxy "contexts") <<< at "model:MyTestDomain" <<< traversed <<< _Newtype <<< (prop (SProxy :: SProxy "nestedContexts")) <<< ix 0) dr'))
  --           assert "'MyCase' should have 'MyRoleInContext' as a role in context"
  --             ((Just $ ENR $ EnumeratedRoleType "model:MyTestDomain$MyCase$MyRoleInContext") == (preview (prop (SProxy :: SProxy "contexts") <<< at "model:MyTestDomain$MyCase" <<< traversed <<< _Newtype <<< (prop (SProxy :: SProxy "rolInContext")) <<< ix 0) dr'))
  --           assert "The Domain should have the id 'MyTestDomain'" (id == "MyTestDomain")

  test "A domain should have an external role, too." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- (DomeinFile dr) <- pure defaultDomeinFile
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              -- logShow context
              assert "The DomeinFile should have context 'model:MyTestDomain'."
                (isJust (lookup "model:MyTestDomain" dr'.contexts))
              assert "The DomeinFile should have role 'model:MyTestDomain$External'."
                (isJust (lookup "model:MyTestDomain$External" dr'.enumeratedRoles))

  -- test "A Context with a CalculatedRole and a position." do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      Calculation : context >> Role" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The role MyRoleInContext should be a calculated role."
  --             (isJust (lookup "model:MyTestDomain$MyCase$MyRoleInContext" dr'.calculatedRoles))

  test "A Context with a Computed Role." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use sys for model:MyTestDomain\n  thing MyRole = callExternal cdb:Models() returns sys:Modellen" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (do
          addAllExternalFunctions
          (traverseDomain ctxt)) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr'@{calculatedRoles})) -> do
              -- logShow dr'
              case lookup "model:MyTestDomain$MyRole" calculatedRoles of
                Nothing -> assert "There should be a role 'MyRole'" false
                Just (CalculatedRole{calculation}) -> do
                  -- logShow calculation
                  case calculation of
                    (Q (MQD _ f _ (RDOM (ST (RoleInContext {context: (ContextType "model:System$PerspectivesSystem"), role: (EnumeratedRoleType "model:System$PerspectivesSystem$Modellen")}))) _ _)) -> assert "The queryfunction of the calculation should be '(ExternalCoreRoleGetter \"model:Couchdb$Models\")'" (f == (ExternalCoreRoleGetter "model:Couchdb$Models"))
                    (Q _) -> assert "The calculation should have '(RDOM (ST EnumeratedRoleType Modellen))' as its Range" false
                    (S (Computation (ComputationStep {computedType})) _) -> assert "The step should have 'model:MyTestDomain$Modellen' as computedType" (computedType == OtherType "model:MyTestDomain$Modellen")
                    otherwise -> assert ("Unexpected result: " <> show otherwise) false
                  -- logShow calculation

  test "A Context with an external property and role." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  case MyCase\n    external\n      property MyProp (mandatory, String) " ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              assert "There should be an external role."
                (isJust (lookup "model:MyTestDomain$MyCase$External" dr'.enumeratedRoles))
              assert "The External Role of MyCase should have a property MyProp"
                case (lookup "model:MyTestDomain$MyCase$External" dr'.enumeratedRoles) of
                  Nothing -> false
                  (Just (EnumeratedRole{properties})) -> isJust (elemIndex (ENP $ EnumeratedPropertyType "model:MyTestDomain$MyCase$External$MyProp") properties)

  -- test "A role with a view" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase\n    Role : RoleInContext : MyRoleInContext\n      View : View : MyView\n        PropertyRef : MyProp" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The DomeinFile should have the view 'MyView'" (isJust (lookup "model:MyTestDomain$MyCase$MyRoleInContext$MyView" dr'.views))
  --           assert "MyRoleInContext should have the view MyView"
  --             case (lookup "model:MyTestDomain$MyCase$MyRoleInContext" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole {views})) -> case (head views) of
  --                 Nothing -> false
  --                 (Just (ViewType v)) -> v == "model:MyTestDomain$MyCase$MyRoleInContext$MyView"

  -- test "A role with attributes" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Functional : False\n    Mandatory : True" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The DomeinFile should have the role 'model:MyTestDomain$MyRoleInContext' with\
  --           \ attribute 'functional' equal to false"
  --             case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole {functional, mandatory})) -> functional == false && mandatory == true

  test "A role withOUT attributes" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  thing MyRole\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              assert "The DomeinFile should have the role 'model:Test$MyRole' with\
              \ attribute 'functional' equal to true and 'mandatory' false"
                case (lookup "model:Test$MyRole" dr'.enumeratedRoles) of
                  Nothing -> false
                  (Just (EnumeratedRole {functional, mandatory})) -> functional == true && mandatory == false

  -- test "A role with binding" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    FilledBy : model:MyTestDomain$MyOtherRole" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The DomeinFile should have the role 'model:MyTestDomain$MyRoleInContext' that is\
  --           \ filled with 'ST EnumeratedRoleType model:MyTestDomain$MyOtherRole'"
  --             case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole {binding})) -> binding == (ST $ EnumeratedRoleType "model:MyTestDomain$MyOtherRole")

  test "A role with binding on a double segmented name" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use sys for model:System\n  user MyUser filledBy sys:PerspectivesSystem$User" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              assert "The DomeinFile should have the role 'model:MyTestDomain$MyUser' that is\
              \ filled with 'ST EnumeratedRoleType model:System$PerspectivesSystem$User'"
                case (lookup "model:MyTestDomain$MyUser" dr'.enumeratedRoles) of
                  Nothing -> false
                  (Just (EnumeratedRole {binding})) -> binding == (Just $ ST $ RoleInContext {context: ContextType "model:System$PerspectivesSystem", role: EnumeratedRoleType "model:System$PerspectivesSystem$User"})

  -- test "Role has context" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    FilledBy : model:MyTestDomain$MyOtherRole" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The role 'model:MyTestDomain$MyRoleInContext' should have context 'model:MyTestDomain'"
  --             case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole {context: cont})) -> cont == ContextType "model:MyTestDomain"

  -- test "Domain has no context, a context does" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Context : Case : MyCase" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The domain 'model:MyTestDomain' should have no context."
  --             case (lookup "model:MyTestDomain" dr'.contexts) of
  --               Nothing -> false
  --               (Just (Context {context: cont})) -> cont == Nothing
  --           assert "The context 'model:MyTestDomain$MyCase' should have context 'model:MyTestDomain'"
  --             case (lookup "model:MyTestDomain$MyCase" dr'.contexts) of
  --               Nothing -> false
  --               (Just (Context {context: cont})) -> cont == Just (ContextType "model:MyTestDomain")

  -- test "A role with a Number property with attributes 'Functional' and 'Mandatory'" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Property : StringProperty : MyProp\n      Functional : False\n      Mandatory: False" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The DomeinFile should have the property 'MyProp'" (isJust (lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties))
  --           assert "MyRoleInContext should have the property MyProp"
  --             case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole {properties})) -> case (head properties) of
  --                 Nothing -> false
  --                 (Just (ENP (EnumeratedPropertyType v))) -> v == "model:MyTestDomain$MyRoleInContext$MyProp"
  --                 otherwise -> false
  --           assert "The property 'MyProp' should have attribute 'Functional' False and 'Mandatory' False."
  --             case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties of
  --               Nothing -> false
  --               (Just (EnumeratedProperty {functional, mandatory})) -> functional == false && mandatory == false
  --           assert "The property 'MyProp' should have 'PString' as 'range'."
  --             case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.enumeratedProperties of
  --               Nothing -> false
  --               (Just (EnumeratedProperty {range})) -> range == PString

  test "A role with a Number property withOUT attributes" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRoleInContext\n    property MyProp" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt) -> do
        -- log "Entering phase two"
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr)) -> do
              ensureEnumeratedProperty "model:MyTestDomain$MyRoleInContext$MyProp" dr >>= all
                [ (enumeratedPropertyIsFunctional true)
                , (enumeratedPropertyIsMandatory false)
                , (enumeratedPropertyHasRange PString)]
              ensureERole "model:MyTestDomain$MyRoleInContext" dr >>=
                ensureEnumeratedRoleHasProperty "model:MyTestDomain$MyRoleInContext$MyProp" >>= exists

  -- test "A role with a CalculatedProperty" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Role : RoleInContext : MyRoleInContext\n    Property : StringProperty : MyProp\n      Calculation : Prop1 + Prop2" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The DomeinFile should have the property 'MyProp'" (isJust (lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.calculatedProperties))
  --           assert "MyRoleInContext should have the property MyProp"
  --             case (lookup "model:MyTestDomain$MyRoleInContext" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole {properties})) -> case (head properties) of
  --                 Nothing -> false
  --                 (Just (CP (CalculatedPropertyType v))) -> v == "model:MyTestDomain$MyRoleInContext$MyProp"
  --                 otherwise -> false
  --           assert "The property 'MyProp' should have a calculation"
  --             case lookup "model:MyTestDomain$MyRoleInContext$MyProp" dr'.calculatedProperties of
  --               Nothing -> false
  --               -- TODO: check the actual calculation.
  --               (Just (CalculatedProperty {calculation})) -> true

  test "Parse a file and pass phase two over it" do
    fileName <- pure "arcsyntax.arc"
    text <- liftEffect (readTextFile ENC.UTF8 (Path.concat [testDirectory, fileName]))
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser text ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right dom) -> do
        -- logShow dom
        evalPhaseTwo (traverseDomain dom) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              assert ("The file '" <> fileName <> "' does not parse") true

  -- test "An Agent role for a Bot without a ForUser clause" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left (e :: PerspectivesError)) -> do
  --           -- logShow e
  --           assert (show e) true
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "A BotRole should specify the user it represents with a 'ForUser' clause." false

  -- test "An Agent role for a Bot adds clauses to its represented user" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) true
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "The BotRole for user 'MySelf' should give rise to an enumeratedRole 'model:MyTestDomain$MySelf'"
  --             case lookup "model:MyTestDomain$MySelf" dr'.enumeratedRoles of
  --               Nothing -> false
  --               (Just (EnumeratedRole{_id})) -> _id == EnumeratedRoleType "model:MyTestDomain$MySelf"

  -- test "Types should have positions in their definining texts." do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Property : StringProperty : NickName\n    Property : BooleanProperty : Happy\n      Calculation : Prop1 and Prop2\n    View : View : MyView\n  Role : RoleInContext : MyRoleInContext\n    Calculation : context >> Role" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) true
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           context <- pure $ unsafePartial $ fromJust $ lookup "model:MyTestDomain" dr'.contexts
  --           assert "The Domain should be on position (1, 20)"
  --             (case context of (Context{pos}) -> pos == ArcPosition {line: 1, column: 20})
  --           assert "The role MySelf should have position (2, 21)"
  --             case (lookup "model:MyTestDomain$MySelf" dr'.enumeratedRoles) of
  --               Nothing -> false
  --               (Just (EnumeratedRole{pos})) -> pos == ArcPosition({line: 2, column: 21})
  --           assert "The role MyRoleInContext should have position (8,26)"
  --             case (lookup "model:MyTestDomain$MyRoleInContext" dr'.calculatedRoles) of
  --               Nothing -> false
  --               (Just (CalculatedRole{pos})) -> pos == ArcPosition({line: 8, column: 26})
  --           assert "'MyView' should have position (7, 19)"
  --             case (lookup "model:MyTestDomain$MySelf$MyView" dr'.views) of
  --               Nothing -> false
  --               (Just (View{pos})) -> pos == ArcPosition{line: 7, column: 19}
  --           assert "Property 'Nickname' should have position (4, 33)"
  --             case (lookup "model:MyTestDomain$MySelf$NickName" dr'.enumeratedProperties) of
  --               Nothing -> false
  --               (Just (EnumeratedProperty{pos})) -> pos == ArcPosition{line: 4, column: 33}
  --           assert "Property 'Happy' should have position (5, 34)"
  --             case (lookup "model:MyTestDomain$MySelf$Happy" dr'.calculatedProperties) of
  --               Nothing -> false
  --               (Just (CalculatedProperty{pos})) -> pos == ArcPosition{line: 5, column: 34}

  -- test "Actions" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultsAnotherRole\n      View : DefaultObjectViewRef : ViewOpWens\n      Action : Change : ChangeAnotherRole\n        View : ObjectViewRef : AnotherView" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) true
  --         (Right (DomeinFile dr')) -> do
  --           -- logShow dr'
  --           assert "An Action in a Perspective should end up in the DomeinFile."
  --             (isJust (lookup "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" dr'.actions))
  --           assert "The Role MySelf should have a Perspective on 'model:MyTestDomain$AnotherRole' that includes the Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole'."
  --             (let
  --               -- _p :: Traversal' DomeinFileRecord ActionType)
  --               -- NOTE: the index at the end of the Traversal' is very dependent on the order of the Actions!
  --               _p = prop (SProxy :: (SProxy "enumeratedRoles")) <<< at "model:MyTestDomain$MySelf" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "perspectives")) <<< ix 1
  --               in case (preview _p dr') of
  --                 (Just (ActionType "model:MyTestDomain$MySelf_bot$ConsultAnotherRole")) -> true
  --                 otherwise -> false)
  --           assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have 'model:MyTestDomain$AnotherRole' as Object"
  --             (let
  --               _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "object"))
  --               in case (preview _o dr') of
  --                 (Just (S (Simple (AST.ArcIdentifier _ "AnotherRole")))) -> true
  --                 -- (Just (ENR (EnumeratedRoleType "AnotherRole"))) -> true
  --                 otherwise -> false
  --               )
  --           assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should be executed by the bot."
  --             (let
  --               _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "executedByBot"))
  --               in case (preview _o dr') of
  --                 (Just true) -> true
  --                 otherwise -> false
  --               )
  --           assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have as Subject 'model:MyTestDomain$MySelf'."
  --             (let
  --               _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "subject"))
  --               in case (preview _s dr') of
  --                 (Just (ENR (EnumeratedRoleType "model:MyTestDomain$MySelf"))) -> true
  --                 otherwise -> false
  --               )
  --           assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have as Verb 'Consult'."
  --             (let
  --               _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "verb"))
  --               in case (preview _s dr') of
  --                 (Just Consult) -> true
  --                 otherwise -> false
  --               )
  --           assert "The Action 'model:MyTestDomain$MySelf_bot$ConsultAnotherRole' should have as requiredObjectProperties 'ViewOpWens'."
  --             (let
  --               _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "requiredObjectProperties")) <<< _Just
  --               in case (preview _s dr') of
  --                 (Just (ViewType "ViewOpWens")) -> true
  --                 otherwise -> false
  --               )
  --           assert "The Action 'model:MyTestDomain$MySelf_bot$ChangeAnotherRole' should have as requiredObjectProperties the custom (non-default) View 'AnotherView'."
  --             (let
  --               _s = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ChangeAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "requiredObjectProperties")) <<< _Just
  --               in case (preview _s dr') of
  --                 (Just (ViewType "AnotherView")) -> true
  --                 otherwise -> false
  --               )
  test "withNamespaces" do
    evalPhaseTwo (unsafePartial $ withNamespaces (Cons (PREFIX "sys" "model:System$System") Nil)
        (expandNamespace "sys:User")) >>=
      case _ of
        (Right eu) -> assert "The expansion of 'sys:User' should be 'model:System$System$User'" (eu == "model:System$System$User")
        (Left e) -> assert (show e) false

  test "Context with Aspect" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Feest\n  aspect model:MyAspectModel$MyAspect" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
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
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Feest\n  aspect MyAspectModel$MyAspect" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left (NotWellFormedName pos _)) -> assert "The position in the error message should be" (pos == ArcPosition {line: 2, column: 11})
            otherwise -> assert "The name of the aspect is not well-formed and that should be detected." false

  test "Role with Aspect" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Feest\n  thing Wens (mandatory)\n    aspect model:MyAspectModel$MyAspect$MyAspectRole" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              case lookup "model:Feest$Wens" dr'.enumeratedRoles of
                Nothing -> assert "Cannot find the role 'model:Feest$Wens'" false
                (Just (EnumeratedRole{roleAspects})) -> assert "The role 'model:Feest$Wens' should have an aspect 'model:MyAspectModel$MyAspect$MyAspectRole'."
                  (isJust (elemIndex (RoleInContext{context: ContextType "model:MyAspectModel$MyAspect$MyAspectRole", role: (EnumeratedRoleType "model:MyAspectModel$MyAspect$MyAspectRole")}) roleAspects))

  test "Role with binding to Context" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Feest\n  context Uitje (mandatory) filledBy Speeltuin" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr'@{enumeratedRoles})) -> do
            -- logShow dr'
            case lookup "model:Feest$Uitje" enumeratedRoles of
              (Just (EnumeratedRole{binding})) -> assert "The binding of Uitje should be an External Role"
                (binding == (Just $ ST $ RoleInContext {context: ContextType "model:Feest$Speeltuin", role: EnumeratedRoleType "model:Feest$Speeltuin$External"}))
              otherwise -> assert "The binding of Uitje should be an External Role" false

  test "Role with a state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr'@{enumeratedRoles})) -> do
              ensureState "model:Test$Party$SomeState" dr' >>=
                exists

  -- test "Action with Condition" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    perspective on: Party\n      Consult with ViewOnGuest\n        subjectView: AnotherView\n        if Prop1 > 10" ARC.domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr'@{actions})) -> do
  --           -- logShow dr'
  --           case lookup "model:Test$Gast$ConsultParty" actions of
  --             (Just (Action{condition})) -> assert "The condition should have operator '>'"
  --               (case condition of
  --                 S (Binary (BinaryStep{operator})) -> case operator of
  --                   (GreaterThan _) -> true
  --                   otherwise -> false
  --                 otherwise -> false)
  --             otherwise -> assert "There should be an action Consult Party" false

  -- test "Action with a DateTime Condition" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    perspective on: Party\n      Consult with ViewOnGuest\n        subjectView: AnotherView\n        if MyProp > '1995-12-17'" ARC.domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr'@{actions})) -> do
  --           -- logShow dr'
  --           case lookup "model:Test$Gast$ConsultParty" actions of
  --             (Just (Action{condition})) -> assert "The condition should have operator '>'"
  --               (case condition of
  --                 S (Binary (BinaryStep{operator})) -> case operator of
  --                   (GreaterThan _) -> true
  --                   otherwise -> false
  --                 otherwise -> false)
  --             otherwise -> assert "There should be an action Consult Party" false

  -- test "Bot Action with if-then rule" do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  bot: for Gast\n    perspective on: Party\n      if Prop1 > 10 then Prop2 = 20\n" ARC.domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       -- logShow ctxt
  --       case evalPhaseTwo (traverseDomain ctxt) of
  --         (Left e) -> assert (show e) false
  --         (Right (DomeinFile dr'@{actions})) -> do
  --           -- logShow dr'
  --           case lookup "model:Test$Gast_bot$ChangeParty" actions of
  --             (Just (Action{condition, effect})) -> do
  --               assert "The condition should have operator '>'"
  --                 (case condition of
  --                   S (Binary (BinaryStep{operator})) -> case operator of
  --                     (GreaterThan _) -> true
  --                     otherwise -> false
  --                   otherwise -> false)
  --               assert "There should be an effect"
  --                 (case effect of
  --                   Just (A _) -> true
  --                   otherwise -> false)
  --             otherwise -> assert "There should be an action Change Party" false

  test "A role with a CalculatedProperty" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  thing Guest (mandatory)\n    property NumberOfGuests = this >>= sum" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        evalPhaseTwo (traverseDomain ctxt) >>=
          case _ of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              assert "The property 'NumberOfGuests' should have a calculation"
                case lookup "model:Test$Guest$NumberOfGuests" dr'.calculatedProperties of
                  (Just (CalculatedProperty {calculation})) -> case calculation of
                    (S (Binary (BinaryStep{operator})) _) -> case operator of
                      Sequence _ -> true
                      _ -> false
                    _ -> false
                  _ -> false
