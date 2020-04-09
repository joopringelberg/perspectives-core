module Test.Parsing.Arc.PhaseThree where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (elemIndex, filter, head, length)
import Data.Either (Either(..))
import Data.Lens (_Just, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Class.Console (logShow, log)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, runTypeLevelToArray, (###=))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Extern.Couchdb (addExternalFunctions) as ExternalCouchdb
import Perspectives.Identifiers (Namespace)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Parsing.TransferFile (domain)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Role (propertiesOfADT)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType(..), propertytype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType_, lookForUnqualifiedRoleType, lookForUnqualifiedRoleTypeOfADT, roleInContext)
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

withDomeinFile :: forall a. Namespace -> DomeinFile -> MonadPerspectives a -> MonadPerspectives a
withDomeinFile ns df mpa = do
  void $ storeDomeinFileInCache ns df
  r <- mpa
  removeDomeinFileFromCache ns
  pure r

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.Parsing.Arc.PhaseThree" do
  test "TypeLevelObjectGetters" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultAnotherRole\n        IndirectObjectRef : AnotherRole\n  Role : RoleInContext : AnotherRole\n    Calculation : context >> Role" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right df@(DomeinFile dr')) -> do
            -- logShow dr'
            (Context {_id}) <- runP $
              withDomeinFile "model:MyTestDomain" df
                (getPerspectType (ContextType "model:MyTestDomain"))
            assert "Should be able to retrieve the context that represents the domain"
              (_id == ContextType "model:MyTestDomain")
            (CalculatedRole {_id:crid}) <- runP $
              withDomeinFile "model:MyTestDomain" df
                (getPerspectType (CalculatedRoleType "model:MyTestDomain$AnotherRole"))
            assert "Should be able to retrieve the role AnotherRole"
              (crid == CalculatedRoleType "model:MyTestDomain$AnotherRole")

            roles <- runP $
              withDomeinFile "model:MyTestDomain" df
                ((ContextType "model:MyTestDomain") ###= roleInContext)
            assert "roleInContext should be able to retrieve the role AnotherRole from the context model:MyTestDomain."
              case head roles of
                (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                otherwise -> false

            roles'' <- runP $
              withDomeinFile "model:MyTestDomain" df
                ((ContextType "model:MyTestDomain") ###= (lookForUnqualifiedRoleType "AnotherRole"))
            assert "lookForUnqualifiedRoleType should be able to retrieve the role AnotherRole from the context model:MyTestDomain."
              (isJust (head roles''))

  -- test "Testing qualifyActionRoles." do
  --   (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultAnotherRole\n        IndirectObjectRef : AnotherRole\n  Role : RoleInContext : AnotherRole\n    Calculation : blabla" domain

  test "Testing qualifyActionRoles." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  bot: for MySelf\n    perspective on: AnotherRole\n      Consult\n        indirectObject: AnotherRole \n  thing: Role (mandatory, functional) filledBy: YetAnotherRole\n  thing: AnotherRole = Role >> binding\n  thing: YetAnotherRole (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR) -> do
                -- logShow correctedDFR
                assert "AnotherRole should be a calculatedRole"
                  (let
                    _r = prop (SProxy :: (SProxy "calculatedRoles")) <<< at "model:MyTestDomain$AnotherRole" <<< traversed
                    in case (preview _r correctedDFR) of
                      (Just _) -> true
                      otherwise -> false
                  )
                assert "The Object of the action 'ConsultAnotherRole' should be a qualified CalculatedRole type."
                  (let
                    _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "object"))
                    in case (preview _o correctedDFR) of
                      (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                      -- (Just (CR _)) -> true
                      otherwise -> false
                    )
                assert "The IndirectObject of the action 'ConsultAnotherRole' should be a qualified CalculatedRole type."
                  (let
                    _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf_bot$ConsultAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "indirectObject")) <<< _Just
                    in case (preview _o correctedDFR) of
                      (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                      -- (Just (CR _)) -> true
                      otherwise -> false
                    )

  test "Testing qualifyActionRoles: External." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  user: Driver\n    perspective on: External\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR) -> do
                -- logShow correctedDFR
                assert "The Object of the action 'ConsultExternal' should be 'model:MyTestDomain$External'."
                  (let
                    _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$Driver$ConsultExternal" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "object"))
                    in case (preview _o correctedDFR) of
                      (Just (ENR (EnumeratedRoleType "model:MyTestDomain$External"))) -> true
                      -- (Just (CR _)) -> true
                      otherwise -> false
                    )

  test "Testing qualifyActionRoles: UnknownRole." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  user: Gast (mandatory, functional)\n    perspective on: SomeRole: Consult\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (RoleMissingInContext _ _ _)) -> assert "" true
              otherwise -> do
                assert "Expected the 'RoleMissingInContext' error" false

  test "Testing qualifyActionRoles: RoleMissingInContext." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  user: Gast (mandatory, functional)\n    perspective on: SomeRole: Consult\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (RoleMissingInContext _ _ _)) -> assert "" true
              otherwise -> assert "Expected the 'RoleMissingInContext' error" false

  test "Testing qualifyBindings: correct reference." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Binder (mandatory, functional) filledBy: Bound\n  thing: Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                  Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                  (Just (EnumeratedRole{binding})) -> assert
                    "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                    (binding == ST (EnumeratedRoleType "model:MyTestDomain$Bound"))

  test "Testing qualifyBindings: qualified reference." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Binder (mandatory, functional) filledBy: model:MyTestDomain$Bound\n  thing: Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                  Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                  (Just (EnumeratedRole{binding})) -> assert
                    "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                    (binding == ST (EnumeratedRoleType "model:MyTestDomain$Bound"))

  test "Testing qualifyBindings: prefixed reference." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  use: my for model:MyTestDomain\n  thing: Binder (mandatory, functional) filledBy: my:Bound\n  thing: Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                  Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                  (Just (EnumeratedRole{binding})) -> assert
                    "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                    (binding == ST (EnumeratedRoleType "model:MyTestDomain$Bound"))

  test "Testing qualifyBindings: missing reference." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Binder (mandatory, functional) filledBy: Bount\n  thing: Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (UnknownRole _ _)) -> assert "" true
              otherwise -> do
                assert "The binding of 'Binder' is not defined and that should have been detected." false

  test "Testing qualifyBindings: two candidates for binding." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Binder (mandatory, functional) filledBy: Bound\n  thing: Bound (mandatory, functional)\n  case: Nested\n    thing: Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            logShow x'
            case x' of
              (Left e@(NotUniquelyIdentifying _ _ _)) -> do
                -- logShow e
                assert "" true
              otherwise -> do
                assert "The binding of 'Binder' is not defined and that should have been detected." false

  test "Testing qualifyPropertyReferences: correct reference." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Feest (mandatory, functional)\n    property: Datum (mandatory, functional, DateTime)\n    view: ViewOpFeest (Datum)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{views}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                  Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'" false
                  (Just (View{propertyReferences})) -> case head (filter (\pref -> propertytype2string pref == "model:MyTestDomain$Feest$Datum") propertyReferences) of
                    (Just (ENP (EnumeratedPropertyType "model:MyTestDomain$Feest$Datum"))) -> assert "" true
                    otherwise -> assert "There should be a Property 'model:MyTestDomain$Feest$Datum' in ViewOpFeest" false

  test "Testing qualifyPropertyReferences: incorrect reference." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Feest (mandatory, functional)\n    property: Datum (mandatory, functional, DateTime)\n    view: ViewOpFeest (Datu)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (UnknownProperty _ _ _)) -> assert "" true
              otherwise -> assert "The view refers to a non-existing property 'Datu' and that should be detected." false

  test "Testing qualifyPropertyReferences: reference to property on binding." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Feest (mandatory, functional) filledBy: FeestVoorbereiding\n    view: ViewOpFeest (Datum)\n  thing: FeestVoorbereiding (mandatory, functional)\n    property: Datum (mandatory, functional, DateTime)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{views}) -> do
                -- logShow correctedDFR
                -- Get the references from the view
                case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                  Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'" false
                  (Just (View{propertyReferences})) -> case elemIndex (ENP $ EnumeratedPropertyType "Datum") propertyReferences of
                    Nothing -> assert "There should be an unqualified PropertyType 'Datum' in 'model:MyTestDomain$Feest$ViewOpFeest'" true
                    (Just pr) -> do
                      candidates <- runP $ withDomeinFile "model:MyTestDomain" (DomeinFile correctedDFR) ((EnumeratedRoleType "model:MyTestDomain$Feest") ###= (lookForUnqualifiedPropertyType_ "Datum"))
                      case head candidates of
                        Nothing -> assert "We should be able to find the qualified version of 'Datum'" false
                        (Just (ENP (EnumeratedPropertyType "model:MyTestDomain$FeestVoorbereiding$Datum")))  | length candidates == 1 -> assert "" true
                        otherwise -> assert "There is only one property with local name 'Datum' defined but we've found more?!" false
                      xx <- runP $ withDomeinFile "model:MyTestDomain" (DomeinFile correctedDFR) (propertiesOfADT (ST (EnumeratedRoleType "model:MyTestDomain$Feest")))
                      case head xx of
                        Nothing -> assert "geen properties" false
                        (Just p) | length xx == 1 -> assert "The properties of 'model:MyTestDomain$Feest' should include 'model:MyTestDomain$FeestVoorbereiding$Datum'" (p == ENP (EnumeratedPropertyType "model:MyTestDomain$FeestVoorbereiding$Datum"))
                        otherwise -> assert "There is only one property defined for 'Feest'" false

                case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                  Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'" false
                  (Just (View{propertyReferences})) -> case head (filter (\pref -> propertytype2string pref == "model:MyTestDomain$FeestVoorbereiding$Datum") propertyReferences) of
                    (Just (ENP (EnumeratedPropertyType "model:MyTestDomain$FeestVoorbereiding$Datum"))) -> assert "" true
                    otherwise -> assert "There should be a Property 'model:MyTestDomain$FeestVoorbereiding$Datum' in ViewOpFeest" false

  test "Testing qualifyViewReferences: reference to View on Action." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Feest (mandatory, functional) filledBy: FeestVoorbereiding\n    property: BigParty = context >> Guest >>= count > 10\n    view: ViewOpFeest (Datum, BigParty)\n  thing: FeestVoorbereiding (mandatory, functional)\n    property: Datum (mandatory, functional, DateTime)\n  user: Guest (mandatory, functional)\n    perspective on: Feest (ViewOpFeest) Consult" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Guest$ConsultFeest" actions of
                  Nothing -> assert "There should be an Action 'model:MyTestDomain$Guest$ConsultFeest'." false
                  (Just (Action{requiredObjectProperties})) -> case requiredObjectProperties of
                    (Just (ViewType "model:MyTestDomain$Feest$ViewOpFeest")) -> assert "bla" true
                    otherwise -> assert "There should be an Object View" false

  test "Testing qualifyPropertyReferences: reference to a Calculated Property." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  thing: Feest (mandatory, functional) filledBy: FeestVoorbereiding\n    property: NrOfGuests (mandatory, functional, Number)\n    property: BigParty = NrOfGuests > 10\n    view: ViewOpFeest (Datum, BigParty)\n  thing: FeestVoorbereiding (mandatory, functional)\n    property: Datum (mandatory, functional, DateTime)\n  user: Guest (mandatory, functional)\n    perspective on: Feest (ViewOpFeest) Consult" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{views}) -> do
                -- logShow correctedDFR
                -- get the action
                case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                  Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'." false
                  (Just (View{propertyReferences})) -> case elemIndex (CP (CalculatedPropertyType "model:MyTestDomain$Feest$BigParty")) propertyReferences of
                    Nothing -> assert "There should be a CalculatedProperty 'BigParty' in the View 'ViewOpFeest'" false
                    otherwise -> assert "" true

  test "A Context with a Computed Role." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain : MyTestDomain\n  thing : MyRole = callExternal cdb:Models() returns : Modellen\n  case : SubContext\n    thing: Modellen (mandatory, functional)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' do
          ExternalCouchdb.addExternalFunctions
          (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{calculatedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$MyRole" calculatedRoles of
                  Nothing -> assert "There should be a role 'MyRole'" false
                  Just (CalculatedRole{calculation}) -> do
                    assert "The calculation should have '(RDOM (ST EnumeratedRoleType model:MyTestDomain$SubContext$Modellen))' as its Range"
                      case calculation of
                        (Q (MQD _ _ _ (RDOM (ST (EnumeratedRoleType "model:MyTestDomain$SubContext$Modellen"))) _ _)) -> true
                        otherwise -> false
                    assert "The queryfunction of the calculation should be '(ExternalCoreRoleGetter cdb:Models)'"
                      case calculation of
                        (Q (MQD _ (ExternalCoreRoleGetter "couchdb_Models") _ _ _ _)) -> true
                        otherwise -> false

  test "Action with Condition" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Voornaam (mandatory, functional, String)\n    view: AnotherView (Voornaam)\n    perspective on: Party\n      Consult with ViewOnParty\n        subjectView: AnotherView\n        if Party >> Datum > '2019-11-04'\n  thing: Party (mandatory, functional)\n    property: Naam (mandatory, functional, String)\n    property: Datum (mandatory, functional, DateTime)\n    view: ViewOnParty (Naam)\n    view: AnotherView (Datum)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast$ConsultParty" actions of
                  (Just (Action{condition})) -> assert "The condition should have operator '>'"
                    (case condition of
                      (Q (BQD _ (BinaryCombinator GreaterThanF) _ _ _ _ _)) -> true
                      otherwise -> false)
                  otherwise -> assert "There should be an action Consult Party" false

  test "Bot Action with if-then rule with two actions" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop2 (mandatory, functional, Number)\n  thing: Party (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on: Party\n      if Party >> Prop1 > 10 then\n        createRole Gast\n        remove Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeParty" actions of
                  (Just (Action{condition, effect})) -> do
                    assert "The condition should have operator '>'"
                      (case condition of
                        (Q (BQD _ (BinaryCombinator GreaterThanF) _ _ _ _ _)) -> true
                        otherwise -> false)
                    case extractEffect effect of
                      (BQD _ (BinaryCombinator SequenceF) crea rem _ _ _) -> do
                          assert "There should be a CreateRole on Gast" (case crea of
                            (UQD _ (CreateRole (EnumeratedRoleType "model:Test$Gast")) (SQD _ Identity _ _ _) _ _ _) -> true
                            otherwise -> false)
                          assert "There should be a Remove assignment on Gast" (case rem of
                            (UQD _ Remove (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast"))) _ _ _) _ _ _) -> true
                            otherwise -> false)
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a sequence" false

  test "Bot Action with move" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        move C1 >> binding >> context >> Employee to C2 >> binding >> context\n  context: C1 filledBy: Company\n  context: C2 filledBy: Company\n  party: Company\n    user: Employee\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf rle cte _ _ _) -> do
                        assert "The queryfunction should be move" (eq qf QF.Move )
                        assert "The binding should be a rolgetter for model:Test$Company$Employee"
                          case rle of
                            (BQD _ _ _ _ (RDOM (ST (EnumeratedRoleType "model:Test$Company$Employee"))) _ _) -> true
                            otherwise -> false
                        -- logShow cte
                        assert "The role to move should come from C2"
                          case cte of
                            (BQD _ _ (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$C2"))) _ _ _) _ _ _ _) -> true
                            otherwise -> false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with move to current context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  context: C1 filledBy: Company\n  context: C2 filledBy: Company\n  party: Company\n    user: Employee\n    user: Gast (mandatory, functional)\n      property: Prop1 (mandatory, functional, Number)\n    bot: for Gast\n      perspective on: Gast\n        if Gast >> Prop1 > 10 then\n          move extern >> binder C1 >> context >> C2 >> binding >> context >> Employee\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Company$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf rle cte _ _ _) -> do
                        assert "The queryfunction should be move" (eq qf QF.Move )
                        -- logShow rle
                        assert "The role to move should come from the current context, an instance of Company"
                          case cte of
                            (SQD (CDOM (ST (ContextType "model:Test$Company"))) Identity _ _ _) -> true
                            otherwise -> false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with bind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf bndg _ _ _ _) -> do
                        assert "The queryfunction should be bind" (eq qf (Bind $ EnumeratedRoleType "model:Test$EreGast") )
                        case bndg of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter" true
                          otherwise -> assert "The binding should be a rolgetter" true
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bind to role without non-matching possible bindings" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: Organisator\n    property: Prop2\n  user: EreGast filledBy: Organisator\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (RoleDoesNotBind _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error RoleDoesNotBind" false

  test "Bind: binding not a role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast >> Prop1 to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (NotARoleDomain _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotARoleDomain" false

  test "Bind: roletype not in context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to AnotherRole\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (ContextHasNoRole _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error ContextHasNoRole" false

  test "Bot Action with bind in another context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  context: AParty filledBy: Party\n  case: Party\n    user: EreGast filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast in AParty >> binding >> context\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf bndg _ _ _ _) -> do
                        assert "The queryfunction should be bind" (eq qf (Bind $ EnumeratedRoleType "model:Test$Party$EreGast") )
                        case bndg of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter" true
                          otherwise -> assert "The binding should be a rolgetter" true
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bind: in-clause does not select a context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  context: AParty filledBy: Party\n  case: Party\n    user: EreGast filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast in AParty >> binding\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (NotAContextDomain _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotAContextDomain" false

  test "Bind: in-clause does selects non-functional context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, not functional, Number)\n  context: AParty (mandatory, not functional) filledBy: Party\n  case: Party\n    user: EreGast (not mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast in AParty >> binding >> context\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (NotFunctional _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotFunctional" false

  test "Bind: bind to calculated role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  context: AParty filledBy: Party\n  case: Party\n    user: EreGast filledBy: Gast\n    user: Organiser = EreGast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to Organiser in AParty >> binding >> context\n\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (ContextHasNoRole _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error CannotCreateCalculatedRole" false

  test "Bind: bind non-functional to functional role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (NotFunctional _ _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotFunctional" false

  test "Bot Action with bind_" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind_ Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf bndg _ _ _ _) -> do
                        assert "The queryfunction should be bind_" (eq qf Bind_ )
                        case bndg of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter" true
                          otherwise -> assert "The binding should be a rolgetter" true
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bind_: bind non-functional to functional role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        bind_ Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (NotFunctional _ _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotFunctional" false

  test "Bot Action with unqualified unbind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (UQD _ qf bndg _ _ _) -> do
                        assert "The queryfunction should be unbind" (case qf of
                          Unbind Nothing -> true
                          otherwise -> false )
                        case bndg of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter for Gast" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with qualified unbind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast from EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (UQD _ qf bndg _ _ _) -> do
                        assert "The queryfunction should be unbind" (case qf of
                          Unbind (Just (EnumeratedRoleType "model:Test$EreGast")) -> true
                          otherwise -> false )
                        case bndg of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter for Gast" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Unbind: non-existing binder" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Gast\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast from AnotherRole\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (UnknownRole _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error UnknownRole" false

  test "Unbind: binder does not bind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: EreGast (mandatory, functional) filledBy: Organisator\n  user: Organisator\n    property: Prop2\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast from EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (LocalRoleDoesNotBind _ _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error LocalRoleDoesNotBind" false

  test "Bot Action with delete role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        delete Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (UQD _ qf bndg _ _ _) -> do
                        assert "The queryfunction should be DeleteRole" (case qf of
                          (DeleteRole _) -> true
                          otherwise -> false )
                        case bndg of
                          (SQD _ Identity _ _ _) ->
                            assert "The binding should be Identity" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with delete property and default object" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        delete property Prop1\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (UQD _ qf bndg _ _ _) -> do
                        assert "The queryfunction should be DeleteProperty" (case qf of
                          QF.DeleteProperty (EnumeratedPropertyType "model:Test$Gast$Prop1") -> true
                          otherwise -> false )
                        case bndg of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter for Gast" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with delete property, property doesn't exist." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        delete property AnotherProp\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (RoleHasNoProperty _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error RoleHasNoProperty" false

  test "Bot Action with delete property, property is calculated." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n    property: Prop2 = Prop1\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        delete property Prop2\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (CannotCreateCalculatedProperty _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error CannotCreateCalculatedProperty" false

  test "Bot Action with property assignment and default object" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n    property: Prop2 = Prop1\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        Prop1 =+ 10\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf val role _ _ _) -> do
                        assert "The queryfunction should be AddPropertyValue" (case qf of
                          QF.AddPropertyValue _-> true
                          otherwise -> false )
                        assert "The value should be a Constant"
                          (case val of
                            (SQD _ (Constant PNumber "10") _ _ _) -> true
                            otherwise -> false)
                        case role of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                            assert "The binding should be a rolgetter for Gast" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with property assignment and wrong value range." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n    property: Prop2 = Prop1\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        Prop1 =+ true\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (WrongPropertyRange _ _ PNumber PBool)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error WrongPropertyRange" false

  test "Bot Action with property assignment and not even a property range." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n    property: Prop2 = Prop1\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        Prop1 =+ Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left (NotAPropertyRange _ _ PNumber)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotAPropertyRange" false

  test "Bot Action with property assignment on another role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, not functional)\n    property: Prop1 (mandatory, functional, Number)\n  user: Organiser\n    property: Prop2 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        Prop2 =+ 10 for Organiser\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (BQD _ qf val role _ _ _) -> do
                        assert "The queryfunction should be AddPropertyValue" (case qf of
                          QF.AddPropertyValue _ -> true
                          otherwise -> false )
                        assert "The value should be a Constant"
                          (case val of
                            (SQD _ (Constant PNumber "10") _ _ _) -> true
                            otherwise -> false)
                        case role of
                          (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Organiser")))_ _ _) ->
                            assert "The binding should be a rolgetter for Organiser" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with callEffect" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: Test\n  user: Gast (mandatory, functional)\n    property: Prop1 (mandatory, functional, Number)\n  thing: AModel\n    property: Name (mandatory, functional, String)\n  bot: for Gast\n    perspective on: Gast\n      if Gast >> Prop1 > 10 then\n        callEffect cdb:AddModelToLocalStore( AModel >> Name )\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP do
              ExternalCouchdb.addExternalFunctions
              phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (MQD _ qf args _ _ _) -> do
                        assert "The queryfunction should be ExternalEffectFullFunction" (eq qf (ExternalEffectFullFunction "couchdb_AddModelToLocalStore") )
                        assert "There should be one argument" (length args == 1)
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

extractEffect :: Maybe SideEffect -> QueryFunctionDescription
extractEffect = unsafePartial extractEffect_
  where
    extractEffect_ :: Partial => Maybe SideEffect -> QueryFunctionDescription
    extractEffect_ (Just (EF (UQD _ _ (BQD _ _ _ mqd _ _ _) _ _ _))) = mqd

x :: DomeinFileRecord -> MonadPerspectives (Array RoleType)
x correctedDFR = withDomeinFile "model:MyTestDomain"
  (DomeinFile correctedDFR)
  ((runTypeLevelToArray (ST (ContextType "model:MyTestDomain")) (lookForUnqualifiedRoleTypeOfADT "Guest")))
