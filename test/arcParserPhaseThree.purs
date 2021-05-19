module Test.Parsing.Arc.PhaseThree where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (elemIndex, filter, find, head, length)
import Data.Array (fromFoldable) as ARR
import Data.Either (Either(..))
import Data.Lens (_Just, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map, lookup, values) as Map
import Data.Map (showTree)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Set (subset, fromFoldable)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff, throwError, error)
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, runTypeLevelToArray, (###=))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (Namespace)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..)) as AST
import Perspectives.Parsing.Arc.Expression.AST (Step(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo', runPhaseTwo')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), queryFunction, range, secondOperand)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getPerspectType)
import Perspectives.Representation.Class.Role (allProperties)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), subsetPSet)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType(..), propertytype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb, RoleVerbList(..))
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType_, lookForUnqualifiedRoleType, lookForUnqualifiedRoleTypeOfADT, roleInContext)
import Perspectives.Utilities (prettyPrint)
import Test.Parsing.DomeinFileSelectors (ensureCRole, ensureDescription, ensureERole, ensureEnumeratedProperty, ensurePerspectiveOn, ensurePropertyVerbsInState, ensureState, enumeratedPropertyIsFunctional, exists, failure, haveVerbs, isCalculationOf, objectOfPerspective, stateQuery)
import Test.Perspectives.Utils (runP)
import Test.Unit (Test, TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

withDomeinFile :: forall a. Namespace -> DomeinFile -> MonadPerspectives a -> MonadPerspectives a
withDomeinFile ns df mpa = do
  void $ storeDomeinFileInCache ns df
  r <- mpa
  removeDomeinFileFromCache ns
  pure r

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Parsing.Arc.PhaseThree" do
  -- test "TypeLevelObjectGetters" do
  --   (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultAnotherRole\n        IndirectObjectRef : AnotherRole\n  Role : RoleInContext : AnotherRole\n    Calculation : context >> Role" domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE{id})) -> do
  --       case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
  --         (Left e) -> assert (show e) false
  --         (Right df@(DomeinFile dr')) -> do
  --           -- logShow dr'
  --           (Context {_id}) <- runP $
  --             withDomeinFile "model:MyTestDomain" df
  --               (getPerspectType (ContextType "model:MyTestDomain"))
  --           assert "Should be able to retrieve the context that represents the domain"
  --             (_id == ContextType "model:MyTestDomain")
  --           (CalculatedRole {_id:crid}) <- runP $
  --             withDomeinFile "model:MyTestDomain" df
  --               (getPerspectType (CalculatedRoleType "model:MyTestDomain$AnotherRole"))
  --           assert "Should be able to retrieve the role AnotherRole"
  --             (crid == CalculatedRoleType "model:MyTestDomain$AnotherRole")
  --
  --           roles <- runP $
  --             withDomeinFile "model:MyTestDomain" df
  --               ((ContextType "model:MyTestDomain") ###= roleInContext)
  --           assert "roleInContext should be able to retrieve the role AnotherRole from the context model:MyTestDomain."
  --             case head roles of
  --               (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
  --               otherwise -> false
  --
  --           roles'' <- runP $
  --             withDomeinFile "model:MyTestDomain" df
  --               ((ContextType "model:MyTestDomain") ###= (lookForUnqualifiedRoleType "AnotherRole"))
  --           assert "lookForUnqualifiedRoleType should be able to retrieve the role AnotherRole from the context model:MyTestDomain."
  --             (isJust (head roles''))

  -- test "Testing qualifyActionRoles." do
  --   (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultAnotherRole\n        IndirectObjectRef : AnotherRole\n  Role : RoleInContext : AnotherRole\n    Calculation : blabla" domain

  test "Testing qualifyActionRoles." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Role (mandatory, functional) filledBy YetAnotherRole\n  thing AnotherRole = Role >> binding\n  thing YetAnotherRole (mandatory, functional)\n  user SomeUser\n    perspective on AnotherRole\n      all roleverbs" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr)) -> do
            (runP $ phaseThree dr state.postponedStateQualifiedParts) >>=
              case _ of
                (Left e) -> assert (show e) false
                (Right dr') -> do
                  -- logShow dr'
                  ensureCRole "model:MyTestDomain$AnotherRole" dr' >>= exists
                  ensureERole "model:MyTestDomain$SomeUser" dr' >>=
                    ensurePerspectiveOn "model:MyTestDomain$YetAnotherRole" >>=
                      objectOfPerspective >>=
                        isCalculationOf (RDOM (ST (EnumeratedRoleType "model:MyTestDomain$YetAnotherRole")))

  test "Testing qualifyActionRoles: External." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  user Driver\n    perspective on External\n      all roleverbs" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left e) -> assert (show e) false
              (Right correctedDFR) -> do
                -- logShow correctedDFR
                ensureERole "model:MyTestDomain$Driver" correctedDFR >>=
                  ensurePerspectiveOn "model:MyTestDomain$External" >>=
                    exists


  test "Testing qualifyActionRoles: ContextHasNoRole." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  user Gast (mandatory, functional)\n    perspective on SomeRole\n      all roleverbs\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left (ContextHasNoRole _ _)) -> assert "" true
              otherwise -> do
                logShow otherwise
                assert "Expected the 'ContextHasNoRole' error" false

  test "Testing qualifyBindings: correct reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory, functional) filledBy Bound\n  thing Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                  Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                  (Just (EnumeratedRole{binding})) -> assert
                    "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                    (binding == ST (EnumeratedRoleType "model:MyTestDomain$Bound"))

  test "Testing qualifyBindings: qualified reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory, functional) filledBy model:MyTestDomain$Bound\n  thing Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                  Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                  (Just (EnumeratedRole{binding})) -> assert
                    "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                    (binding == ST (EnumeratedRoleType "model:MyTestDomain$Bound"))

  test "Testing qualifyBindings: prefixed reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use my for model:MyTestDomain\n  thing Binder (mandatory, functional) filledBy my:Bound\n  thing Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                  Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                  (Just (EnumeratedRole{binding})) -> assert
                    "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                    (binding == ST (EnumeratedRoleType "model:MyTestDomain$Bound"))

  test "Testing qualifyBindings: missing reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory, functional) filledBy Bount\n  thing Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left (UnknownRole _ _)) -> assert "" true
              otherwise -> do
                assert "The binding of 'Binder' is not defined and that should have been detected." false

  test "Testing qualifyBindings: two candidates for binding." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory, functional) filledBy Bound\n  thing Bound (mandatory, functional)\n  case Nested\n    thing Bound (mandatory, functional)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            -- logShow x'
            case _ of
              (Left e@(NotUniquelyIdentifying _ _ _)) -> do
                -- logShow e
                assert "" true
              otherwise -> do
                assert "The binding of 'Binder' is not defined and that should have been detected." false

  test "Testing qualifyPropertyReferences: correct reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory, functional)\n    property Datum (mandatory, functional, DateTime)\n    view ViewOpFeest (Datum)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            (runP $ phaseThree dr' state.postponedStateQualifiedParts) >>=
            case _ of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{views}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                  Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'" false
                  (Just (View{propertyReferences})) -> case head (filter (\pref -> propertytype2string pref == "model:MyTestDomain$Feest$Datum") propertyReferences) of
                    (Just (ENP (EnumeratedPropertyType "model:MyTestDomain$Feest$Datum"))) -> assert "" true
                    otherwise -> assert "There should be a Property 'model:MyTestDomain$Feest$Datum' in ViewOpFeest" false
                ensureEnumeratedProperty "model:MyTestDomain$Feest$Datum" correctedDFR >>=
                  enumeratedPropertyIsFunctional true


  test "Testing qualifyPropertyReferences: incorrect reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory, functional)\n    property Datum (mandatory, functional, DateTime)\n    view ViewOpFeest (Datu)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (UnknownProperty _ _ _)) -> assert "" true
              otherwise -> assert "The view refers to a non-existing property 'Datu' and that should be detected." false

  test "Testing qualifyPropertyReferences: reference to property on binding." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory, functional) filledBy FeestVoorbereiding\n    view ViewOpFeest (Datum)\n  thing FeestVoorbereiding (mandatory, functional)\n    property Datum (mandatory, functional, DateTime)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
                      xx <- runP $ withDomeinFile "model:MyTestDomain" (DomeinFile correctedDFR) (allProperties (ST (EnumeratedRoleType "model:MyTestDomain$Feest")))
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
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory, functional) filledBy FeestVoorbereiding\n    property BigParty = context >> Guest >>= count > 10\n    view ViewOpFeest (Datum, BigParty)\n  thing FeestVoorbereiding (mandatory, functional)\n    property Datum (mandatory, functional, DateTime)\n  user Guest (mandatory, functional)\n    perspective on Feest\n      view ViewOpFeest (Consult)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left e) -> assert (show e) false
              Right dfr ->
                ensureERole "model:MyTestDomain$Guest" dfr >>=
                  ensurePerspectiveOn "model:MyTestDomain$Feest" >>=
                    ensurePropertyVerbsInState "model:MyTestDomain$Guest" >>=
                      Universal `haveVerbs` [Consult]


  test "Testing qualifyPropertyReferences: reference to a Calculated Property." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory, functional) filledBy FeestVoorbereiding\n    property NrOfGuests (mandatory, functional, Number)\n    property BigParty = NrOfGuests > 10\n    view ViewOpFeest (Datum, BigParty)\n  thing FeestVoorbereiding (mandatory, functional)\n    property Datum (mandatory, functional, DateTime)\n  user Guest (mandatory, functional)\n    perspective on Feest\n      view ViewOpFeest (Consult)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole = callExternal cdb:Models() returns Modellen\n  case SubContext\n    thing Modellen (mandatory, functional)\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error: " <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert ("PhaseTwo error:" <> show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left e) -> assert ("PhaseThree error:" <> show e) false
              (Right correctedDFR@{calculatedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$MyRole" calculatedRoles of
                  Nothing -> assert "There should be a role 'MyRole'" false
                  Just (CalculatedRole{calculation}) ->
                  case calculation of
                    Q c -> do
                      assert "The calculation should have '(RDOM (ST EnumeratedRoleType model:MyTestDomain$SubContext$Modellen))' as its Range"
                        (range c == (RDOM (ST (EnumeratedRoleType "model:MyTestDomain$SubContext$Modellen"))))
                      assert "The queryfunction of the calculation should be '(ExternalCoreRoleGetter model:Couchdb$Models)'"
                        case queryFunction <$> secondOperand c of
                          Just (ExternalCoreRoleGetter "model:Couchdb$Models") -> true
                          otherwise -> false
                    otherwise -> assert "calculation is not compiled" false


  test "Perspective in state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Voornaam (mandatory, functional, String)\n    view AnotherView (Voornaam)\n    in state Party$LateParty\n      --perspective on Party\n        --view ViewOnParty (Consult)\n  thing Party (mandatory, functional)\n    state LateParty = Datum > '2019-11-04'\n      perspective of Gast\n        view ViewOnParty (Consult)\n    property Naam (mandatory, functional, String)\n    property Datum (mandatory, functional, DateTime)\n    view ViewOnParty (Naam)\n    view AnotherView (Datum)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR) -> do
                -- logShow correctedDFR
                ensureERole "model:Test$Gast" correctedDFR >>=
                  ensurePerspectiveOn "model:Test$Party" >>=
                    ensurePropertyVerbsInState "model:Test$Party$LateParty" >>=
                      exists

  test "Automatic effect on entry (two assignments)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop2 (mandatory, functional, Number)\n  thing Party (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do for Gast\n          Prop2 = 10 for Gast\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt "model:") >>= \(Tuple r state) ->
        case r of
          (Left e) -> assert ("PhaseTwo error:" <> show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              Left e -> assert ("PhaseThree error:" <> show e) false
              Right correctedDFR -> do
                -- logShow correctedDFR
                ensureEnumeratedProperty "model:Test$Party$Prop1" correctedDFR >>=
                  enumeratedPropertyIsFunctional true
                ensureState "model:Test$Party$SomeState" correctedDFR >>=
                  stateQuery >>=
                    ensureDescription >>=
                      \qfd -> do
                        case queryFunction <$> secondOperand qfd of
                          Just (BinaryCombinator GreaterThanF) -> pure unit
                          _ -> failure "The condition should have operator '>'"

                -- case lookup "model:Test$Gast_bot$ChangeParty" actions of
                --   (Just (Action{condition, effect})) -> do
                --     assert "The condition should have operator '>'"
                --       (case condition of
                --         (Q (BQD _ (BinaryCombinator GreaterThanF) _ _ _ _ _)) -> true
                --         otherwise -> false)
                --     case extractEffect effect of
                --       (BQD _ (BinaryCombinator SequenceF) crea rem _ _ _) -> do
                --           assert "There should be a CreateRole on Gast" (case crea of
                --             (UQD _ (CreateRole (EnumeratedRoleType "model:Test$Gast")) (SQD _ (DataTypeGetter IdentityF) _ _ _) _ _ _) -> true
                --             otherwise -> false)
                --           assert "There should be a Remove assignment on Gast" (case rem of
                --             (UQD _ Remove (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast"))) _ _ _) _ _ _) -> true
                --             otherwise -> false)
                --       otherwise -> assert "Side effect expected" false
                --   Nothing -> assert "The effect should compile to a sequence" false
{-

  test "Bot Action with move" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        move C1 >> binding >> context >> Employee to C2 >> binding >> context\n  context: C1 filledBy Company\n  context: C2 filledBy Company\n  party: Company\n    user Employee\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  context: C1 filledBy Company\n  context: C2 filledBy Company\n  party: Company\n    user Employee\n    user Gast (mandatory, functional)\n      property Prop1 (mandatory, functional, Number)\n    bot: for Gast\n      perspective on Gast\n        if Gast >> Prop1 > 10 then\n          move extern >> binder C1 >> context >> C2 >> binding >> context >> Employee\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
                            (SQD (CDOM (ST (ContextType "model:Test$Company"))) (DataTypeGetter IdentityF) _ _ _) -> true
                            otherwise -> false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with bind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  user Organisator\n    property Prop2\n  user EreGast filledBy Organisator\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (RoleDoesNotBind _ _ _)) -> assert "ok" true
              otherwise -> do
                logShow otherwise
                assert "Expected the error RoleDoesNotBind" false

  test "Bind: binding not a role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast >> Prop1 to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (NotARoleDomain _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotARoleDomain" false

  test "Bind: roletype not in context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to AnotherRole\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (ContextHasNoRole _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error ContextHasNoRole" false

  test "Bot Action with bind in another context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  context: AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast in AParty >> binding >> context\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  context: AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast in AParty >> binding\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (NotAContextDomain _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotAContextDomain" false

  test "Bind: in-clause does selects non-functional context" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, not functional, Number)\n  context: AParty (mandatory, not functional) filledBy Party\n  case Party\n    user EreGast (not mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast in AParty >> binding >> context\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (NotFunctional _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotFunctional" false

  test "Bind: bind to calculated role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  context: AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n    user Organiser = EreGast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to Organiser in AParty >> binding >> context\n\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                assert "Expected the error CannotCreateCalculatedRole" false

  test "Bind: bind non-functional to functional role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (NotFunctional _ _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotFunctional" false

  test "Bot Action with bind_" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind_ Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        bind_ Gast to EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (NotFunctional _ _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotFunctional" false

  test "Bot Action with unqualified unbind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast from EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Gast\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast from AnotherRole\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (UnknownRole _ _)) -> assert "ok" true
              -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error UnknownRole" false

  test "Unbind: binder does not bind" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user EreGast (mandatory, functional) filledBy Organisator\n  user Organisator\n    property Prop2\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        unbind Gast from EreGast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (LocalRoleDoesNotBind _ _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error LocalRoleDoesNotBind" false

  test "Bot Action with delete role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        delete Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
                          (SQD _ (DataTypeGetter IdentityF) _ _ _) ->
                            assert "The binding should be (DataTypeGetter IdentityF)" true
                          otherwise -> assert "The binding should be a rolgetter" false
                      otherwise -> assert "Side effect expected" false
                  Nothing -> assert "The effect should compile to a binary query function" false

  test "Bot Action with delete property and default object" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        delete property Prop1\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        delete property AnotherProp\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (RoleHasNoProperty _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error RoleHasNoProperty" false

  test "Bot Action with delete property, property is calculated." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n    property Prop2 = Prop1\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        delete property Prop2\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (CannotCreateCalculatedProperty _ _ _)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error CannotCreateCalculatedProperty" false

  test "Bot Action with property assignment and default object" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n    property Prop2 = Prop1\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        Prop1 =+ 10\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n    property Prop2 = Prop1\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        Prop1 =+ true\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (WrongPropertyRange _ _ PNumber PBool)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error WrongPropertyRange" false

  test "Bot Action with property assignment and not even a property range." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n    property Prop2 = Prop1\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        Prop1 =+ Gast\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
            case x' of
              (Left (NotAPropertyRange _ _ PNumber)) -> assert "ok" true
              otherwise -> do
                -- logShow otherwise
                assert "Expected the error NotAPropertyRange" false

  test "Bot Action with property assignment on another role" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, not functional)\n    property Prop1 (mandatory, functional, Number)\n  user Organiser\n    property Prop2 (mandatory, functional, Number)\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        Prop2 =+ 10 for Organiser\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts
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
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain Test\n  user Gast (mandatory, functional)\n    property Prop1 (mandatory, functional, Number)\n  thing AModel\n    property Name (mandatory, functional, String)\n  bot: for Gast\n    perspective on Gast\n      if Gast >> Prop1 > 10 then\n        callEffect cdb:AddModelToLocalStore( AModel >> Name )\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x' <- runP do

              phaseThree dr'
            case x' of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{actions}) -> do
                -- logShow correctedDFR
                case lookup "model:Test$Gast_bot$ChangeGast" actions of
                  Just (Action{effect}) -> do
                    case extractEffect effect of
                      (MQD _ qf args _ _ _) -> do
                        assert "The queryfunction should be ExternalEffectFullFunction" (eq qf (ExternalEffectFullFunction "model:Couchdb$AddModelToLocalStore") )
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
-}
