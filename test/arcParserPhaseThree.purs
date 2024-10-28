module Test.Parsing.Arc.PhaseThree where 

import Prelude

import Control.Monad.Free (Free)
import Data.Array (elemIndex, filter, head, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup)
import Node.Encoding as ENC
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Perspectives.CoreTypes (MonadPerspectives, (###=))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (Namespace)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), queryFunction, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (effectOfAction)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Role (allProperties)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (StateSpec(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), DomeinFileId(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..), propertytype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType_)
import Test.Parsing.DomeinFileSelectors (ensureCRole, ensureDescription, ensureERole, ensureEnumeratedProperty, ensurePerspectiveOn, ensurePropertyVerbsInState, ensureState, enumeratedPropertyIsFunctional, exists, failure, haveVerbs, isCalculationOf, objectOfPerspective, stateQuery, ensureOnEntry)
import Test.Perspectives.Utils (runP)
import Test.Unit (Test, TestF, TestSuite, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Parsing (ParseError)

withDomeinFile :: forall a. Namespace -> DomeinFile -> MonadPerspectives a -> MonadPerspectives a
withDomeinFile ns df mpa = do
  void $ storeDomeinFileInCache (DomeinFileId ns) df
  r <- mpa
  removeDomeinFileFromCache (DomeinFileId ns)
  pure r

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.PhaseThree" do

  test "Testing qualifyActionRoles." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Role (mandatory) filledBy YetAnotherRole\n  thing AnotherRole = Role >> binding\n  thing YetAnotherRole (mandatory)\n  user SomeUser\n    perspective on AnotherRole\n      all roleverbs" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr)) -> do
              (runP $ phaseThree dr state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple dr' _)) -> do
                    -- logShow dr'
                    ensureCRole "model:MyTestDomain$AnotherRole" dr' >>= exists
                    ensureERole "model:MyTestDomain$SomeUser" dr' >>=
                      ensurePerspectiveOn "model:MyTestDomain$YetAnotherRole" >>=
                        objectOfPerspective >>=
                          isCalculationOf (RDOM (ST $ RoleInContext {context: (ContextType "model:MyTestDomain"), role: (EnumeratedRoleType "model:MyTestDomain$YetAnotherRole")}))

  test "Testing qualifyActionRoles: External." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  user Driver\n    perspective on External\n      all roleverbs" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR _)) -> do
                    -- logShow correctedDFR
                    ensureERole "model:MyTestDomain$Driver" correctedDFR >>=
                      ensurePerspectiveOn "model:MyTestDomain$External" >>=
                        exists


  test "Testing qualifyActionRoles: ContextHasNoRole." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  user Gast (mandatory)\n    perspective on SomeRole\n      all roleverbs\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left [(ContextHasNoRole _ _ _ _)]) -> assert "" true
                  otherwise -> do
                    logShow otherwise
                    assert "Expected the 'ContextHasNoRole' error" false

  test "Testing qualifyBindings: correct reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory) filledBy Bound\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{enumeratedRoles} _)) -> do
                    -- logShow correctedDFR
                    case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                      Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                      (Just (EnumeratedRole{binding})) -> assert
                        "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                        (binding == (Just $ ST (RoleInContext {context: (ContextType "model:MyTestDomain"), role: (EnumeratedRoleType "model:MyTestDomain$Bound")})))

  test "Testing qualifyBindings: qualified reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory) filledBy model:MyTestDomain$Bound\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{enumeratedRoles} _)) -> do
                    -- logShow correctedDFR
                    case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                      Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                      (Just (EnumeratedRole{binding})) -> assert
                        "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                        (binding == (Just $ ST (RoleInContext {context: (ContextType "model:MyTestDomain"), role: (EnumeratedRoleType "model:MyTestDomain$Bound")})))

  test "Testing qualifyBindings: prefixed reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  use my for model:MyTestDomain\n  thing Binder (mandatory) filledBy my:Bound\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{enumeratedRoles} _)) -> do
                    -- logShow correctedDFR
                    case lookup "model:MyTestDomain$Binder" enumeratedRoles of
                      Nothing -> assert "The model should have role 'model:MyTestDomain$Binder'." false
                      (Just (EnumeratedRole{binding})) -> assert
                        "The binding of 'model:MyTestDomain$Binder' should be 'model:MyTestDomain$Bound'"
                        (binding == (Just $ ST (RoleInContext {context: (ContextType "model:MyTestDomain"), role: (EnumeratedRoleType "model:MyTestDomain$Bound")})))

  test "Testing qualifyBindings: missing reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory) filledBy Bount\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left [(UnknownRole _ _)]) -> assert "" true
                  otherwise -> do
                    assert "The binding of 'Binder' is not defined and that should have been detected." false

  test "Testing qualifyBindings: two candidates for binding." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Binder (mandatory) filledBy Bound\n  thing Bound (mandatory)\n  case Nested\n    thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
              -- logShow x'
                case _ of
                  (Left [e@(NotUniquelyIdentifying _ _ _)]) -> do
                    -- logShow e
                    assert "" true
                  otherwise -> do
                    assert "The binding of 'Binder' is not defined and that should have been detected." false

  test "Testing qualifyPropertyReferences: correct reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory)\n    property Datum (mandatory, DateTime)\n    view ViewOpFeest (Datum)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{views} _)) -> do
                    -- logShow correctedDFR
                    case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                      Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'" false
                      (Just (View{propertyReferences})) -> case head (filter (\pref -> propertytype2string pref == "model:MyTestDomain$Feest$Datum") propertyReferences) of
                        (Just (ENP (EnumeratedPropertyType "model:MyTestDomain$Feest$Datum"))) -> assert "" true
                        otherwise -> assert "There should be a Property 'model:MyTestDomain$Feest$Datum' in ViewOpFeest" false
                    ensureEnumeratedProperty "model:MyTestDomain$Feest$Datum" correctedDFR >>=
                      enumeratedPropertyIsFunctional true


  test "Testing qualifyPropertyReferences: incorrect reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory)\n    property Datum (mandatory, DateTime)\n    view ViewOpFeest (Datu)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left [(UnknownProperty _ _ _)]) -> assert "" true
                otherwise -> assert "The view refers to a non-existing property 'Datu' and that should be detected." false

  test "Testing qualifyPropertyReferences: reference to property on binding." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory) filledBy FeestVoorbereiding\n    view ViewOpFeest (Datum)\n  thing FeestVoorbereiding (mandatory)\n    property Datum (mandatory, DateTime)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                Right (Tuple correctedDFR@{views} _) -> do
                  -- logShow correctedDFR
                  -- Get the references from the view
                  case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                    Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'" false
                    (Just (View{propertyReferences})) -> case elemIndex (ENP $ EnumeratedPropertyType "Datum") propertyReferences of
                      Nothing -> assert "There should be an unqualified PropertyType 'Datum' in 'model:MyTestDomain$Feest$ViewOpFeest'" true
                      (Just pr) -> do
                        candidates <- runP $ withDomeinFile "model:MyTestDomain" (DomeinFile correctedDFR) ((ENR $ EnumeratedRoleType "model:MyTestDomain$Feest") ###= (lookForUnqualifiedPropertyType_ "Datum"))
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
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory) filledBy FeestVoorbereiding\n    property BigParty = context >> Guest >>= count > 10\n    view ViewOpFeest (Datum, BigParty)\n  thing FeestVoorbereiding (mandatory)\n    property Datum (mandatory, DateTime)\n  user Guest (mandatory)\n    perspective on Feest\n      view ViewOpFeest (Consult)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                Right (Tuple dfr _) ->
                  ensureERole "model:MyTestDomain$Guest" dfr >>=
                    ensurePerspectiveOn "model:MyTestDomain$Feest" >>=
                      ensurePropertyVerbsInState (SubjectState $ StateIdentifier "model:MyTestDomain$Guest") >>=
                        Universal `haveVerbs` [Consult]


  test "Testing qualifyPropertyReferences: reference to a Calculated Property." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing Feest (mandatory) filledBy FeestVoorbereiding\n    property NrOfGuests (mandatory, Number)\n    property BigParty = NrOfGuests > 10\n    view ViewOpFeest (Datum, BigParty)\n  thing FeestVoorbereiding (mandatory)\n    property Datum (mandatory, DateTime)\n  user Guest (mandatory)\n    perspective on Feest\n      view ViewOpFeest (Consult)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                Right (Tuple correctedDFR@{views} _) -> do
                  -- logShow correctedDFR
                  -- get the action
                  case lookup "model:MyTestDomain$Feest$ViewOpFeest" views of
                    Nothing -> assert "There should be a View 'model:MyTestDomain$Feest$ViewOpFeest'." false
                    (Just (View{propertyReferences})) -> case elemIndex (CP (CalculatedPropertyType "model:MyTestDomain$Feest$BigParty")) propertyReferences of
                      Nothing -> assert "There should be a CalculatedProperty 'BigParty' in the View 'ViewOpFeest'" false
                      otherwise -> assert "" true

  test "A Context with a Computed Role." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain MyTestDomain\n  thing MyRole = callExternal cdb:Models() returns Modellen\n  case SubContext\n    thing Modellen (mandatory)\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error: " <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert ("PhaseThree error:" <> show e) false
                (Right (Tuple correctedDFR@{calculatedRoles} _)) -> do
                  -- logShow correctedDFR
                  case lookup "model:MyTestDomain$MyRole" calculatedRoles of
                    Nothing -> assert "There should be a role 'MyRole'" false
                    Just (CalculatedRole{calculation}) ->
                      case calculation of
                        Q c -> do
                          assert "The calculation should have '(RDOM (ST EnumeratedRoleType model:MyTestDomain$SubContext$Modellen))' as its Range"
                            (range c == (RDOM (ST $ RoleInContext {context: (ContextType "model:MyTestDomain$SubContext"), role: (EnumeratedRoleType "model:MyTestDomain$SubContext$Modellen")})))
                          assert "The queryfunction of the calculation should be '(ExternalCoreRoleGetter model:Couchdb$Models)'"
                            case queryFunction c of
                              (ExternalCoreRoleGetter "model:Couchdb$Models") -> true
                              otherwise -> false
                        otherwise -> assert "calculation is not compiled" false


  test "Perspective in state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    property Voornaam (mandatory, String)\n    view AnotherView (Voornaam)\n    in state Party$LateParty\n      --perspective on Party\n        --view ViewOnParty (Consult)\n  thing Party (mandatory)\n    state LateParty = Datum > '2019-11-04'\n      perspective of Gast\n        view ViewOnParty (Consult)\n    property Naam (mandatory, String)\n    property Datum (mandatory, DateTime)\n    view ViewOnParty (Naam)\n    view AnotherView (Datum)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                (Right (Tuple correctedDFR _)) -> do
                  -- logShow correctedDFR
                  ensureERole "model:Test$Gast" correctedDFR >>=
                    ensurePerspectiveOn "model:Test$Party" >>=
                      ensurePropertyVerbsInState (ObjectState $ StateIdentifier "model:Test$Party$LateParty") >>=
                        exists

  test "Automatic effect on entry (two assignments)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    property Prop2 (mandatory, Number)\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do for Gast\n          Prop2 = 10 for Gast\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  -- logShow correctedDFR
                  ensureEnumeratedProperty "model:Test$Party$Prop1" correctedDFR >>=
                    enumeratedPropertyIsFunctional true
                  ensureState "model:Test$Party$SomeState" correctedDFR >>=
                    stateQuery >>=
                      ensureDescription >>=
                        \qfd -> do
                          case queryFunction qfd of
                            (BinaryCombinator GreaterThanF) -> pure unit
                            _ -> failure "The condition should have operator '>'"

  test "Automatic effect on entry (delete property)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    property Prop2 (mandatory, Number)\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do for Gast\n          delete property Prop2 from Gast\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  -- logShow correctedDFR
                  ensureEnumeratedProperty "model:Test$Party$Prop1" correctedDFR >>=
                    enumeratedPropertyIsFunctional true
                  ensureState "model:Test$Party$SomeState" correctedDFR >>=
                    stateQuery >>=
                      ensureDescription >>=
                        \qfd -> do
                          case queryFunction qfd of
                            (BinaryCombinator GreaterThanF) -> pure unit
                            _ -> failure "The condition should have operator '>'"

  test "Automatic effect on entry (use of object state)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    property Prop2 (mandatory, Number)\n    perspective on Party\n      on entry of object state SomeState\n        do\n          Prop1 = 10\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  -- logShow correctedDFR
                  ensureEnumeratedProperty "model:Test$Party$Prop1" correctedDFR >>=
                    enumeratedPropertyIsFunctional true
                  ensureState "model:Test$Party$SomeState" correctedDFR >>=
                    stateQuery >>=
                      ensureDescription >>=
                        \qfd -> do
                          -- logShow qfd
                          case queryFunction qfd of
                            (BinaryCombinator GreaterThanF) -> pure unit
                            _ -> failure "The condition should have operator '>'"

  test "Bot Action with move" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry \n        do for Gast\n          move C1 >> binding >> context >> Employee to C2 >> binding >> context\n  context C1 filledBy Company\n  context C2 filledBy Company\n  party Company\n    user Employee\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  ensureState "model:Test$Gast$SomeState" correctedDFR >>=
                    ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
                      (case _ of
                        (BQD _ qf rle cte _ _ _) -> do
                          assert "The queryfunction should be move" (eq qf QF.Move )
                          assert "The binding should be a rolgetter for model:Test$Company$Employee"
                            case rle of
                              (BQD _ _ _ _ (RDOM (ST (RoleInContext{context: (ContextType "model:Test$Company"), role: EnumeratedRoleType "model:Test$Company$Employee"}))) _ _) -> true
                              otherwise -> false
                          -- logShow cte
                          assert "The role to move should come from C2"
                            case cte of
                              (BQD _ _ (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$C2"))) _ _ _) _ _ _ _) -> true
                              otherwise -> false
                        otherwise -> assert "Side effect expected" false)

  test "Bot Action with move to current context" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  context C1 filledBy Company\n  context C2 filledBy Company\n  party Company\n    user Employee\n    user Gast (mandatory)\n      property Prop1 (mandatory, Number)\n      state SomeState = Prop1 > 10\n        on entry\n          do\n            move extern >> binder C1 >> context >> C2 >> binding >> context >> Employee\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) ->
                  ensureState "model:Test$Company$Gast$SomeState" correctedDFR >>=
                    ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Company$Gast")) >>= pure <<< effectOfAction >>=
                      (case _ of
                        (BQD _ qf rle cte _ _ _) -> do
                          assert "The queryfunction should be move" (eq qf QF.Move )
                          -- logShow rle
                          assert "The role to move should come from the current context, an instance of Company"
                            case cte of
                              (SQD (CDOM (ST (ContextType "model:Test$Company"))) (DataTypeGetter IdentityF) _ _ _) -> true
                              otherwise -> false
                        otherwise -> assert "Side effect expected" false)

  domainTest "Bot Action with bind"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to EreGast\n  user EreGast filledBy Gast"
    \correctedDFR ->
      ensureState "model:Test$Gast$SomeState" correctedDFR >>=
        ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
          case _ of
            (BQD _ qf bndg _ _ _ _) -> do
              assert "The queryfunction should be bind" (eq qf (Bind $ EnumeratedRoleType "model:Test$EreGast") )
              case bndg of
                (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                  assert "The binding should be a rolgetter" true
                otherwise -> assert "The binding should be a rolgetter" true
            otherwise -> assert "Side effect expected" false

  expectError "Bind to role without non-matching possible bindings"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to EreGast\n  user Organisator\n    property Prop2\n  user EreGast filledBy Organisator\n"
    case _ of
      (Left (RoleDoesNotBind _ _ _)) -> assert "ok" true
      otherwise -> do
        logShow otherwise
        assert "Expected the error RoleDoesNotBind" false

  expectError "Bind: binding not a role"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast >> Prop1 to EreGast\n  user EreGast\n"
    case _ of
      (Left (NotARoleDomain _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error NotARoleDomain" false


  expectError "Bind: roletype not in context"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to AnotherRole\n  user EreGast\n"
    case _ of
      (Left (ContextHasNoRole _ _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error ContextHasNoRole" false

  domainTest "On entry with bind in another context"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to EreGast in AParty >> binding >> context\n  context AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n"
    \correctedDFR ->
      ensureState "model:Test$Gast$SomeState" correctedDFR >>=
        ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
          case _ of
            (BQD _ qf bndg _ _ _ _) -> do
              assert "The queryfunction should be bind" (eq qf (Bind $ EnumeratedRoleType "model:Test$Party$EreGast") )
              case bndg of
                (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                  assert "The binding should be a rolgetter" true
                otherwise -> assert "The binding should be a rolgetter" true
            otherwise -> assert "Side effect expected" false

  expectError "Bind: in-clause does not select a context"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to EreGast in AParty >> binding\n  context AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n"
    case _ of
      (Left (NotAContextDomain _ _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error NotAContextDomain" false

  expectError "Bind: in-clause does selects non-functional context"
    "domain Test\n  user Gast (relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to EreGast in AParty >> binding >> context\n  context AParty (mandatory, relational) filledBy Party\n  case Party\n    user EreGast filledBy Gast\n"
    case _ of
      (Left (NotFunctional _ _ _)) -> assert "ok" true
      otherwise -> do
        logShow otherwise
        assert "Expected the error NotFunctional" false

  expectError "Bind: bind to calculated role"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do \n          bind Gast to Organiser in AParty >> binding >> context\n  context AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n    user Organiser = EreGast\n"
    case _ of
      (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        assert "Expected the error CannotCreateCalculatedRole" false

  expectError "Bind: bind non-functional to functional role"
   "domain Test\n  user Gast (mandatory, relational)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind Gast to EreGast\n    property Prop1 (mandatory, Number)\n  user EreGast (mandatory) filledBy Gast\n"
    case _ of
      (Left (NotFunctional _ _ _)) -> assert "ok" true
      -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error NotFunctional" false

  domainTest "Bot Action with bind_"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind_ Gast to EreGast\n  user EreGast (mandatory) filledBy Gast\n"
      \correctedDFR -> do
        -- logShow correctedDFR
        ensureState "model:Test$Gast$SomeState" correctedDFR >>=
          ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
            case _ of
              (BQD _ qf bndg _ _ _ _) -> do
                assert "The queryfunction should be bind_" (eq qf Bind_ )
                case bndg of
                  (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                    assert "The binding should be a rolgetter" true
                  otherwise -> assert "The binding should be a rolgetter" true
              otherwise -> assert "Side effect expected" false

  expectError "Bind_: bind non-functional to functional role"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind_ Gast to EreGast\n  user EreGast (mandatory) filledBy Gast\n"
    case _ of
      (Left (NotFunctional _ _ _)) -> assert "ok" true
      -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error NotFunctional" false

  domainTest "Bot Action with unqualified unbind"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast\n  user EreGast (mandatory) filledBy Gast\n"
    \correctedDFR -> do
        -- logShow correctedDFR
        ensureState "model:Test$Gast$SomeState" correctedDFR >>=
          ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
            case _ of
              (UQD _ qf bndg _ _ _) -> do
                assert "The queryfunction should be unbind" (case qf of
                  Unbind Nothing -> true
                  otherwise -> false )
                case bndg of
                  (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                    assert "The binding should be a rolgetter for Gast" true
                  otherwise -> assert "The binding should be a rolgetter" false
              otherwise -> assert "Side effect expected" false

  domainTest "Bot Action with qualified unbind"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast from EreGast\n  user EreGast (mandatory) filledBy Gast\n"
    \correctedDFR -> do
        ensureState "model:Test$Gast$SomeState" correctedDFR >>=
          ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
            case _ of
              (UQD _ qf bndg _ _ _) -> do
                assert "The queryfunction should be unbind" (case qf of
                  Unbind (Just (EnumeratedRoleType "model:Test$EreGast")) -> true
                  otherwise -> false )
                case bndg of
                  (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                    assert "The binding should be a rolgetter for Gast" true
                  otherwise -> assert "The binding should be a rolgetter" false
              otherwise -> assert "Side effect expected" false

  expectError "Unbind: non-existing binder"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast from AnotherRole\n  user EreGast (mandatory) filledBy Gast\n"
    case _ of
      (Left (UnknownRole _ _)) -> assert "ok" true
      -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error UnknownRole" false

  expectError "Unbind: binder does not bind"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast from EreGast\n  user EreGast (mandatory) filledBy Organisator\n  user Organisator\n    property Prop2\n"
    case _ of
      (Left (LocalRoleDoesNotBind _ _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error LocalRoleDoesNotBind" false

  domainTest "Bot Action with delete role"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete role Gast\n"
    \correctedDFR -> do
        ensureState "model:Test$Gast$SomeState" correctedDFR >>=
          ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
            case _ of
              (UQD _ qf bndg _ _ _) -> do
                assert "The queryfunction should be DeleteRole" (case qf of
                  (DeleteRole _) -> true
                  otherwise -> false )
                case bndg of
                  (SQD _ (DataTypeGetter IdentityF) _ _ _) ->
                    assert "The binding should be (DataTypeGetter IdentityF)" true
                  otherwise -> assert "The binding should be a rolgetter" false
              otherwise -> assert "Side effect expected" false

  domainTest "Bot Action with delete property and default object"
    "domain Test\n  use: cdb for model:Couchdb\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          callEffect cdb:AddModelToLocalStore( AModel >> Name )\n  thing AModel\n    property Name (mandatory, String)\n"
    \correctedDFR -> do
      ensureState "model:Test$Gast$SomeState" correctedDFR >>=
        ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
          case _ of
            (UQD _ qf bndg _ _ _) -> do
              assert "The queryfunction should be DeleteProperty" (case qf of
                QF.DeleteProperty (EnumeratedPropertyType "model:Test$Gast$Prop1") -> true
                otherwise -> false )
              case bndg of
                (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Gast")))_ _ _) ->
                  assert "The binding should be a rolgetter for Gast" true
                otherwise -> assert "The binding should be a rolgetter" false
            otherwise -> assert "Side effect expected" false

  expectError "Bot Action with delete property, property doesn't exist."
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete property AnotherProp\n"
    case _ of
      (Left (MissingRoleForPropertyAssignment _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error MissingRoleForPropertyAssignment" false

  expectError "Bot Action with delete property, property is calculated."
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete property Prop2 from Gast\n"
    case _ of
      (Left (CannotCreateCalculatedProperty _ _ _)) -> assert "ok" true
      otherwise -> do
        logShow otherwise
        assert "Expected the error CannotCreateCalculatedProperty" false

  domainTest "Bot Action with property assignment and default object"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          Prop1 =+ 10 for Gast\n"
    \correctedDFR -> do
      ensureState "model:Test$Gast$SomeState" correctedDFR >>=
        ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
          case _ of
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

  expectError "Bot Action with property assignment and wrong value range."
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          Prop1 =+ true for Gast\n"
    case _ of
      (Left (WrongPropertyRange _ _ PNumber PBool)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error WrongPropertyRange" false

  expectError "Bot Action with property assignment and not even a property range."
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Gast >> Prop1 > 10\n      on entry\n        do\n          Prop1 =+ Gast for Gast\n"
    case _ of
      (Left (NotAPropertyRange _ _ PNumber)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert "Expected the error NotAPropertyRange" false

  domainTest "Bot Action with property assignment on another role"
    "domain Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do \n          Prop2 =+ 10 for Organiser\n  user Organiser\n    property Prop2 (mandatory, Number)\n"
    \correctedDFR ->
      ensureState "model:Test$Gast$SomeState" correctedDFR >>=
        ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
          case _ of
            (BQD _ qf val role _ _ _) -> do
              assert "The queryfunction should be AddPropertyValue" (case qf of
                QF.AddPropertyValue _ -> true
                otherwise -> false )
              assert "The value should be a Constant"
                (case val of
                  (SQD _ (Constant PNumber "10") _ _ _) -> true
                  otherwise -> false)
              case role of
                (SQD _ (RolGetter (ENR (EnumeratedRoleType "model:Test$Organiser")))_ _ _) -> assert "The binding should be a rolgetter for Organiser" true
                otherwise -> assert "The binding should be a rolgetter" false
            otherwise -> assert "Side effect expected" false

  domainTest "Bot Action with callEffect"
    "domain Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          callEffect cdb:AddModelToLocalStore( AModel >> Name )\n  thing AModel\n    property Name (mandatory, String)\n"
    \correctedDFR ->
      ensureState "model:Test$Gast$SomeState" correctedDFR >>=
        ensureOnEntry (ENR (EnumeratedRoleType "model:Test$Gast")) >>= pure <<< effectOfAction >>=
          case _ of
            (MQD _ qf args _ _ _) -> do
              assert "The queryfunction should be ExternalEffectFullFunction" (eq qf (ExternalEffectFullFunction "model:Couchdb$AddModelToLocalStore") )
              assert "There should be one argument" (length args == 1)
            otherwise -> assert "Side effect expected" false

  -- fileTestOnly "Read model:System" "perspectivesSysteem.arc"

-- extractEffect :: Maybe SideEffect -> QueryFunctionDescription
-- extractEffect = unsafePartial extractEffect_
--   where
--     extractEffect_ :: Partial => Maybe SideEffect -> QueryFunctionDescription
--     extractEffect_ (Just (EF (UQD _ _ (BQD _ _ _ mqd _ _ _) _ _ _))) = mqd

-- x :: DomeinFileRecord -> MonadPerspectives (Array RoleType)
-- x correctedDFR = withDomeinFile "model:MyTestDomain"
--   (DomeinFile correctedDFR)
--   ((runTypeLevelToArray (ST (ContextType "model:MyTestDomain")) (lookForUnqualifiedRoleTypeOfADT "Guest")))

expectError :: TestName -> ModelText -> Criterium -> TestSuite
expectError = expectErrorX test

expectErrorOnly :: TestName -> ModelText -> Criterium -> TestSuite
expectErrorOnly = expectErrorX testOnly

type TestName = String
type ModelText = String
type Criterium = ((Either PerspectivesError DomeinFileRecord) -> Aff Unit)
expectErrorX ::
  (String -> Test -> TestSuite) ->
  TestName ->
  ModelText ->
  Criterium ->
  TestSuite
expectErrorX theTest testName modelText resultTester = do
  theTest testName do
    (r :: Either ParseError ContextE) <- runIndentParser modelText ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r' state) ->
          case r' of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x of
                Right (Tuple r _) -> resultTester (Right r)
                Left errs -> for_ errs (resultTester <<< Left)
              -- resultTester x

type DomainTester = (DomeinFileRecord -> Aff Unit)

domainTest :: TestName -> ModelText -> DomainTester -> TestSuite
domainTest = domainTestX test

domainTestOnly :: TestName -> ModelText -> DomainTester -> TestSuite
domainTestOnly = domainTestX testOnly

type FileName = String
fileTestOnly :: TestName -> FileName -> TestSuite
fileTestOnly testName fileName =
  testOnly testName do
    modelDirectory <- pure "/Users/joopringelberg/Code/perspectives-core/src/model"
    modelText <- readTextFile ENC.UTF8 (Path.concat [modelDirectory, fileName])
    (r :: Either ParseError ContextE) <- runIndentParser modelText ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r' state) ->
          case r' of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> assert "OK" true

domainTestX ::
    (String -> Test -> TestSuite) ->
    TestName ->
    ModelText ->
    DomainTester ->
    TestSuite
domainTestX theTest testName modelText domainTester = theTest testName do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser modelText ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (addAllExternalFunctions *> traverseDomain ctxt) >>= \(Tuple r' state) ->
          case r' of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> domainTester correctedDFR
