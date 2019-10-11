module Test.Parsing.Arc.PhaseThree where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Lens (_Just, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Class.Console (logShow, log)
import Perspectives.CoreTypes ((###=))
import Perspectives.DomeinCache (withDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (evalPhaseTwo', traverseDomain)
import Perspectives.Parsing.TransferFile (domain)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), RoleType(..))
import Perspectives.Types.ObjectGetters (contextAspectsClosure, lookForUnqualifiedRoleType, roleInContext)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.Parsing.Arc.PhaseThree" do
  test "TypeLevelObjectGetters" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultsAnotherRole\n        IndirectObjectRef : AnotherRole\n  Role : RoleInContext : AnotherRole\n    Calculation : blabla" domain
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

            contexts <- runP $
              withDomeinFile "model:MyTestDomain" df
                ((ContextType "model:MyTestDomain") ###= contextAspectsClosure)
            assert "contextAspectsClosure applied to model:MyTestDomain should return a set containing model:MyTestDomain"
              case head contexts of
                (Just (ContextType "model:MyTestDomain")) -> true
                otherwise -> false

            roles <- runP $
              withDomeinFile "model:MyTestDomain" df
                ((ContextType "model:MyTestDomain") ###= (contextAspectsClosure >=> roleInContext))
            assert "(contextAspectsClosure >=> roleInContext) should be able to retrieve the role AnotherRole from the context model:MyTestDomain."
              case head roles of
                (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                otherwise -> false

            roles <- runP $
              withDomeinFile "model:MyTestDomain" df
                ((ContextType "model:MyTestDomain") ###= (lookForUnqualifiedRoleType "AnotherRole"))
            assert "lookForUnqualifiedRoleType should be able to retrieve the role AnotherRole from the context model:MyTestDomain."
              (isJust (head roles))

  test "Testing qualifyActionRoles." do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "Context : Domain : MyTestDomain\n  Agent : BotRole : MyBot\n    ForUser : MySelf\n    Perspective : Perspective : BotPerspective\n      ObjectRef : AnotherRole\n      Action : Consult : ConsultsAnotherRole\n        IndirectObjectRef : AnotherRole\n  Role : RoleInContext : AnotherRole\n    Calculation : blabla" domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- (DomeinFile dr) <- pure defaultDomeinFile
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x <- runP $ phaseThree dr'
            case x of
              (Left e) -> assert (show e) false
              (Right correctedDFR) -> do
                logShow correctedDFR
                assert "AnotherRole should be a calculatedRole"
                  (let
                    _r = prop (SProxy :: (SProxy "calculatedRoles")) <<< at "model:MyTestDomain$AnotherRole" <<< traversed
                    in case (preview _r correctedDFR) of
                      (Just _) -> true
                      otherwise -> false
                  )
                assert "The Object of the action 'ConsultsAnotherRole' should be a qualified CalculatedRole type."
                  (let
                    _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "object"))
                    in case (preview _o correctedDFR) of
                      (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                      -- (Just (CR _)) -> true
                      otherwise -> false
                    )
                assert "The IndirectObject of the action 'ConsultsAnotherRole' should be a qualified CalculatedRole type."
                  (let
                    _o = prop (SProxy :: (SProxy "actions")) <<< at "model:MyTestDomain$MySelf$ConsultsAnotherRole" <<< traversed <<< _Newtype <<< prop (SProxy :: (SProxy "indirectObject")) <<< _Just
                    in case (preview _o correctedDFR) of
                      (Just (CR (CalculatedRoleType "model:MyTestDomain$AnotherRole"))) -> true
                      -- (Just (CR _)) -> true
                      otherwise -> false
                    )
