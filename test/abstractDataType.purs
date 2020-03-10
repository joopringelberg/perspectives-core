module Test.Representation.ADT where


import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class.Console (logShow)
import Foreign.Object (lookup)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo')
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (binding, roleADT)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.Representation.ADT" do
  test "bindingOfADT" do
    (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser "domain: MyTestDomain\n  bot: for MySelf\n    perspective on: AnotherRole\n      Consult\n        indirectObject: AnotherRole \n  thing: Role (mandatory, functional) filledBy: YetAnotherRole\n  thing: AnotherRole = Role >> binding\n  thing: YetAnotherRole (mandatory, functional)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
          (Left e) -> assert (show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x <- runP $ phaseThree dr'
            case x of
              (Left e) -> assert (show e) false
              (Right correctedDFR@{enumeratedRoles, calculatedRoles}) -> do
                -- logShow correctedDFR
                case lookup "model:MyTestDomain$Role" enumeratedRoles of
                  Nothing -> assert "There should be a role 'model:MyTestDomain$Role'" false
                  -- otherwise -> assert "" true
                  Just rl -> do
                    b <- runP $ binding rl
                    -- logShow b
                    assert "binding of 'model:MyTestDomain$Role' is '(ST (EnumeratedRoleType \"model:MyTestDomain$YetAnotherRole\"))'"
                      -- (b == NOTYPE)
                      (b == (ST (EnumeratedRoleType "model:MyTestDomain$YetAnotherRole")))
                case lookup "model:MyTestDomain$AnotherRole" calculatedRoles of
                  Nothing -> assert "There should be a role 'model:MyTestDomain$AnotherRole'" false
                  Just arl -> do
                    tp <- runP $ roleADT arl
                    assert "The type of AnotherRole should be equal to YetAnotherRole"
                      (tp == ST (EnumeratedRoleType "model:MyTestDomain$YetAnotherRole"))
