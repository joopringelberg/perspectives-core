module Test.TypeLevelQueries where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.CoreTypes (type (~~>), (##>>), (##=))
import Perspectives.LoadCRL.FS (loadAndSaveCrlFile)
import Perspectives.Parsing.Arc.Expression (simpleStep, step)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo')
import Perspectives.Query.DescriptionCompiler (compileStep)
import Perspectives.Query.QueryTypes (Domain(..))
import Perspectives.Query.UnsafeCompiler (compileFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleType)
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP, withSimpleChat, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "TypeLevelQueries" do

  test "Retrieve a context type" $ runP $ withSystem do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "contextType" simpleStep
    case r of
      (Left e) -> liftAff $ assert (show e) false
      (Right step) -> do
        -- logShow step -- (Simple (TypeOfContext (ArcPosition { column: 1, line: 1 })))
        -- Request the type of MySystem
        r' <- evalPhaseTwo' $ compileStep (CDOM $ ST $ ContextType "model:System$PerspectivesSystem") step
        case r' of
          Left e -> liftAff $ assert (show e) false
          Right qfd -> do
            -- log $ prettyPrint qfd
            (getter :: ContextInstance ~~> ContextType) <- unsafeCoerce (compileFunction qfd)
            -- Now get the type of MySystem
            theType <- (ContextInstance "model:User$test") ##>> getter
            liftAff $ assert "The type retrieved should be model:System$PerspectivesSystem" (theType == (ContextType "model:System$PerspectivesSystem"))

  test "Retrieve the role types of a context type" $ runP $ withSystem do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "roleTypes" step
    case r of
      (Left e) -> liftAff $ assert (show e) false
      (Right step) -> do
        -- logShow step -- (Simple (RoleTypes (ArcPosition { column: 1, line: 1 })))
        -- Request the role types of sys:PerspectivesSystem
        r' <- evalPhaseTwo' $ compileStep ContextKind step
        case r' of
          Left e -> liftAff $ assert (show e) false
          Right qfd -> do
            -- log $ prettyPrint qfd
            -- SQD
            --   ContextKind
            --   (TypeGetter RoleTypesF)
            --   RoleKind
            --   True
            --   True
            getter <- unsafeCoerce (compileFunction qfd)
            -- Now get the roletypes of MySystem
            (theRoles :: Array RoleType) <- (ContextType "model:System$PerspectivesSystem") ##= getter
            -- logShow theRoles
            liftAff $ assert "There should be 9 roletypes " (length theRoles == 9)

  test "Retrieve the role types of the type of a context instance" $ runP $ withSystem do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "contextType >> roleTypes" step
    case r of
      (Left e) -> liftAff $ assert (show e) false
      (Right step) -> do
        -- logShow step
        -- (Binary (BinaryStep { end: (ArcPosition { column: 25, line: 1 }), left: (Simple (TypeOfContext (ArcPosition { column: 1, line: 1 }))), operator: (Compose (ArcPosition { column: 13, line: 1 })), parenthesised: false, right: (Simple (RoleTypes (ArcPosition { column: 16, line: 1 }))), start: (ArcPosition { column: 1, line: 1 }) }))
        -- Request the type of MySystem
        r' <- evalPhaseTwo' $ compileStep (CDOM $ ST $ ContextType "model:System$PerspectivesSystem") step
        case r' of
          Left e -> liftAff $ assert (show e) false
          Right qfd -> do
            -- log $ prettyPrint qfd
            -- BQD
            --   (CDOM (ST ContextType model:System$PerspectivesSystem))
            --   (BinaryCombinator compose)
            --   SQD
            --     (CDOM (ST ContextType model:System$PerspectivesSystem))
            --     (TypeGetter TypeOfContextF)
            --     ContextKind
            --     True
            --     True
            --   SQD
            --     ContextKind
            --     (TypeGetter RoleTypesF)
            --     RoleKind
            --     True
            --     True
            --   RoleKind
            --   True
            --   True
            (getter :: ContextInstance ~~> RoleType) <- unsafeCoerce (compileFunction qfd)
            -- Now get the roletypes of MySystem
            theRoles <- (ContextInstance "model:User$test") ##= getter
            -- logShow theRoles
            liftAff $ assert "There should be 9 roletypes " (length theRoles == 9)

  test "Filter the role types of the type of a context instance" $ runP $ withSimpleChat do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "filter contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee" step
    case r of
      (Left e) -> liftAff $ assert (show e) false
      (Right step) -> do
        -- logShow step
        r' <- evalPhaseTwo' $ compileStep (CDOM $ ST $ ContextType "model:System$PerspectivesSystem") step
        case r' of
          Left e -> liftAff $ assert (show e) false
          Right qfd -> do
            log $ prettyPrint qfd
            -- BQD
            --   (CDOM (ST ContextType model:System$PerspectivesSystem))
            --   (BinaryCombinator compose)
            --   SQD
            --     (CDOM (ST ContextType model:System$PerspectivesSystem))
            --     (TypeGetter TypeOfContextF)
            --     ContextKind
            --     True
            --     True
            --   BQD
            --     ContextKind
            --     (BinaryCombinator filter)
            --     SQD
            --       ContextKind
            --       (TypeGetter RoleTypesF)
            --       RoleKind
            --       True
            --       True
            --     SQD
            --       RoleKind
            --       (DataTypeGetterWithParameter SpecialisesRoleTypeF "model:System$Invitation$Invitee")
            --       (VDOM PBool Nothing)
            --       False
            --       False
            --     RoleKind
            --     True
            --     False
            --   RoleKind
            --   True
            --   False
            (getter :: ContextInstance ~~> RoleType) <- unsafeCoerce (compileFunction qfd)
            -- Get a Chat instance.
            errs <- loadAndSaveCrlFile "chatInvitation.crl" testDirectory
            -- Now get the roletypes of MySystem
            theRoles <- (ContextInstance "model:User$MyChatInvitation") ##= getter
            logShow theRoles
            liftAff $ assert "There should be 1 roletype " (length theRoles == 1)

  test "Filter the role types of the type of a context starting with the external role" $ runP $ withSimpleChat do
    (r :: Either ParseError Step) <- pure $ unwrap $ runIndentParser "filter context >> contextType >> roleTypes with specialisesRoleType model:System$Invitation$Invitee" step
    case r of
      (Left e) -> liftAff $ assert (show e) false
      (Right step) -> do
        -- logShow step
        r' <- evalPhaseTwo' $ compileStep (RDOM $ ST $ EnumeratedRoleType "model:System$Invitation$External") step
        case r' of
          Left e -> liftAff $ assert (show e) false
          Right qfd -> do
            log $ prettyPrint qfd
            -- BQD
            --   (RDOM (ST EnumeratedRoleType model:System$Invitation$External))
            --   (BinaryCombinator compose)
            --   SQD
            --     (RDOM (ST EnumeratedRoleType model:System$Invitation$External))
            --     (DataTypeGetter context)
            --     (CDOM (ST ContextType model:System$Invitation))
            --     True
            --     True
            --   BQD
            --     (CDOM (ST ContextType model:System$Invitation))
            --     (BinaryCombinator compose)
            --     SQD
            --       (CDOM (ST ContextType model:System$Invitation))
            --       (TypeGetter TypeOfContextF)
            --       ContextKind
            --       True
            --       True
            --     BQD
            --       ContextKind
            --       (BinaryCombinator filter)
            --       SQD
            --         ContextKind
            --         (TypeGetter RoleTypesF)
            --         RoleKind
            --         True
            --         True
            --       SQD
            --         RoleKind
            --         (DataTypeGetterWithParameter SpecialisesRoleTypeF "model:System$Invitation$Invitee")
            --         (VDOM PBool Nothing)
            --         False
            --         False
            --       RoleKind
            --       True
            --       False
            --     RoleKind
            --     True
            --     False
            --   RoleKind
            --   True
            --   False
            (getter :: RoleInstance ~~> RoleType) <- unsafeCoerce (compileFunction qfd)
            -- Get a Chat instance.
            errs <- loadAndSaveCrlFile "chatInvitation.crl" testDirectory
            -- Now get the roletypes of MySystem
            theRoles <- (RoleInstance "model:User$MyChatInvitation_External") ##= getter
            logShow theRoles
            liftAff $ assert "There should be 1 roletype " (length theRoles == 1)
