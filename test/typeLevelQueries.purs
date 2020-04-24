module Test.TypeLevelQueries where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.CoreTypes (type (~~>), (##>>))
import Perspectives.Parsing.Arc.Expression (simpleStep)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo')
import Perspectives.Query.DescriptionCompiler (compileStep)
import Perspectives.Query.QueryTypes (Domain(..))
import Perspectives.Query.UnsafeCompiler (compileFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..))
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suiteOnly "TypeLevelQueries" do

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
