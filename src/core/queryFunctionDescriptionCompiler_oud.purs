module Perspectives.QueryFunctionDescriptionCompilerOud where

import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Either (Either)
import Perspectives.ContextRoleParser (definition)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQueryCompiler, QueryCompilerEnvironment, QueryEnvironment, PerspectivesState)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.PrettyPrinter (PerspectText, PerspectTextM, identifier, runPerspectText)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Prelude (Unit, flip, otherwise, ($), (<<<), (>=>), bind, pure, unit)
import Text.Parsing.Parser (ParseError)

compileElementaryQueryStep :: forall e. ElementaryQueryStep -> (PerspectTextM (MonadPerspectivesQueryCompiler e)) Unit
compileElementaryQueryStep s = case s of
  -- Context -> identifier "model:QueryAst$context"
  -- otherwise -> identifier "Hello world"
  otherwise -> do
    (i :: Int) <- get
    _ <- tell "aap"
    (e1 :: QueryCompilerEnvironment) <- lift get
    (e2 :: QueryEnvironment) <- lift $ lift $ lift get
    (e3 :: Ref PerspectivesState) <- ask
    pure unit

-- compileQueryStep :: forall e. QueryStep -> (PerspectText e)
-- compileQueryStep s = case s of
--   LastElement s -> identifier "model:QueryAst$context"
--   Terminal es -> compileElementaryQueryStep es
--   otherwise -> identifier "Hello world"
--
-- compileQuery :: forall e. QueryStep -> MonadPerspectives (AjaxAvarCache e) String
-- compileQuery = runPerspectText <<< compileQueryStep
--
-- -- | This function is redundant as an elementary querystep es can be compiled using compileQuery $ Terminal es
-- compileElementaryQuery :: forall e. ElementaryQueryStep -> MonadPerspectives (AjaxAvarCache e) String
-- compileElementaryQuery = runPerspectText <<< compileElementaryQueryStep
--
-- -- | Any query can be compiled into a representation in terms of Contexts and Roles with this function.
-- -- | (turn an elementary querystep into a QueryStep by using constructor Terminal)
-- compileAndSaveQuery :: forall e. QueryStep -> MonadPerspectives (AjaxAvarCache e) (Either ParseError ID)
-- compileAndSaveQuery = compileQuery >=> (flip runIndentParser definition)
