module Perspectives.Query.DescriptionCompiler where

-- From an AST data constructor, create a QueryFunction data element after
-- checking that the required path can indeed be followed.

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Context (lookForRoleType)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType(..))
import Prelude (class Show, bind, pure, ($), (<<<), (>>=), (<>), show)

-- From an AST data constructor, create a QueryFunctionDescription data element after
-- checking that the required path can indeed be followed.

compileElementaryStep :: Domain -> ElementaryQueryStep -> FD
-- Check whether the domain is a ContextType. Then check whether this type has a role with
-- the given qualified name. We know it is not an external role (that would be asked for directly), but it may be a user-, bot-, context- or rolInContext role.
-- We also don't know whether it is enumerated or calculated.
compileElementaryStep currentDomain s@(QualifiedRol qn) = do
  case currentDomain of
    (CDOM c) -> do
      mb <- lift $ getPerspectType c >>= pure <<< lookForRoleType qn
      case mb of
        Nothing -> throwError $ ContextHasNoRole c qn
        (Just et) -> pure $ QD currentDomain (RolGetter et) (RDOM $ EnumeratedRoleType
         qn)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

-- The last case.
compileElementaryStep _ _ = throwError $ UnknownElementaryQueryStep

compileQueryStep :: Domain -> QueryStep -> FD
compileQueryStep currentDomain s@(Compose op1 op2) = do
  (QD _ f1 range1) <- compileQueryStep currentDomain op1
  (QD _ f2 range2) <- compileQueryStep range1 op2
  pure $ QD currentDomain (BinaryCombinator "compose" f1 f2) range2

-- Elementary steps:
compileQueryStep currentDomain (Terminal e) = compileElementaryStep currentDomain e

-- the last case
compileQueryStep _ _ = throwError $ UnknownElementaryQueryStep

-- type FD = Either CompilerMessage QueryFunctionDescription

type FD = ExceptT CompilerMessage MonadPerspectives QueryFunctionDescription

data CompilerMessage =
  UnknownElementaryQueryStep
  | IncompatibleQueryArgument Domain ElementaryQueryStep
  | ContextHasNoRole ContextType String

instance showCompilerMessage :: Show CompilerMessage where
  show (UnknownElementaryQueryStep) = "(UnknownElementaryQueryStep) This step is unknown"
  show (IncompatibleQueryArgument dom step) = "(IncompatibleQueryArgument) Cannot get " <> show step <> " from " <> show dom
  show (ContextHasNoRole ctype qn) = "(ContextHasNoRole) The Context-type '" <> show ctype <> "' has no role with the name '" <> qn <> "'."

compileRoleDescription :: ContextType -> QueryStep -> MonadPerspectives QueryFunctionDescription
compileRoleDescription ct s = do
  r <- runExceptT (compileQueryStep (CDOM ct) s)
  case r of
    (Left m) -> throwError (error (show m))
    (Right d) -> pure d
