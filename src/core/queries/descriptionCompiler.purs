module Perspectives.Query.DescriptionCompiler where

-- From an AST data constructor, create a QueryFunction data element after
-- checking that the required path can indeed be followed.

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Context (lookForRoleType)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType(..))
import Prelude (($), pure, bind, (>>=), (<<<))

-- From an AST data constructor, create a QueryFunctionDescription data element after
-- checking that the required path can indeed be followed.

compileElementaryStep :: Domain -> ElementaryQueryStep -> MonadPerspectives FD
-- Check whether the domain is a ContextType. Then check whether this type has a role with
-- the given qualified name. We know it is not an external role (that would be asked for directly), but it may be a user-, bot-, context- or rolInContext role.
-- We also don't know whether it is enumerated or calculated.
compileElementaryStep currentDomain s@(QualifiedRol qn) = do
  case currentDomain of
    (CDOM c) -> do
      mb <- getPerspectType c >>= pure <<< lookForRoleType qn
      case mb of
        Nothing -> pure $ Left $ ContextHasNoRole c qn
        (Just et) -> pure $ Right $ EQD currentDomain (RolGetter et) (RDOM $ EnumeratedRoleType
         qn)
    otherwise -> pure $ Left $ IncompatibleQueryArgument currentDomain s

-- The last case.
compileElementaryStep _ _ = pure $ Left $ UnknownElementaryQueryStep

compileQueryStep :: Domain -> QueryStep -> MonadPerspectives FD
compileQueryStep currentDomain s@(BinaryCombinator functionName op1 op2) =
  case functionName of
    "compose" -> do
      f1 <- compileQueryStep currentDomain op1
    _ ->  pure $ Left $ UnknownElementaryQueryStep
compileQueryStep currentDomain (Terminal e) = compileElementaryStep currentDomain e
compileQueryStep _ _ = pure $ Left $ UnknownElementaryQueryStep

type FD = Either CompilerMessage QueryFunctionDescription

data CompilerMessage =
  UnknownElementaryQueryStep
  | IncompatibleQueryArgument Domain ElementaryQueryStep
  | ContextHasNoRole ContextType String

-- instance showCompilerMessage :: Show CompilerMessage where
--   show (UnknownElementaryQueryStep) = "(UnknownElementaryQueryStep) This step is unknown"
--   show (IncompatibleQueryArgument dom step) = "(IncompatibleQueryArgument) Cannot get " <> show step <> " from " <> show dom
--
