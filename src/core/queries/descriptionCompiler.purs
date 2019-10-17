module Perspectives.Query.DescriptionCompiler where

-- | From the Abstract Syntax Tree that results from a query-path expression (see `Perspectives.QueryAST` for the AST),
-- | create a QueryFunctionDescription data element.
-- | The code in this module sees to it that each step in the path is followed by a step that takes as its domain the
-- | range of its predecessor. Otherwise, an error is thrown that will be presented to the modeller.

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), SimpleStep(..), Step(..), UnaryStep)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), range, sumOfDomains, productOfDomains)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Property (effectivePropertyType)
import Perspectives.Representation.Class.Role (bindingOfADT, contextOfADT, expandedADT_)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType)
import Perspectives.Types.ObjectGetters (externalRoleOfADT, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT)
import Prelude (bind, pure, ($), show)

compileStep :: Domain -> Step -> FD
compileStep currentDomain (Simple st) = compileElementaryStep currentDomain st
compileStep currentDomain (Unary st) = compileUnaryStep currentDomain st
compileStep currentDomain (Binary st) = compileBinaryStep currentDomain st

compileUnaryStep :: Domain -> UnaryStep -> FD
compileUnaryStep currentDomain _ = throwError $ Custom "Implement compileUnaryStep"

compileElementaryStep :: Domain -> SimpleStep -> FD
compileElementaryStep currentDomain s@(ArcIdentifier pos ident) =
  case currentDomain of
    (CDOM c) -> do
      (rts :: Array RoleType) <- if isQualifiedWithDomein ident
        then lift $ runArrayT $ lookForRoleTypeOfADT ident c
        else lift $ runArrayT $ lookForUnqualifiedRoleTypeOfADT ident c
      case head rts of
        Nothing -> throwError $ ContextHasNoRole c ident
        (Just (rt :: RoleType)) -> do
          (expandedType :: ADT EnumeratedRoleType) <- lift $ expandedADT_ rt
          pure $ SQD currentDomain (RolGetter rt) (RDOM $ expandedType)
    (RDOM r) -> do
      (pts :: Array PropertyType) <- if isQualifiedWithDomein ident
        then  lift $ runArrayT $ lookForPropertyType ident r
        else lift $ runArrayT $ lookForUnqualifiedPropertyType ident r
      case head pts of
        Nothing -> throwError $ RoleHasNoProperty r ident
        (Just (pt :: PropertyType)) -> do
          -- TODO: controleer of hier niet een 'expandedADT_' voor PropertyClass gebruikt moet worden.
          (effectiveType :: EnumeratedPropertyType) <- lift $ effectivePropertyType pt
          pure $ SQD currentDomain (PropertyGetter pt) (PDOM $ effectiveType)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain (Simple s)

-- compileElementaryStep currentDomain s@(QualifiedExternalProperty qn) = do
--   compileQueryStep currentDomain (Compose (Terminal ExternalRole) (Terminal (QualifiedRole qn)))
--
-- compileElementaryStep currentDomain s@(UnqualifiedExternalProperty qn) = do
--   compileQueryStep currentDomain (Compose (Terminal ExternalRole) (Terminal (UnqualifiedRole qn)))
--
-- compileElementaryStep currentDomain s@(Binding) = do
--   case currentDomain of
--     (RDOM (r :: ADT EnumeratedRoleType)) -> do
--       -- The binding of a role is always an ADT EnumeratedRoleType.
--       (typeOfBinding :: (ADT EnumeratedRoleType)) <- lift $ bindingOfADT r
--       case typeOfBinding of
--         NOTYPE -> throwError $ RoleHasNoBinding r
--         adt -> pure $ SQD currentDomain (DataTypeGetter "binding") (RDOM adt)
--     otherwise -> throwError $ IncompatibleQueryArgument currentDomain s
--
-- compileElementaryStep currentDomain s@(Context) = do
--   case currentDomain of
--     (RDOM (r :: ADT EnumeratedRoleType)) -> do
--       (typeOfContext :: ADT ContextType) <- lift $ contextOfADT r
--       pure $ SQD currentDomain (DataTypeGetter "context") (CDOM typeOfContext)
--     otherwise -> throwError $ IncompatibleQueryArgument currentDomain s
--
-- compileElementaryStep currentDomain s@(ExternalRole) = do
--   case currentDomain of
--     (CDOM c) -> do
--       (rts :: ADT EnumeratedRoleType) <- lift $ externalRoleOfADT c
--       pure $ SQD currentDomain (DataTypeGetter "externalRole") (RDOM $ rts)
--     otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

-- The last case.
compileElementaryStep _ _ = throwError $ UnknownElementaryQueryStep

compileBinaryStep :: Domain -> BinaryStep -> FD
compileBinaryStep currentDomain _ = throwError $ Custom "Implement compileBinaryStep"

{-
compileQueryStep :: Domain -> QueryStep -> FD
compileQueryStep currentDomain s@(Compose op1 op2) = do
  f1 <- compileQueryStep currentDomain op1
  f2 <- compileQueryStep (range f1) op2
  pure $ BQD currentDomain (BinaryCombinator "compose") f1 f2 (range f2)

-- Elementary steps:
compileQueryStep currentDomain (Terminal e) = compileElementaryStep currentDomain e

compileQueryStep currentDomain s@(Filter criterium source) = do
  criterium' <- compileQueryStep currentDomain criterium
  source' <- compileQueryStep currentDomain source
  pure $ BQD currentDomain (BinaryCombinator "filter") criterium' source' currentDomain

compileQueryStep currentDomain s@(Disjunction op1 op2) = do
  op1' <- compileQueryStep currentDomain op1
  op2' <- compileQueryStep currentDomain op2
  case sumOfDomains (range op1') (range op2') of
    (Just dom) -> pure $ BQD currentDomain (BinaryCombinator "disjunction") op1' op2' dom
    _ -> throwError $ IncompatibleDomainsForJunction (range op1') (range op2')

compileQueryStep currentDomain s@(Conjunction op1 op2) = do
  op1' <- compileQueryStep currentDomain op1
  op2' <- compileQueryStep currentDomain op2
  case productOfDomains (range op1') (range op2') of
    (Just dom) -> pure $ BQD currentDomain (BinaryCombinator "conjunction") op1' op2' dom
    _ -> throwError $ IncompatibleDomainsForJunction (range op1') (range op2')

-- the last case
compileQueryStep _ _ = throwError $ UnknownElementaryQueryStep
-}

type FD = ExceptT PerspectivesError MonadPerspectives QueryFunctionDescription

compileRoleDescription :: ContextType -> Step -> MonadPerspectives QueryFunctionDescription
compileRoleDescription ct s = do
  r <- runExceptT (compileStep (CDOM $ ST ct) s)
  case r of
    (Left m) -> throwError (error (show m))
    (Right d) -> pure d
