module Perspectives.Query.DescriptionCompiler where

-- From an AST data constructor, create a QueryFunction data element after
-- checking that the required path can indeed be followed.

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), range, sumOfDomains, productOfDomains)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Property (effectivePropertyType)
import Perspectives.Representation.Class.Role (bindingOfADT, contextOfADT, effectiveRoleType)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType)
import Perspectives.Types.ObjectGetters (externalRoleOfADT, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT)
import Prelude (class Show, bind, pure, ($), (<>), show)

-- From an AST data constructor, create a QueryFunctionDescription data element after
-- checking that the required path can indeed be followed.

compileElementaryStep :: Domain -> ElementaryQueryStep -> FD
-- Check whether the domain is a ContextType. Then check whether this type has a role with
-- the given qualified name. We know it is not an external role (that would be asked for directly), but it may be a user-, bot-, context- or rolInContext role.
compileElementaryStep currentDomain s@(QualifiedRole qn) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: Array RoleType) <- lift $ runArrayT $ lookForRoleTypeOfADT qn c
      case head rts of
        Nothing -> throwError $ ContextHasNoRole c qn
        -- rt can be Enumerated or Calculated!
        (Just (rt :: RoleType)) -> do
          (effectiveType :: ADT EnumeratedRoleType) <- lift $ effectiveRoleType rt
          pure $ SQD currentDomain (RolGetter rt) (RDOM $ effectiveType)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

compileElementaryStep currentDomain s@(UnqualifiedRole ln) = do
  case currentDomain of
    (CDOM c) -> do
      (at :: Array RoleType) <- lift $ runArrayT $ lookForUnqualifiedRoleTypeOfADT ln c
      case head at of
        Nothing -> throwError $ ContextHasNoRole c ln
        (Just (et :: RoleType)) -> do
          (effectiveType :: ADT EnumeratedRoleType) <- lift $ effectiveRoleType et
          pure $ SQD currentDomain (RolGetter et) (RDOM $ effectiveType)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

compileElementaryStep currentDomain s@(QualifiedProperty qn) = do
  case currentDomain of
    (RDOM r) -> do
      (pts :: Array PropertyType) <- lift $ runArrayT $ lookForPropertyType qn r
      case head pts of
        Nothing -> throwError $ RoleHasNoProperty r qn
        -- pt can be Enumerated or Calculated!
        (Just (pt :: PropertyType)) -> do
          (effectiveType :: EnumeratedPropertyType) <- lift $ effectivePropertyType pt
          pure $ SQD currentDomain (PropertyGetter pt) (PDOM $ effectiveType)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

compileElementaryStep currentDomain s@(UnqualifiedProperty qn) = do
  case currentDomain of
    (RDOM r) -> do
      (pts :: Array PropertyType) <- lift $ runArrayT $ lookForUnqualifiedPropertyType qn r
      case head pts of
        Nothing -> throwError $ RoleHasNoProperty r qn
        -- pt can be Enumerated or Calculated!
        (Just (pt :: PropertyType)) -> do
          (effectiveType :: EnumeratedPropertyType) <- lift $ effectivePropertyType pt
          pure $ SQD currentDomain (PropertyGetter pt) (PDOM $ effectiveType)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

compileElementaryStep currentDomain s@(QualifiedExternalProperty qn) = do
  compileQueryStep currentDomain (Compose (Terminal ExternalRole) (Terminal (QualifiedRole qn)))

compileElementaryStep currentDomain s@(UnqualifiedExternalProperty qn) = do
  compileQueryStep currentDomain (Compose (Terminal ExternalRole) (Terminal (UnqualifiedRole qn)))

compileElementaryStep currentDomain s@(Binding) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      -- The binding of a role is always an ADT EnumeratedRoleType.
      (typeOfBinding :: (ADT EnumeratedRoleType)) <- lift $ bindingOfADT r
      case typeOfBinding of
        NOTYPE -> throwError $ RoleHasNoBinding r
        adt -> pure $ SQD currentDomain (DataTypeGetter "binding") (RDOM adt)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

compileElementaryStep currentDomain s@(Context) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      (typeOfContext :: ADT ContextType) <- lift $ contextOfADT r
      pure $ SQD currentDomain (DataTypeGetter "context") (CDOM typeOfContext)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

compileElementaryStep currentDomain s@(ExternalRole) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: ADT EnumeratedRoleType) <- lift $ externalRoleOfADT c
      pure $ SQD currentDomain (DataTypeGetter "externalRole") (RDOM $ rts)
    otherwise -> throwError $ IncompatibleQueryArgument currentDomain s

-- The last case.
compileElementaryStep _ _ = throwError $ UnknownElementaryQueryStep

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

type FD = ExceptT CompilerMessage MonadPerspectives QueryFunctionDescription

data CompilerMessage =
  UnknownElementaryQueryStep
  | IncompatibleQueryArgument Domain ElementaryQueryStep
  | ContextHasNoRole (ADT ContextType) String
  | RoleHasNoProperty (ADT EnumeratedRoleType) String
  | RoleHasNoBinding (ADT EnumeratedRoleType)
  | IncompatibleDomainsForJunction Domain Domain

instance showCompilerMessage :: Show CompilerMessage where
  show (UnknownElementaryQueryStep) = "(UnknownElementaryQueryStep) This step is unknown"
  show (IncompatibleQueryArgument dom step) = "(IncompatibleQueryArgument) Cannot get " <> show step <> " from " <> show dom
  show (ContextHasNoRole ctype qn) = "(ContextHasNoRole) The Context-type '" <> show ctype <> "' has no role with the name '" <> qn <> "'."
  show (RoleHasNoProperty rtype qn) = "(RoleHasNoProperty) The Role-type '" <> show rtype <> "' has no property with the name '" <> qn <> "'."
  show (RoleHasNoBinding rtype) = "(RoleHasNoBinding) The role '" <> show rtype <> "' has no binding. If it is a Sum-type, one of its members may have no binding."
  show (IncompatibleDomainsForJunction dom1 dom2) = "(IncompatibleDomainsForJunction) These two domains cannot be joined in a disjunction of conjunction: '" <> show dom1 <> "', '" <> show dom2 <> "'."

compileRoleDescription :: ContextType -> QueryStep -> MonadPerspectives QueryFunctionDescription
compileRoleDescription ct s = do
  r <- runExceptT (compileQueryStep (CDOM $ ST ct) s)
  case r of
    (Left m) -> throwError (error (show m))
    (Right d) -> pure d
