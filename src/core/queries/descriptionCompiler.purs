module Perspectives.Query.DescriptionCompiler where

-- | From the Abstract Syntax Tree that results from a query-path expression (see `Perspectives.QueryAST` for the AST),
-- | create a QueryFunctionDescription data element.
-- | The code in this module sees to it that each step in the path is followed by a step that takes as its domain the
-- | range of its predecessor. Otherwise, an error is thrown that will be presented to the modeller.

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (gets)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Identifiers (deconstructModelName, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), SimpleStep(..), Step(..), UnaryStep)
import Perspectives.Parsing.Arc.PhaseThree (PhaseThree, lift2)
import Perspectives.Parsing.Arc.PhaseTwo (getDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), range, sumOfDomains, productOfDomains)
import Perspectives.Representation.ADT (ADT(..), lessThenOrEqualTo)
import Perspectives.Representation.Class.Property (effectivePropertyType)
import Perspectives.Representation.Class.Role (bindingOfADT, contextOfADT, expandedADT, expandedADT_, getRoleType)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType, RoleType(..), roletype2string)
import Perspectives.Types.ObjectGetters (externalRoleOfADT, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT, qualifyEnumeratedRoleInDomain, qualifyRoleInDomain)
import Prelude (bind, map, pure, show, ($), (==), discard)

compileStep :: Domain -> Step -> FD
compileStep currentDomain (Simple st) = compileSimpleStep currentDomain st
compileStep currentDomain (Unary st) = compileUnaryStep currentDomain st
compileStep currentDomain (Binary st) = compileBinaryStep currentDomain st

compileSimpleStep :: Domain -> SimpleStep -> FD
compileSimpleStep currentDomain s@(ArcIdentifier pos ident) =
  case currentDomain of
    (CDOM c) -> do
      (rts :: Array RoleType) <- if isQualifiedWithDomein ident
        then lift2 $ runArrayT $ lookForRoleTypeOfADT ident c
        else lift2 $ runArrayT $ lookForUnqualifiedRoleTypeOfADT ident c
      case head rts of
        Nothing -> throwError $ ContextHasNoRole c ident
        (Just (rt :: RoleType)) -> do
          (expandedType :: ADT EnumeratedRoleType) <- lift $ lift $ expandedADT_ rt
          pure $ SQD currentDomain (RolGetter rt) (RDOM $ expandedType)
    (RDOM r) -> do
      (pts :: Array PropertyType) <- if isQualifiedWithDomein ident
        then  lift2 $ runArrayT $ lookForPropertyType ident r
        else lift2 $ runArrayT $ lookForUnqualifiedPropertyType ident r
      case head pts of
        Nothing -> throwError $ RoleHasNoProperty r ident
        (Just (pt :: PropertyType)) -> do
          -- TODO: controleer of hier niet een 'expandedADT_' voor PropertyClass gebruikt moet worden.
          (effectiveType :: EnumeratedPropertyType) <- lift2 $ effectivePropertyType pt
          pure $ SQD currentDomain (PropertyGetter pt) (PDOM $ effectiveType)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Value pos range stringRepresentation) = pure $
  SQD currentDomain (Constant range stringRepresentation) (VDOM range)

compileSimpleStep currentDomain s@(Binding pos) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      -- The binding of a role is always an ADT EnumeratedRoleType.
      (typeOfBinding :: (ADT EnumeratedRoleType)) <- lift2 $ bindingOfADT r
      case typeOfBinding of
        NOTYPE -> throwError $ RoleHasNoBinding pos r
        adt -> pure $ SQD currentDomain (DataTypeGetter "binding") (RDOM adt)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Binder pos binderName) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      (qBinderType :: EnumeratedRoleType) <- if isQualifiedWithDomein binderName
        then pure $ EnumeratedRoleType binderName
        -- Try to qualify the name within the Domain.
        else do
          {_id:namespace} <- lift $ gets _.dfr
          (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain binderName (unsafePartial $ fromJust $ (deconstructModelName namespace))
          case head qnames of
            Nothing -> throwError $ UnknownRole pos binderName
            (Just qn) | length qnames == 1 -> pure qn
            otherwise -> throwError $ NotUniquelyIdentifying pos binderName (map unwrap qnames)

      -- Now we have a qualified Rolename for the binder, check if it indeed binds the role that is the currentDomain.
      -- That is, its binding (an ADT) must be more general than the currentDomain.
      (bindingOfBinder :: (ADT EnumeratedRoleType)) <- lift2 $ expandedADT_ (ENR qBinderType)
      if r `lessThenOrEqualTo` bindingOfBinder
        then pure $ SQD currentDomain (DataTypeGetterWithParameter "getUnqualifiedRoleBinders" binderName) (RDOM $ ST qBinderType)
        else throwError $ RoleDoesNotBind pos (ENR qBinderType) r
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Context pos) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      (typeOfContext :: ADT ContextType) <- lift2 $ contextOfADT r
      pure $ SQD currentDomain (DataTypeGetter "context") (CDOM typeOfContext)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Extern pos) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: ADT EnumeratedRoleType) <- lift2 $ externalRoleOfADT c
      pure $ SQD currentDomain (DataTypeGetter "externalRole") (RDOM $ rts)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileUnaryStep :: Domain -> UnaryStep -> FD
compileUnaryStep currentDomain _ = throwError $ Custom "Implement compileUnaryStep"

compileBinaryStep :: Domain -> BinaryStep -> FD
compileBinaryStep currentDomain _ = throwError $ Custom "Implement compileBinaryStep"

{-
compileQueryStep :: Domain -> QueryStep -> FD
compileQueryStep currentDomain s@(Compose op1 op2) = do
  f1 <- compileQueryStep currentDomain op1
  f2 <- compileQueryStep (range f1) op2
  pure $ BQD currentDomain (BinaryCombinator "compose") f1 f2 (range f2)

-- Elementary steps:
compileQueryStep currentDomain (Terminal e) = compileSimpleStep currentDomain e

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

-- type FD = ExceptT PerspectivesError MonadPerspectives QueryFunctionDescription
type FD = PhaseThree QueryFunctionDescription

-- compileRoleDescription :: ContextType -> Step -> MonadPerspectives QueryFunctionDescription
-- compileRoleDescription ct s = do
--   r <- runExceptT (compileStep (CDOM $ ST ct) s)
--   case r of
--     (Left m) -> throwError (error (show m))
--     (Right d) -> pure d
