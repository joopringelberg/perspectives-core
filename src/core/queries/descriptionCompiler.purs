module Perspectives.Query.DescriptionCompiler where

-- | From the Abstract Syntax Tree that results from a query-path expression (see `Perspectives.QueryAST` for the AST),
-- | create a QueryFunctionDescription data element.
-- | The code in this module sees to it that each function is applied to the right type of arguments.
-- | For example, each step in a path must be followed by a step that takes as its domain the
-- | range of its predecessor. Otherwise, an error is thrown that will be presented to the modeller.

import Control.Monad.Except (lift, throwError)
import Control.Monad.State (gets)
import Data.Array (elemIndex, head, length)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Identifiers (deconstructModelName, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.Expression (startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), Operator(..), SimpleStep(..), Step(..), UnaryStep(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseThree, lift2)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), range)
import Perspectives.Representation.ADT (ADT(..), lessThenOrEqualTo)
import Perspectives.Representation.Class.Property (rangeOfPropertyType)
import Perspectives.Representation.Class.Role (bindingOfADT, contextOfADT, expandedADT_)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), PropertyType, RoleType(..))
import Perspectives.Types.ObjectGetters (externalRoleOfADT, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT, qualifyContextInDomain, qualifyEnumeratedRoleInDomain)
import Prelude (bind, eq, flip, map, pure, ($), (==), (&&))

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
          (rOfpt :: Range) <- lift2 $ rangeOfPropertyType pt
          pure $ SQD currentDomain (PropertyGetter pt) (VDOM rOfpt)
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
        then pure $ SQD currentDomain (DataTypeGetterWithParameter "getRoleBinders" (unwrap qBinderType)) (RDOM $ ST qBinderType)
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

compileSimpleStep currentDomain (CreateContext pos ident) = do
  -- If `ident` is not qualified, try to qualify it in the Domain.
  (qcontextType :: ContextType) <- if isQualifiedWithDomein ident
    then pure $ ContextType ident
    -- Try to qualify the name within the Domain.
    else do
      {_id:namespace} <- lift $ gets _.dfr
      (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain ident (unsafePartial $ fromJust $ (deconstructModelName namespace))
      case head qnames of
        Nothing -> throwError $ UnknownContext pos ident
        (Just qn) | length qnames == 1 -> pure qn
        otherwise -> throwError $ NotUniquelyIdentifying pos ident (map unwrap qnames)
  pure $ SQD currentDomain (DataTypeGetterWithParameter "createContext" (unwrap qcontextType)) (CDOM (ST qcontextType))

compileSimpleStep currentDomain (CreateEnumeratedRole pos ident) = do
  -- If `ident` is not qualified, try to qualify it in the Domain.
  (qroleType :: EnumeratedRoleType) <- if isQualifiedWithDomein ident
    then pure $ EnumeratedRoleType ident
    -- Try to qualify the name within the Domain.
    else do
      {_id:namespace} <- lift $ gets _.dfr
      (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain ident (unsafePartial $ fromJust $ (deconstructModelName namespace))
      case head qnames of
        Nothing -> throwError $ UnknownRole pos ident
        (Just qn) | length qnames == 1 -> pure qn
        otherwise -> throwError $ NotUniquelyIdentifying pos ident (map unwrap qnames)
  pure $ SQD currentDomain (DataTypeGetterWithParameter "createRole" (unwrap qroleType)) (RDOM (ST qroleType))

compileSimpleStep currentDomain (NoOp _) = pure $ SQD currentDomain (DataTypeGetter "identity") currentDomain

compileUnaryStep :: Domain -> UnaryStep -> FD
compileUnaryStep currentDomain (LogicalNot pos s) = do
  -- First compile s. Then check that the resulting QueryFunctionDescription a (VDOM PBool) range value.
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM PBool -> pure $ UQD currentDomain (UnaryCombinator "not") descriptionOfs (VDOM PBool)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain s

compileUnaryStep currentDomain st@(Exists pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    CDOM _ -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)
    otherwise -> pure $ UQD currentDomain (UnaryCombinator "exists") descriptionOfs (VDOM PBool)

compileBinaryStep :: Domain -> BinaryStep -> FD
compileBinaryStep currentDomain s@(BinaryStep{operator, left, right}) =
  case operator of
    Filter pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep (range f1) right
      -- f1 is the source to be filtered, f2 is the criterium.
      case range f2 of
        VDOM PBool -> pure $ BQD currentDomain (BinaryCombinator "filter") f1 f2 (range f1)
        otherwise -> throwError $ NotABoolean (startOf right)
    Compose pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep (range f1) right
      pure $ BQD currentDomain (BinaryCombinator "compose") f1 f2 (range f2)

    otherwise -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right

      case operator of

        Equals pos -> comparison pos f1 f2 "equals"
        NotEquals pos -> comparison pos f1 f2 "notEquals"
        LessThan pos -> comparison pos f1 f2 "lessThan"
        LessThanEqual pos -> comparison pos f1 f2 "lessThanEqual"
        GreaterThan pos -> comparison pos f1 f2 "greaterThan"
        GreaterThanEqual pos -> comparison pos f1 f2 "greaterThanEqual"

        LogicalAnd pos -> binOp pos f1 f2 [PBool] "and"
        LogicalOr pos -> binOp pos f1 f2 [PBool] "or"
        Add pos -> binOp pos f1 f2 [PNumber, PString] "add"
        Subtract pos -> binOp pos f1 f2 [PNumber, PString] "subtract"
        Divide pos -> binOp pos f1 f2 [PNumber] "divide"
        Multiply pos -> binOp pos f1 f2 [PNumber] "multiply"

        Compose _ -> throwError $ Custom "This case in compileBinaryStep should never be reached"
        Filter _ -> throwError $ Custom "This case in compileBinaryStep should never be reached"

  where
    comparison :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> String -> PhaseThree QueryFunctionDescription
    comparison pos left' right' functionName = do
      -- Both ranges must be equal
      gt <- lift2 $ pure ((range left') `eq` (range right'))
      if gt
        then pure $ BQD currentDomain (BinaryCombinator functionName) left' right' (range right')
        else throwError $ TypesCannotBeCompared pos (range left') (range right')

    binOp :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> Array Range -> String -> PhaseThree QueryFunctionDescription
    binOp pos left' right' allowedRangeConstructors functionName = case range left', range right' of
      (VDOM rc1), (VDOM rc2) | rc1 == rc2 ->
        if  allowed rc1 && allowed rc2
          then pure $ BQD currentDomain (BinaryCombinator functionName) left' right' (VDOM rc1)
          else throwError $ WrongTypeForOperator pos allowedRangeConstructors
      l, r -> throwError $ TypesCannotBeCompared pos l r
      where
        allowed :: Range -> Boolean
        allowed r = isJust $ elemIndex r allowedRangeConstructors

type FD = PhaseThree QueryFunctionDescription

-- compileRoleDescription :: ContextType -> Step -> MonadPerspectives QueryFunctionDescription
-- compileRoleDescription ct s = do
--   r <- runExceptT (compileStep (CDOM $ ST ct) s)
--   case r of
--     (Left m) -> throwError (error (show m))
--     (Right d) -> pure d

greaterThanOrEqualTo_ :: Domain -> Domain -> MonadPerspectives Boolean
greaterThanOrEqualTo_ = flip lessThenOrEqualTo_

-- | `p lessThenOrEqualTo q` means: p is less specific than q, or equal to q.
-- | This function is semantically correct only on a fully expanded types: use `Perspectives.Representation.Class.Role.expandedADT`.
lessThenOrEqualTo_ :: Domain -> Domain -> MonadPerspectives Boolean
lessThenOrEqualTo_ (RDOM adtL) (RDOM adtR) = pure (adtL `lessThenOrEqualTo` adtR)
lessThenOrEqualTo_ (CDOM adtL) (CDOM adtR) = pure (adtL `lessThenOrEqualTo` adtR)
lessThenOrEqualTo_ (VDOM r1) (VDOM r2) = pure $ r1 `eq` r2
lessThenOrEqualTo_ _ _ = pure false
