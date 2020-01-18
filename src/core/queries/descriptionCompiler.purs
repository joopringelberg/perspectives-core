-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Query.DescriptionCompiler where

-- | From the Abstract Syntax Tree that results from a query-path expression (see `Perspectives.QueryAST` for the AST),
-- | create a QueryFunctionDescription data element.
-- | The code in this module sees to it that each function is applied to the right type of arguments.
-- | For example, each step in a path must be followed by a step that takes as its domain the
-- | range of its predecessor. Otherwise, an error is thrown that will be presented to the modeller.

import Control.Monad.Except (lift, throwError)
import Control.Monad.State (gets)
import Data.Array (elemIndex, head, length, uncons, foldM)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Identifiers (deconstructModelName, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.Expression (startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, lift2, lookupVariableBinding, withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain, functional, mandatory, range)
import Perspectives.Representation.ADT (ADT(..), lessThanOrEqualTo)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, rangeOfPropertyType)
import Perspectives.Representation.Class.Role (bindingOfADT, contextOfADT, expandedADT_, roleTypeIsFunctional, roleTypeIsMandatory, typeExcludingBinding_)
import Perspectives.Representation.QueryFunction (FunctionName(..), isFunctionalFunction)
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued)
import Perspectives.Representation.ThreeValuedLogic (and, or) as THREE
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), PropertyType, RoleType(..))
import Perspectives.Types.ObjectGetters (externalRoleOfADT, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT, qualifyContextInDomain, qualifyEnumeratedRoleInDomain)
import Prelude (bind, eq, map, pure, ($), (==), (&&), discard, (<$>), (<*>))

compileStep :: Domain -> Step -> FD
compileStep currentDomain (Simple st) = compileSimpleStep currentDomain st
compileStep currentDomain (Unary st) = compileUnaryStep currentDomain st
compileStep currentDomain (Binary st) = compileBinaryStep currentDomain st
compileStep currentDomain (PureLet st) = compileLetStep currentDomain st
compileStep currentDomain (Let st) = throwError $ NotAPureLet st

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
          (typeExcludingBinding :: ADT EnumeratedRoleType) <- lift $ lift $ typeExcludingBinding_ rt
          isF <- lift2 $ roleTypeIsFunctional rt
          isM <- lift2 $ roleTypeIsMandatory rt
          pure $ SQD currentDomain (QF.RolGetter rt) (RDOM $ typeExcludingBinding) (bool2threeValued isF) (bool2threeValued isM)
    (RDOM r) -> do
      (pts :: Array PropertyType) <- if isQualifiedWithDomein ident
        then  lift2 $ runArrayT $ lookForPropertyType ident r
        else lift2 $ runArrayT $ lookForUnqualifiedPropertyType ident r
      case head pts of
        Nothing -> throwError $ RoleHasNoProperty r ident
        (Just (pt :: PropertyType)) -> do
          isF <- lift2 $ propertyTypeIsFunctional pt
          isM <- lift2 $ propertyTypeIsMandatory pt
          (rOfpt :: Range) <- lift2 $ rangeOfPropertyType pt
          pure $ SQD currentDomain (QF.PropertyGetter pt) (VDOM rOfpt (Just pt)) (bool2threeValued isF) (bool2threeValued isM)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Value pos range stringRepresentation) = pure $
  SQD currentDomain (QF.Constant range stringRepresentation) (VDOM range Nothing) True True

compileSimpleStep currentDomain s@(Binding pos) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      -- The binding of a role is always an ADT EnumeratedRoleType.
      (typeOfBinding :: (ADT EnumeratedRoleType)) <- lift2 $ bindingOfADT r
      case typeOfBinding of
        UNIVERSAL -> throwError $ RoleHasNoBinding pos r
        otherwise -> pure $ SQD currentDomain (QF.DataTypeGetter BindingF) (RDOM typeOfBinding) True False
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
      if r `lessThanOrEqualTo` bindingOfBinder
        then pure $ SQD currentDomain (QF.DataTypeGetterWithParameter GetRoleBindersF (unwrap qBinderType)) (RDOM $ ST qBinderType) False False
        else throwError $ RoleDoesNotBind pos (ENR qBinderType) r
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Context pos) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      (typeOfContext :: ADT ContextType) <- lift2 $ contextOfADT r
      pure $ SQD currentDomain (QF.DataTypeGetter ContextF) (CDOM typeOfContext) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Extern pos) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: ADT EnumeratedRoleType) <- lift2 $ externalRoleOfADT c
      pure $ SQD currentDomain (QF.DataTypeGetter ExternalRoleF) (RDOM $ rts) True True
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
  pure $ SQD currentDomain (QF.DataTypeGetterWithParameter CreateContextF (unwrap qcontextType)) (CDOM (ST qcontextType)) True True

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
  pure $ SQD currentDomain (QF.DataTypeGetterWithParameter CreateRoleF (unwrap qroleType)) (RDOM (ST qroleType)) True True

compileSimpleStep currentDomain (Identity _) = pure $ SQD currentDomain (QF.DataTypeGetter IdentityF) currentDomain Unknown True

-- We compile the SequenceFunction as a UnaryCombinator, which is a stretch.
compileSimpleStep currentDomain (SequenceFunction _ fname) = pure $ SQD currentDomain (QF.UnaryCombinator fname) currentDomain (isFunctionalFunction fname) True

compileSimpleStep currentDomain (Variable pos varName) = do
  mBinding <- lookupVariableBinding varName
  case mBinding of
    Nothing -> throwError $ UnknownVariable pos varName
    (Just fdesc) -> pure $ SQD currentDomain (QF.VariableLookup varName) (range fdesc) (functional fdesc) (mandatory fdesc)

compileUnaryStep :: Domain -> UnaryStep -> FD
compileUnaryStep currentDomain (LogicalNot pos s) = do
  -- First compile s. Then check that the resulting QueryFunctionDescription is a (VDOM PBool _) range value.
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM PBool _ -> pure $ UQD currentDomain (QF.UnaryCombinator NotF) descriptionOfs (VDOM PBool Nothing) (functional descriptionOfs) (mandatory descriptionOfs)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain s

compileUnaryStep currentDomain st@(Exists pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    CDOM _ -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)
    otherwise -> pure $ UQD currentDomain (QF.UnaryCombinator ExistsF) descriptionOfs (VDOM PBool Nothing) True True

compileUnaryStep currentDomain st@(Available pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM _ _ -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)
    otherwise -> pure $ UQD currentDomain (QF.UnaryCombinator AvailableF) descriptionOfs (VDOM PBool Nothing) True True

compileBinaryStep :: Domain -> BinaryStep -> FD
compileBinaryStep currentDomain s@(BinaryStep{operator, left, right}) =
  case operator of
    Filter pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep (range f1) right
      -- f1 is the source to be filtered, f2 is the criterium.
      case range f2 of
        VDOM PBool _ -> pure $ BQD currentDomain (QF.BinaryCombinator FilterF) f1 f2 (range f1) (functional f1) False
        otherwise -> throwError $ NotABoolean (startOf right)
    Compose pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep (range f1) right
      -- TODO. An optimalisation: if the left or right term is Identity, replace the entire composition by the other term.
      pure $ BQD currentDomain (QF.BinaryCombinator ComposeF) f1 f2 (range f2) (THREE.and (functional f1)(functional f2)) (THREE.and (mandatory f1)(mandatory f2))

    otherwise -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right

      case operator of

        Equals pos -> comparison pos f1 f2 EqualsF
        NotEquals pos -> comparison pos f1 f2 NotEqualsF
        LessThan pos -> comparison pos f1 f2 LessThanF
        LessThanEqual pos -> comparison pos f1 f2 LessThanEqualF
        GreaterThan pos -> comparison pos f1 f2 GreaterThanF
        GreaterThanEqual pos -> comparison pos f1 f2 GreaterThanEqualF

        LogicalAnd pos -> binOp pos f1 f2 [PBool] AndF
        LogicalOr pos -> binOp pos f1 f2 [PBool] OrF
        Add pos -> binOp pos f1 f2 [PNumber, PString] AddF
        Subtract pos -> binOp pos f1 f2 [PNumber, PString] SubtractF
        Divide pos -> binOp pos f1 f2 [PNumber] DivideF
        Multiply pos -> binOp pos f1 f2 [PNumber] MultiplyF

        Compose _ -> throwError $ Custom "This case in compileBinaryStep should never be reached"
        Filter _ -> throwError $ Custom "This case in compileBinaryStep should never be reached"

        -- >>= is parsed as the operator Sequence.
        -- "sum", "product", "minimum", "maximum" and "count" are parsed as SequenceFunction
        -- step >>= f is parsed as the BinaryStep we're dealing here with now.
        Sequence pos -> case f2 of
          -- f2 results from the expression that follows `>>=` (must have been: "sum", "product", etc.).
          -- This was parsed as `SequenceFunction f` and is now compiled as `UnaryCombinator f` in an SQD.
          -- Notice by the domain and range that we assume functions that are Monoids.
          -- Notice the strangeness of compiling a binary expression into an SQD description.
          SQD dom (QF.UnaryCombinator fname) _ _ _-> case fname of
            -- we can count anything and the result is a number.
            CountF -> pure $ SQD currentDomain (QF.DataTypeGetter fname) (VDOM PNumber Nothing) True True
            -- We have interpretations of AddF, SubtractF for numbers and strings only.
            -- For MinimumF and MaximumF we have interpretations for numbers and strings and booleans and dates.
            -- For AndF and OrF we have an interpretation for Booleans only.
            -- We also require that the VDOM should have an EnumeratedPropertyType.
            AddF -> ensureDomainIsRange dom [PNumber, PString] pos (pure $ SQD currentDomain (QF.DataTypeGetter fname) currentDomain True True)
            SubtractF -> ensureDomainIsRange dom [PNumber, PString] pos (pure $ SQD currentDomain (QF.DataTypeGetter fname) currentDomain True True)
            MinimumF -> ensureDomainIsRange dom [PNumber, PString, PBool, PDate] pos (pure $ SQD currentDomain (QF.DataTypeGetter fname) currentDomain True True)
            MaximumF -> ensureDomainIsRange dom [PNumber, PString, PBool, PDate] pos (pure $ SQD currentDomain (QF.DataTypeGetter fname) currentDomain True True)
            AndF -> ensureDomainIsRange dom [PBool] pos (pure $ SQD currentDomain (QF.DataTypeGetter fname) currentDomain True True)
            OrF -> ensureDomainIsRange dom [PBool] pos (pure $ SQD currentDomain (QF.DataTypeGetter fname) currentDomain True True)
            _ -> throwError $ ArgumentMustBeSequenceFunction pos
          _ -> throwError $ ArgumentMustBeSequenceFunction pos

  where
    ensureDomainIsRange :: Domain -> Array Range -> ArcPosition -> FD -> FD
    ensureDomainIsRange (VDOM r p) allowedRangeConstructors pos fd = if (isJust $ elemIndex r allowedRangeConstructors) && (isJust p)
      then fd
      else throwError $ WrongTypeForOperator pos allowedRangeConstructors
    ensureDomainIsRange _ allowedRangeConstructors pos _ = throwError $ WrongTypeForOperator pos allowedRangeConstructors

    comparison :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> FunctionName -> PhaseThree QueryFunctionDescription
    comparison pos left' right' functionName = do
      -- Both ranges must be equal
      gt <- lift2 $ pure ((range left') `eq` (range right'))
      if gt
        then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM PBool Nothing) (isFunctionalFunction functionName) True
        else throwError $ TypesCannotBeCompared pos (range left') (range right')

    binOp :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> Array Range -> FunctionName -> PhaseThree QueryFunctionDescription
    binOp pos left' right' allowedRangeConstructors functionName = case range left', range right' of
      (VDOM rc1 _), (VDOM rc2 _) | rc1 == rc2 ->
        if  allowed rc1 && allowed rc2
          then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM rc1 Nothing) (isFunctionalFunction functionName) True
          else throwError $ WrongTypeForOperator pos allowedRangeConstructors
      l, r -> throwError $ TypesCannotBeCompared pos l r
      where
        allowed :: Range -> Boolean
        allowed r = isJust $ elemIndex r allowedRangeConstructors

-- | Compile a PureLetStep into a sequence of QueryFunctionDescriptions that ends with the body.
-- | Each binding compiles to a description of a function that will add a name-value pair to the runtime environment.
compileLetStep :: Domain -> PureLetStep -> FD
compileLetStep currentDomain (PureLetStep{bindings, body}) = withFrame
  case uncons bindings of
    -- no bindings at all. Just the body. This will probably never occur as the parser breaks on it.
    Nothing -> compileStep currentDomain body
    (Just {head: bnd, tail}) -> do
      head_ <- compileVarBinding currentDomain bnd
      makeSequence <$> foldM addVarBindingToSequence head_ tail <*> compileStep currentDomain body

-- The range of a sequence equals that of its second term.
-- The fold is left associative: ((binding1 *> binding2) *> binding3). The compiler handles that ok.
addVarBindingToSequence :: QueryFunctionDescription -> VarBinding -> FD
addVarBindingToSequence seq v = makeSequence <$> pure seq <*> (compileVarBinding (domain seq) v)

makeSequence :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
makeSequence left right = BQD (domain left) (QF.BinaryCombinator SequenceF) left right (range right) (THREE.and (functional left) (functional right)) (THREE.or (functional left) (functional right))

-- | Make a QueryFunctionDescription of a runtime function that evaluates the step of the binding and
-- | adds a name-value pair to the runtime environment. Add the name-QueryFunctionDescription pair to the
-- | compile time environment (PhaseThree).
compileVarBinding :: Domain -> VarBinding -> FD
compileVarBinding currentDomain (VarBinding varName step) = do
  step_ <- compileStep currentDomain step
  addBinding varName step_
  pure $ UQD currentDomain (QF.BindVariable varName) step_ (range step_) (functional step_) (mandatory step_)

type FD = PhaseThree QueryFunctionDescription
