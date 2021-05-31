-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Query.ExpressionCompiler where

-- | From the Abstract Syntax Tree that results from a query-path expression (see `Perspectives.QueryAST` for the AST),
-- | create a QueryFunctionDescription data element.
-- | The code in this module sees to it that each function is applied to the right type of arguments.
-- | For example, each step in a path must be followed by a step that takes as its domain the
-- | range of its predecessor. Otherwise, an error is thrown that will be presented to the modeller.
-- | Use `compileAndDistributeStep` to create the QueryFunctionDescription *and* invert it,
-- | and distribute it throughout the domain.

import Control.Monad.Except (lift, throwError)
import Control.Monad.State (gets)
import Data.Array (elemIndex, filter, foldM, fromFoldable, head, length, null, reverse, uncons)
import Data.Map (Map, empty, singleton)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (keys)
import Partial.Unsafe (unsafePartial)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (modifyCalculatedPropertyInDomeinFile, modifyCalculatedRoleInDomeinFile)
import Perspectives.External.CoreModuleList (isExternalCoreModule)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs)
import Perspectives.Identifiers (deconstructModelName, endsWithSegments, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.ContextualVariables (addContextualVariablesToExpression)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, isIndexedContext, isIndexedRole, lift2, lookupVariableBinding, withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Kinked (setInvertedQueries)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain, domain2roleType, functional, mandatory, propertyOfRange, range, roleRange, sumOfDomains, traverseQfd)
import Perspectives.Query.QueryTypes (Range) as QT
import Perspectives.Representation.ADT (ADT(..), sum)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getCalculatedProperty, getCalculatedRole, getEnumeratedProperty, getEnumeratedRole, typeExists)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, range) as PROP
import Perspectives.Representation.Class.Role (binding, bindingOfADT, contextOfADT, externalRoleOfADT, hasNotMorePropertiesThan, roleADT, roleTypeIsFunctional, roleTypeIsMandatory, typeExcludingBinding)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.QueryFunction (FunctionName(..), isFunctionalFunction)
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued, pessimistic)
import Perspectives.Representation.ThreeValuedLogic (and, or, ThreeValuedLogic(..)) as THREE
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Types.ObjectGetters (isUnlinked_, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT, qualifyEnumeratedRoleInDomain)
import Prelude (bind, discard, eq, map, pure, show, void, ($), (&&), (<$>), (<*>), (<<<), (==), (>>=), (<>))

------------------------------------------------------------------------------------
------ MONAD TYPE FOR DESCRIPTIONCOMPILER
------------------------------------------------------------------------------------
type FD = PhaseThree QueryFunctionDescription

------------------------------------------------------------------------------------
------ COMPILING EXPRESSIONS
-- Compiles a Step and qualifies any returns clauses in it.
------------------------------------------------------------------------------------
compileExpression :: Domain -> Step -> FD
compileExpression domain stp = compileStep domain stp >>= traverseQfd (qualifyReturnsClause (startOf stp))

------------------------------------------------------------------------------------
------ COMPILING ROLE REFERENCES
------------------------------------------------------------------------------------
-- Describe a conjunction of rolegetters.
makeConjunction :: Domain -> QueryFunctionDescription -> RoleType -> FD
makeConjunction currentDomain left rt2 = do
  right <- makeRoleGetter currentDomain rt2
  (rightADT :: ADT EnumeratedRoleType) <- lift2 $ typeExcludingBinding rt2
  pure $ BQD currentDomain (QF.BinaryCombinator UnionF) left right (RDOM (sum [unsafePartial roleRange left, rightADT])) THREE.False (THREE.or (mandatory left) (mandatory right))

-- | Constructs a QueryFunctionDescription that describes getting a role of the given type.
-- | CalculatedRoles are compiled, when necessary. The result of such an on-the-fly compilation is saved
-- | in the domeinCache.
makeRoleGetter :: Domain -> RoleType -> PhaseThree QueryFunctionDescription
makeRoleGetter currentDomain rt@(CR ct) = do
  (adt :: ADT EnumeratedRoleType) <- do
    crole@(CalculatedRole{calculation}) <- lift2 $ getCalculatedRole ct
    case calculation of
      Q qfd -> pure $ unsafePartial domain2roleType $ range qfd
      S step -> compileAndSaveRole currentDomain step crole
  isF <- lift2 $ roleTypeIsFunctional rt
  isM <- lift2 $ roleTypeIsMandatory rt
  pure $ SQD currentDomain (QF.RolGetter rt) (RDOM $ adt) (bool2threeValued isF) (bool2threeValued isM)

makeRoleGetter currentDomain rt@(ENR et) = do
  unlinked <- lift2 $ isUnlinked_ et
  adt <- lift2 (getEnumeratedRole et >>= roleADT)
  isF <- lift2 $ roleTypeIsFunctional rt
  isM <- lift2 $ roleTypeIsMandatory rt
  if unlinked
    then pure $ SQD currentDomain (QF.DataTypeGetterWithParameter QF.GetRoleInstancesForContextFromDatabaseF (unwrap et)) (RDOM $ adt) (bool2threeValued isF) (bool2threeValued isM)
    else pure $ SQD currentDomain (QF.RolGetter rt) (RDOM $ adt) (bool2threeValued isF) (bool2threeValued isM)

-- | Compiles the parsed expression (type Step) that defines the CalculatedRole.
-- | Saves it in the DomainCache.
compileAndSaveRole :: Domain -> Step -> CalculatedRole -> PhaseThree (ADT EnumeratedRoleType)
compileAndSaveRole dom step (CalculatedRole cr@{_id}) = withFrame do
  expressionWithEnvironment <- addContextualVariablesToExpression step Nothing
  compiledExpression <- compileExpression dom expressionWithEnvironment
  -- Save the result in DomeinCache.
  lift2 $ void $ modifyCalculatedRoleInDomeinFile (unsafePartial fromJust $ deconstructModelName (unwrap _id)) (CalculatedRole cr {calculation = Q compiledExpression})
  pure $ unsafePartial $ domain2roleType $ range compiledExpression

-- | Ensures that the range of the QueryFunctionDescription is a qualified
-- | EnumeratedRole.
qualifyReturnsClause :: ArcPosition -> QueryFunctionDescription -> PhaseThree QueryFunctionDescription
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCoreRoleGetter f) args (RDOM (ST (EnumeratedRoleType computedType))) isF isM) = do
  -- Note that it doesn't matter if we take the roles from the cache or from
  -- PhaseThreeState: the role identifiers are identical.
  enumeratedRoles <- (lift $ gets _.dfr) >>= pure <<< _.enumeratedRoles
  computedTypeADT <- ST <$> qualifyLocalEnumeratedRoleName pos computedType (keys enumeratedRoles)
  case computedTypeADT of
    ST (EnumeratedRoleType qComputedType) | computedType == qComputedType -> pure qfd
    _ -> pure (MQD dom' (QF.ExternalCoreRoleGetter f) args (RDOM computedTypeADT) isF isM)
qualifyReturnsClause pos qfd = pure qfd

qualifyLocalEnumeratedRoleName :: ArcPosition -> String -> Array String -> PhaseThree EnumeratedRoleType
qualifyLocalEnumeratedRoleName pos ident roleIdentifiers = EnumeratedRoleType <$> (qualifyLocalRoleName_ pos ident roleIdentifiers )

qualifyLocalCalculatedRoleName :: ArcPosition -> String -> Array String -> PhaseThree CalculatedRoleType
qualifyLocalCalculatedRoleName pos ident roleIdentifiers = CalculatedRoleType <$> (qualifyLocalRoleName_ pos ident roleIdentifiers )

qualifyLocalRoleName_ :: ArcPosition -> String -> Array String -> PhaseThree String
qualifyLocalRoleName_ pos ident roleIdentifiers = if isQualifiedWithDomein ident
  then pure ident
  else do
    (candidates :: Array String) <- pure $ filter (\_id -> _id `endsWithSegments` ident) roleIdentifiers
    case head candidates of
      Nothing -> throwError $ UnknownRole pos ident
      (Just qname) | length candidates == 1 -> pure qname
      otherwise -> throwError $ NotUniquelyIdentifying pos ident candidates

------------------------------------------------------------------------------------
------ COMPILING PROPERTY REFERENCES
------------------------------------------------------------------------------------

-- | Constructs a QueryFunctionDescription that describes getting a property of the given type.
-- | CalculatedRoles and Properties are compiled, when necessary. The result of such an on-the-fly compilation is saved
-- | in the domeinCache.
makePropertyGetter :: Domain -> PropertyType -> PhaseThree QueryFunctionDescription
makePropertyGetter currentDomain pt = do
  (ran :: QT.Range) <- case pt of
    ENP ep -> lift2 (getEnumeratedProperty ep >>= PROP.range >>= \r -> pure $ VDOM r (Just pt))
    CP cp -> do
      cprop@(CalculatedProperty{calculation}) <- lift2 $ getCalculatedProperty cp
      case calculation of
        Q qfd -> pure (range qfd)
        S step -> compileAndSaveProperty currentDomain step cprop
  isF <- lift2 $ PROP.propertyTypeIsFunctional pt
  isM <- lift2 $ PROP.propertyTypeIsMandatory pt
  pure $ SQD currentDomain (QF.PropertyGetter pt) ran (bool2threeValued isF) (bool2threeValued isM)

-- | Compiles the parsed expression (type Step) that defines the CalculatedRole.
-- | Saves it in the DomainCache.
compileAndSaveProperty :: Domain -> Step -> CalculatedProperty -> PhaseThree QT.Range
compileAndSaveProperty dom step (CalculatedProperty cp@{_id}) = withFrame do
  -- We add the role as the variable "object"
  expressionWithEnvironment <- addContextualVariablesToExpression step (Just $ Simple $ Identity (startOf step))
  compiledExpression <- compileExpression dom expressionWithEnvironment
  -- Save the result in DomeinCache.
  lift2 $ void $ modifyCalculatedPropertyInDomeinFile (unsafePartial fromJust $ deconstructModelName (unwrap _id)) (CalculatedProperty cp {calculation = Q compiledExpression})
  pure $ range compiledExpression

------------------------------------------------------------------------------------
------ COMPILING STEPS AND DISTRIBUTING THEIR INVERSION OVER THE DOMEINFILE.
------------------------------------------------------------------------------------
-- | Use `compileAndDistributeStep` to compile a parsed expression into a QueryFunctionDescription, and to
-- | distribute inverted versions of it over all definitions of EnumeratedRoles and EnumeratedProperties that
-- | are visited during query traversal. These inverted versions are used to compute the users that should be
-- | informed of changes.
-- | This function calls [compileStep](Perspectives.Query.ExpressionCompiler.html#t:compileStep).
-- | It also has a side effect on the DomeinFileRecord that is kept in [PhaseTwoState](Perspectives.Parsing.Arc.PhaseTwoDefs.html#t:PhaseTwoState): it
-- |  * changes EnumeratedRoles
-- |  * changes EnumeratedProperties
-- | We only call `compileAndDistributeStep` in the function `compileStates`. This function
-- | also modifies the DomeinFileRecord, but just the CalculatedRole, CalculatedProperty and Action definitions in it.
-- | Hence we do not risk to modify a definition that will be overwritten soon after without including that modification.
compileAndDistributeStep ::
  Domain ->
  Step ->
  Array RoleType ->
  Array StateIdentifier ->
  PhaseThree QueryFunctionDescription
compileAndDistributeStep dom stp users stateIdentifiers = do
  -- log ("compileAndDistributeStep:\n" <> "  step = " <> show stp <> "\n  users = " <> show users <> "\n  stateIdentifiers = " <> show stateIdentifiers)
  descr <- compileExpression dom stp
  -- logShow descr
  -- The description may be a path and then should be seen as an implicit perspective on its results, like a CalculatedProperty (it could also be a constant, or it could result in a ContextInstance or a RoleInstance).
  -- Hence we should create a Map of the PropertyType and the StateIdentifier.
  (statesPerProperty :: Map PropertyType (Array StateIdentifier)) <- pure case propertyOfRange descr of
    Nothing -> empty
    Just p -> singleton p stateIdentifiers
  setInvertedQueries users statesPerProperty stateIdentifiers descr
  pure descr


------------------------------------------------------------------------------------
------ COMPILING STEPS
------------------------------------------------------------------------------------
compileStep :: Domain -> Step -> FD
compileStep currentDomain (Simple st) = compileSimpleStep currentDomain st
compileStep currentDomain (Unary st) = compileUnaryStep currentDomain st
compileStep currentDomain (Binary st) = compileBinaryStep currentDomain st
compileStep currentDomain (PureLet st) = compileLetStep currentDomain st
compileStep currentDomain (Computation st) = compileComputationStep currentDomain st

------------------------------------------------------------------------------------
------ COMPILING SIMPLE STEPS
------------------------------------------------------------------------------------
compileSimpleStep :: Domain -> SimpleStep -> FD
compileSimpleStep currentDomain s@(ArcIdentifier pos ident) = do
  mindexedContextType <- isIndexedContext ident
  case mindexedContextType of
    Just indexedContextType -> pure $ SQD currentDomain (QF.ContextIndividual (ContextInstance ident)) (CDOM (ST indexedContextType)) True True
    Nothing -> do
      mindexedRoleType <- isIndexedRole ident
      case mindexedRoleType of
        Just indexedRoleType -> pure $ SQD currentDomain (QF.RoleIndividual (RoleInstance ident)) (RDOM (ST indexedRoleType)) True True
        Nothing -> case currentDomain of
          (CDOM c) -> do
            (rts :: Array RoleType) <- if isQualifiedWithDomein ident
              then lift2 $ runArrayT $ lookForRoleTypeOfADT ident c
              else if ident == "External"
                then case c of
                  (ST (ContextType cid)) -> pure [ENR (EnumeratedRoleType (cid <> "$External"))]
                  otherwise -> throwError $ Custom ("Cannot get the external role of a compound type: " <> show otherwise)
                else lift2 $ runArrayT $ lookForUnqualifiedRoleTypeOfADT ident c
            case uncons rts of
              Nothing -> throwError $ ContextHasNoRole c ident
              Just {head, tail} -> if null tail
                then makeRoleGetter currentDomain head
                else do
                  -- TODO. Is dit wat we willen? Een conjunctie (disjunctie?)
                  -- van rollen wier lokale namen matchen?
                  head' <- makeRoleGetter currentDomain head
                  foldM (makeConjunction currentDomain) head' tail
          (RDOM r) -> do
            (pts :: Array PropertyType) <- if isQualifiedWithDomein ident
              then  lift2 $ runArrayT $ lookForPropertyType ident r
              else lift2 $ runArrayT $ lookForUnqualifiedPropertyType ident r
            case uncons pts of
              Nothing -> throwError $ RoleHasNoProperty r ident
              Just {head:pt, tail} -> if null tail
                then makePropertyGetter currentDomain pt
                else throwError $ NotUniquelyIdentifying pos ident (show <$> pts)
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
      (bindingOfBinder :: (ADT EnumeratedRoleType)) <- lift2 $ getEnumeratedRole qBinderType >>= binding
      lessEq <- lift2 $ hasNotMorePropertiesThan bindingOfBinder r
      if lessEq
        then pure $ SQD currentDomain (QF.DataTypeGetterWithParameter GetRoleBindersF (unwrap qBinderType)) (RDOM $ ST qBinderType) True False
        else throwError $ RoleDoesNotBind pos (ENR qBinderType) r
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Context pos) = do
  case currentDomain of
    (RDOM (r :: ADT EnumeratedRoleType)) -> do
      (typeOfContext :: ADT ContextType) <- lift2 $ contextOfADT r
      pure $ SQD currentDomain (QF.DataTypeGetter ContextF) (CDOM typeOfContext) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(TypeOfContext pos) = do
  case currentDomain of
    (CDOM (r :: ADT ContextType)) -> do
      pure $ SQD currentDomain (QF.TypeGetter TypeOfContextF) ContextKind True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(RoleTypes pos) = do
  case currentDomain of
    ContextKind -> do
      pure $ SQD currentDomain (QF.TypeGetter RoleTypesF) RoleKind True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(SpecialisesRoleType pos roleName) = do
  case currentDomain of
    RoleKind -> do
      -- TODO: controleer of roleName inderdaad een EnumeratedRole is!
      (qRoleName :: EnumeratedRoleType) <- if isQualifiedWithDomein roleName
        then pure $ EnumeratedRoleType roleName
        -- Try to qualify the name within the Domain.
        else do
          {_id:namespace} <- lift $ gets _.dfr
          (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain roleName (unsafePartial $ fromJust $ (deconstructModelName namespace))
          case head qnames of
            Nothing -> throwError $ UnknownRole pos roleName
            (Just qn) | length qnames == 1 -> pure qn
            otherwise -> throwError $ NotUniquelyIdentifying pos roleName (map unwrap qnames)
      pure $ SQD currentDomain (QF.DataTypeGetterWithParameter SpecialisesRoleTypeF (unwrap qRoleName)) (VDOM PBool Nothing) (isFunctionalFunction SpecialisesRoleTypeF) False
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Extern pos) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: ADT EnumeratedRoleType) <- lift2 $ externalRoleOfADT c
      pure $ SQD currentDomain (QF.DataTypeGetter ExternalRoleF) (RDOM $ rts) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

-- compileSimpleStep currentDomain (CreateContext pos ident) = do
--   -- If `ident` is not qualified, try to qualify it in the Domain.
--   (qcontextType :: ContextType) <- if isQualifiedWithDomein ident
--     then pure $ ContextType ident
--     -- Try to qualify the name within the Domain.
--     else do
--       {_id:namespace} <- lift $ gets _.dfr
--       (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain ident (unsafePartial $ fromJust $ (deconstructModelName namespace))
--       case head qnames of
--         Nothing -> throwError $ UnknownContext pos ident
--         (Just qn) | length qnames == 1 -> pure qn
--         otherwise -> throwError $ NotUniquelyIdentifying pos ident (map unwrap qnames)
--   pure $ SQD currentDomain (QF.DataTypeGetterWithParameter CreateContextF (unwrap qcontextType)) (CDOM (ST qcontextType)) True True

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

compileSimpleStep currentDomain s@(Modelname _) = case currentDomain of
  VDOM _ Nothing -> throwError $ NoPropertyTypeWithValue (startOf (Simple s)) (endOf (Simple s))
  _ -> pure $ SQD currentDomain (QF.DataTypeGetter ModelNameF) (VDOM PString Nothing) Unknown True

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

compileUnaryStep currentDomain st@(Binds pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    RDOM _ -> pure $ UQD currentDomain (QF.UnaryCombinator BindsF) descriptionOfs (VDOM PBool Nothing) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)

compileUnaryStep currentDomain st@(BoundBy pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    RDOM _ -> pure $ UQD currentDomain (QF.UnaryCombinator BoundByF) descriptionOfs (VDOM PBool Nothing) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)

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
        VDOM PBool _ -> if pessimistic $ functional f2
          then pure $ BQD currentDomain (QF.BinaryCombinator FilterF) f1 f2 (range f1) (functional f1) False
          else throwError $ NotFunctional (startOf right) (endOf right) right
        otherwise -> throwError $ NotABoolean (startOf right)
    Compose pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep (range f1) right
      -- TODO. An optimalisation: if the left or right term is Identity, replace the entire composition by the other term.
      pure $ BQD currentDomain (QF.BinaryCombinator ComposeF) f1 f2 (range f2) (THREE.and (functional f1)(functional f2)) (THREE.and (mandatory f1)(mandatory f2))
    Union pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right
      case (range f1), (range f2) of
        (RDOM _), (RDOM _) -> pure $ BQD currentDomain (QF.BinaryCombinator UnionF) f1 f2 (unsafePartial $ fromJust $ sumOfDomains (range f1)(range f2)) False (THREE.and (mandatory f1)(mandatory f2))
        _, _ -> throwError $ NotARoleDomain currentDomain (startOf left) (endOf right)
    Intersection pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right
      -- TODO. Als de types een lege doorsnede hebben, een waarschuwing geven?
      case (range f1), (range f2) of
        (RDOM _), (RDOM _) -> pure $ BQD currentDomain (QF.BinaryCombinator IntersectionF) f1 f2 (unsafePartial $ fromJust $ sumOfDomains (range f1)(range f2)) False (THREE.and (mandatory f1)(mandatory f2))
        _, _ -> throwError $ NotARoleDomain currentDomain (startOf left) (endOf right)

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

        Compose _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Compose"
        Filter _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Filter"
        Union _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Union"
        Intersection _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Intersection"

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
      -- Both ranges must be equal, both sides must be functional.
      gt <- lift2 $ pure (((range left') `eq` (range right')) && (pessimistic $ functional left') && (pessimistic $ functional right'))
      if gt
        then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM PBool Nothing) (isFunctionalFunction functionName) True
        else throwError $ TypesCannotBeCompared pos (range left') (range right')

    binOp :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> Array Range -> FunctionName -> PhaseThree QueryFunctionDescription
    binOp pos left' right' allowedRangeConstructors functionName = case range left', range right' of
      -- Both ranges must be equal, both sides must be functional.
      (VDOM rc1 _), (VDOM rc2 _) | rc1 == rc2 ->
        if  allowed rc1 && allowed rc2  && (pessimistic $ functional left') && (pessimistic $ functional right')
          then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM rc1 Nothing) (isFunctionalFunction functionName) True
          else throwError $ WrongTypeForOperator pos allowedRangeConstructors
      l, r -> throwError $ TypesCannotBeCompared pos l r
      where
        allowed :: Range -> Boolean
        allowed r = isJust $ elemIndex r allowedRangeConstructors

-- | Compile a PureLetStep into a sequence of QueryFunctionDescriptions that ends with the body.
-- | Each binding compiles to a description of a function that will add a name-value pair to the runtime environment.
compileLetStep :: Domain -> PureLetStep -> FD
compileLetStep currentDomain (PureLetStep{bindings, body}) = do
  let_ <- compileLetStep_
  pure (UQD currentDomain QF.WithFrame let_ (range let_) (functional let_) (mandatory let_))

  where
    compileLetStep_ :: FD
    compileLetStep_ = withFrame
      case uncons bindings of
        -- no bindings at all. Just the body. This will probably never occur as the parser breaks on it.
        Nothing -> compileStep currentDomain body
        (Just {head: bnd, tail}) -> do
          -- compileVarBinding also adds a variable binding to the compile time environment.
          head_ <- compileVarBinding currentDomain bnd
          makeSequence <$> foldM addVarBindingToSequence head_ tail <*> compileStep currentDomain body

-- The range of a sequence equals that of its second term.
-- The fold is left associative: ((binding1 *> binding2) *> binding3). The compiler handles that ok.
addVarBindingToSequence :: QueryFunctionDescription -> VarBinding -> FD
addVarBindingToSequence seq v = makeSequence <$> pure seq <*> (compileVarBinding (domain seq) v)

makeSequence :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
makeSequence left right = BQD (domain left) (QF.BinaryCombinator SequenceF) left right (range right) (THREE.and (functional left) (functional right)) (THREE.or (functional left) (functional right))

-- | Make a QueryFunctionDescription of a runtime function that evaluates the step of the binding and
-- | adds a name-value pair to the runtime environment. Add the name-QueryFunctionDescription pair to
-- | the compile time environment (PhaseThree).
compileVarBinding :: Domain -> VarBinding -> FD
compileVarBinding currentDomain (VarBinding varName step) = do
  step_ <- compileStep currentDomain step
  addBinding varName step_
  pure $ UQD currentDomain (QF.BindVariable varName) step_ (range step_) (functional step_) (mandatory step_)

compileComputationStep :: Domain -> ComputationStep -> FD
compileComputationStep currentDomain (ComputationStep {functionName, arguments, computedType, start, end}) =
  case (deconstructModelName functionName) of
    Nothing -> throwError (NotWellFormedName start functionName)
    Just modelName -> if isExternalCoreModule modelName
      then do
        -- We cannot do this without introducing a cycle.
        -- Instead, we start up main with addAllExternalFunctions
        -- addExternalFunctionForModule modelName
        compiledArgs <- traverse (compileStep currentDomain) arguments
        (let
          mexpectedNrOfArgs = lookupHiddenFunctionNArgs functionName
          in case mexpectedNrOfArgs of
            Nothing -> throwError (UnknownExternalFunction start end functionName)
            Just expectedNrOfArgs -> if expectedNrOfArgs == length arguments
              then case mapToRange computedType of
                -- Collect property instances.
                Just r -> pure $ MQD currentDomain (QF.ExternalCorePropertyGetter functionName) (fromFoldable compiledArgs) (VDOM r Nothing) Unknown Unknown
                Nothing -> (lift $ lift $ typeExists (ContextType computedType)) >>= if _
                  -- Collect Context instances.
                  then pure $ SQD currentDomain (QF.ExternalCoreContextGetter functionName) (CDOM (ST (ContextType computedType))) Unknown Unknown

                  -- Collect role instances.
                  else pure $ MQD currentDomain (QF.ExternalCoreRoleGetter functionName) (fromFoldable compiledArgs) (RDOM (ST (EnumeratedRoleType computedType))) Unknown Unknown
              else throwError (WrongNumberOfArguments start end functionName expectedNrOfArgs (length arguments)))
      else do
        compiledArgs <- traverse (compileStep currentDomain) arguments
        -- TODO. Check whether the foreign function exists and whether it has been given the right number of arguments.
        pure $ MQD currentDomain (QF.ForeignRoleGetter functionName) (fromFoldable compiledArgs) (RDOM (ST (EnumeratedRoleType computedType))) Unknown Unknown

  where

    mapToRange :: String -> Maybe Range
    mapToRange s = case s of
      "String" -> Just PString
      "Boolean" -> Just PBool
      "Number" -> Just PNumber
      "DateTime" -> Just PDate
      otherwise -> Nothing
