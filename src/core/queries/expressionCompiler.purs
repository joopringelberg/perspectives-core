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

import Control.Monad.Error.Class (catchError, try)
import Control.Monad.Except (lift)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (gets)
import Data.Array (elemIndex, filter, foldM, foldMap, fromFoldable, head, length, null, uncons)
import Data.Either (Either(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((###=))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (modifyCalculatedPropertyInDomeinFile, modifyCalculatedRoleInDomeinFile)
import Perspectives.External.CoreModuleList (isExternalCoreModule)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionCardinality, lookupHiddenFunctionIsEffect, lookupHiddenFunctionNArgs)
import Perspectives.Identifiers (endsWithSegments, isExternalRole, isTypeUri, qualifyWith, typeUri2ModelUri)
import Perspectives.Instances.ObjectGetters (contextType_, roleType_)
import Perspectives.Names (lookupIndexedContext, lookupIndexedRole)
import Perspectives.Parsing.Arc.ContextualVariables (addContextualBindingsToExpression, makeContextStep, makeIdentityStep, stepContainsVariableReference)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), ComputedType(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (setInvertedQueries)
import Perspectives.Parsing.Arc.PhaseTwoDefs (CurrentlyCalculated(..), PhaseThree, addBinding, getsDF, isBeingCalculated, isIndexedContext, isIndexedRole, lift2, lookupVariableBinding, loopErrorMessage, throwError, withCurrentCalculation, withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), context2RoleInContextADT, domain, domain2roleType, equalDomainKinds, functional, makeComposition, mandatory, productOfDomains, range, replaceContext, replaceRange, roleInContext2Role, setCardinality, sumOfDomains, traverseQfd)
import Perspectives.Query.QueryTypes (Range) as QT
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (DomeinFileId(..), StateIdentifier(..), getCalculatedProperty, getCalculatedRole, getContext, getEnumeratedProperty, getEnumeratedRole, typeExists)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, range) as PROP
import Perspectives.Representation.Class.Role (adtIsFunctional, bindingOfADT, contextOfADT, contextOfRepresentation, externalRoleOfADT, getRoleADTFromString, getRoleType, roleADT, roleTypeIsFunctional, roleTypeIsMandatory)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.QueryFunction (FunctionName(..), isFunctionalFunction)
import Perspectives.Representation.Range (Duration_(..), Range(..), isDate, isPDuration, isPMonth, isTime, isTimeDuration)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued, pessimistic)
import Perspectives.Representation.ThreeValuedLogic (and, or) as THREE
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), roletype2string)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..)) as RTI
import Perspectives.Types.ObjectGetters (allTypesInContextADT, allTypesInRoleADT, enumeratedRoleContextType, equals, isUnlinked_, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT, qualifyContextInDomain, qualifyEnumeratedRoleInDomain, qualifyRoleInDomain)
import Prelude (bind, discard, eq, map, pure, show, unit, void, ($), (&&), (-), (<$>), (<*>), (<<<), (<>), (==), (>>=), (||))

------------------------------------------------------------------------------------
------ MONAD TYPE FOR DESCRIPTIONCOMPILER
------------------------------------------------------------------------------------
type FD = PhaseThree QueryFunctionDescription

------------------------------------------------------------------------------------
------ COMPILING EXPRESSIONS
-- Compiles a Step and qualifies any returns clauses in it.
-- Assume that the model being compiled is available.
------------------------------------------------------------------------------------
compileExpression :: Domain -> Step -> FD
compileExpression domain stp = compileStep domain stp >>= traverseQfd (qualifyReturnsClause (startOf stp))

------------------------------------------------------------------------------------
------ COMPILING ROLE REFERENCES
------------------------------------------------------------------------------------
-- | Constructs a QueryFunctionDescription that describes getting a role of the given type.
-- | CalculatedRoles are compiled, when necessary. The result of such an on-the-fly compilation is saved
-- | in the domeinCache.
makeRoleGetter :: Partial => Domain -> RoleType -> PhaseThree QueryFunctionDescription
makeRoleGetter currentDomain rt@(CR ct) = do
  (adt :: ADT RoleInContext) <- do
    crole@(CalculatedRole{calculation}) <- lift2 $ getCalculatedRole ct
    case calculation of
      Q qfd -> lift2 $ roleADT crole
      S step isFunctional -> compileAndSaveRole currentDomain step crole isFunctional
  isF <- lift2 $ roleTypeIsFunctional rt
  isM <- lift2 $ roleTypeIsMandatory rt
  pure $ SQD currentDomain (QF.RolGetter rt) (RDOM adt) (bool2threeValued isF) (bool2threeValued isM)

makeRoleGetter currentDomain@(CDOM contextAdt) rt@(ENR et) = do
  unlinked <- lift2 $ isUnlinked_ et
  isF <- lift2 $ roleTypeIsFunctional rt
  isM <- lift2 $ roleTypeIsMandatory rt
  if unlinked
    then pure $ SQD currentDomain (QF.DataTypeGetterWithParameter QF.GetRoleInstancesForContextFromDatabaseF (unwrap et)) (RDOM $ context2RoleInContextADT contextAdt et) (bool2threeValued isF) (bool2threeValued isM)
    else pure $ SQD currentDomain (QF.RolGetter rt) (RDOM $ context2RoleInContextADT contextAdt et) (bool2threeValued isF) (bool2threeValued isM)

-- | Compiles the parsed expression (type Step) that defines the CalculatedRole.
-- | Saves it in the DomainCache.
-- | Returns the range of the calculation.
compileAndSaveRole :: Domain -> Step -> CalculatedRole -> Boolean -> PhaseThree (ADT RoleInContext)
compileAndSaveRole dom step (CalculatedRole cr@{id, kindOfRole, pos}) considerFunctional = withFrame do
  loops <- isBeingCalculated (Role id)
  if loops
    then throwError $ (RecursiveDefinition $ loopErrorMessage (Role id) pos pos) 
    else withCurrentCalculation (Role id)
      do
        expressionWithEnvironment <- pure $ addContextualBindingsToExpression
          [ makeIdentityStep "currentcontext" (startOf step)
          , makeIdentityStep "origin" (startOf step)
          ]
          step
        compiledExpression <- compileExpression dom expressionWithEnvironment
        compiledExpression' <- if considerFunctional
          then pure $ setCardinality compiledExpression True
          else pure compiledExpression
        -- Save the result in DomeinCache.
        lift2 $ void $ modifyCalculatedRoleInDomeinFile (DomeinFileId $ unsafePartial fromJust $ typeUri2ModelUri (unwrap id)) (CalculatedRole cr {calculation = Q compiledExpression'})
        pure $ unsafePartial $ domain2roleType $ range compiledExpression'

-- | Ensures that the range of the QueryFunctionDescription is a qualified
-- | EnumeratedRole.
qualifyReturnsClause :: ArcPosition -> QueryFunctionDescription -> PhaseThree QueryFunctionDescription
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCoreRoleGetter f) args r@(RDOM (UET (RoleInContext{context:embeddingContext, role:computedType}))) isF isM) = do
  -- Note that it doesn't matter if we take the roles from the cache or from
  -- PhaseThreeState: the role identifiers are identical.
  enumeratedRoles <- (lift $ gets _.dfr) >>= pure <<< _.enumeratedRoles
  computedTypeADT <- catchError
    (UET <$> qualifyLocalEnumeratedRoleName pos (unwrap computedType) (keys enumeratedRoles))
    (\_ -> lift $ lift $ getEnumeratedRole computedType >>= \(EnumeratedRole{id}) -> pure $ UET id)
  case computedTypeADT of
    UET qComputedType | computedType == qComputedType -> pure qfd
    _ -> pure (MQD dom' (QF.ExternalCoreRoleGetter f) args r isF isM)
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCorePropertyGetter f) args (VDOM ran mrop) isF isM) = pure qfd
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCoreContextGetter f) args (CDOM (UET (ContextType computedType))) isF isM) = throwError $ Custom "qualifyReturnsClause: implement case ExternalCoreContextGetter"
qualifyReturnsClause pos qfd@(MQD dom' (QF.ForeignRoleGetter f) args ran isF isM) = throwError $ Custom "qualifyReturnsClause: implement case ForeignRoleGetter"
qualifyReturnsClause pos qfd = pure qfd

-- | Finds a RoleType defined in the model we're compiling whose string value ends with the given segments,
-- | or throws an error.
-- | If the name happens to be fully qualified, we check whether it occurs in the model we're compiling or
-- | in another known model.
-- | Notice that this function can return both an Enumerated and a Calculated role type!
qualifyLocalRoleName :: ArcPosition -> String -> PhaseThree RoleType
qualifyLocalRoleName pos ident = do
  {enumeratedRoles, calculatedRoles} <- lift $ gets _.dfr
  if isTypeUri ident
    then case lookup ident enumeratedRoles of
      Just _ -> pure $ ENR $ EnumeratedRoleType ident
      Nothing -> case lookup ident calculatedRoles of
        Just _ -> pure $ CR $ CalculatedRoleType ident
        Nothing -> lift $ lift $ getRoleType ident
    else (try (ENR <$> qualifyLocalEnumeratedRoleName pos ident (keys enumeratedRoles))) >>= case _ of
      Left _ -> (CR <$> qualifyLocalCalculatedRoleName pos ident (keys calculatedRoles))
      Right r -> pure r

qualifyLocalEnumeratedRoleName :: ArcPosition -> String -> Array String -> PhaseThree EnumeratedRoleType
qualifyLocalEnumeratedRoleName pos ident roleIdentifiers = EnumeratedRoleType <$> (qualifyLocalRoleName_ pos ident roleIdentifiers)

qualifyLocalCalculatedRoleName :: ArcPosition -> String -> Array String -> PhaseThree CalculatedRoleType
qualifyLocalCalculatedRoleName pos ident roleIdentifiers = CalculatedRoleType <$> (qualifyLocalRoleName_ pos ident roleIdentifiers )

qualifyLocalRoleName_ :: ArcPosition -> String -> Array String -> PhaseThree String
qualifyLocalRoleName_ pos ident roleIdentifiers = do
  (candidates :: Array String) <- pure $ filter (\id -> id `endsWithSegments` ident) roleIdentifiers
  case head candidates of
    Nothing -> throwError $ UnknownRole pos ident
    (Just qname) | length candidates == 1 -> pure qname
    otherwise -> throwError $ NotUniquelyIdentifying pos ident candidates

qualifyLocalContextName :: ArcPosition -> String -> Array String -> PhaseThree ContextType
qualifyLocalContextName pos ident roleIdentifiers = ContextType <$> (qualifyLocalContextName_ pos ident roleIdentifiers)

qualifyLocalContextName_ :: ArcPosition -> String -> Array String -> PhaseThree String
qualifyLocalContextName_ pos ident roleIdentifiers = do
  (candidates :: Array String) <- pure $ filter (\id -> id `endsWithSegments` ident) roleIdentifiers
  case head candidates of
    Nothing -> throwError $ UnknownContext pos ident
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
        S step isFunctional -> compileAndSaveProperty currentDomain step cprop isFunctional
  isF <- lift2 $ PROP.propertyTypeIsFunctional pt
  isM <- lift2 $ PROP.propertyTypeIsMandatory pt
  pure $ SQD currentDomain (QF.PropertyGetter pt) ran (bool2threeValued isF) (bool2threeValued isM)

-- | Compiles the parsed expression (type Step) that defines the CalculatedRole.
-- | Saves it in the DomainCache.
compileAndSaveProperty :: Domain -> Step -> CalculatedProperty -> Boolean -> PhaseThree QT.Range
compileAndSaveProperty dom step (CalculatedProperty cp@{id, role, pos}) considerFunctional = withFrame do
  loops <- isBeingCalculated (Prop id)
  if loops
    then throwError $ (RecursiveDefinition $ loopErrorMessage (Prop id) pos pos) 
    else withCurrentCalculation (Prop id)
      do
      -- We add the role as the variable "currentobject"
      kindOfRole <- unsafePartial $ roleKind role
      if kindOfRole == RTI.UserRole && stepContainsVariableReference "currentobject" step
        then throwError (CurrentObjectNotAllowed (startOf step) (endOf step))
        else pure unit
      expressionWithEnvironment <- pure $ addContextualBindingsToExpression
        [ makeContextStep "currentcontext" (startOf step)
        , makeIdentityStep "origin" (startOf step)
        ]
        step
      (compiledExpression :: QueryFunctionDescription) <- compileExpression dom expressionWithEnvironment
      compiledExpression' <- if considerFunctional
        then pure $ setCardinality compiledExpression True
        else pure compiledExpression
      -- Save the result in DomeinCache.
      lift2 $ void $ modifyCalculatedPropertyInDomeinFile (DomeinFileId $ unsafePartial fromJust $ typeUri2ModelUri (unwrap id)) (CalculatedProperty cp {calculation = Q compiledExpression'})
      pure $ range compiledExpression'
      where
        roleKind :: Partial => EnumeratedRoleType -> PhaseThree RTI.RoleKind
        roleKind (EnumeratedRoleType s) = gets _.dfr >>= \{enumeratedRoles} -> pure $ _.kindOfRole $ unwrap $ fromJust (lookup s enumeratedRoles)

------------------------------------------------------------------------------------
------ COMPILING STEPS AND DISTRIBUTING THEIR INVERSION OVER THE DOMEINFILE.
------------------------------------------------------------------------------------
-- | Use `compileAndDistributeStep` to compile a parsed expression into a QueryFunctionDescription, and to
-- | distribute inverted versions of it over all definitions of EnumeratedRoles and EnumeratedProperties that
-- | are visited during query traversal. These inverted versions are used to compute state changes only!
-- | Only perspective objects are used to synchronize.
-- | This function calls [compileStep](Perspectives.Query.ExpressionCompiler.html#t:compileStep).
-- | It also has a side effect on the DomeinFileRecord that is kept in [PhaseTwoState](Perspectives.Parsing.Arc.PhaseTwoDefs.html#t:PhaseTwoState): it
-- |  * changes EnumeratedRoles
-- |  * changes EnumeratedProperties
-- | We only call `compileAndDistributeStep` in:
-- |  - the function `compileStateQueries`. This function also modifies the DomeinFileRecord, but just the
-- |    State definitions in it.
-- |  - in the function `handlePart` in PhaseThree, in
-- |    - casus `AutomaticEffectE`. Here we just modify State definitinons in the DomeinFile.
-- |    - casus `NotificationE`. Here we just modify State definitinons in the DomeinFile.
-- | Hence we do not risk to modify a definition that will be overwritten soon after without including that
-- | modification.
-- TODO. #8 Parameter users should have type Maybe RoleType.
compileAndDistributeStep ::
  Domain ->
  Step ->
  Array StateIdentifier ->
  PhaseThree QueryFunctionDescription
compileAndDistributeStep dom stp stateIdentifiers = do
  -- log ("compileAndDistributeStep:\n" <> "  step = " <> show stp <> "\n  users = " <> show users <> "\n  stateIdentifiers = " <> show stateIdentifiers)
  descr <- compileExpression dom stp
  runReaderT
    (setInvertedQueries [] empty stateIdentifiers descr notSelfOnly notAuthorOnly)
    { modifiesRoleInstancesOf: []
    , modifiesRoleBindingOf: []
    , modifiesPropertiesOf: empty
    }
  pure descr
  where
    notSelfOnly :: Boolean
    notSelfOnly = false

    notAuthorOnly :: Boolean
    notAuthorOnly = false

------------------------------------------------------------------------------------
------ COMPILING STEPS
------------------------------------------------------------------------------------
-- | IMPORTANT. This function should only be called when all Calculated roles and Properties are compiled and 
-- | when StateIdentifiers are qualified.
compileStep :: Domain -> Step -> FD
compileStep currentDomain (Simple st) = compileSimpleStep currentDomain st
compileStep currentDomain (Unary st) = compileUnaryStep currentDomain st
compileStep currentDomain (Binary st) = (compileBinaryStep currentDomain st)
compileStep currentDomain (PureLet st) = compileLetStep currentDomain st
compileStep currentDomain (Computation st) = compileComputationStep currentDomain st

------------------------------------------------------------------------------------
------ COMPILING SIMPLE STEPS
------------------------------------------------------------------------------------
compileSimpleStep :: Domain -> SimpleStep -> FD
compileSimpleStep currentDomain s@(ArcIdentifier pos ident) = do
  -- The next line just looks for indexed contexts in the current model.
  mLocalIndexedContextType <- isIndexedContext ident
  mIndexedContextType <- lift $ lift $ (lookupIndexedContext ident >>= traverse contextType_)
  case mLocalIndexedContextType, mIndexedContextType of
    Just indexedContextType, _ -> pure $ SQD currentDomain (QF.ContextIndividual (ContextInstance ident)) (CDOM (UET indexedContextType)) True True
    _, Just indexedContextType -> pure $ SQD currentDomain (QF.ContextIndividual (ContextInstance ident)) (CDOM (UET indexedContextType)) True True
    _, _ -> do
      -- The next line just looks for indexed roles in the current model.
      mLocalIndexedRoleType <- isIndexedRole ident
      mIndexedRoleType <- lift $ lift (lookupIndexedRole ident >>= traverse roleType_)
      case mLocalIndexedRoleType, mIndexedRoleType of
        Just role, _ -> do
          -- For context, we take the syntactically embedding context type of the indexed role.
          context <- lift2 $ enumeratedRoleContextType role
          pure $ SQD currentDomain (QF.RoleIndividual (RoleInstance ident)) (RDOM (UET (RoleInContext {context, role}))) True True
        _, Just role -> do
          -- For context, we take the syntactically embedding context type of the indexed role.
          context <- lift2 $ enumeratedRoleContextType role
          pure $ SQD currentDomain (QF.RoleIndividual (RoleInstance ident)) (RDOM (UET (RoleInContext {context, role}))) True True
        _, _ -> case currentDomain of
          (CDOM c) -> if ident == "External"
            then do 
              (rts :: ADT RoleInContext) <- lift2 $ externalRoleOfADT c
              pure $ SQD currentDomain (QF.DataTypeGetter ExternalRoleF) (RDOM rts) True True
            else do
              (rts :: Array RoleType) <- if isTypeUri ident
                then if isExternalRole ident
                  then pure [ENR $ EnumeratedRoleType ident]
                  else lift2 $ runArrayT $ lookForRoleTypeOfADT ident c
                else lift2 $ runArrayT $ lookForUnqualifiedRoleTypeOfADT ident c
              case uncons rts of
                Nothing -> throwError $ ContextHasNoRole c ident pos (endOf $ Simple s)
                Just {head, tail} -> if null tail
                  then if isExternalRole ident
                    then do 
                      (rts' :: ADT RoleInContext) <- lift2 $ externalRoleOfADT c
                      pure $ SQD currentDomain (QF.DataTypeGetter ExternalRoleF) (RDOM rts') True True
                    else unsafePartial $ makeRoleGetter currentDomain head
                  else throwError (NotUniquelyIdentifying pos ident (roletype2string <$> rts))
          (RDOM r) -> do
            (pts :: Array PropertyType) <- if isTypeUri ident
              then  lift2 $ runArrayT $ lookForPropertyType ident (roleInContext2Role <$> r)
              else lift2 $ runArrayT $ lookForUnqualifiedPropertyType ident (roleInContext2Role <$> r)
            case uncons pts of
              Nothing -> throwError $ RoleHasNoProperty (roleInContext2Role <$> r) ident pos pos
              Just {head:pt, tail} -> if null tail
                then makePropertyGetter currentDomain pt
                else throwError $ NotUniquelyIdentifying pos ident (show <$> pts)
          otherwise -> throwError $ DomainTypeRequired "context or role" currentDomain pos (endOf (Simple s))

compileSimpleStep currentDomain (PublicRole pos ident) = do
  rType <- lift2 $ roleType_ (RoleInstance ident)
  cType <- lift2 $ enumeratedRoleContextType rType
  pure $ SQD currentDomain (QF.PublicRole (RoleInstance ident)) (RDOM $ UET $ RoleInContext {context: cType, role: rType}) True True

compileSimpleStep currentDomain (PublicContext pos ident) = do
  rType <- lift2 $ contextType_ (ContextInstance ident)
  pure $ SQD currentDomain (QF.PublicContext (ContextInstance ident)) (CDOM $ UET $ rType) True True

compileSimpleStep currentDomain s@(Value pos range stringRepresentation) = pure $
  SQD currentDomain (QF.Constant range stringRepresentation) (VDOM range Nothing) True True

compileSimpleStep currentDomain s@(Filler pos membeddingContext) = do
  case currentDomain of
    RDOM (r :: ADT RoleInContext) -> do
      -- The binding of a role is always an ADT RoleInContext.
      (madtOfBinding :: Maybe (ADT RoleInContext)) <- lift2 $ bindingOfADT r
      unsafePartial case madtOfBinding of
        Just adtOfBinding -> case membeddingContext of
          Nothing -> pure $ SQD currentDomain (QF.DataTypeGetter FillerF) (RDOM adtOfBinding) True False
          -- This is the step type "binding in <context type>".
          Just context -> if isTypeUri context
            then pure $ SQD currentDomain (QF.DataTypeGetterWithParameter FillerF context) (RDOM $ replaceContext adtOfBinding (ContextType context)) True False
            -- Try to qualify the name within the Domain.
            else do
              {id} <- lift $ gets _.dfr
              (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain context id
              case head qnames of
                Nothing -> throwError $ UnknownContext pos context
                (Just qn) | length qnames == 1 -> pure $ SQD currentDomain (QF.DataTypeGetterWithParameter FillerF (unwrap qn)) (RDOM $ replaceContext adtOfBinding qn) True False
                _ -> throwError $ NotUniquelyIdentifying pos context (map unwrap qnames)
    otherwise -> throwError $ DomainTypeRequired "role" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(Filled pos binderName membeddingContext) = do
  case currentDomain of
    (RDOM (adtOfBinder :: ADT RoleInContext)) -> do
      (qBinderType :: EnumeratedRoleType) <- if isTypeUri binderName
        then pure $ EnumeratedRoleType binderName
        -- Try to qualify the name within the Domain.
        else do
          {id} <- lift $ gets _.dfr
          (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain binderName id
          case head qnames of
            Nothing -> throwError $ UnknownRole pos binderName
            (Just qn) | length qnames == 1 -> pure qn
            otherwise -> throwError $ NotUniquelyIdentifying pos binderName (map unwrap qnames)
      case membeddingContext of
        -- <origin> fills R in <optional context>. Clearly, <optional context> is the context that holds the filled
        -- role (binder).
        Nothing -> do
          EnumeratedRole{context} <- lift2 $ getEnumeratedRole qBinderType
          pure $ SQD currentDomain (QF.FilledF qBinderType context) (RDOM (UET $ RoleInContext{context, role: qBinderType})) False False
        Just context -> if isTypeUri context
          then pure $ SQD currentDomain (QF.FilledF qBinderType (ContextType context)) (RDOM $ replaceContext adtOfBinder (ContextType context) ) False False
          -- Try to qualify the name within the Domain.
          else do
            {id} <- lift $ gets _.dfr
            (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain context id
            case head qnames of
              Nothing -> throwError $ UnknownContext pos context
              (Just qn) | length qnames == 1 -> pure $ SQD currentDomain (QF.FilledF qBinderType (ContextType context)) (RDOM $ replaceContext adtOfBinder qn) False False
              otherwise -> throwError $ NotUniquelyIdentifying pos context (map unwrap qnames) 
    otherwise -> throwError $ DomainTypeRequired "role" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(Context pos) = do
  case currentDomain of
    (RDOM (r :: ADT RoleInContext)) -> do
      (typeOfContext :: ADT ContextType) <- pure $ contextOfADT r
      pure $ SQD currentDomain (QF.DataTypeGetter ContextF) (CDOM typeOfContext) True True
    otherwise -> throwError $ DomainTypeRequired "role" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(TypeOfContext pos) = do
  case currentDomain of
    (CDOM (r :: ADT ContextType)) -> do
      pure $ SQD currentDomain (QF.TypeGetter TypeOfContextF) ContextKind True True
    otherwise -> throwError $ DomainTypeRequired "context" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(RoleTypeIndividual pos typeName) = do
  nameSpace <- getsDF _.id
  typeCandidates <- lift $ lift (nameSpace ###= qualifyRoleInDomain typeName )
  case length typeCandidates, head typeCandidates of 
    0, _ -> throwError $ UnknownRole pos typeName
    1, Just qualifiedType -> pure $ SQD currentDomain (QF.RoleTypeConstant qualifiedType) RoleKind True True
    _, _ -> throwError $ NotUniquelyIdentifying pos typeName (map roletype2string typeCandidates)

compileSimpleStep currentDomain s@(ContextTypeIndividual pos typeName) = do
  nameSpace <- getsDF _.id
  typeCandidates <- lift $ lift (nameSpace ###= qualifyContextInDomain typeName )
  case length typeCandidates, head typeCandidates of 
    0, _ -> throwError $ UnknownContext pos typeName
    1, Just qualifiedType -> pure $ SQD currentDomain (QF.ContextTypeConstant qualifiedType) ContextKind True True
    _, _ -> throwError $ NotUniquelyIdentifying pos typeName (map unwrap typeCandidates)

compileSimpleStep currentDomain s@(RoleTypes pos) = do
  case currentDomain of
    ContextKind -> do
      pure $ SQD currentDomain (QF.TypeGetter RoleTypesF) RoleKind True True
    otherwise -> throwError $ DomainTypeRequired "context type" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(SpecialisesRoleType pos roleName) = do
  case currentDomain of
    RoleKind -> do
      -- TODO: controleer of roleName inderdaad een EnumeratedRole is!
      (qRoleName :: EnumeratedRoleType) <- if isTypeUri roleName
        then pure $ EnumeratedRoleType roleName
        -- Try to qualify the name within the Domain.
        else do
          {id} <- lift $ gets _.dfr
          (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain roleName id
          case head qnames of
            Nothing -> throwError $ UnknownRole pos roleName
            (Just qn) | length qnames == 1 -> pure qn
            otherwise -> throwError $ NotUniquelyIdentifying pos roleName (map unwrap qnames)
      pure $ SQD currentDomain (QF.DataTypeGetterWithParameter SpecialisesRoleTypeF (unwrap qRoleName)) (VDOM PBool Nothing) (isFunctionalFunction SpecialisesRoleTypeF) False
    otherwise -> throwError $ DomainTypeRequired "role type" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(IsInState pos stateName) = do
  -- If the domain is a SUM, the stateName must belong to at least one of the members.
  -- (if the same (local) name has been defined on multiple members, we'll throw an error. This will be rare)
  -- Example: (Male | Female) and state Pregnant. Obviously, semantically, Males will never be pregnant but who cares?
  -- If the domain is a PRODUCT, the same reasoning holds.
  -- Example: (AccountHolder & Patient) and state Solvent. 
  qualifiedStateName <- case currentDomain of
    -- Get all RoleTypes in the ADT
    RDOM adt -> do 
      (allRoles :: Array EnumeratedRoleType) <- lift $ lift ((roleInContext2Role <$> adt) ###= allTypesInRoleADT)
      f (unwrap <$> allRoles)
    -- Get all ContextTypes in the ADT.
    CDOM adt -> do 
      (allContexts :: Array ContextType) <- lift $ lift (adt ###= allTypesInContextADT)
      f (unwrap <$> allContexts)
    otherwise -> throwError $ DomainTypeRequired "role or context" currentDomain pos (endOf $ Simple s)
  pure $ SQD currentDomain (QF.DataTypeGetterWithParameter IsInStateF (unwrap qualifiedStateName)) (VDOM PBool Nothing) (isFunctionalFunction IsInStateF) False

  where
    -- If the combination of namespace and stateName occurs in the states in the DomeinFileRecord, return it.
    -- Otherwise find the StateIdentifier in the DomeinFileRecord that ends on the stateName.
    f :: Array String -> PhaseThree StateIdentifier
    f namespaces = if isTypeUri stateName
        then pure $ StateIdentifier stateName
        else do
          -- If we can find state names that equal the composition of a namespace and the stateName, we're done.
          states <- lift (_.states <$> gets _.dfr)
          allMatchingStateNames <- pure $ foldMap (\ns -> case lookup (qualifyWith ns stateName) states of
            Nothing -> []
            Just _ -> [(qualifyWith ns stateName)] ) namespaces
          case length allMatchingStateNames of
            -- Otherwise we'll have to find state names whose suffix equals the stateName.
            0 -> case (filter (\id -> id `endsWithSegments` stateName) (keys states)) of
              none | length none == 0 -> throwError $ UnknownState pos stateName
              single | length single == 1 -> pure $ StateIdentifier $ unsafePartial $ fromJust (head single)
              multiple -> throwError $ NotUniquelyIdentifying pos stateName (keys states)
            1 -> pure $ StateIdentifier $ unsafePartial $ fromJust (head allMatchingStateNames)
            _ -> throwError $ NotUniquelyIdentifying pos stateName allMatchingStateNames

compileSimpleStep currentDomain s@(RegEx pos (reg :: RegExP)) = do
  case currentDomain of
    VDOM PString _ -> pure $ SQD currentDomain (QF.RegExMatch reg) (VDOM PBool Nothing) True False
    otherwise -> throwError $ DomainTypeRequired "string" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain (TypeTimeOnlyContext pos ctype) = pure $
  SQD currentDomain (QF.TypeTimeOnlyContextF ctype) (CDOM (UET $ ContextType ctype)) True True

compileSimpleStep currentDomain s@(TypeTimeOnlyEnumeratedRole pos ctype rtype) = pure $ SQD currentDomain (QF.TypeTimeOnlyEnumeratedRoleF rtype) (RDOM $ UET $ RoleInContext {context: ContextType ctype, role: (EnumeratedRoleType rtype)}) True True

compileSimpleStep currentDomain s@(TypeTimeOnlyCalculatedRole pos rtype) = do
  -- Get the ADT for the calculated role.
  roleAdt <- lift $ lift $ getRoleADTFromString (CalculatedRoleType rtype)
  pure $ SQD currentDomain (QF.TypeTimeOnlyCalculatedRoleF rtype) (RDOM roleAdt) True True

compileSimpleStep currentDomain s@(Extern pos) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: ADT RoleInContext) <- lift2 $ externalRoleOfADT c
      pure $ SQD currentDomain (QF.DataTypeGetter ExternalRoleF) (RDOM rts) True True
    otherwise -> throwError $ DomainTypeRequired "context" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain s@(IndexedName pos) = do
  case currentDomain of
    (CDOM c) -> pure $ SQD currentDomain (QF.DataTypeGetter IndexedContextName) (VDOM PString Nothing) True False
    (RDOM r) -> pure $ SQD currentDomain (QF.DataTypeGetter IndexedRoleName) (VDOM PString Nothing) True False
    otherwise -> throwError $ DomainTypeRequired "role or context" currentDomain pos (endOf $ Simple s)

compileSimpleStep currentDomain (Identity _) = do
  isFunctional <- case currentDomain of
    RDOM r -> (lift $ lift $ adtIsFunctional r) >>= if _ then pure True else pure False
    _ -> pure Unknown
  pure $ SQD currentDomain (QF.DataTypeGetter IdentityF) currentDomain isFunctional True

compileSimpleStep currentDomain s@(Modelname _) = case currentDomain of
  VDOM _ Nothing -> throwError $ NoPropertyTypeWithValue (startOf (Simple s)) (endOf (Simple s))
  _ -> pure $ SQD currentDomain (QF.DataTypeGetter ModelNameF) (VDOM PString Nothing) Unknown True

-- We compile the SequenceFunction as a UnaryCombinator, which is a stretch.
compileSimpleStep currentDomain (SequenceFunction _ fname) = pure $ SQD currentDomain (QF.UnaryCombinator fname) currentDomain (isFunctionalFunction fname) True

compileSimpleStep currentDomain (Variable pos varName) = do
  mBinding <- lookupVariableBinding varName
  case mBinding of
    Nothing -> throwError $ UnknownVariable pos varName
    (Just fdesc) -> do 
      isF <- if isJust $ elemIndex varName ["currentcontext", "origin", "currentactor", "notifieduser"]
        then pure True
        else pure $ functional fdesc
      pure $ SQD currentDomain (QF.VariableLookup varName) (range fdesc) isF (mandatory fdesc)

compileUnaryStep :: Domain -> UnaryStep -> FD
compileUnaryStep currentDomain (LogicalNot pos s) = do
  -- First compile s. Then check that the resulting QueryFunctionDescription is a (VDOM PBool _) range value.
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM PBool _ -> pure $ UQD currentDomain (QF.UnaryCombinator NotF) descriptionOfs (VDOM PBool Nothing) (functional descriptionOfs) (mandatory descriptionOfs)
    otherwise -> throwError $ NotABoolean pos

compileUnaryStep currentDomain st@(Exists pos s) = do
  descriptionOfs <- compileStep currentDomain s
  pure $ UQD currentDomain (QF.UnaryCombinator ExistsF) descriptionOfs (VDOM PBool Nothing) True True

compileUnaryStep currentDomain st@(FilledBy pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    RDOM _ -> pure $ UQD currentDomain (QF.UnaryCombinator FilledByF) descriptionOfs (VDOM PBool Nothing) True True
    otherwise -> throwError $ NotARoleDomain (range descriptionOfs) pos (endOf s)

compileUnaryStep currentDomain st@(Fills pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    RDOM _ -> pure $ UQD currentDomain (QF.UnaryCombinator FillsF) descriptionOfs (VDOM PBool Nothing) True True
    otherwise -> throwError $ NotARoleDomain (range descriptionOfs) pos (endOf s)

compileUnaryStep currentDomain st@(Available pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM _ _ -> throwError $ ValueExpressionNotAllowed (range descriptionOfs) pos (endOf s)
    otherwise -> pure $ UQD currentDomain (QF.UnaryCombinator AvailableF) descriptionOfs (VDOM PBool Nothing) True True

compileUnaryStep currentDomain st@(DurationOperator start duration s) = do
  descriptionOfs <- compileStep currentDomain s
  durationRange <- unsafePartial case duration of 
    Year _ -> pure Year_
    Month _ -> pure Month_
    Week _ -> pure Week_
    Day _ -> pure Day_
    Hour _ -> pure Hour_
    Minute _ -> pure Minute_
    Second _ -> pure Second_ 
    Millisecond _ -> pure MilliSecond_
  case range descriptionOfs of
    VDOM PNumber _ -> pure $ replaceRange descriptionOfs (VDOM (PDuration durationRange) Nothing)
    otherwise -> throwError $ DomainTypeRequired "number" (range descriptionOfs) start (endOf s)

compileUnaryStep currentDomain st@(ContextIndividual pos qualifiedIdentifier s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM PString _ -> do 
      -- Check whether tye type exists.
      mqualifiedContext <- lift $ lift $ try (getContext $ ContextType qualifiedIdentifier)
      case mqualifiedContext of
        Left _ -> throwError $ UnknownContext pos qualifiedIdentifier
        Right _ -> pure $ UQD currentDomain (QF.UnaryCombinator ContextIndividualF) descriptionOfs (CDOM $ UET $ ContextType qualifiedIdentifier) True True
    otherwise -> throwError $ DomainTypeRequired "role" currentDomain pos (endOf s)

compileUnaryStep currentDomain st@(RoleIndividual pos qualifiedIdentifier s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    VDOM PString _ -> do 
      -- Check whether tye type exists.
      mqualifiedRole <- lift $ lift $ try $ getEnumeratedRole $ EnumeratedRoleType qualifiedIdentifier
      case mqualifiedRole of 
        Left _ -> throwError $ UnknownRole pos qualifiedIdentifier
        Right _ -> do 
          role <- lift $ lift $ getEnumeratedRole (EnumeratedRoleType qualifiedIdentifier)
          pure $ UQD currentDomain (QF.UnaryCombinator RoleIndividualF) descriptionOfs (RDOM $ UET $ RoleInContext {role: EnumeratedRoleType qualifiedIdentifier, context: contextOfRepresentation role}) True True
    otherwise -> throwError $ DomainTypeRequired "string" (range descriptionOfs) pos (endOf s)

compileBinaryStep :: Domain -> BinaryStep -> FD
compileBinaryStep currentDomain s@(BinaryStep{operator, left, right}) =
  case operator of
    Filter pos -> do
      source <- compileStep currentDomain left
      criterium <- compileStep (range source) right
      case range criterium of
        VDOM PBool _ -> if pessimistic $ functional criterium
          then pure $ makeComposition source (UQD currentDomain QF.FilterF criterium (range source) (functional source) False)
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
      if equalDomainKinds (range f1) (range f2)
        then pure $ BQD currentDomain (QF.BinaryCombinator UnionF) f1 f2 (unsafePartial $ fromJust $ sumOfDomains (range f1)(range f2)) False (THREE.and (mandatory f1)(mandatory f2))
        else throwError $ IncompatibleDomains (startOf left) (endOf right)
    Intersection pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right
      -- TODO. Als de types een lege doorsnede hebben, een waarschuwing geven?
      if equalDomainKinds (range f1) (range f2)
        then pure $ BQD currentDomain (QF.BinaryCombinator IntersectionF) f1 f2 (unsafePartial $ fromJust $ productOfDomains (range f1)(range f2)) False (THREE.and (mandatory f1)(mandatory f2))
        else throwError $ IncompatibleDomains (startOf left) (endOf right)
    OrElse pos -> do 
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right
      -- TODO. Als de types een lege doorsnede hebben, een waarschuwing geven?
      if equalDomainKinds (range f1) (range f2)
        then pure $ BQD currentDomain (QF.BinaryCombinator OrElseF) f1 f2 (unsafePartial $ fromJust $ sumOfDomains (range f1)(range f2)) False (THREE.and (mandatory f1)(mandatory f2))
        else throwError $ IncompatibleDomains (startOf left) (endOf right)
    BindsOp pos -> do
      f1 <- compileStep currentDomain left
      f2 <- compileStep currentDomain right
      if (pessimistic $ functional f1)
        then if (pessimistic $ functional f2)
          then case (range f1), (range f2) of
            (RDOM _), (RDOM _) -> pure $ BQD
              currentDomain
              (QF.BinaryCombinator FilledByF)
              f1
              f2
              (VDOM PBool Nothing)
              True
              (THREE.and (mandatory f1)(mandatory f2))
            _, _ -> throwError $ NotARoleDomain currentDomain (startOf left) (endOf right)
          else throwError (NotFunctional (startOf right) (endOf right) right)
        else throwError (NotFunctional (startOf left) (endOf left) left)

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
        -- Possibly allow PBool
        Add pos -> binOp pos f1 f2 
          [ PNumber
          , PString
          , PDuration Year_
          , PDuration Month_
          , PDuration Week_
          , PDuration Day_
          , PDuration Hour_
          , PDuration Minute_
          , PDuration Second_
          , PDuration MilliSecond_] AddF
        Subtract pos -> binOp pos f1 f2 
          [ PNumber
          , PString
          , PDuration Year_
          , PDuration Month_
          , PDuration Week_
          , PDuration Day_
          , PDuration Hour_
          , PDuration Minute_
          , PDuration Second_
          , PDuration MilliSecond_] SubtractF
        Divide pos -> binOp pos f1 f2 [ PNumber ] DivideF
        -- Possibly allow PBool
        Multiply pos -> binOp pos f1 f2 [ PNumber ] MultiplyF

        Compose _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Compose"
        Filter _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Filter"
        Union _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Union"
        Intersection _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Intersection"
        OrElse _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: OrElse"
        BindsOp _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: BindsOp"
        Matches _ -> throwError $ Custom "This case in compileBinaryStep should never be reached: Matches"

        -- >>= is parsed as the operator Sequence.
        -- "sum", "product", "minimum", "maximum" and "count" are parsed as SequenceFunction
        -- step >>= f is parsed as the BinaryStep we're dealing here with now, where `step` is the left and `f` the right operand.
        Sequence pos -> do
          -- We must compile right again, because in a >>= construction, what is produced on the left is offered to the right as domain, whereas above we compiled the right step with the same domain as the left step.
          f2' <- compileStep (range f1) right
          case f2' of
            -- f2 results from the expression that follows `>>=` (must have been: "sum", "product", etc.).
            -- This was parsed as `SequenceFunction f` and is now compiled as `UnaryCombinator f` in an SQD.
            -- Notice by the domain and range that we assume functions that are Monoids.
            -- Notice the strangeness of compiling a binary expression into an SQD description.
            SQD dom (QF.UnaryCombinator fname) ran _ _-> case fname of
              -- we can count anything and the result is a number.
              -- We must change the range of f2' as all SequenceFunctions are compiled to have equal domain and range.
              CountF -> pure (BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 (SQD dom (QF.UnaryCombinator CountF) (VDOM PNumber Nothing) True True) (VDOM PNumber Nothing) True True)
              -- We have interpretations of AddF, SubtractF for numbers and strings only.
              -- For MinimumF and MaximumF we have interpretations for numbers and strings and booleans and dates.
              -- For AndF and OrF we have an interpretation for Booleans only.
              -- We also require that the VDOM should have an EnumeratedPropertyType.
              AddF -> ensureDomainIsRange dom [PNumber, PString, PDuration Year_, PDuration Month_, PDuration Week_, PDuration Day_, PDuration Hour_, PDuration Minute_, PDuration Second_, PDuration MilliSecond_] pos
                (pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True)
              SubtractF -> ensureDomainIsRange dom [PNumber, PString] pos
                (pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True)
              MinimumF -> ensureDomainIsRange dom [PNumber, PString, PBool, PDate] pos
                (pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True)
              MaximumF -> ensureDomainIsRange dom [PNumber, PString, PBool, PDate] pos
                (pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True)
              AndF -> ensureDomainIsRange dom [PBool] pos
                (pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True)
              OrF -> ensureDomainIsRange dom [PBool] pos
                (pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True)
              FirstF -> pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeSequenceF) f1 f2' ran True True
              _ -> throwError $ ArgumentMustBeSequenceFunction pos
            _ -> throwError $ ArgumentMustBeSequenceFunction pos
        op -> throwError $ Custom ("This case in compileBinaryStep should never be reached: " <> show op)

  where
    ensureDomainIsRange :: Domain -> Array Range -> ArcPosition -> FD -> FD
    ensureDomainIsRange d@(VDOM r p) allowedRangeConstructors pos fd = if (isJust $ elemIndex r allowedRangeConstructors) -- && (isJust p) Don't know why I originally added this constraint?
      then fd
      else throwError $ WrongTypeForOperator pos allowedRangeConstructors d
    ensureDomainIsRange d allowedRangeConstructors pos _ = throwError $ WrongTypeForOperator pos allowedRangeConstructors d

    comparison :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> FunctionName -> PhaseThree QueryFunctionDescription
    comparison pos left' right' functionName = do
      -- Both ranges must be equal, both sides must be functional.
      ((range left') `equalDomains` (range right')) >>= if _
        then if (pessimistic $ functional left') && (pessimistic $ functional right')
          then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM PBool Nothing) (isFunctionalFunction functionName) True
          else throwError $ ExpressionsShouldBeFunctional (pessimistic $ functional left') (pessimistic $ functional right') pos
        else throwError $ TypesCannotBeCompared pos (range left') (range right')
      where 
        equalDomains :: Domain -> Domain -> PhaseThree Boolean
        -- special case for RDOM
        equalDomains (RDOM r1) (RDOM r2) = lift $ lift $ equals r1 r2
        equalDomains d1 d2 = pure $ eq d1 d2

    binOp :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> Array Range -> FunctionName -> PhaseThree QueryFunctionDescription
    binOp pos left' right' allowedRangeConstructors functionName = if (pessimistic $ functional left') && (pessimistic $ functional right')
      then case range left', range right' of
        -- In calling binOp we have made sure that if one operand is a Duration, the functions are either AddF or SubtractF
        -- Notice that only in runtime we make sure that the duration is subtracted from the time, by switching arguments.
        d1@(VDOM rc1 _), (VDOM rc2 _) | (isDate rc1 && isPDuration rc2 || isPDuration rc1 && isDate rc2) -> 
          if isPMonth rc1 || isPMonth rc2
            then throwError $ NoMonths pos
            else pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM rc1 Nothing) (isFunctionalFunction functionName) True
        d1@(VDOM rc1 _), (VDOM rc2 _) | (isTime rc1 && isTimeDuration rc2 || isTimeDuration rc2 && isTime rc1) -> 
          pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM rc1 Nothing) (isFunctionalFunction functionName) True
        -- Both ranges must be equal, both sides must be functional.
        d1@(VDOM rc1 _), (VDOM rc2 _) | rc1 == rc2 ->
          if allowed rc1 && allowed rc2
            then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM rc1 Nothing) (isFunctionalFunction functionName) True
            else throwError $ WrongTypeForOperator pos allowedRangeConstructors d1
        l, r -> throwError $ TypesCannotBeCompared pos l r
      else throwError $ ExpressionsShouldBeFunctional (pessimistic $ functional left') (pessimistic $ functional right') pos
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
  -- TODO. Misschien: als varName=="currentsubject", en step is niet Identity, maak er dan "filter <step> with binds sys:Me" van.
  isF <- if isJust $ elemIndex varName ["currentcontext", "origin", "currentactor", "notifieduser"]
    then pure True
    else pure $ functional step_
  pure $ UQD currentDomain (QF.BindVariable varName) step_ (range step_) isF (mandatory step_)

compileComputationStep :: Domain -> ComputationStep -> FD
compileComputationStep currentDomain (ComputationStep {functionName, arguments, computedType, start, end}) = if lookupHiddenFunctionIsEffect functionName
  then throwError (NotAFunction start end functionName)
  else do
    case (typeUri2ModelUri functionName) of
      Nothing -> throwError (NotWellFormedName start functionName)
      Just modelName -> if isExternalCoreModule modelName
        then do
          -- We cannot call addAllExternalFunctions without introducing a cycle.
          -- Instead, we start up main with addAllExternalFunctions
          compiledArgs <- traverse (compileStep currentDomain) arguments
          (let
            mexpectedNrOfArgs = lookupHiddenFunctionNArgs functionName
            in case mexpectedNrOfArgs of
              Nothing -> throwError (UnknownExternalFunction start end functionName)
              Just expectedNrOfArgs -> if expectedNrOfArgs == length arguments || expectedNrOfArgs == length arguments - 1
                then do 
                  isFunctional <- pure $ unsafePartial $ fromJust $ lookupHiddenFunctionCardinality functionName
                  case computedType of
                    -- Collect property instances.
                    ComputedRange r -> pure $ MQD currentDomain (QF.ExternalCorePropertyGetter functionName) (fromFoldable compiledArgs) (VDOM r Nothing) isFunctional Unknown
                    OtherType s -> (lift $ lift $ typeExists (ContextType s)) >>= if _
                      -- Collect Context instances.
                      then pure $ SQD currentDomain (QF.ExternalCoreContextGetter functionName) (CDOM (UET (ContextType s))) isFunctional Unknown

                      -- Collect role instances. Having no other information, we conjecture these instances to have their
                      -- role type in their lexical context.
                      else do
                        context <- lift2 $ enumeratedRoleContextType (EnumeratedRoleType s)
                        pure $ MQD currentDomain (QF.ExternalCoreRoleGetter functionName) (fromFoldable compiledArgs) (RDOM (UET $ RoleInContext {context, role: EnumeratedRoleType s})) isFunctional Unknown
                else throwError (WrongNumberOfArguments start end functionName expectedNrOfArgs (length arguments)))
        else do
          compiledArgs <- traverse (compileStep currentDomain) arguments
          -- TODO. This is a stub.
          -- TODO. Check whether the foreign function exists and whether it has been given the right number of arguments.
          case computedType of 
            ComputedRange r -> 
              pure $ MQD currentDomain (QF.ForeignPropertyGetter functionName) (fromFoldable compiledArgs) (VDOM r Nothing) Unknown Unknown
            OtherType s -> 
              pure $ MQD currentDomain (QF.ForeignRoleGetter functionName) (fromFoldable compiledArgs) (RDOM (UET $ RoleInContext {context: ContextType "", role: EnumeratedRoleType s})) Unknown Unknown

  -- where

    -- mapToRange :: String -> Maybe Range
    -- mapToRange s = case s of
    --   "String" -> Just PString
    --   "Boolean" -> Just PBool
    --   "Number" -> Just PNumber
    --   "DateTime" -> Just PDateTime
    --   "Date" -> Just PDate
    --   "Time" -> Just PTime
    --   -- durations
    --   -> PDuration 
    --   "Email" -> Just PEmail
    --   otherwise -> Nothing
