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
import Data.Array (elemIndex, filter, foldM, fromFoldable, head, length, null, uncons)
import Data.Either (Either(..))
import Data.Map (Map, empty, singleton)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (modifyCalculatedPropertyInDomeinFile, modifyCalculatedRoleInDomeinFile)
import Perspectives.External.CoreModuleList (isExternalCoreModule)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs)
import Perspectives.Identifiers (typeUri2ModelUri, endsWithSegments, isExternalRole, isTypeUri)
import Perspectives.Instances.ObjectGetters (contextType_, roleType_)
import Perspectives.Parsing.Arc.ContextualVariables (addContextualBindingsToExpression, makeContextStep, makeIdentityStep, stepContainsVariableReference)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (setInvertedQueries)
import Perspectives.Parsing.Arc.PhaseTwoDefs (CurrentlyCalculated(..), PhaseThree, addBinding, isBeingCalculated, isIndexedContext, isIndexedRole, lift2, lookupVariableBinding, loopErrorMessage, throwError, withCurrentCalculation, withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), adtContext2AdtRoleInContext, context2RoleInContextADT, domain, domain2roleType, equalDomainKinds, functional, mandatory, productOfDomains, propertyOfRange, range, replaceContext, roleInContext2Role, sumOfDomains, traverseQfd)
import Perspectives.Query.QueryTypes (Range) as QT
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getCalculatedProperty, getCalculatedRole, getEnumeratedProperty, getEnumeratedRole, typeExists)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, range) as PROP
import Perspectives.Representation.Class.Role (adtIsFunctional, bindingOfADT, contextOfADT, externalRoleOfADT, getRoleADTFromString, getRoleType, roleADT, roleTypeIsFunctional, roleTypeIsMandatory)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.QueryFunction (FunctionName(..), isFunctionalFunction)
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued, pessimistic)
import Perspectives.Representation.ThreeValuedLogic (and, or) as THREE
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), roletype2string)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..)) as RTI
import Perspectives.Types.ObjectGetters (enumeratedRoleContextType, isUnlinked_, lookForPropertyType, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT, qualifyContextInDomain, qualifyEnumeratedRoleInDomain)
import Prelude (bind, discard, eq, map, pure, show, unit, void, ($), (&&), (<$>), (<*>), (<<<), (<>), (==), (>>=))

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
-- | Constructs a QueryFunctionDescription that describes getting a role of the given type.
-- | CalculatedRoles are compiled, when necessary. The result of such an on-the-fly compilation is saved
-- | in the domeinCache.
makeRoleGetter :: Partial => Domain -> RoleType -> PhaseThree QueryFunctionDescription
makeRoleGetter currentDomain rt@(CR ct) = do
  (adt :: ADT RoleInContext) <- do
    crole@(CalculatedRole{calculation}) <- lift2 $ getCalculatedRole ct
    case calculation of
      Q qfd -> lift2 $ roleADT crole
      S step -> compileAndSaveRole currentDomain step crole
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
compileAndSaveRole :: Domain -> Step -> CalculatedRole -> PhaseThree (ADT RoleInContext)
compileAndSaveRole dom step (CalculatedRole cr@{_id, kindOfRole, pos}) = withFrame do
  loops <- isBeingCalculated (Role _id)
  if loops
    then throwError $ (RecursiveDefinition $ loopErrorMessage (Role _id) pos pos) 
    else withCurrentCalculation (Role _id)
      do
        expressionWithEnvironment <- pure $ addContextualBindingsToExpression
          [ makeIdentityStep "currentcontext" (startOf step)
          , makeIdentityStep "origin" (startOf step)
          ]
          step
        compiledExpression <- compileExpression dom expressionWithEnvironment
        -- Save the result in DomeinCache.
        lift2 $ void $ modifyCalculatedRoleInDomeinFile (unsafePartial fromJust $ typeUri2ModelUri (unwrap _id)) (CalculatedRole cr {calculation = Q compiledExpression})
        pure $ unsafePartial $ domain2roleType $ range compiledExpression

-- | Ensures that the range of the QueryFunctionDescription is a qualified
-- | EnumeratedRole.
qualifyReturnsClause :: ArcPosition -> QueryFunctionDescription -> PhaseThree QueryFunctionDescription
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCoreRoleGetter f) args r@(RDOM (ST (RoleInContext{context:embeddingContext, role:computedType}))) isF isM) = do
  -- Note that it doesn't matter if we take the roles from the cache or from
  -- PhaseThreeState: the role identifiers are identical.
  enumeratedRoles <- (lift $ gets _.dfr) >>= pure <<< _.enumeratedRoles
  computedTypeADT <- catchError
    (ST <$> qualifyLocalEnumeratedRoleName pos (unwrap computedType) (keys enumeratedRoles))
    (\_ -> lift $ lift $ getEnumeratedRole computedType >>= \(EnumeratedRole{_id}) -> pure $ ST _id)
  case computedTypeADT of
    ST qComputedType | computedType == qComputedType -> pure qfd
    _ -> pure (MQD dom' (QF.ExternalCoreRoleGetter f) args r isF isM)
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCorePropertyGetter f) args (VDOM ran mrop) isF isM) = pure qfd
qualifyReturnsClause pos qfd@(MQD dom' (QF.ExternalCoreContextGetter f) args (CDOM (ST (ContextType computedType))) isF isM) = throwError $ Custom "qualifyReturnsClause: implement case ExternalCoreContextGetter"
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
  (candidates :: Array String) <- pure $ filter (\_id -> _id `endsWithSegments` ident) roleIdentifiers
  case head candidates of
    Nothing -> throwError $ UnknownRole pos ident
    (Just qname) | length candidates == 1 -> pure qname
    otherwise -> throwError $ NotUniquelyIdentifying pos ident candidates

qualifyLocalContextName :: ArcPosition -> String -> Array String -> PhaseThree ContextType
qualifyLocalContextName pos ident roleIdentifiers = ContextType <$> (qualifyLocalContextName_ pos ident roleIdentifiers)

qualifyLocalContextName_ :: ArcPosition -> String -> Array String -> PhaseThree String
qualifyLocalContextName_ pos ident roleIdentifiers = do
  (candidates :: Array String) <- pure $ filter (\_id -> _id `endsWithSegments` ident) roleIdentifiers
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
        S step -> compileAndSaveProperty currentDomain step cprop
  isF <- lift2 $ PROP.propertyTypeIsFunctional pt
  isM <- lift2 $ PROP.propertyTypeIsMandatory pt
  pure $ SQD currentDomain (QF.PropertyGetter pt) ran (bool2threeValued isF) (bool2threeValued isM)

-- | Compiles the parsed expression (type Step) that defines the CalculatedRole.
-- | Saves it in the DomainCache.
compileAndSaveProperty :: Domain -> Step -> CalculatedProperty -> PhaseThree QT.Range
compileAndSaveProperty dom step (CalculatedProperty cp@{_id, role, pos}) = withFrame do
  loops <- isBeingCalculated (Prop _id)
  if loops
    then throwError $ (RecursiveDefinition $ loopErrorMessage (Prop _id) pos pos) 
    else withCurrentCalculation (Prop _id)
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
      compiledExpression <- compileExpression dom expressionWithEnvironment
      -- Save the result in DomeinCache.
      lift2 $ void $ modifyCalculatedPropertyInDomeinFile (unsafePartial fromJust $ typeUri2ModelUri (unwrap _id)) (CalculatedProperty cp {calculation = Q compiledExpression})
      pure $ range compiledExpression
      where
        roleKind :: Partial => EnumeratedRoleType -> PhaseThree RTI.RoleKind
        roleKind (EnumeratedRoleType s) = gets _.dfr >>= \{enumeratedRoles} -> pure $ _.kindOfRole $ unwrap $ fromJust (lookup s enumeratedRoles)

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
  runReaderT
    (setInvertedQueries users statesPerProperty stateIdentifiers descr notSelfOnly)
    { modifiesRoleInstancesOf: []
    , modifiesRoleBindingOf: []
    , modifiesPropertiesOf: empty
    }
  pure descr
  where
    notSelfOnly :: Boolean
    notSelfOnly = false


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
        Just role -> do
          -- For context, we take the syntactically embedding context type of the indexed role.
          context <- lift2 $ enumeratedRoleContextType role
          pure $ SQD currentDomain (QF.RoleIndividual (RoleInstance ident)) (RDOM (ST (RoleInContext {context, role}))) True True
        Nothing -> case currentDomain of
          (CDOM c) -> do
            (rts :: Array RoleType) <- if isTypeUri ident
              then if isExternalRole ident
                then pure [ENR $ EnumeratedRoleType ident]
                else lift2 $ runArrayT $ lookForRoleTypeOfADT ident c
              else if ident == "External"
                then case c of
                  (ST (ContextType cid)) -> pure [ENR (EnumeratedRoleType (cid <> "$External"))]
                  otherwise -> throwError $ Custom ("Cannot get the external role of a compound type: " <> show otherwise)
                else lift2 $ runArrayT $ lookForUnqualifiedRoleTypeOfADT ident c
            case uncons rts of
              Nothing -> throwError $ ContextHasNoRole c ident
              Just {head, tail} -> if null tail
                then unsafePartial $ makeRoleGetter currentDomain head
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
          otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain (PublicRole pos ident) = do
  rType <- lift2 $ roleType_ (RoleInstance ident)
  cType <- lift2 $ enumeratedRoleContextType rType
  pure $ SQD currentDomain (QF.PublicRole (RoleInstance ident)) (RDOM $ ST $ RoleInContext {context: cType, role: rType}) True True

compileSimpleStep currentDomain (PublicContext pos ident) = do
  rType <- lift2 $ contextType_ (ContextInstance ident)
  pure $ SQD currentDomain (QF.PublicContext (ContextInstance ident)) (CDOM $ ST $ rType) True True

compileSimpleStep currentDomain s@(Value pos range stringRepresentation) = pure $
  SQD currentDomain (QF.Constant range stringRepresentation) (VDOM range Nothing) True True

compileSimpleStep currentDomain s@(Binding pos membeddingContext) = do
  case currentDomain of
    RDOM (r :: ADT RoleInContext) -> do
      -- The binding of a role is always an ADT RoleInContext.
      (adtOfBinding :: (ADT RoleInContext)) <- lift2 $ bindingOfADT r
      case adtOfBinding of
        UNIVERSAL -> throwError $ RoleHasNoBinding pos (roleInContext2Role <$> r)
        -- TODO. If the 'in context' clause is used, add the context
        -- type as EmbeddingContext to the RDOM.
        -- If not, just use the static context of adtOfBinding.
        -- If the modeller has specified a particular context to navigate the filledBy link to,
        -- Construct a RoleInContext with it.
        -- Otherwise, take the default specified with the role(s) that is(are) the current domain.
        otherwise -> case membeddingContext of
          Nothing -> pure $ SQD currentDomain (QF.DataTypeGetter BindingF) (RDOM adtOfBinding) True False
          Just context -> if isTypeUri context
            then pure $ SQD currentDomain (QF.DataTypeGetterWithParameter BindingF context) (RDOM $ replaceContext adtOfBinding (ContextType context)) True False
            -- Try to qualify the name within the Domain.
            else do
              {namespace} <- lift $ gets _.dfr
              (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain context namespace
              case head qnames of
                Nothing -> throwError $ UnknownContext pos context
                (Just qn) | length qnames == 1 -> pure $ SQD currentDomain (QF.DataTypeGetterWithParameter BindingF (unwrap qn)) (RDOM $ replaceContext adtOfBinding qn) True False
                _ -> throwError $ NotUniquelyIdentifying pos context (map unwrap qnames)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Binder pos binderName membeddingContext) = do
  case currentDomain of
    (RDOM (adtOfBinder :: ADT RoleInContext)) -> do
      (qBinderType :: EnumeratedRoleType) <- if isTypeUri binderName
        then pure $ EnumeratedRoleType binderName
        -- Try to qualify the name within the Domain.
        else do
          {namespace} <- lift $ gets _.dfr
          (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain binderName namespace
          case head qnames of
            Nothing -> throwError $ UnknownRole pos binderName
            (Just qn) | length qnames == 1 -> pure qn
            otherwise -> throwError $ NotUniquelyIdentifying pos binderName (map unwrap qnames)
      case membeddingContext of
        -- <origin> fills R in <optional context>. Clearly, <optional context> is the context that holds the filled
        -- role (binder).
        Nothing -> do
          EnumeratedRole{context} <- lift2 $ getEnumeratedRole qBinderType
          pure $ SQD currentDomain (QF.GetRoleBindersF qBinderType context) (RDOM (ST $ RoleInContext{context, role: qBinderType})) True False
        Just context -> if isTypeUri context
          then pure $ SQD currentDomain (QF.GetRoleBindersF qBinderType (ContextType context)) (RDOM $ replaceContext adtOfBinder (ContextType context) ) True False
          -- Try to qualify the name within the Domain.
          else do
            {namespace} <- lift $ gets _.dfr
            (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain context namespace
            case head qnames of
              Nothing -> throwError $ UnknownContext pos context
              (Just qn) | length qnames == 1 -> pure $ SQD currentDomain (QF.GetRoleBindersF qBinderType (ContextType context)) (RDOM $ replaceContext adtOfBinder qn) True False
              otherwise -> throwError $ NotUniquelyIdentifying pos context (map unwrap qnames)
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(Context pos) = do
  case currentDomain of
    (RDOM (r :: ADT RoleInContext)) -> do
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
      (qRoleName :: EnumeratedRoleType) <- if isTypeUri roleName
        then pure $ EnumeratedRoleType roleName
        -- Try to qualify the name within the Domain.
        else do
          {namespace} <- lift $ gets _.dfr
          (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain roleName namespace
          case head qnames of
            Nothing -> throwError $ UnknownRole pos roleName
            (Just qn) | length qnames == 1 -> pure qn
            otherwise -> throwError $ NotUniquelyIdentifying pos roleName (map unwrap qnames)
      pure $ SQD currentDomain (QF.DataTypeGetterWithParameter SpecialisesRoleTypeF (unwrap qRoleName)) (VDOM PBool Nothing) (isFunctionalFunction SpecialisesRoleTypeF) False
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(RegEx pos (reg :: RegExP)) = do
  case currentDomain of
    VDOM PString _ -> pure $ SQD currentDomain (QF.RegExMatch reg) (VDOM PBool Nothing) True False
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain (TypeTimeOnlyContext pos ctype) = pure $
  SQD currentDomain (QF.TypeTimeOnlyContextF ctype) (CDOM (ST $ ContextType ctype)) True True

compileSimpleStep currentDomain s@(TypeTimeOnlyEnumeratedRole pos ctype rtype) = pure $ SQD currentDomain (QF.TypeTimeOnlyEnumeratedRoleF rtype) (RDOM $ ST $ RoleInContext {context: ContextType ctype, role: (EnumeratedRoleType rtype)}) True True

compileSimpleStep currentDomain s@(TypeTimeOnlyCalculatedRole pos rtype) = do
  -- Get the ADT for the calculated role.
  roleAdt <- lift $ lift $ getRoleADTFromString rtype
  pure $ SQD currentDomain (QF.TypeTimeOnlyCalculatedRoleF rtype) (RDOM roleAdt) True True

compileSimpleStep currentDomain s@(Extern pos) = do
  case currentDomain of
    (CDOM c) -> do
      (rts :: ADT RoleInContext) <- lift2 $ externalRoleOfADT c
      pure $ SQD currentDomain (QF.DataTypeGetter ExternalRoleF) (RDOM rts) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain s@(IndexedName pos) = do
  case currentDomain of
    (CDOM c) -> pure $ SQD currentDomain (QF.DataTypeGetter IndexedContextName) (VDOM PString Nothing) True False
    (RDOM r) -> pure $ SQD currentDomain (QF.DataTypeGetter IndexedRoleName) (VDOM PString Nothing) True False
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

-- compileSimpleStep currentDomain (CreateContext pos ident) = do
--   -- If `ident` is not qualified, try to qualify it in the Domain.
--   (qcontextType :: ContextType) <- if isTypeUri ident
--     then pure $ ContextType ident
--     -- Try to qualify the name within the Domain.
--     else do
--       {_id:namespace} <- lift $ gets _.dfr
--       (qnames :: Array ContextType) <- lift2 $ runArrayT $ qualifyContextInDomain ident (unsafePartial $ fromJust $ (typeUri2ModelUri namespace))
--       case head qnames of
--         Nothing -> throwError $ UnknownContext pos ident
--         (Just qn) | length qnames == 1 -> pure qn
--         otherwise -> throwError $ NotUniquelyIdentifying pos ident (map unwrap qnames)
--   pure $ SQD currentDomain (QF.DataTypeGetterWithParameter CreateContextF (unwrap qcontextType)) (CDOM (ST qcontextType)) True True

compileSimpleStep currentDomain s@(CreateEnumeratedRole pos ident) = case currentDomain of
  (CDOM contextADT) -> do
    -- If `ident` is not qualified, try to qualify it in the Domain.
    (qroleType :: EnumeratedRoleType) <- if isTypeUri ident
      then pure $ EnumeratedRoleType ident
      -- Try to qualify the name within the Domain.
      else do
        {namespace} <- lift $ gets _.dfr
        (qnames :: Array EnumeratedRoleType) <- lift2 $ runArrayT $ qualifyEnumeratedRoleInDomain ident namespace
        case head qnames of
          Nothing -> throwError $ UnknownRole pos ident
          (Just qn) | length qnames == 1 -> pure qn
          otherwise -> throwError $ NotUniquelyIdentifying pos ident (map unwrap qnames)
    pure $ SQD currentDomain (QF.DataTypeGetterWithParameter CreateRoleF (unwrap qroleType)) (RDOM (adtContext2AdtRoleInContext contextADT qroleType)) True True
  otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Simple s)

compileSimpleStep currentDomain (Identity _) = do
  isFunctional <- case currentDomain of
    RDOM r -> (lift $ lift $ adtIsFunctional (map roleInContext2Role r)) >>= if _ then pure True else pure False
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
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain s

compileUnaryStep currentDomain st@(Exists pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    CDOM _ -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)
    otherwise -> pure $ UQD currentDomain (QF.UnaryCombinator ExistsF) descriptionOfs (VDOM PBool Nothing) True True

compileUnaryStep currentDomain st@(FilledBy pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    RDOM _ -> pure $ UQD currentDomain (QF.UnaryCombinator FilledByF) descriptionOfs (VDOM PBool Nothing) True True
    otherwise -> throwError $ IncompatibleQueryArgument pos currentDomain (Unary st)

compileUnaryStep currentDomain st@(Fills pos s) = do
  descriptionOfs <- compileStep currentDomain s
  case range descriptionOfs of
    RDOM _ -> pure $ UQD currentDomain (QF.UnaryCombinator FillsF) descriptionOfs (VDOM PBool Nothing) True True
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
      -- TODO. #18 Add a way to ensure that the left term of the criterium is a functional expression.
      -- The criterium is applied to a single instance each time. Therefore, its left term is, by definition, functional.
      -- We have no way of handling this, currently.
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
        Add pos -> binOp pos f1 f2 [PNumber, PString] AddF
        Subtract pos -> binOp pos f1 f2 [PNumber, PString] SubtractF
        Divide pos -> binOp pos f1 f2 [PNumber] DivideF
        -- Possibly allow PBool
        Multiply pos -> binOp pos f1 f2 [PNumber] MultiplyF

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
              AddF -> ensureDomainIsRange dom [PNumber, PString] pos
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
              FirstF -> pure $ BQD currentDomain (QF.BinaryCombinator QF.ComposeF) f1 f2' ran True True
              _ -> throwError $ ArgumentMustBeSequenceFunction pos
            _ -> throwError $ ArgumentMustBeSequenceFunction pos

  where
    ensureDomainIsRange :: Domain -> Array Range -> ArcPosition -> FD -> FD
    ensureDomainIsRange d@(VDOM r p) allowedRangeConstructors pos fd = if (isJust $ elemIndex r allowedRangeConstructors) -- && (isJust p) Don't know why I originally added this constraint?
      then fd
      else throwError $ WrongTypeForOperator pos allowedRangeConstructors d
    ensureDomainIsRange d allowedRangeConstructors pos _ = throwError $ WrongTypeForOperator pos allowedRangeConstructors d

    comparison :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> FunctionName -> PhaseThree QueryFunctionDescription
    comparison pos left' right' functionName = do
      -- Both ranges must be equal, both sides must be functional.
      if ((range left') `eq` (range right'))
        then if (pessimistic $ functional left') && (pessimistic $ functional right')
          then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM PBool Nothing) (isFunctionalFunction functionName) True
          else throwError $ CardinalitiesDoNotMatch (pessimistic $ functional left') (pessimistic $ functional right') pos
        else throwError $ TypesCannotBeCompared pos (range left') (range right')

    binOp :: ArcPosition -> QueryFunctionDescription -> QueryFunctionDescription -> Array Range -> FunctionName -> PhaseThree QueryFunctionDescription
    binOp pos left' right' allowedRangeConstructors functionName = case range left', range right' of
      -- Both ranges must be equal, both sides must be functional.
      d1@(VDOM rc1 _), (VDOM rc2 _) | rc1 == rc2 ->
        if  allowed rc1 && allowed rc2
          then if (pessimistic $ functional left') && (pessimistic $ functional right')
            then pure $ BQD currentDomain (QF.BinaryCombinator functionName) left' right' (VDOM rc1 Nothing) (isFunctionalFunction functionName) True
            else throwError $ CardinalitiesDoNotMatch (pessimistic $ functional left') (pessimistic $ functional right') pos
          else throwError $ WrongTypeForOperator pos allowedRangeConstructors d1
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
  -- TODO. Misschien: als varName=="currentsubject", en step is niet Identity, maak er dan "filter <step> with binds sys:Me" van.
  isF <- if isJust $ elemIndex varName ["currentcontext", "origin", "currentactor", "notifieduser"]
    then pure True
    else pure $ functional step_
  pure $ UQD currentDomain (QF.BindVariable varName) step_ (range step_) isF (mandatory step_)

compileComputationStep :: Domain -> ComputationStep -> FD
compileComputationStep currentDomain (ComputationStep {functionName, arguments, computedType, start, end}) = do
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
            Just expectedNrOfArgs -> if expectedNrOfArgs == length arguments
              then case mapToRange computedType of
                -- Collect property instances.
                Just r -> pure $ MQD currentDomain (QF.ExternalCorePropertyGetter functionName) (fromFoldable compiledArgs) (VDOM r Nothing) Unknown Unknown
                Nothing -> (lift $ lift $ typeExists (ContextType computedType)) >>= if _
                  -- Collect Context instances.
                  then pure $ SQD currentDomain (QF.ExternalCoreContextGetter functionName) (CDOM (ST (ContextType computedType))) Unknown Unknown

                  -- Collect role instances. Having no other information, we conjecture these instances to have their
                  -- role type in their lexical context.
                  else do
                    context <- lift2 $ enumeratedRoleContextType (EnumeratedRoleType computedType)
                    pure $ MQD currentDomain (QF.ExternalCoreRoleGetter functionName) (fromFoldable compiledArgs) (RDOM (ST $ RoleInContext {context, role: EnumeratedRoleType computedType})) Unknown Unknown
              else throwError (WrongNumberOfArguments start end functionName expectedNrOfArgs (length arguments)))
      else do
        compiledArgs <- traverse (compileStep currentDomain) arguments
        -- TODO. This is a stub.
        -- TODO. Check whether the foreign function exists and whether it has been given the right number of arguments.
        pure $ MQD currentDomain (QF.ForeignRoleGetter functionName) (fromFoldable compiledArgs) (RDOM (ST $ RoleInContext {context: ContextType "", role: EnumeratedRoleType computedType})) Unknown Unknown

  where

    mapToRange :: String -> Maybe Range
    mapToRange s = case s of
      "String" -> Just PString
      "Boolean" -> Just PBool
      "Number" -> Just PNumber
      "DateTime" -> Just PDate
      "Email" -> Just PEmail
      otherwise -> Nothing
