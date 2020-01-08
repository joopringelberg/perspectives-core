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

module Perspectives.Parsing.Arc.PhaseThree where

-- | Phase Three of the parser solves problems that arise due to forward reference.
-- | In a View, for example, the modeller can reference a property of a Role that
-- | has not yet been parsed in phase two (this may happen if that Role is filled with
-- | a role that is 'later' in the source text).
-- |

import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, foldM, head, length, null, uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert, keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((###=), MP, (###>))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (Namespace, endsWithSegments, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), AssignmentOperator(..), LetStep(..), Step)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Arc.PhaseThree.SetAffectedContextCalculations (setAffectedContextCalculations)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseThree, lift2, modifyDF, runPhaseTwo_', withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.DescriptionCompiler (addVarBindingToSequence, compileStep, compileVarBinding, makeSequence)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain2roleType, functional, range)
import Perspectives.Representation.ADT (ADT(..), lessThanOrEqualTo, reduce)
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedProperty, getEnumeratedRole, typeExists)
import Perspectives.Representation.Class.Property (range) as PT
import Perspectives.Representation.Class.Role (bindingOfRole, expansionOfADT, getCalculation, getRole)
import Perspectives.Representation.Class.Role (contextOfRepresentation, expandedADT_, roleTypeIsFunctional) as ROLE
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), ContextType, EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType, lookForUnqualifiedPropertyType_, lookForUnqualifiedRoleType, lookForUnqualifiedRoleTypeOfADT, lookForUnqualifiedViewType)
import Prelude (Unit, bind, discard, map, pure, unit, void, ($), (<$>), (<*>), (<<<), (<>), (==), (>>=), (&&), (>=>))

phaseThree :: DomeinFileRecord -> MP (Either PerspectivesError DomeinFileRecord)
phaseThree df@{_id} = do
    (Tuple ei {dfr}) <- runPhaseTwo_'
      (do
        qualifyActionRoles
        qualifyBindings
        qualifyPropertyReferences
        qualifyViewReferences
        -- inverseBindings  -- not yet implemented, probably unnecessary.
        qualifyReturnsClause
        compileExpressions
        compileRules
        )
      df
    case ei of
      (Left e) -> pure $ Left e
      otherwise -> pure $ Right dfr

getDF :: Unit -> PhaseThree DomeinFileRecord
getDF _ = lift $ gets _.dfr

withDomeinFile :: forall a. Namespace -> DomeinFile -> PhaseThree a -> PhaseThree a
withDomeinFile ns df mpa = do
  void $ lift2 $ storeDomeinFileInCache ns df
  r <- mpa
  lift2 $ removeDomeinFileFromCache ns
  pure r

-- | Qualifies the identifiers used in the object- and indirectObject field of an Action.
-- | All Objects are by default constructed as enumerated; this function corrects that if
-- | applicable.
-- | Note that this function requires the DomeinFile to be available in the cache!
qualifyActionRoles :: PhaseThree Unit
qualifyActionRoles = do
  df@{_id} <- lift $ gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (qualifyActionRoles' df)
  where
  qualifyActionRoles' :: DomeinFileRecord -> PhaseThree Unit
  qualifyActionRoles' {contexts, enumeratedRoles, actions, calculatedRoles} = for_ contexts
    \(Context{_id:ctxtId, gebruikerRol, rolInContext, contextRol}) -> for_ gebruikerRol
      \(EnumeratedRoleType ur) -> case lookup ur enumeratedRoles of
        Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> ur <> "' in model.")
        (Just (EnumeratedRole {perspectives})) -> for_ (values perspectives)
          \acts -> for_ acts
            \(ActionType a) -> case lookup a actions of
              Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> a <> "' in model.")
              (Just (Action ar@{_id: actId, object, indirectObject: mindirectObject, pos})) -> do
                ar' <- do
                  qname <- case object of
                    (ENR (EnumeratedRoleType "External")) -> pure $ ENR $ EnumeratedRoleType ((unwrap ctxtId) <> "$" <> "External")  -- TODO: check naamgeving externe rol!
                    other -> qualifiedRoleType ctxtId pos (roletype2string other)
                  -- qname <- qualifiedRoleType ctxtId pos (roletype2string object)
                  pure $ ar {object = qname}
                ar'' <- case mindirectObject of
                  (Just indirectObject) -> do
                    qname <- qualifiedRoleType ctxtId pos (roletype2string indirectObject)
                    pure $ ar' {indirectObject = Just qname}
                  otherwise -> pure ar'
                if ar'' == ar
                  then pure unit
                  -- A change, so modify the DomeinFileRecord
                  else modifyDF (\df@{actions: actions'} -> df {actions = insert (unwrap actId) (Action ar'') actions'})
    where
      qualifiedRoleType :: ContextType -> ArcPosition -> String -> PhaseThree RoleType
      qualifiedRoleType ctxtId pos ident = if isQualifiedWithDomein ident
        then case lookup ident calculatedRoles of
          Nothing -> do
            -- Does the role exist at all (in some other model)?
            exists <- lift2 $ typeExists (EnumeratedRoleType ident)
            if exists
              then pure $ ENR $ EnumeratedRoleType ident
              else throwError $ UnknownRole pos ident
          (Just (CalculatedRole{_id:id'})) -> pure $ CR id'
        else do
          types <- lift2 $ ctxtId ###= lookForUnqualifiedRoleType ident
          case head types of
            Nothing -> throwError $ RoleMissingInContext pos ident (unwrap ctxtId)
            (Just t) -> pure t

-- | Qualifies the identifiers used in the filledBy part of an EnumeratedRole declaration.
-- | A binding is represented as an ADT. We transform all elements of the form `ST segmentedName` in the tree
-- | to `ST qualifiedName`, using the `Reducible a (ADT b)` instance.
-- | We qualify a name only by searching the roles of the domain. Role names that have the segmentedName as a suffix
-- | are candidates to qualify it. Only one such Role may exist in the domain!
-- | Note that this function requires the DomeinFile to be available in the cache!
-- | This function just uses the DomeinFileRecord that is passed in as an argument.
qualifyBindings :: PhaseThree Unit
qualifyBindings = (lift $ gets _.dfr) >>= qualifyBindings'
  where
    qualifyBindings' :: DomeinFileRecord -> PhaseThree Unit
    qualifyBindings' {enumeratedRoles:roles} = for_ roles
      (\(EnumeratedRole rr@{_id, binding, pos}) -> do
        qbinding <- reduce (qualifyBinding pos) binding
        if binding == qbinding
          then pure unit
          else -- change the role in the domain
            modifyDF (\df@{enumeratedRoles} -> df {enumeratedRoles = insert (unwrap _id) (EnumeratedRole rr {binding = qbinding}) enumeratedRoles}))
      where
        qualifyBinding :: ArcPosition -> EnumeratedRoleType -> PhaseThree (ADT EnumeratedRoleType)
        qualifyBinding pos i@(EnumeratedRoleType ident) = qualifyRoleType pos ident roles >>= pure <<< ST

-- | If the name is unqualified, look for an EnumeratedRol with matching local name in the Domain.
qualifyRoleType :: ArcPosition -> String -> Object EnumeratedRole -> PhaseThree EnumeratedRoleType
qualifyRoleType pos ident enumeratedRoles = if isQualifiedWithDomein ident
  then pure $ EnumeratedRoleType ident
  else do
    (candidates :: Array String) <- pure $ filter (\_id -> _id `endsWithSegments` ident) (keys enumeratedRoles)
    case head candidates of
      Nothing -> throwError $ UnknownRole pos ident
      (Just qname) | length candidates == 1 -> pure $ EnumeratedRoleType qname
      otherwise -> throwError $ NotUniquelyIdentifying pos ident candidates

-- | Qualify the references to Properties in each View.
qualifyPropertyReferences :: PhaseThree Unit
qualifyPropertyReferences = do
  df@{_id} <- lift $ gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (qualifyPropertyReferences' df)
  where
    qualifyPropertyReferences' :: DomeinFileRecord -> PhaseThree Unit
    qualifyPropertyReferences' df@{_id, views, calculatedProperties} = do
      qviews <- traverseWithIndex qualifyView views
      modifyDF \dfr -> dfr {views = qviews}

      where
        qualifyView :: String -> View -> PhaseThree View
        qualifyView viewName (View vr@{propertyReferences, role, pos}) = do
          qprops <- traverse (qualifyProperty role pos) propertyReferences
          pure $ View $ vr {propertyReferences = qprops}

        qualifyProperty :: EnumeratedRoleType -> ArcPosition -> PropertyType -> PhaseThree PropertyType
        qualifyProperty erole pos propType = do
          -- Note that we need the DomeinFile with qualified bindings in the cache
          -- for this function to work correctly!
          if isQualifiedWithDomein (propertytype2string propType)
            -- The modeller has provided a qualified property. He cannot say whether it is Calculated, or Enumerated,
            -- however. If it is Calculated, change now.
            then if isJust (lookup (propertytype2string propType) calculatedProperties)
              then pure $ CP $ CalculatedPropertyType (propertytype2string propType)
              else pure propType
            else do
              (candidates :: Array PropertyType) <- lift2 (erole ###= lookForUnqualifiedPropertyType_ (propertytype2string propType))
              case head candidates of
                Nothing -> throwError $ UnknownProperty pos (propertytype2string propType)
                (Just t) | length candidates == 1 -> pure t
                otherwise -> throwError $ NotUniquelyIdentifying pos (propertytype2string propType) (map propertytype2string candidates)

-- | The views on the subject, object and indirectObject of an Action can be specified
-- | with a local name. It should be possible to qualify such a name by comparing it with
-- | the views that are available on the roles bound to the subject, object and
-- | indirectObject, respectively.
qualifyViewReferences :: PhaseThree Unit
qualifyViewReferences = do
  df@{_id} <- lift $ gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (qualifyViewReferences' df)
  where
    qualifyViewReferences' :: DomeinFileRecord -> PhaseThree Unit
    qualifyViewReferences' df@{_id, actions} = do
      qactions <- traverseWithIndex qualifyAction actions
      modifyDF \dfr -> dfr {actions = qactions}

      where
        qualifyAction :: String -> Action -> PhaseThree Action
        qualifyAction actionName (Action ar@{subject, requiredSubjectProperties, object, requiredObjectProperties, indirectObject, requiredIndirectObjectProperties, pos}) = do
          (subjectView :: Maybe ViewType) <- qualifyViewForRole requiredSubjectProperties (ENR subject)
          (objectView :: Maybe ViewType) <- qualifyViewForRole requiredObjectProperties object
          (indirectObjectView :: Maybe ViewType) <- case indirectObject of
            (Just indirectObject') -> qualifyViewForRole requiredIndirectObjectProperties indirectObject'
            Nothing -> pure Nothing
          pure $ Action ar
            { requiredSubjectProperties = subjectView
            , requiredObjectProperties = objectView
            , requiredIndirectObjectProperties = indirectObjectView}

          where
            qualifyViewForRole :: Maybe ViewType -> RoleType -> PhaseThree (Maybe ViewType)
            qualifyViewForRole requiredProperties role =
              case requiredProperties of
                Nothing -> pure Nothing
                (Just rqp) -> do
                  viewCandidates <- lift2 do
                    adt <- ROLE.expandedADT_ role
                    (adt ###= lookForUnqualifiedViewType (unwrap rqp))
                  case head viewCandidates of
                    Nothing -> throwError $ UnknownView pos (unwrap rqp)
                    (Just v) | length viewCandidates == 1 -> pure $ Just v
                    otherwise -> throwError $ NotUniquelyIdentifying pos (unwrap rqp) (map unwrap viewCandidates)

-- | For each Role with a binding, record that Role as an inverse binding for the value of the binding.
-- TODO. Implement inverseBindings. Or don't we really need it?
inverseBindings :: PhaseThree Unit
inverseBindings = throwError (Custom "Implement inverseBindings")

-- | A Computed Role has a clause that specifies the type of Role that is computed.
-- | The modeller can use an unqualified name, that should be resolved against all Roles in the Domain.
-- TODO: qualificeer Computed properties!
qualifyReturnsClause :: PhaseThree Unit
qualifyReturnsClause = (lift $ gets _.dfr) >>= qualifyReturnsClause'
  where
    qualifyReturnsClause' :: DomeinFileRecord -> PhaseThree Unit
    qualifyReturnsClause' {calculatedRoles:roles, enumeratedRoles} = for_ roles
      (\(CalculatedRole rr@{_id, calculation, pos}) -> do
        case calculation of
          Q (SQD dom (QF.ComputedRoleGetter f) (RDOM (ST (EnumeratedRoleType computedType))) isF isM) -> do
            qComputedType <- qualifyRoleType pos computedType enumeratedRoles
            if computedType == unwrap qComputedType
              then pure unit
              else -- change the role in the domain
                modifyDF (\df@{calculatedRoles} -> df {calculatedRoles = insert (unwrap _id) (CalculatedRole rr {calculation = Q $ SQD dom (QF.ComputedRoleGetter f) (RDOM (ST qComputedType)) isF isM}) calculatedRoles})
          otherwise -> pure unit)

-- | The calculation of a CalculatedRole or CalculatedProperty, and the condition of an Action are all expressions. This function compiles the parser AST output that represents these expressions to QueryFunctionDescriptions.
-- | All names are qualified in the process.
compileExpressions :: PhaseThree Unit
compileExpressions = do
  df@{_id} <- lift $ gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (compileExpressions' df)
  where
    compileExpressions' :: DomeinFileRecord -> PhaseThree Unit
    compileExpressions' {calculatedRoles, calculatedProperties} = do
      -- TODO. Collect references to Calculated Properties from the QueryFunctionDescriptions and sort
      -- the calculatedRoles accordingly before compiling the descriptions. That will detect cycles
      -- (throw an error) and prevent forward reference errors.
      compRoles <- traverseWithIndex compileRolExpr calculatedRoles
      compProps <- traverseWithIndex compilePropertyExpr calculatedProperties
      modifyDF \dfr -> dfr {calculatedRoles = compRoles, calculatedProperties = compProps}

    compileRolExpr :: String -> CalculatedRole -> PhaseThree CalculatedRole
    compileRolExpr roleName (CalculatedRole cr@{calculation, context}) = case calculation of
      Q _ -> pure $ CalculatedRole cr
      S stp -> do
        descr <- compileStep (CDOM $ ST context) stp
        pure $ CalculatedRole (cr {calculation = Q descr})

    compilePropertyExpr :: String -> CalculatedProperty -> PhaseThree CalculatedProperty
    compilePropertyExpr propertyName (CalculatedProperty cr@{calculation, role}) = case calculation of
      Q _ -> pure $ CalculatedProperty cr
      S stp -> do
        descr <- compileStep (RDOM $ ST role) stp
        pure $ CalculatedProperty (cr {calculation = Q descr})

-- | For each Action that has a SideEffect for its `effect` member, compile the List of Assignments, or the Let* expression in it to a `QueryFunctionDescription`.
-- | All names are qualified in the process.
compileRules :: PhaseThree Unit
compileRules = do
  df@{_id} <- lift $ gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (compileRules' df)
  where
    compileRules' :: DomeinFileRecord -> PhaseThree Unit
    compileRules' {actions, enumeratedRoles} = do
      compActions <- traverseWithIndex compileRule actions
      modifyDF \dfr -> dfr {actions = compActions}
      where
        compileRule :: String -> Action -> PhaseThree Action
        compileRule actionName a@(Action ar@{_id, subject, condition, effect, object}) = do
          -- Compileer hier de conditie ook.
          conditionDescription <- compileActionCondition
          ctxt <- lift2 (getEnumeratedRole subject >>= pure <<< ROLE.contextOfRepresentation)
          currentDomain <- pure (CDOM $ ST ctxt)
          -- The expression below returns a QueryFunctionDescription that describes either a single assignment, or
          -- a BQD with QueryFunction equal to (BinaryCombinator SequenceF).
          case effect of
            -- Compile a series of Assignments into a QueryDescription.
            (Just (A assignments)) -> do
              (aStatements :: QueryFunctionDescription) <- sequenceOfAssignments currentDomain assignments
              pure $ Action ar {condition = Q conditionDescription, effect = Just $ EF aStatements}
              -- Compile the LetStep into a QueryDescription.
            (Just (L (LetStep {bindings, assignments}))) -> withFrame
              case uncons bindings of
                -- no variableBindings at all. Just the body. This will probably never occur as the parser breaks on it.
                -- Note we cannot factor sequenceOfAssignments out, even though it occurs
                -- in both cases. In the second case, we first need to build up the
                -- variable bindings.
                Nothing -> do
                  (aStatements :: QueryFunctionDescription) <- sequenceOfAssignments currentDomain assignments
                  pure $ Action ar {condition = Q conditionDescription, effect = Just $ EF aStatements}
                (Just {head: bnd, tail}) -> do
                  head_ <- compileVarBinding currentDomain bnd
                  compiledLet <- makeSequence <$> foldM addVarBindingToSequence head_ tail <*> sequenceOfAssignments currentDomain assignments
                  pure $ Action ar {condition = Q conditionDescription, effect = Just $ EF compiledLet}
            otherwise -> pure a

          where
            compileActionCondition :: PhaseThree QueryFunctionDescription
            compileActionCondition = case condition of
              Q d -> pure d
              S stp -> do
                ctxt <- lift2 (getEnumeratedRole subject >>= pure <<< ROLE.contextOfRepresentation)
                descr <- compileStep (CDOM $ ST ctxt) stp
                setAffectedContextCalculations _id descr
                pure descr

            -- This will return a QueryFunctionDescription that describes either a single assignment, or
            -- a BQD with QueryFunction equal to (BinaryCombinator SequenceF)
            sequenceOfAssignments :: Domain -> Array Assignment -> PhaseThree QueryFunctionDescription
            sequenceOfAssignments currentDomain assignments = case uncons assignments of
              Nothing -> throwError $ Custom "There must be at least one assignment in a let*"
              (Just {head, tail}) -> do
                head_ <- describeAssignmentStatement currentDomain head
                foldM (addAssignmentToSequence currentDomain) head_ tail

            -- Returns a BQD with QueryFunction (BinaryCombinator SequenceF)
            addAssignmentToSequence :: Domain -> QueryFunctionDescription -> Assignment -> PhaseThree QueryFunctionDescription
            addAssignmentToSequence currentDomain seq v = makeSequence <$> pure seq <*> (describeAssignmentStatement currentDomain v)

            -- we need the Object of the Perspective. Right now it is a RoleType, possibly a(n anonymous) CalculatedRole.
            -- The assignment functions arbitrarily return the currentContext. Hence,
            -- we declare the functions to be both functional and mandatory.
            describeAssignmentStatement :: Domain -> Assignment -> PhaseThree QueryFunctionDescription
            describeAssignmentStatement currentDomain ass = case ass of
              Remove {roleExpression} -> do
                rle <- ensureRole currentDomain roleExpression
                pure $ UQD currentDomain QF.Remove rle currentDomain True True
              CreateRole {roleIdentifier, contextExpression, start, end} -> do
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain QF.Identity currentDomain True True)
                  (Just stp) -> ensureContext currentDomain stp
                qualifiedRoleIdentifier <- qualifyWithRespectTo roleIdentifier cte start end
                pure $ UQD currentDomain (QF.CreateRole qualifiedRoleIdentifier) cte currentDomain True True
              Move {roleExpression, contextExpression} -> do
                rle <- ensureRole currentDomain roleExpression
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain QF.Identity currentDomain True True)
                  (Just (stp :: Step)) -> ensureContext currentDomain stp >>= ensureFunctional stp
                pure $ BQD currentDomain QF.Move rle cte currentDomain True True
              Bind f@{bindingExpression, roleIdentifier, contextExpression} -> do
                -- Bind <binding-expression> to <binderType> [in <context-expression>]. Check:
                -- bindingExpression should result in roles
                (bindings :: QueryFunctionDescription) <- ensureRole currentDomain bindingExpression
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain QF.Identity currentDomain True True)
                  (Just (stp :: Step)) -> ensureContext currentDomain stp
                -- binderType should be an EnumeratedRoleType (local name should resolve w.r.t. the contextExpression)
                (qualifiedRoleIdentifier :: EnumeratedRoleType) <- qualifyWithRespectTo roleIdentifier cte f.start f.end
                -- If the roleIdentifier is functional, the bindings should be functional too.
                (lift $ lift $ ROLE.roleTypeIsFunctional (ENR qualifiedRoleIdentifier)) >>= if _
                  then case functional bindings of
                    True -> pure unit
                    Unknown -> throwError $ MaybeNotFunctional f.start f.end bindingExpression
                    False -> throwError $ NotFunctional f.start f.end bindingExpression
                  else pure unit
                -- the possible bindings of binderType (qualifiedRoleIdentifier) should be less specific (=more general) than or equal to the type of the results of binderExpression (bindings).
                qualifies <- do
                  possibleBinding <- lift $ lift (bindingOfRole (ENR qualifiedRoleIdentifier) >>= expansionOfADT)
                  bindings' <- pure (unsafePartial $ domain2roleType (range bindings))
                  pure (possibleBinding `lessThanOrEqualTo` bindings')
                if qualifies
                  -- Create a function description that describes the actual role creating and binding.
                  then pure $ BQD currentDomain (QF.Bind qualifiedRoleIdentifier) bindings cte currentDomain True True
                  else throwError $ RoleDoesNotBind f.start (ENR qualifiedRoleIdentifier) (unsafePartial $ domain2roleType (range bindings))

              Bind_ {bindingExpression, binderExpression} -> do
                -- bindingExpression should result in a functional role
                (bindings :: QueryFunctionDescription) <- ensureRole currentDomain bindingExpression >>= ensureFunctional bindingExpression
                -- binderExpression should result in a functional role
                (binders :: QueryFunctionDescription) <- ensureRole currentDomain binderExpression >>= ensureFunctional binderExpression
                -- Now create a function description.
                pure $ BQD currentDomain QF.Bind_ bindings binders currentDomain True True

              Unbind f@{bindingExpression, roleIdentifier} -> do
                (bindings :: QueryFunctionDescription) <- ensureRole currentDomain bindingExpression
                -- binderType (roleIdentifier) should be an EnumeratedRoleType (local name should resolve w.r.t. the binders of the bindings). We try to resolve in the model and then filter candidates on whether they bind the bindings.
                (qualifiedRoleIdentifier :: Maybe EnumeratedRoleType) <- qualifyBinderType roleIdentifier (unsafePartial $ domain2roleType $ range bindings) f.start f.end
                pure $ UQD currentDomain (QF.Unbind qualifiedRoleIdentifier) bindings currentDomain True True

              Unbind_ {bindingExpression, binderExpression} -> do
                -- bindingExpression should result in a functional role
                (bindings :: QueryFunctionDescription) <- ensureRole currentDomain bindingExpression >>= ensureFunctional bindingExpression
                -- binderExpression should result in a functional role
                (binders :: QueryFunctionDescription) <- ensureRole currentDomain binderExpression >>= ensureFunctional binderExpression
                -- Now create a function description.
                pure $ BQD currentDomain QF.Unbind_ bindings binders currentDomain True True

              DeleteRole {roleExpression} -> do
                roleQfd <- ensureRole currentDomain roleExpression
                pure $ UQD currentDomain QF.DeleteRole roleQfd currentDomain True True

              DeleteProperty f@{propertyIdentifier, roleExpression} -> do
                (roleQfd :: QueryFunctionDescription) <- case roleExpression of
                  Nothing -> lift $ lift $ getRole object >>= getCalculation
                  Just e -> ensureRole currentDomain e
                (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
                pure $ UQD currentDomain (QF.DeleteProperty qualifiedProperty) roleQfd currentDomain True True

              PropertyAssignment f@{propertyIdentifier, operator, valueExpression, roleExpression} -> do
                (roleQfd :: QueryFunctionDescription) <- case roleExpression of
                  Nothing -> lift $ lift $ (getRole >=> getCalculation) object
                  Just e -> ensureRole currentDomain e
                (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
                -- Compile the value expression to a QueryFunctionDescription. Its range must comply with the range of the qualifiedProperty. It is compiled relative to the current context; not relative to the object!
                valueQfd <- compileStep currentDomain valueExpression
                rangeOfProperty <- lift $ lift $ getEnumeratedProperty qualifiedProperty >>= PT.range
                fname <- case operator of
                  Set _ -> pure $ QF.SetPropertyValue qualifiedProperty
                  AddTo _ -> pure $ QF.AddPropertyValue qualifiedProperty
                  DeleteFrom _ -> pure $ QF.RemovePropertyValue qualifiedProperty
                case range valueQfd of
                  (VDOM r _) | r == rangeOfProperty -> pure unit
                  (VDOM r _) -> throwError $ WrongPropertyRange (startOf valueExpression) (endOf valueExpression) rangeOfProperty r
                  otherwise -> throwError $ NotAPropertyRange (startOf valueExpression) (endOf valueExpression) rangeOfProperty
                pure $ BQD currentDomain fname valueQfd roleQfd currentDomain True True

              where
                qualifyWithRespectTo :: String -> QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree EnumeratedRoleType
                qualifyWithRespectTo roleIdentifier contextFunctionDescription start end = do
                  (ct :: ADT ContextType) <- case range contextFunctionDescription of
                    (CDOM ct') -> pure ct'
                    otherwise -> throwError $ NotAContextDomain otherwise start end
                  mrt <- lift2 (ct ###> lookForUnqualifiedRoleTypeOfADT roleIdentifier)
                  case mrt of
                    Just (ENR et) -> pure et
                    Just (CR ct') -> throwError $ CannotCreateCalculatedRole ct' start end
                    otherwise -> throwError $ ContextHasNoRole ct roleIdentifier

                qualifyPropertyWithRespectTo :: String -> QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree EnumeratedPropertyType
                qualifyPropertyWithRespectTo propertyIdentifier roleQfdunctionDescription start end = do
                  (rt :: ADT EnumeratedRoleType) <- case range roleQfdunctionDescription of
                    (RDOM rt') -> pure rt'
                    otherwise -> throwError $ NotARoleDomain otherwise start end
                  (mrt :: Maybe PropertyType) <- lift2 (rt ###> lookForUnqualifiedPropertyType propertyIdentifier)
                  case mrt of
                    Just (ENP et) -> pure et
                    Just (CP ct') -> throwError $ CannotCreateCalculatedProperty ct' start end
                    otherwise -> throwError $ RoleHasNoProperty rt propertyIdentifier

                -- | If the name is unqualified, look for an EnumeratedRol with matching local name in the Domain.
                qualifyBinderType :: Maybe String -> ADT EnumeratedRoleType -> ArcPosition -> ArcPosition -> PhaseThree (Maybe EnumeratedRoleType)
                qualifyBinderType Nothing _ _ _ = pure Nothing
                qualifyBinderType (Just ident) bindings start end = if isQualifiedWithDomein ident
                  then pure $ Just $ EnumeratedRoleType ident
                  else do
                    (nameMatches :: Array EnumeratedRole) <- pure (filter (\(EnumeratedRole{_id:roleId}) -> (unwrap roleId) `endsWithSegments` ident) (values enumeratedRoles))
                    (candidates :: Array String) <- pure (map (unwrap <<< _._id <<< unwrap) (filter (\(EnumeratedRole{binding}) -> binding `lessThanOrEqualTo` bindings) nameMatches))
                    case head candidates of
                      Nothing -> if null nameMatches
                        then throwError $ UnknownRole start ident
                        else throwError $ LocalRoleDoesNotBind start end ident bindings
                      (Just qname) | length candidates == 1 -> pure $ Just $ EnumeratedRoleType qname
                      otherwise -> throwError $ NotUniquelyIdentifying start ident candidates
                  where
                    f :: EnumeratedRole -> Boolean
                    f (EnumeratedRole {_id:roleId, binding}) = (unwrap roleId `endsWithSegments` ident) && (binding `lessThanOrEqualTo` bindings)


ensureContext :: Domain -> Step -> PhaseThree QueryFunctionDescription
ensureContext currentDomain stp = do
  qfd <- compileStep currentDomain stp
  case range qfd of
    (CDOM _) -> pure qfd
    otherwise -> throwError $ NotAContextDomain (range qfd) (startOf stp) (endOf stp)

ensureRole :: Domain -> Step -> PhaseThree QueryFunctionDescription
ensureRole currentDomain stp = do
  qfd <- compileStep currentDomain stp
  case range qfd of
    (RDOM _) -> pure qfd
    otherwise -> throwError $ NotARoleDomain (range qfd) (startOf stp) (endOf stp)

ensureFunctional :: Step -> QueryFunctionDescription -> PhaseThree QueryFunctionDescription
ensureFunctional stp qfd = case functional qfd of
  True -> pure qfd
  Unknown -> throwError $ MaybeNotFunctional (startOf stp) (endOf stp) stp
  False -> throwError $ NotFunctional (startOf stp) (endOf stp) stp
