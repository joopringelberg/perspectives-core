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

import Control.Monad.Except (catchJust, throwError)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, head, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List, foldM, uncons)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert, keys, lookup, values)
import Perspectives.CoreTypes ((###=), MP, type (~~~>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (Namespace, endsWithSegments, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), AssignmentOperator(..), LetStep(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseThree, lift2, runPhaseTwo_', withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.DescriptionCompiler (addVarBindingToSequence, compileStep, compileVarBinding, makeSequence)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole, typeExists)
import Perspectives.Representation.Class.Property (rangeOfPropertyType)
import Perspectives.Representation.Class.Role (contextOfRepresentation, expandedADT_)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (Range)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), ContextType, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType, lookForUnqualifiedPropertyType_, lookForUnqualifiedRoleType, lookForUnqualifiedViewType)
import Prelude (Unit, bind, discard, map, pure, unit, void, ($), (<<<), (<>), (==), (>>=), (>=>), (<$>), (<*>))

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
        -- compileRules
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

modifyDF :: (DomeinFileRecord -> DomeinFileRecord) -> PhaseThree Unit
modifyDF f = void $ modify \s@{dfr} -> s {dfr = f dfr}

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
                    adt <- expandedADT_ role
                    (adt ###= lookForUnqualifiedViewType (unwrap rqp))
                  case head viewCandidates of
                    Nothing -> throwError $ UnknownView pos (unwrap rqp)
                    (Just v) | length viewCandidates == 1 -> pure $ Just v
                    otherwise -> throwError $ NotUniquelyIdentifying pos (unwrap rqp) (map unwrap viewCandidates)

-- | For each Role with a binding, record that Role as an inverse binding for the value of the binding.
-- TODO. Implement inverseBindings. Or don't we really need it?
inverseBindings :: PhaseThree Unit
inverseBindings = pure unit

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
          Q (SQD dom (ComputedRoleGetter f) (RDOM (ST (EnumeratedRoleType computedType)))) -> do
            qComputedType <- qualifyRoleType pos computedType enumeratedRoles
            if computedType == unwrap qComputedType
              then pure unit
              else -- change the role in the domain
                modifyDF (\df@{calculatedRoles} -> df {calculatedRoles = insert (unwrap _id) (CalculatedRole rr {calculation = Q $ SQD dom (ComputedRoleGetter f) (RDOM (ST qComputedType))}) calculatedRoles})
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
    compileExpressions' {calculatedRoles, calculatedProperties, actions} = do
      compRoles <- traverseWithIndex compileRolExpr calculatedRoles
      compProps <- traverseWithIndex compilePropertyExpr calculatedProperties
      compActions <- traverseWithIndex compileActionCondition actions
      modifyDF \dfr -> dfr {calculatedRoles = compRoles, calculatedProperties = compProps, actions = compActions}

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

    compileActionCondition :: String -> Action -> PhaseThree Action
    compileActionCondition actionName (Action ar@{subject, condition}) = case condition of
      Q _ -> pure $ Action ar
      S stp -> do
        ctxt <- lift2 (getEnumeratedRole subject >>= pure <<< contextOfRepresentation)
        descr <- compileStep (CDOM $ ST ctxt) stp
        pure $ Action (ar {condition = Q descr})

{-
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
        compileRule actionName a@(Action ar@{subject, effect, object}) = do
          ctxt <- lift2 (getEnumeratedRole subject >>= pure <<< contextOfRepresentation)
          currentDomain <- pure (CDOM $ ST ctxt)
          case effect of
            -- Compile a series of Assignments into a QueryDescription.
            (Just (A assignments)) -> do
              (aStatements :: QueryFunctionDescription) <- sequenceOfAssignments currentDomain assignments
              pure $ Action ar {effect = Just $ EF aStatements}
              -- Compile the LetStep into a QueryDescription.
            (Just (L (LetStep {bindings, assignments}))) -> withFrame
              case uncons bindings of
                -- no variableBindings at all. Just the body. This will probably never occur as the parser breaks on it.
                -- Note we cannot factor sequenceOfAssignments out, even though it occurs
                -- in both cases. In the second case, we first need to build up the
                -- variable bindings.
                Nothing -> do
                  (aStatements :: QueryFunctionDescription) <- sequenceOfAssignments currentDomain assignments
                  pure $ Action ar {effect = Just $ EF aStatements}
                (Just {head: bnd, tail}) -> do
                  head_ <- compileVarBinding currentDomain bnd
                  compiledLet <- makeSequence <$> foldM addVarBindingToSequence head_ tail <*> sequenceOfAssignments currentDomain assignments
                  pure $ Action ar {effect = Just $ EF compiledLet}
            otherwise -> pure a

          where
            sequenceOfAssignments :: Domain -> List Assignment -> PhaseThree QueryFunctionDescription
            sequenceOfAssignments currentDomain assignments = case uncons assignments of
              Nothing -> throwError $ Custom "There must be at least one assignment in a let*"
              (Just {head, tail}) -> do
                head_ <- compileAssignment currentDomain head
                foldM (addAssignmentToSequence currentDomain) head_ tail

            addAssignmentToSequence :: Domain -> QueryFunctionDescription -> Assignment -> PhaseThree QueryFunctionDescription
            addAssignmentToSequence currentDomain seq v = makeSequence <$> pure seq <*> (compileAssignment currentDomain v)

            -- Assume the assignment is on a Role. When that fails, compile it as
            -- an assignment on a Property - but not when it fails because an
            -- unqualified name can be qualified in more than one way.
            compileAssignment :: Domain -> Assignment -> PhaseThree QueryFunctionDescription
            compileAssignment currentDomain ass = catchJust
              (\e -> case e of
                UnknownRole _ _ -> Just ass
                -- Let the NotUniquelyIdentifying error fall through.
                otherwise -> Nothing)
              (compileRoleAssignment currentDomain ass)
              (compilePropertyAssignment object)

            -- Assignment on an EnumeratedRole.
            compileRoleAssignment :: Domain -> Assignment -> PhaseThree QueryFunctionDescription
            compileRoleAssignment currentDomain (Assignment{lhs, operator, value, start, end}) = do
              (er :: EnumeratedRoleType) <- qualifyRoleType start lhs enumeratedRoles
              (mValueDescription :: Maybe QueryFunctionDescription) <- traverse (compileStep currentDomain) value
              case mValueDescription of
                Nothing -> case operator of
                  (Delete p) -> pure $ UQD currentDomain (AssignmentOperator "DeleteRol") (SQD currentDomain (RolGetter (ENR er)) (RDOM (ST er))) currentDomain
                  otherwise -> throwError $ MissingValueForAssignment start end
                Just valueDescription -> case operator of
                  Set _ -> pure $ makeAssignment er valueDescription "SetRol"
                  AddTo _ -> pure $ makeAssignment er valueDescription "AddToRol"
                  DeleteFrom _ -> pure $ makeAssignment er valueDescription "RemoveFromRol"
                  Delete _ -> throwError $ Custom "An Assignment with operator Delete should not be followed by a value expression. This counts as a system programming error."
                  where
                    -- The function we describe has an CDOM result, equal to the
                    -- current domain.
                    makeAssignment :: EnumeratedRoleType -> QueryFunctionDescription -> String -> QueryFunctionDescription
                    makeAssignment er valueDescription opName = BQD
                      currentDomain
                      (AssignmentOperator opName)
                      (SQD currentDomain (RolGetter (ENR er)) (RDOM (ST er)))
                      valueDescription
                      currentDomain

            -- The left hand side must identify an EnumeratedProperty on the Object of the Action.
            compilePropertyAssignment :: RoleType -> Assignment -> PhaseThree QueryFunctionDescription
            compilePropertyAssignment roletype (Assignment{lhs, operator, value, start, end}) = do
              (qualifiedProp :: EnumeratedPropertyType) <- qualifyProperty roletype start lhs
              dom <- lift2 $ expandedADT_ roletype
              propRange <- lift $ lift $ rangeOfPropertyType (ENP qualifiedProp)
              (mValueDescription :: Maybe QueryFunctionDescription) <- traverse (compileStep (RDOM dom)) value
              case mValueDescription of
                Nothing -> case operator of
                  (Delete p) -> pure $ UQD (RDOM dom) (AssignmentOperator "DeleteProperty") (SQD (RDOM dom) (PropertyGetter (ENP qualifiedProp)) (VDOM propRange)) (VDOM propRange)
                  otherwise -> throwError $ MissingValueForAssignment start end
                Just valueDescription -> case operator of
                  Set _ -> pure $ makeAssignment (RDOM dom) propRange qualifiedProp valueDescription "SetProperty"
                  AddTo _ -> pure $ makeAssignment (RDOM dom) propRange qualifiedProp valueDescription "AddToProperty"
                  DeleteFrom _ -> pure $ makeAssignment (RDOM dom) propRange qualifiedProp valueDescription "RemoveFromProperty"
                  Delete _ -> throwError $ Custom "An Assignment with operator Delete should not be followed by a value expression. This counts as a system programming error."
              where
                -- The function we describe has a VDOM Range (e.g. VDOM Boolean), where
                -- Range is the range of the Property.
                makeAssignment :: Domain -> Range -> EnumeratedPropertyType -> QueryFunctionDescription -> String -> QueryFunctionDescription
                makeAssignment currentDomain propRange qualifiedProp valueDescription opName = BQD
                  currentDomain
                  (AssignmentOperator opName)
                  (SQD currentDomain (PropertyGetter (ENP qualifiedProp)) (VDOM propRange))
                  valueDescription
                  (VDOM propRange)

            qualifyProperty :: RoleType -> ArcPosition -> String -> PhaseThree EnumeratedPropertyType
            qualifyProperty erole pos propType = do
              -- Note that we need the DomeinFile with qualified bindings in the cache
              -- for this function to work correctly!
              if isQualifiedWithDomein propType
                then pure $ EnumeratedPropertyType propType
                else do
                  (candidates :: Array PropertyType) <- lift2 (erole ###= (f >=> lookForUnqualifiedPropertyType propType))
                  case head candidates of
                    Nothing -> throwError $ UnknownProperty pos propType
                    (Just (ENP t)) | length candidates == 1 -> pure t
                    otherwise -> throwError $ NotUniquelyIdentifying pos propType (map propertytype2string candidates)
              where
                f :: RoleType ~~~> ADT EnumeratedRoleType
                f rt = ArrayT do
                  adt <- expandedADT_ rt
                  pure [adt]
-}
