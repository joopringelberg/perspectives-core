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

module Perspectives.Parsing.Arc.PhaseThree where

-- | Phase Three of the parser solves problems that arise due to forward reference.
-- | In a View, for example, the modeller can reference a property of a Role that
-- | has not yet been parsed in phase two (this may happen if that Role is filled with
-- | a role that is 'later' in the source text).
-- |

import Control.Monad.Error.Class (try)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets) as State
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, filter, findIndex, foldr, fromFoldable, head, index, length, updateAt)
import Data.Array.Partial (head) as ARRP
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.List (List(..))
import Data.Map (Map, empty, insert, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert, keys, lookup, unions, singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((###=), MP)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..), DomeinFileRecord, indexedContexts, indexedRoles)
import Perspectives.Identifiers (Namespace, endsWithSegments, isQualifiedWithDomein)
import Perspectives.InvertedQuery (RelevantProperties(..))
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), NotificationE(..), PropertyVerbE(..), PropsOrView(..), RoleVerbE(..), StateQualifiedPart(..), StateTransitionE(..), StateSpecification(..)) as AST
import Perspectives.Parsing.Arc.AST (RoleIdentification(..), SegmentedPath, StateSpecification(..), StateTransitionE(..))
import Perspectives.Parsing.Arc.ContextualVariables (addContextualVariablesToExpression)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, lift2, modifyDF, runPhaseTwo_', withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.ExpressionCompiler (compileAndDistributeStep, compileAndSaveProperty, compileAndSaveRole, qualifyLocalEnumeratedRoleName, compileExpression)
import Perspectives.Query.Kinked (setInvertedQueries)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription, range)
import Perspectives.Query.StatementCompiler (compileStatement)
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getCalculatedProperty, getCalculatedRole)
import Perspectives.Representation.Class.Role (roleADT)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.Sentence (Sentence(..), SentencePart(..)) as Sentence
import Perspectives.Representation.State (State(..), StateFulObject(..), StateRecord, constructState)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType, ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType_, roleStates, statesPerProperty)
import Prelude (class Ord, Unit, append, bind, discard, flip, join, map, pure, unit, void, ($), (<$>), (<*), (<<<), (==), (>=>), (>>=))

phaseThree :: DomeinFileRecord -> List AST.StateQualifiedPart -> MP (Either PerspectivesError DomeinFileRecord)
phaseThree df@{_id} postponedParts = do
  -- Store the DomeinFile in cache. If a prefix for the domain is defined in the file,
  -- phaseThree_ will try to retrieve it.
  void $ storeDomeinFileInCache _id (DomeinFile df)
  phaseThree_ df postponedParts <* removeDomeinFileFromCache _id

phaseThree_ :: DomeinFileRecord -> List AST.StateQualifiedPart -> MP (Either PerspectivesError DomeinFileRecord)
phaseThree_ df@{_id, referredModels} postponedParts = do
  -- We don't expect an error on retrieving the DomeinFile, as we've only just put it into cache!
  indexedContexts <- unions <$> traverse (getDomeinFile >=> pure <<< indexedContexts) referredModels
  indexedRoles <- unions <$> traverse (getDomeinFile >=> pure <<< indexedRoles) referredModels
  (Tuple ei {dfr}) <- runPhaseTwo_'
    (do
      qualifyBindings
      compileCalculatedRolesAndProperties
      handlePostponedStateQualifiedParts
      requalifyBindingsToCalculatedRoles
      qualifyPropertyReferences
      invertPerspectiveObjects
      compileStates
      )
    df
    indexedContexts
    indexedRoles
    postponedParts
  case ei of
    (Left e) -> pure $ Left e
    otherwise -> pure $ Right dfr

getDF :: Unit -> PhaseThree DomeinFileRecord
getDF _ = lift $ State.gets _.dfr

withDomeinFile :: forall a. Namespace -> DomeinFile -> PhaseThree a -> PhaseThree a
withDomeinFile ns df mpa = do
  void $ lift2 $ storeDomeinFileInCache ns df
  r <- mpa
  lift2 $ removeDomeinFileFromCache ns
  pure r

-- | Qualifies the identifiers used in the filledBy part of an EnumeratedRole declaration.
-- | A binding is represented as an ADT. We transform all elements of the form `ST segmentedName` in the tree
-- | to `ST qualifiedName`, using the `Reducible a (ADT b)` instance.
-- | We qualify a name only by searching the roles of the domain. Role names that have the segmentedName as a suffix
-- | are candidates to qualify it. Only one such Role may exist in the domain!
-- | Note that this function requires the DomeinFile to be available in the cache.
-- | This function just uses the DomeinFileRecord that is passed in as an argument.
qualifyBindings :: PhaseThree Unit
qualifyBindings = (lift $ State.gets _.dfr) >>= qualifyBindings'
  where
    qualifyBindings' :: DomeinFileRecord -> PhaseThree Unit
    qualifyBindings' {enumeratedRoles:eroles, calculatedRoles:croles} = for_ eroles
      (\(EnumeratedRole rr@{_id, binding, pos}) -> do
        qbinding <- reduce (qualifyBinding pos) binding
        if binding == qbinding
          then pure unit
          else -- change the role in the domain
            modifyDF (\df@{enumeratedRoles} -> df {enumeratedRoles = insert (unwrap _id) (EnumeratedRole rr {binding = qbinding}) enumeratedRoles}))
      where
        qualifyBinding :: ArcPosition -> EnumeratedRoleType -> PhaseThree (ADT EnumeratedRoleType)
        qualifyBinding pos i@(EnumeratedRoleType ident) = do
          q <- try $ ST <$> qualifyLocalEnumeratedRoleName pos ident (keys eroles)
          case q of
            -- We introduce an intentional semantic error here by attempting to qualify the binding, that we know not
            -- to be an EnumeratedRole, as an EnumeratedRole. However, with requalifyBindingsToCalculatedRoles we will
            -- correct that error. We cannot do otherwise because at this state we don't have compiled the expressions
            -- of the CalculatedRoles yet.
            -- If not found in the EnumeratedRoles, try the CalculatedRoles
            Left (UnknownRole _ _) -> ST <$> qualifyLocalEnumeratedRoleName pos ident (keys croles)
            Left e -> throwError e
            Right adt -> pure adt

-- | For each (Enumerated) role with a binding to the name of a CalculatedRole (falsely declared to be Enumerated!),
-- | replace that binding with the ADT of the (now compiled) CalculatedRole.
requalifyBindingsToCalculatedRoles :: PhaseThree Unit
requalifyBindingsToCalculatedRoles = (lift $ State.gets _.dfr) >>= qualifyBindings'
  where
    qualifyBindings' :: DomeinFileRecord -> PhaseThree Unit
    qualifyBindings' {enumeratedRoles:eroles, calculatedRoles:croles} = for_ eroles
      (\(EnumeratedRole rr@{_id, binding, pos}) -> case binding of
        -- If the binding is to a calculated role, replace it with the roleADT of that calculated role.
        ST (EnumeratedRoleType cr) -> case lookup cr croles of
          Nothing -> pure unit
          Just crole -> do
            adt <- lift2 $ roleADT crole
            modifyDF (\df@{enumeratedRoles} -> df {enumeratedRoles = insert (unwrap _id) (EnumeratedRole rr {binding = adt}) enumeratedRoles})
        otherwise -> pure unit)

-- | Qualify the references to Properties in each View.
-- | Note that we need the DomeinFile with qualified bindings in the cache
-- | for this function to work correctly!
qualifyPropertyReferences :: PhaseThree Unit
qualifyPropertyReferences = do
  df@{_id} <- lift $ State.gets _.dfr
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
          if isQualifiedWithDomein (propertytype2string propType)
            -- The modeller has provided a qualified property. He cannot say whether it is Calculated, or Enumerated,
            -- however. If it is Calculated, change now.
            then if isJust (lookup (propertytype2string propType) calculatedProperties)
              then pure $ CP $ CalculatedPropertyType (propertytype2string propType)
              else pure propType
            else do
              (candidates :: Array PropertyType) <- lift2 (erole ###= lookForUnqualifiedPropertyType_ (propertytype2string propType))
              case head candidates of
                Nothing -> throwError $ UnknownProperty pos (propertytype2string propType) erole
                (Just t) | length candidates == 1 -> pure t
                otherwise -> throwError $ NotUniquelyIdentifying pos (propertytype2string propType) (map propertytype2string candidates)

-- | For each EnumeratedRole R in the model, add InvertedQueries to make deltas
-- | available to User Roles in the context of R that have a Perspective on R.
-- | For an explanation, see https://joopringelberg.github.io/perspectives-documentation/Perspectives%20on%20bindings.pdf
-- invertedQueriesForLocalRolesAndProperties :: PhaseThree Unit
-- invertedQueriesForLocalRolesAndProperties = do
--   df@{_id} <- lift $ State.gets _.dfr
--   withDomeinFile
--     _id
--     (DomeinFile df)
--     (invertedQueriesForLocalRolesAndProperties' df)
--   where
--     invertedQueriesForLocalRolesAndProperties' :: DomeinFileRecord -> PhaseThree Unit
--     invertedQueriesForLocalRolesAndProperties' {enumeratedRoles} = do
--       for_ enumeratedRoles
--         \(EnumeratedRole {_id, context, mandatory, functional}) -> do
--           (userTypes :: Array RoleType) <- lift $ lift (context ###= unsafePartial localEnumeratedRolesWithPerspectiveOnRole (ENR _id))
--           qwk <- pure $ ZQ
--             (Just (SQD (RDOM (ST _id)) (QF.DataTypeGetter QF.ContextF) (CDOM (ST context)) True (bool2threeValued mandatory)))
--             Nothing
--           for_ userTypes \userType -> do
--             pv <- lift2 $ unsafePartial relevantPropertiesForObjectRole (ENR _id) userType
--             -- Now add those verbs to the inverted query.
--             setInvertedQueriesForUserAndRole userType (ST _id) pv true qwk

-- | The calculation of a CalculatedRole, of a CalculatedProperty, the object of a Perspective
-- | and the object and query of a state are all expressions.
-- | This function compiles the parser AST output that represents these expressions to QueryFunctionDescriptions.
-- | All names are qualified in the process.
compileCalculatedRolesAndProperties :: PhaseThree Unit
compileCalculatedRolesAndProperties = do
  df@{_id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    _id
    (DomeinFile df)
    (compileCalculatedRolesAndProperties' df _id)
  where
    compileCalculatedRolesAndProperties' :: DomeinFileRecord -> Namespace -> PhaseThree Unit
    compileCalculatedRolesAndProperties' {_id,calculatedRoles, calculatedProperties, states, enumeratedRoles} ns = do
      traverse_ compileRolExpr (identifier <$> calculatedRoles)
      traverse_ compilePropertyExpr (identifier <$> calculatedProperties)
      -- The objects of perspectives are compiled when we handle the postponedStateQualifiedParts.
      -- Get the DomeinFile out of cache and replace the one in PhaseTwoState with it.
      -- We will not have errors on trying to retrieve the DomeinFile here.
      DomeinFile modifiedDomeinFile <- lift2 $ getDomeinFile (DomeinFileId ns)
      modifyDF \dfr -> modifiedDomeinFile
      where
        compileRolExpr :: CalculatedRoleType -> PhaseThree Unit
        compileRolExpr roleType = do
          cr@(CalculatedRole {calculation, context}) <- lift2 $ getCalculatedRole roleType
          case calculation of
            Q _ -> pure unit
            -- Compiles the parsed expression and stores the modified CalculatedRole in
            -- the DomeinCache.
            S stp -> void $ compileAndSaveRole (CDOM $ ST context) stp cr

        compilePropertyExpr :: CalculatedPropertyType -> PhaseThree Unit
        compilePropertyExpr propertyType = do
          cp@(CalculatedProperty {calculation, role}) <- lift2 $ getCalculatedProperty propertyType
          case calculation of
            Q _ -> pure unit
            S stp -> void $ compileAndSaveProperty (RDOM $ ST role) stp cp

handlePostponedStateQualifiedParts  :: PhaseThree Unit
handlePostponedStateQualifiedParts = do
  df@{_id} <- lift $ State.gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    do
      postponedStateQualifiedParts <- lift $ State.gets _.postponedStateQualifiedParts
      for_ postponedStateQualifiedParts (unsafePartial handlePart)
  where

    collectRoles :: RoleIdentification -> PhaseThree (Array EnumeratedRoleType)
    collectRoles (ExplicitRole _ rt _) = pure [rt]
    collectRoles (ImplicitRole ctxt s) = compileExpression (CDOM (ST ctxt)) s >>= \qfd ->
      case range qfd of
        RDOM adt -> pure $ reduce adt
        otherwise -> throwError $ NotARoleDomain otherwise (startOf s) (endOf s)
      where
        -- Translate the RoleIdentification to an array of EnumeratedRoleTypes.
        -- Notice we do not fail on UNIVERSAL or EMPTY.
        reduce :: ADT EnumeratedRoleType -> Array EnumeratedRoleType
        reduce (ST t) = [t]
        reduce (SUM args) = join $ map reduce args
        reduce (PROD args) = maybe [] reduce (head args)
        reduce UNIVERSAL = []
        reduce EMPTY = []

    collectStates :: (Maybe SegmentedPath) -> RoleIdentification -> PhaseThree (Array StateIdentifier)
    collectStates mpath r = collectRoles r >>= \roles -> case mpath of
      Nothing -> pure (StateIdentifier <<< unwrap <$> roles)
      Just p ->  pure (StateIdentifier <<< flip append p <<< flip append "$" <<< unwrap <$> roles)

    stateSpec2States :: StateSpecification -> PhaseThree (Array StateIdentifier)
    stateSpec2States spec = case spec of
      -- Execute the effect for all subjects in the context state.
      AST.ContextState ctx mpath -> pure [(StateIdentifier $ (maybe (unwrap ctx) (append (unwrap ctx)) mpath))]
      -- Execute the effect for each qualifiedUser in each of the subject states.
      -- Note that the subjects in whose states we execute, do not need be the qualified users we execute for:
      -- 'do this automatically for me when you are at home' illustrates this independence.
      AST.SubjectState ridentification mpath -> collectStates mpath ridentification
      -- Execute the effect for all subjects in all object states.
      AST.ObjectState ridentification mpath -> collectStates mpath ridentification

    addAll :: forall key value. Ord key => value -> Map.Map key value -> Array key -> Map.Map key value
    addAll value = foldr (\key map -> Map.insert key value map)

    -- Compile the object of a StateQualifiedPart to a sequence of
    -- a runtime variable binding of "currentcontext" (that can be referred in the
    -- expression that defines the object) and the object expression itself.
    -- This function leaves the compile time environment as it is.
    objectToQueryFunctionDescription :: RoleIdentification -> Domain -> PhaseThree QueryFunctionDescription
    objectToQueryFunctionDescription syntacticObject currentDomain = do
      (syntacticObjectWithEnvironment :: Step) <- addContextualVariablesToExpression (roleIdentification2Step syntacticObject) Nothing
        -- Make a QueryFunctionDescription of a function that computes the object.
      withFrame
        (compileExpression currentDomain syntacticObjectWithEnvironment)

    roleIdentification2Step :: RoleIdentification -> Step
    roleIdentification2Step (ExplicitRole ctxt (EnumeratedRoleType rt) pos) = Simple $ ArcIdentifier pos rt
    roleIdentification2Step (ImplicitRole ctxt stp) = stp

    handlePart :: Partial => AST.StateQualifiedPart -> PhaseThree Unit

    -- Compiles and distributes all expressions in the message.
    handlePart (AST.N (AST.NotificationE{user, transition, message, object:syntacticObject, start, end})) = do
      -- Take the context from the state specification. If we have context state, it must be the context that
      -- we compute the object in. If it is either subject- or object state, their RoleSpecification contains the
      -- context type we compute the subject or object from.
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType $ transition2stateSpec transition)
      -- Add "currentcontext" in a Let if it is used in the syntacticObject.
      (syntacticObjectWithEnvironment :: Maybe Step) <- traverse (flip addContextualVariablesToExpression Nothing) (roleIdentification2Step <$> syntacticObject)
        -- Make a QueryFunctionDescription of a function that computes the object.
      (compiledObject :: Maybe QueryFunctionDescription) <-
        withFrame
          (traverse (compileExpression currentDomain) syntacticObjectWithEnvironment)
      qualifiedUsers <- map ENR <$> collectRoles user
      states <- stateSpec2States (transition2stateSpec transition)
      -- Then compile the parts of the sentence, tacking each compiled part onto that sequence.
      -- The expressions in the Sentence are compiled with respect to the current context.
      compiledMessage <- compileSentence currentDomain message qualifiedUsers states
      modifyAllStates compiledMessage compiledObject qualifiedUsers states
      where
          modifyAllStates :: Sentence.Sentence -> Maybe QueryFunctionDescription -> Array RoleType -> Array StateIdentifier -> PhaseThree Unit
          modifyAllStates compiledMessage objectCalculation qualifiedUsers states = for_ states
            \stateId -> modifyPartOfState
              start
              end
              (\(sr@{notifyOnEntry, notifyOnExit, object}) -> do
                object' <- case object of
                  -- No object in the state? Replace it by the calculation we've computed here.
                  Nothing -> pure objectCalculation
                  -- Otherwise, just keep what we have.
                  Just _ -> pure object
                case transition of
                  AST.Entry _ -> pure $ sr {notifyOnEntry = EncodableMap $ addAll compiledMessage (unwrap notifyOnEntry) qualifiedUsers, object = object'}
                  AST.Exit _ -> pure $ sr {notifyOnExit = EncodableMap $ addAll compiledMessage (unwrap notifyOnExit) qualifiedUsers, object = object'})
              stateId

          compileSentence :: Domain -> Sentence.Sentence -> Array RoleType -> Array StateIdentifier -> PhaseThree Sentence.Sentence
          compileSentence currentDomain (Sentence.Sentence parts) qualifiedUsers' states = Sentence.Sentence <$> traverse compilePart parts
            where
              compilePart :: Sentence.SentencePart -> PhaseThree Sentence.SentencePart
              compilePart hr@(Sentence.HR _) = pure hr
              compilePart cp@(Sentence.CP (Q _)) = pure cp
              compilePart (Sentence.CP (S stp)) = do
                expressionWithEnvironment <- addContextualVariablesToExpression stp (roleIdentification2Step <$> syntacticObject)
                compiledPart <- withFrame (compileAndDistributeStep
                  currentDomain
                  expressionWithEnvironment
                  qualifiedUsers'
                  states
                  )
                pure (Sentence.CP (Q compiledPart))

    -- Compiles and distributes all expressions in the automatic effect.
    handlePart (AST.AE (AST.AutomaticEffectE{subject, object: syntacticObject, transition, effect, start, end})) = do
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType $ transition2stateSpec transition)
      -- Add "currentcontext" in a Let if it is used in the syntacticObject.
      (syntacticObjectWithEnvironment :: Maybe Step) <- traverse (flip addContextualVariablesToExpression Nothing) (roleIdentification2Step <$> syntacticObject)
        -- Make a QueryFunctionDescription of a function that computes the object.
      (compiledObject :: Maybe QueryFunctionDescription) <-
        withFrame
          (traverse (compileExpression currentDomain) syntacticObjectWithEnvironment)
      qualifiedUsers <- map ENR <$> collectRoles subject
      -- Compile the side effect. Will invert all expressions in the statements, too, including
      -- the object if it is referenced.
      states <- stateSpec2States (transition2stateSpec transition)
      (sideEffect :: QueryFunctionDescription) <- compileStatement
        states
        currentDomain
        compiledObject
        qualifiedUsers
        effect
      modifyAllStates compiledObject sideEffect qualifiedUsers states
      where
        modifyAllStates :: Maybe QueryFunctionDescription -> QueryFunctionDescription -> Array RoleType -> Array StateIdentifier -> PhaseThree Unit
        modifyAllStates objectCalculation sideEffect qualifiedUsers states = for_ states
          (modifyPartOfState start end
            \(sr@{automaticOnEntry, automaticOnExit, object}) -> do
              object' <- case object of
                -- No object in the state? Replace it by the calculation we've computed here.
                Nothing -> pure objectCalculation
                -- Otherwise, just keep what we have.
                Just _ -> pure object
              case transition of
                AST.Entry _ -> pure $ sr {automaticOnEntry = EncodableMap $ addAll sideEffect (unwrap automaticOnEntry) qualifiedUsers, object = object'}
                AST.Exit _ -> pure $ sr {automaticOnExit = EncodableMap $ addAll sideEffect (unwrap automaticOnExit) qualifiedUsers, object = object'})

    handlePart (AST.R (AST.RoleVerbE{subject, object, state, roleVerbs:rv, start})) = do
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType state)
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- objectToQueryFunctionDescription object currentDomain
      -- ... for these states only...
      states <- stateSpec2States state
      -- ... the role verbs.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd states
      where
        modifyAllSubjectPerspectives :: Array EnumeratedRoleType -> QueryFunctionDescription -> Array StateIdentifier -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd states = for_ qualifiedUsers
          (modifyPerspective objectQfd start
            (\(Perspective pr@{roleVerbs}) -> Perspective pr {roleVerbs = EncodableMap $ addAll rv (unwrap roleVerbs) states}))

    handlePart (AST.AC (AST.ActionE{id, subject, object, state, effect, start})) = do
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType state)
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- objectToQueryFunctionDescription object currentDomain
      -- ... for these states only...
      -- Note that the subjects in whose states we execute, do not need be the qualified users we give access to the Action:
      -- 'I can check your heart beat when you are at home' illustrates this independence.
      states <- stateSpec2States state
      -- ... this action.
      -- (Compile the side effect. Will invert all expressions in the statements, too, including the
      -- object if it is referenced)
      (theAction :: QueryFunctionDescription) <- compileStatement
        states
        currentDomain
        (Just objectQfd)
        (ENR <$> qualifiedUsers)
        effect
      modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction states
      where
        -- Add the action for all users to their perspective on the object in all states.
        modifyAllSubjectPerspectives :: Array EnumeratedRoleType -> QueryFunctionDescription -> QueryFunctionDescription -> Array StateIdentifier -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction states = for_ qualifiedUsers
          (modifyPerspective
            objectQfd
            start
            \(Perspective pr@{actions}) -> Perspective $ pr {actions = EncodableMap (foldr
              (\stateId actionsMap -> case Map.lookup stateId actionsMap of
                Nothing -> Map.insert stateId (singleton id theAction) actionsMap
                Just actionsInState -> Map.insert stateId (insert id theAction actionsInState) actionsMap)
              (unwrap actions)
              states)})

    handlePart (AST.P (AST.PropertyVerbE{subject, object, state, propertyVerbs, propsOrView, start})) = do
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType state)
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- objectToQueryFunctionDescription object currentDomain
      propertyTypes <- constructPropertyVerbs propsOrView
      (propertyVerbs' :: PropertyVerbs) <- pure $ PropertyVerbs propertyTypes (fromFoldable propertyVerbs)
      -- ... for these states only...
      states <- stateSpec2States state
      -- ... the action.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' states
      where
        modifyAllSubjectPerspectives :: Array EnumeratedRoleType -> QueryFunctionDescription -> PropertyVerbs -> Array StateIdentifier -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' states = for_ qualifiedUsers
          (modifyPerspective
            objectQfd
            start
            \(Perspective pr@{propertyVerbs:pverbs}) -> Perspective $ pr {propertyVerbs = EncodableMap (foldr
              (\stateId pverbsMap -> case Map.lookup stateId pverbsMap of
                Nothing -> Map.insert stateId [propertyVerbs'] pverbsMap
                Just propertyVerbsArray -> Map.insert stateId (cons propertyVerbs' propertyVerbsArray) pverbsMap)
              (unwrap pverbs)
              states)})

        constructPropertyVerbs :: AST.PropsOrView -> PhaseThree (ExplicitSet PropertyType)
        constructPropertyVerbs AST.AllProperties = pure Universal
        constructPropertyVerbs (AST.Properties ps) =
          -- The (partial) names for properties used here may be defined outside
          -- of the model (due to role filling). Hence we postpone looking up their
          -- real referents to phase three. Here we assume an Enumerated PropertyType.
          pure $ PSet (ENP <<< EnumeratedPropertyType <$> (fromFoldable ps))
        constructPropertyVerbs (AST.View view) = do
          (views :: Object View) <- getsDF _.views
          -- As we have postponed handling these parse tree fragments after
          -- handling all others, there can be no forward references.
          -- Notice that the property references may not be fully qualified!
          candidates <- pure $ filter (flip endsWithSegments view) (keys views)
          case length candidates of
            0 -> throwError $ UnknownView start view
            1 -> unsafePartial case lookup (unsafePartial ARRP.head candidates) views of
              Just (View {propertyReferences}) -> pure $ PSet propertyReferences
            _ -> throwError $ NotUniquelyIdentifying start view candidates

    -- Apply, for this user, the modifier to his perspective on the object (and create a perspective if necessary).
    modifyPerspective :: QueryFunctionDescription -> ArcPosition -> (Perspective -> Perspective) -> EnumeratedRoleType -> PhaseThree Unit
    modifyPerspective objectQfd start modifier (EnumeratedRoleType qualifiedSubject) = do
      -- The user role
      mUserRole <- getsDF ((lookup qualifiedSubject) <<< _.enumeratedRoles)
      case mUserRole of
        Nothing -> throwError $ UnknownRole start qualifiedSubject
        Just (EnumeratedRole er@{perspectives}) -> do
          -- The perspective on the object
          mi <- pure $ findIndex (\(Perspective{object}) -> object == objectQfd) perspectives
          perspective <- case mi of
            -- TODO. Invert objectQfd hier.
            Nothing -> pure $ Perspective
              { object: objectQfd
              , roleVerbs: EncodableMap Map.empty
              , propertyVerbs: EncodableMap Map.empty
              , actions: EncodableMap Map.empty
              }
            Just i -> pure (unsafePartial $ fromJust $ index perspectives i)
          -- Apply the modifier, save the changed or created perspective with the user and save in the DomeinFile.
          modifyDF \dfr@{enumeratedRoles} -> dfr {enumeratedRoles = insert
            qualifiedSubject
            (EnumeratedRole $ er {perspectives = case mi of
              Nothing -> cons (modifier perspective) perspectives
              Just i -> unsafePartial $ fromJust $ updateAt i (modifier perspective) perspectives })
            enumeratedRoles
            }
      pure unit

    modifyPartOfState :: ArcPosition -> ArcPosition -> (StateRecord -> PhaseThree StateRecord) -> StateIdentifier -> PhaseThree Unit
    modifyPartOfState start end modifyState stateId = do
      -- log ("Will modify state " <> show stateId)
      mstate <- State.gets _.dfr >>= pure <<< lookup (unwrap stateId) <<< _.states
      case mstate of
        Nothing -> isContextRootState stateId >>= if _
          then do
            state' <- State <$> modifyState (unwrap $ constructState stateId (Simple (Value (ArcPosition {column: 0, line: 0}) PBool "true")) (Cnt (ContextType (unwrap stateId))) Nil)
            modifyDF \drf@{states} -> drf {states = insert (unwrap stateId) state' states}
          else isRoleRootState stateId >>= if _
            then do
              state' <- State <$> modifyState (unwrap $ constructState stateId (Simple (Value (ArcPosition {column: 0, line: 0}) PBool "true")) (Rle (EnumeratedRoleType (unwrap stateId))) Nil)
              modifyDF \drf@{states} -> drf {states = insert (unwrap stateId) state' states}
            else throwError $ StateDoesNotExist stateId start end
        Just (State sr) -> do
          -- modify the state
          state' <- State <$> (modifyState sr)
          modifyDF \dfr@{states} -> dfr {states = insert (unwrap stateId) state' states}
      pure unit
      where
        isContextRootState :: StateIdentifier -> PhaseThree Boolean
        isContextRootState (StateIdentifier s) = State.gets _.dfr >>= pure <<< isJust <<< lookup s <<< _.contexts

        isRoleRootState :: StateIdentifier -> PhaseThree Boolean
        isRoleRootState (StateIdentifier s) = State.gets _.dfr >>= pure <<< isJust <<< lookup s <<< _.enumeratedRoles

invertPerspectiveObjects :: PhaseThree Unit
invertPerspectiveObjects = do
  df@{_id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    _id
    (DomeinFile df)
    (addInvertedQueries' df _id)
  where
    addInvertedQueries' :: DomeinFileRecord -> Namespace -> PhaseThree Unit
    addInvertedQueries' {enumeratedRoles, calculatedRoles} ns = do
      traverse_ perspectivesInEnumeratedRole enumeratedRoles
      traverse_ perspectivesInCalculatedRole calculatedRoles

      where
        perspectivesInEnumeratedRole :: EnumeratedRole -> PhaseThree Unit
        perspectivesInEnumeratedRole (EnumeratedRole{_id, perspectives}) = for_ perspectives (addInvertedQueriesForPerspectiveObject (ENR _id))

        perspectivesInCalculatedRole :: CalculatedRole -> PhaseThree Unit
        perspectivesInCalculatedRole (CalculatedRole{_id, perspectives}) = for_ perspectives (addInvertedQueriesForPerspectiveObject (CR _id))

        addInvertedQueriesForPerspectiveObject :: RoleType -> Perspective -> PhaseThree Unit
        addInvertedQueriesForPerspectiveObject roleType p@(Perspective {object, propertyVerbs}) = do
          -- Sets the inverted queries directly in the EnumeratedRoles and Properties in the
          -- DomeinFile we keep in PhaseTwoState.
          sPerProp <- lift2 $ statesPerProperty p
          setInvertedQueries [roleType] sPerProp (roleStates p) object

        explicitSet2RelevantProperties :: ExplicitSet PropertyType -> RelevantProperties
        explicitSet2RelevantProperties Universal = All
        explicitSet2RelevantProperties Empty = Properties []
        explicitSet2RelevantProperties (PSet ps) = Properties ps

-- | For each State, compile the query, the object (if any) and the SideEffects.
-- | All names are qualified in the process. Notice that all other names are qualified, by now:
-- |  * object
-- |  * binding of role definitions
-- |  * references of properties (in views)
-- |  * references to views
-- |  * the type of value that is returned from a computed role
-- | This means we can look for the qualified version of a local name using the functions in
-- | Perspectives.Types.ObjectGetters, as long as we make sure the model under construction is in the DomainCache.
compileStates :: PhaseThree Unit
compileStates = do
  df@{_id} <- lift $ State.gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (compileStates' df)
  where
    compileStates' :: DomeinFileRecord -> PhaseThree Unit
    compileStates' {states, enumeratedRoles} = do
      compStates <- traverse compileState states
      modifyDF \dfr -> dfr {states = compStates}
      where
        compileState :: State -> PhaseThree State
        compileState s@(State{stateFulObject}) = case stateFulObject of
          Cnt context -> compileContextState context s
          Rle role -> compileRoleState role s

        compileRoleState :: EnumeratedRoleType -> State -> PhaseThree State
        compileRoleState roleType state@(State ar@{id:stateIdentifier, query, notifyOnEntry, notifyOnExit, automaticOnEntry, automaticOnExit}) = do
          currentDomain <- pure (RDOM $ ST roleType)
          queryDescription <- case query of
            Q d -> pure d
            -- We have to do this for all user role types in the notifyOnEntry, notifyOnExit, automaticOnEntry
            -- and automaticOnExit members.
            S stp -> do
              -- A State query only has to return the ContextInstance or RoleInstance that the state query is executed on.
              expressionWithEnvironment <- addContextualVariablesToExpression stp Nothing
              compileAndDistributeStep currentDomain expressionWithEnvironment [] [stateIdentifier]
          pure $ State ar { query = Q queryDescription }

        compileContextState :: ContextType -> State -> PhaseThree State
        compileContextState context state@(State ar@{id:stateIdentifier, query, object, notifyOnEntry, notifyOnExit, automaticOnEntry, automaticOnExit}) = withFrame $ do
          currentDomain <- pure (CDOM $ ST context)
          queryDescription <- case query of
            Q d -> pure d
            -- We have to do this for all user role types in the notifyOnEntry, notifyOnExit, automaticOnEntry
            -- and automaticOnExit members.
            S stp -> do
              -- A State query only has to return the ContextInstance or RoleInstance that the state query is executed on.
              expressionWithEnvironment <- addContextualVariablesToExpression stp Nothing
              -- The empty list of user types implies deltas are not distributed; however, the inverted queries
              -- computed are used to trigger state change.
              compileAndDistributeStep currentDomain expressionWithEnvironment [] [stateIdentifier]
          pure $ State ar { query = Q queryDescription }

transition2stateSpec :: StateTransitionE -> StateSpecification
transition2stateSpec (Entry s) = s
transition2stateSpec (Exit s) = s

stateSpec2ContextType :: StateSpecification -> ContextType
stateSpec2ContextType (ContextState c _) = c
stateSpec2ContextType (SubjectState (ExplicitRole c _ _) _) = c
stateSpec2ContextType (SubjectState (ImplicitRole c _) _) = c
stateSpec2ContextType (ObjectState (ExplicitRole c _ _) _) = c
stateSpec2ContextType (ObjectState (ImplicitRole c _) _) = c
