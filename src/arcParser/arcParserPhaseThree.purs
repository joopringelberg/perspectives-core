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
import Data.Array (cons, filter, filterA, findIndex, foldM, foldr, fromFoldable, head, index, length, nub, null, reverse, uncons, updateAt)
import Data.Array.Partial (head) as ARRP
import Data.Char.Unicode (toLower)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.List (List)
import Data.Map (Map, keys, empty, singleton, filter, insert, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.CodeUnits (fromCharArray, uncons) as CU
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (logShow)
import Foreign.Object (Object, insert, keys, lookup, unions, values, singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((###=), MP, (###>))
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..), DomeinFileRecord, indexedContexts, indexedRoles)
import Perspectives.External.CoreModuleList (isExternalCoreModule)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs)
import Perspectives.Identifiers (Namespace, deconstructModelName, endsWithSegments, isQualifiedWithDomein)
import Perspectives.InvertedQuery (RelevantProperties(..))
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), NotificationE(..), PropertyVerbE(..), PropsOrView(..), RoleVerbE(..), StateQualifiedPart(..), StateTransitionE(..), StateSpecification(..)) as AST
import Perspectives.Parsing.Arc.AST (RoleIdentification(..), SegmentedPath, StateSpecification(..), StateTransitionE(..))
import Perspectives.Parsing.Arc.ContextualVariables (addContextualVariablesToExpression, addContextualVariablesToStatements)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..)) as AE
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..), VarBinding(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, getsDF, lift2, modifyDF, runPhaseTwo_', withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), AssignmentOperator(..), LetStep(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.ExpressionCompiler (addVarBindingToSequence, compileAndSaveProperty, compileAndSaveRole, compileStep, compileVarBinding, makeRoleGetter, makeSequence, qualifyLocalEnumeratedRoleName, qualifyReturnsClause)
import Perspectives.Query.Kinked (setInvertedQueries)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain, domain2roleType, firstOperand, functional, hasQueryFunction, mandatory, propertyOfRange, range, secondOperand, traverseQfd)
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Identifiable (identifier, identifier_)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getCalculatedProperty, getCalculatedRole, getEnumeratedProperty)
import Perspectives.Representation.Class.Property (range) as PT
import Perspectives.Representation.Class.Role (bindingOfRole, contextOfADT, hasNotMorePropertiesThan, lessThanOrEqualTo, roleADT)
import Perspectives.Representation.Class.Role (roleTypeIsFunctional) as ROLE
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.Sentence (Sentence(..), SentencePart(..)) as Sentence
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.State (State(..), StateFulObject(..), StateRecord)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType, ContextType, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedContextType, lookForUnqualifiedPropertyType, lookForUnqualifiedPropertyType_, lookForUnqualifiedRoleTypeOfADT, roleStates, statesPerProperty)
import Prelude (class Ord, Unit, append, bind, discard, flip, identity, join, map, pure, unit, void, ($), (<$>), (<*), (<*>), (<<<), (<>), (==), (>=>), (>>=))

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
      compileExpressions
      handlePostponedStateQualifiedParts
      requalifyBindingsToCalculatedRoles
      qualifyPropertyReferences
      addInvertedQueries
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
compileExpressions :: PhaseThree Unit
compileExpressions = do
  df@{_id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    _id
    (DomeinFile df)
    (compileExpressions' df _id)
  where
    compileExpressions' :: DomeinFileRecord -> Namespace -> PhaseThree Unit
    compileExpressions' {_id,calculatedRoles, calculatedProperties, states, enumeratedRoles} ns = do
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
    collectRoles (ImplicitRole ctxt s) = compileStep (CDOM (ST ctxt)) s >>= \qfd -> case range qfd of
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


    addAll :: forall key value. Ord key => value -> Map.Map key value -> Array key -> Map.Map key value
    addAll value = foldr (\key map -> Map.insert key value map)

    -- TODO in alle combinaties van runtime variabele bindingen en andere expressies hieronder: maak er een let van
    -- zodat de variabelen telkens hersteld worden.

    -- Compile the object of a StateQualifiedPart to a sequence of
    -- a runtime variable binding of "currentcontext" (that can be referred in the
    -- expression that defines the object) and the object expression itself.
    -- This function leaves the compile time environment as it is.
    objectToQueryFunctionDescription :: RoleIdentification -> PhaseThree QueryFunctionDescription
    objectToQueryFunctionDescription (ExplicitRole ctxt rt pos) =
      -- No need for compile- or runtime binding of "currentcontext" here.
      makeRoleGetter (CDOM $ ST ctxt) (ENR rt)

    objectToQueryFunctionDescription (ImplicitRole ctxt stp) = withFrame do
      varb <- compileVarBinding (CDOM (ST ctxt)) (VarBinding "currentcontext" (Simple $ AE.Identity (startOf stp)))
      currentDomain <- pure (CDOM $ ST ctxt)
      -- Add "currentcontext" in this frame before compiling the object expression.
      addBinding "currentcontext" (SQD (currentDomain) (QF.VariableLookup "currentcontext") currentDomain True True)
      compiledCalculation <- compileStep (CDOM (ST ctxt)) stp >>= traverseQfd (qualifyReturnsClause (startOf stp))
      -- Put the variable binding and the compiled step in a sequence.
      pure $ makeSequence varb compiledCalculation

    addContextualVariablesToRoleIdentification :: RoleIdentification -> PhaseThree Step
    addContextualVariablesToRoleIdentification (ExplicitRole ctxt (EnumeratedRoleType rt) pos) = pure $ (Simple $ ArcIdentifier pos rt)
    addContextualVariablesToRoleIdentification (ImplicitRole ctxt stp) = addContextualVariablesToExpression stp Nothing

    roleIdentification2Step :: RoleIdentification -> Step
    roleIdentification2Step (ExplicitRole ctxt (EnumeratedRoleType rt) pos) = Simple $ ArcIdentifier pos rt
    roleIdentification2Step (ImplicitRole ctxt stp) = stp

    -- Set compile time variables for "currentcontext" and "object"
    addCompileTimeVars :: Domain -> Maybe QueryFunctionDescription -> PhaseThree Unit
    addCompileTimeVars currentDomain mcompiledObject = do
      -- Add a binding for "currentcontext" to the compile time environment.
      addBinding "currentcontext" (SQD currentDomain (QF.VariableLookup "currentcontext") currentDomain True True)
      case mcompiledObject of
        Just compiledObject -> addBinding "object" (SQD (domain compiledObject) (QF.VariableLookup "object") (range compiledObject) (functional compiledObject) (mandatory compiledObject))
        _ -> pure unit

    -- Create a QueryFunctionDescription that will make the current context available in "currentcontext" in runtime.
    compileRuntimeContextBinding :: Domain -> ArcPosition -> PhaseThree QueryFunctionDescription
    compileRuntimeContextBinding currentDomain pos = compileVarBinding currentDomain (VarBinding "currentcontext" (Simple $ AE.Identity pos))

    handlePart :: Partial => AST.StateQualifiedPart -> PhaseThree Unit
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
          (traverse (compileStep currentDomain) syntacticObjectWithEnvironment)
      -- Then compile the parts of the sentence, tacking each compiled part onto that sequence.
      -- The expressions in the Sentence are compiled with respect to the current context.
      compiledMessage <- compileSentence currentDomain message

      qualifiedUsers <- map ENR <$> collectRoles user
      case transition2stateSpec transition of
        -- Notify all subjects in the context state.
        AST.ContextState ctx mpath -> modifyAllStates compiledMessage compiledObject qualifiedUsers [(StateIdentifier $ (maybe (unwrap ctx) (append (unwrap ctx)) mpath))]
        -- Notify each qualifiedUser in each of the subject states.
        -- Note that the subjects in whose states we notify, do not need be the qualified users:
        -- 'warn me when you are at home' illustrates this independence.
        AST.SubjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllStates compiledMessage compiledObject qualifiedUsers
        -- Notify all subjects in all object states.
        AST.ObjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllStates compiledMessage compiledObject qualifiedUsers
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

          compileSentence :: Domain -> Sentence.Sentence -> PhaseThree Sentence.Sentence
          compileSentence currentDomain (Sentence.Sentence parts) = Sentence.Sentence <$> traverse compilePart parts
            where
              compilePart :: Sentence.SentencePart -> PhaseThree Sentence.SentencePart
              compilePart hr@(Sentence.HR _) = pure hr
              compilePart cp@(Sentence.CP (Q _)) = pure cp
              compilePart (Sentence.CP (S stp)) = do
                -- compiledPart <- makeSequence <$> pure vars <*> compileStep currentDomain stp
                expressionWithEnvironment <- addContextualVariablesToExpression stp (roleIdentification2Step <$> syntacticObject)
                compiledPart <- withFrame (compileStep currentDomain expressionWithEnvironment)
                pure (Sentence.CP (Q compiledPart))

    handlePart (AST.AE (AST.AutomaticEffectE{subject, object: syntacticObject, transition, effect, start, end})) = do
      -- logShow transition
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType $ transition2stateSpec transition)
      -- Add "currentcontext" in a Let if it is used in the syntacticObject.
      (syntacticObjectWithEnvironment :: Maybe Step) <- traverse (flip addContextualVariablesToExpression Nothing) (roleIdentification2Step <$> syntacticObject)
        -- Make a QueryFunctionDescription of a function that computes the object.
      (compiledObject :: Maybe QueryFunctionDescription) <-
        withFrame
          (traverse (compileStep currentDomain) syntacticObjectWithEnvironment)
      -- Compile the side effect.
      (sideEffect :: QueryFunctionDescription) <- case effect of
        Left assignments -> pure $ A (fromFoldable assignments)
        Right letstep -> addContextualVariablesToStatements letstep syntacticObjectWithEnvironment >>=
          compileLetStep_ currentDomain
      qualifiedUsers <- map ENR <$> collectRoles subject
      case transition2stateSpec transition of
        -- Execute the effect for all subjects in the context state.
        AST.ContextState ctx mpath -> modifyAllStates compiledObject sideEffect qualifiedUsers [(StateIdentifier $ (maybe (unwrap ctx) (append (unwrap ctx)) mpath))]
        -- Execute the effect for each qualifiedUser in each of the subject states.
        -- Note that the subjects in whose states we execute, do not need be the qualified users we execute for:
        -- 'do this automatically for me when you are at home' illustrates this independence.
        AST.SubjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllStates compiledObject sideEffect qualifiedUsers
        -- Execute the effect for all subjects in all object states.
        AST.ObjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllStates compiledObject sideEffect qualifiedUsers
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
                AST.Entry _ -> pure $ sr {automaticOnEntry = EncodableMap $ addAll (EF sideEffect) (unwrap automaticOnEntry) qualifiedUsers, object = object'}
                AST.Exit _ -> pure $ sr {automaticOnExit = EncodableMap $ addAll (EF sideEffect) (unwrap automaticOnExit) qualifiedUsers, object = object'})

    handlePart (AST.R (AST.RoleVerbE{subject, object, state, roleVerbs:rv, start})) = do
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- objectToQueryFunctionDescription object
      case state of
        -- ... the role verbs, but only only in this context state:
        AST.ContextState ctx mpath -> modifyAllSubjectPerspectives qualifiedUsers objectQfd [(StateIdentifier $ (maybe (unwrap ctx) (append (unwrap ctx)) mpath))]
        -- ... the role verbs, for all these subject states:
        -- Note that the subjects in whose states we execute, do not need be the qualified users we execute for:
        -- 'I can check your heart beat when you are at home' illustrates this independence.
        AST.SubjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllSubjectPerspectives qualifiedUsers objectQfd
        -- ... the role verbs, for these object states.
        AST.ObjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllSubjectPerspectives qualifiedUsers objectQfd
      where
        modifyAllSubjectPerspectives :: Array EnumeratedRoleType -> QueryFunctionDescription -> Array StateIdentifier -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd states = for_ qualifiedUsers
          (modifyPerspective objectQfd start
            (\(Perspective pr@{roleVerbs}) -> Perspective pr {roleVerbs = EncodableMap $ addAll rv (unwrap roleVerbs) states}))

    handlePart (AST.AC (AST.ActionE{id, subject, object, state, effect, start})) = do
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- objectToQueryFunctionDescription object
      -- ... this action ...
      theAction <- case effect of
        Left assignments -> pure $ A (fromFoldable assignments)
        Right letstep -> pure $ L letstep
      -- TODO. Compileer de actie!
      case state of
        -- ... but only only in this context state:
        AST.ContextState ctx mpath -> modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction [(StateIdentifier $ (maybe (unwrap ctx) (append (unwrap ctx)) mpath))]
        -- ... for all these subject states:
        -- Note that the subjects in whose states we execute, do not need be the qualified users we give access to the Action:
        -- 'I can check your heart beat when you are at home' illustrates this independence.
        AST.SubjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction
        -- ... for these object states.
        AST.ObjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction
      where
        -- Add the action for all users to their perspective on the object in all states.
        modifyAllSubjectPerspectives :: Array EnumeratedRoleType -> QueryFunctionDescription -> SideEffect -> Array StateIdentifier -> PhaseThree Unit
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
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- objectToQueryFunctionDescription object
      propertyTypes <- constructPropertyVerbs propsOrView
      (propertyVerbs' :: PropertyVerbs) <- pure $ PropertyVerbs propertyTypes (fromFoldable propertyVerbs)
      case state of
        -- ... the action, but only only in this context state:
        AST.ContextState ctx mpath -> modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' [(StateIdentifier $ (maybe (unwrap ctx) ((append (unwrap ctx)) <<< (append "$")) mpath))]
        -- ... the action, for all these subject states:
        -- Note that the subjects in whose states we execute, do not need be the qualified users we give access to the Action:
        -- 'I can check your heart beat when you are at home' illustrates this independence.
        AST.SubjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs'
        -- ... the action, for these object states.
        AST.ObjectState ridentification mpath -> collectStates mpath ridentification >>= modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs'
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
      mstate <- State.gets _.dfr >>= pure <<< lookup (unwrap stateId) <<< _.states
      case mstate of
        Nothing -> throwError $ StateDoesNotExist stateId start end
        Just (State sr) -> do
          -- modify the state
          state' <- State <$> (modifyState sr)
          modifyDF \dfr@{states} -> dfr {states = insert (unwrap stateId) state' states}
      pure unit

addInvertedQueries :: PhaseThree Unit
addInvertedQueries = do
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
          setInvertedQueries (Just roleType) sPerProp (roleStates p) object

        explicitSet2RelevantProperties :: ExplicitSet PropertyType -> RelevantProperties
        explicitSet2RelevantProperties Universal = All
        explicitSet2RelevantProperties Empty = Properties []
        explicitSet2RelevantProperties (PSet ps) = Properties ps

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
  Maybe RoleType ->
  StateIdentifier ->
  PhaseThree QueryFunctionDescription
compileAndDistributeStep dom stp user stateIdentifier = do
  descr' <- compileStep dom stp
  descr <- traverseQfd (qualifyReturnsClause (startOf stp)) descr'
  -- The description may be a path and then should be seen as an implicit perspective on its results, like a CalculatedProperty (it could also be a constant, or it could result in a ContextInstance or a RoleInstance).
  -- Hence we should create a Map of the PropertyType and the StateIdentifier.
  (statesPerProperty :: Map.Map PropertyType (Array StateIdentifier)) <- pure case propertyOfRange descr of
    Nothing -> Map.empty
    Just p -> Map.singleton p [stateIdentifier]
  setInvertedQueries user statesPerProperty [stateIdentifier] descr
  pure descr

-- | For each State, compile the query, the object (if any) and the SideEffects.
-- | All names are qualified in the process. Notice that all other names are qualified, by now:
-- |  * object
-- |  * binding of role definitions
-- |  * references of properties (in views)
-- |  * references to views
-- |  * the type of value that is returned from a computed role
-- | This means we can look for the qualified version of a local name using the functions in
-- | Perspectives.Types.ObjectGetters, as long as we make sure the model under construction is in the DomainCache.
-- | Each expression (in statements) is handled with compileAndDistributeStep, hence the inverted queries are set as well.
-- TODO: Controleer of het type argument van de assignment operatoren wel hetzelfde zijn als het type van het object van de Actie.
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
        compileRoleState roleType state@(State ar@{id:stateIdentifier, query, notifyOnEntry, notifyOnExit, automaticOnEntry, automaticOnExit}) =
          withFrame
            do
              currentDomain <- pure (RDOM $ ST roleType)
              -- add declaraton for object. Replace object expr with
              -- lookup in the runtime environment.
              addBinding "object" (SQD currentDomain (QF.VariableLookup "object") currentDomain True True)
              queryDescription <- case query of
                Q d -> pure d
                -- We have to do this for all user role types in the notifyOnEntry, notifyOnExit, automaticOnEntry
                -- and automaticOnExit members.
                S stp -> do
                  -- A State query only has to return the ContextInstance or RoleInstance that the state query is executed on.
                  expressionWithEnvironment <- addContextualVariablesToExpression stp Nothing
                  compileAndDistributeStep currentDomain expressionWithEnvironment Nothing stateIdentifier
              -- Add inverted queries for the object calculation.
              setInvertedQueriesForStateObject state
              -- Now for the entries in automaticOnEntry and automaticOnExit, compile the SideEffects.
              automaticOnEntry' <- traverseWithIndex (compileSideEffect stateIdentifier currentDomain Nothing) (unwrap automaticOnEntry)
              automaticOnExit' <- traverseWithIndex (compileSideEffect stateIdentifier currentDomain Nothing) (unwrap automaticOnExit)
              pure $ State ar
                { query = Q queryDescription
                , automaticOnEntry = EncodableMap automaticOnEntry'
                , automaticOnExit = EncodableMap automaticOnExit'
                }

        -- Invert the object calculation. Does not change the compile time environment.
        setInvertedQueriesForStateObject :: State -> PhaseThree Unit
        setInvertedQueriesForStateObject (State ar@{id:stateIdentifier, object, automaticOnEntry, automaticOnExit}) = case object of
          Nothing -> pure unit
          Just currentContextAndObject -> withFrame $
            case secondOperand currentContextAndObject of
              Nothing -> throwError $ Custom "Programming error (compileContextState): object in state should be compiled to sequence of varbinding and the query for the object. This happens in handlePostponedStateQualifiedParts"
              Just objectCalculation -> do
                -- Explanation.
                -- The calculation of object in state is the sequence of a statement
                --  * whose first operand is a binding that gives the "currentcontext"
                --    variable a value (the 'binding statement')
                --  * whose second operand is the computation of the object itself.
                -- The first operand of the binding statement itself is exactly the
                -- computation of the current context in runtime. We add that to the
                -- compile time environment here, because inverse query calculation
                -- needs to access it.
                addBinding "currentcontext" (unsafePartial fromJust $ firstOperand $ unsafePartial fromJust $ firstOperand currentContextAndObject)
                -- The subjects are all keys of the maps in automaticOnEntry and
                -- automaticOnExit with expressions that refer the object.
                subjects <- pure $ nub $ append
                  (fromFoldable $ Map.keys $ Map.filter (hasQueryFunction (QF.VariableLookup "object") <<< unsafePartial \(EF qfd') -> qfd') (unwrap automaticOnEntry))
                  (fromFoldable $ Map.keys $ Map.filter (hasQueryFunction (QF.VariableLookup "object") <<< unsafePartial \(EF qfd') -> qfd') (unwrap automaticOnExit))
                -- Set inverted queries for those subjects for the object calculation.
                for_ subjects \subject ->
                  setInvertedQueries (Just subject) Map.empty [stateIdentifier] objectCalculation
                pure unit

        compileContextState :: ContextType -> State -> PhaseThree State
        compileContextState context state@(State ar@{id:stateIdentifier, query, object, notifyOnEntry, notifyOnExit, automaticOnEntry, automaticOnExit}) = withFrame $ do
          currentDomain <- pure (CDOM $ ST context)
          -- add declaraton for currentcontext. Replace currentcontext expr with
          -- lookup in the runtime environment.
          addBinding "currentcontext" (SQD currentDomain (QF.VariableLookup "currentcontext") currentDomain True True)
          queryDescription <- case query of
            Q d -> pure d
            -- We have to do this for all user role types in the notifyOnEntry, notifyOnExit, automaticOnEntry
            -- and automaticOnExit members.
            S stp -> do
              varb <- compileVarBinding currentDomain (VarBinding "currentcontext" (Simple $ AE.Identity (startOf stp)))
              -- A State query only has to return the ContextInstance or RoleInstance that the state query is executed on.
              compiledCalculation <- compileAndDistributeStep currentDomain stp Nothing stateIdentifier
              pure $ makeSequence varb compiledCalculation
          -- Add inverted queries for the object calculation.
          setInvertedQueriesForStateObject state
          -- Now for the entries in automaticOnEntry and automaticOnExit, compile the SideEffects.
          -- The "object" variable should be available in the compile time environment
          -- when handling the statements in the side effects.
          case object of
            Nothing -> pure unit
            Just objectCalculation -> addBinding "object" (SQD (domain objectCalculation) (QF.VariableLookup "object") (range objectCalculation) (functional objectCalculation) (mandatory objectCalculation))

          automaticOnEntry' <- traverseWithIndex (compileSideEffect stateIdentifier currentDomain object) (unwrap automaticOnEntry)
          automaticOnExit' <- traverseWithIndex (compileSideEffect stateIdentifier currentDomain object) (unwrap automaticOnExit)
          pure $ State ar
            { query = Q queryDescription
            , automaticOnEntry = EncodableMap automaticOnEntry'
            , automaticOnExit = EncodableMap automaticOnExit'
            }

        compileSideEffect :: StateIdentifier -> Domain -> Maybe QueryFunctionDescription -> RoleType -> SideEffect -> PhaseThree SideEffect
        compileSideEffect stateIdentifier currentDomain mobjectCalculation' userRoleType effect =
          case effect of
            -- Compile a series of Assignments into a QueryDescription.
            A assignments -> EF <$> sequenceOfAssignments userRoleType (reverse assignments) mobjectCalculation'
              -- Compile the LetStep into a QueryDescription.
            L letstep -> do
              let_ <- compileLetStep_ letstep
              pure $ EF (UQD currentDomain QF.WithFrame let_ (range let_) (functional let_) (mandatory let_))
            EF qfd -> pure $ EF qfd
          where

          compileLetStep_ :: LetStep -> PhaseThree QueryFunctionDescription
          compileLetStep_ (LetStep {bindings, assignments}) = withFrame
            -- We have to reverse the bindings, because foldM associates the wrong way.
            case uncons (reverse bindings) of
              -- no bindings at all. Just the body. This will probably never occur as the parser breaks on it.
              Nothing -> sequenceOfAssignments userRoleType assignments mobjectCalculation'
              (Just {head: bnd, tail}) -> do
                -- compileVarBinding also adds a variable binding to the compile time environment.
                head_ <- compileVarBinding currentDomain bnd
                makeSequence <$> foldM addVarBindingToSequence head_ tail <*> sequenceOfAssignments userRoleType assignments mobjectCalculation'

          -- This will return a QueryFunctionDescription that describes either a single assignment, or
          -- a BQD with QueryFunction equal to (BinaryCombinator SequenceF)
          sequenceOfAssignments :: RoleType -> Array Assignment -> Maybe QueryFunctionDescription -> PhaseThree QueryFunctionDescription
          sequenceOfAssignments subject assignments' objectCalculation = sequenceOfAssignments_ assignments'
            where
              sequenceOfAssignments_ :: Array Assignment -> PhaseThree QueryFunctionDescription
              sequenceOfAssignments_ assignments = case uncons assignments of
                Nothing -> throwError $ Custom "There must be at least one assignment in a let*"
                (Just {head, tail}) -> do
                  head_ <- describeAssignmentStatement subject head objectCalculation
                  foldM addAssignmentToSequence head_ tail

              -- Returns a BQD with QueryFunction (BinaryCombinator SequenceF)
              addAssignmentToSequence :: QueryFunctionDescription -> Assignment -> PhaseThree QueryFunctionDescription
              addAssignmentToSequence seq v = makeSequence <$> pure seq <*> (describeAssignmentStatement subject v objectCalculation)

          -- we need the Object of the Perspective. Right now it is a RoleType, possibly a(n anonymous) CalculatedRole.
          -- The assignment functions arbitrarily return the currentContext. Hence,
          -- we declare the functions to be both functional and mandatory.
          -- All inverted queries that need be created are created in this function.
          -- TODO: Controleer of de assignment operators wel corresponderen met de toegekende Verbs.
          describeAssignmentStatement :: RoleType -> Assignment -> Maybe QueryFunctionDescription -> PhaseThree QueryFunctionDescription
          describeAssignmentStatement subject ass mobjectCalculation = case ass of
              Remove {roleExpression} -> do
                rle <- ensureRole subject roleExpression
                pure $ UQD currentDomain QF.Remove rle currentDomain True True
              CreateRole {roleIdentifier, contextExpression, start, end} -> do
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain (QF.DataTypeGetter QF.IdentityF) currentDomain True True)
                  (Just stp) -> ensureContext subject stp
                qualifiedRoleIdentifier <- qualifyWithRespectTo roleIdentifier cte start end
                pure $ UQD currentDomain (QF.CreateRole qualifiedRoleIdentifier) cte currentDomain True True

              CreateContext {contextTypeIdentifier, roleTypeIdentifier, contextExpression, start, end} -> do
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain (QF.DataTypeGetter QF.IdentityF) currentDomain True True)
                  (Just stp) -> ensureContext subject stp
                qualifiedContextTypeIdentifier <- qualifyContextTypeWithRespectTo contextTypeIdentifier cte start end
                (qualifiedRoleIdentifier :: EnumeratedRoleType) <- qualifyWithRespectTo roleTypeIdentifier cte start end
                pure $ UQD currentDomain (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) cte currentDomain True True

              CreateContext_ {contextTypeIdentifier, roleExpression, start, end} -> do
                cte <- pure $ (SQD currentDomain (QF.DataTypeGetter QF.IdentityF) currentDomain True True)
                roleQfd <- ensureRole subject roleExpression
                qualifiedContextTypeIdentifier <- qualifyContextTypeWithRespectTo contextTypeIdentifier cte start end
                pure $ UQD currentDomain (QF.CreateContext_ qualifiedContextTypeIdentifier) roleQfd currentDomain True True

              Move {roleExpression, contextExpression} -> do
                rle <- ensureRole subject roleExpression
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain (QF.DataTypeGetter QF.IdentityF) currentDomain True True)
                  (Just (stp :: Step)) -> ensureContext subject stp >>= ensureFunctional stp
                pure $ BQD currentDomain QF.Move rle cte currentDomain True True
              Bind f@{bindingExpression, roleIdentifier, contextExpression} -> do
                -- Bind <binding-expression> to <binderType> [in <context-expression>]. Check:
                -- bindingExpression should result in roles
                (bindings :: QueryFunctionDescription) <- ensureRole subject bindingExpression
                (cte :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain (QF.DataTypeGetter QF.IdentityF) currentDomain True True)
                  (Just (stp :: Step)) -> ensureContext subject stp
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
                  possibleBinding <- lift $ lift (bindingOfRole (ENR qualifiedRoleIdentifier))
                  bindings' <- pure (unsafePartial $ domain2roleType (range bindings))
                  lift2 $ possibleBinding `hasNotMorePropertiesThan` bindings'
                if qualifies
                  -- Create a function description that describes the actual role creating and binding.
                  then pure $ BQD currentDomain (QF.Bind qualifiedRoleIdentifier) bindings cte currentDomain True True
                  else throwError $ RoleDoesNotBind f.start (ENR qualifiedRoleIdentifier) (unsafePartial $ domain2roleType (range bindings))

              Bind_ {bindingExpression, binderExpression} -> do
                -- bindingExpression should result in a functional role
                (bindings :: QueryFunctionDescription) <- ensureRole subject  bindingExpression >>= ensureFunctional bindingExpression
                -- binderExpression should result in a functional role
                (binders :: QueryFunctionDescription) <- ensureRole subject binderExpression >>= ensureFunctional binderExpression
                -- Now create a function description.
                pure $ BQD currentDomain QF.Bind_ bindings binders currentDomain True True

              Unbind f@{bindingExpression, roleIdentifier} -> do
                (bindings :: QueryFunctionDescription) <- ensureRole subject  bindingExpression
                -- the type of the binder (indicated by roleIdentifier) should be an EnumeratedRoleType (local name should resolve w.r.t. the binders of the bindings). We try to resolve in the model and then filter candidates on whether they bind the bindings. If they don't, the expression has no meaning.
                (qualifiedRoleIdentifier :: Maybe EnumeratedRoleType) <- qualifyBinderType roleIdentifier (unsafePartial $ domain2roleType $ range bindings) f.start f.end
                pure $ UQD currentDomain (QF.Unbind qualifiedRoleIdentifier) bindings currentDomain True True

              Unbind_ {bindingExpression, binderExpression} -> do
                -- bindingExpression should result in a functional role
                (bindings :: QueryFunctionDescription) <- ensureRole subject  bindingExpression >>= ensureFunctional bindingExpression
                -- binderExpression should result in a functional role
                (binders :: QueryFunctionDescription) <- ensureRole subject binderExpression >>= ensureFunctional binderExpression
                -- Now create a function description.
                pure $ BQD currentDomain QF.Unbind_ bindings binders currentDomain True True

              DeleteRole f@{roleIdentifier, contextExpression} -> do
                (contextQfd :: QueryFunctionDescription) <- case contextExpression of
                  Nothing -> pure $ (SQD currentDomain (QF.DataTypeGetter QF.IdentityF) currentDomain True True)
                  (Just stp) -> ensureContext subject stp
                (qualifiedRoleIdentifier :: EnumeratedRoleType) <- qualifyWithRespectTo roleIdentifier contextQfd f.start f.end
                pure $ UQD currentDomain (QF.DeleteRole qualifiedRoleIdentifier) contextQfd currentDomain True True

              DeleteProperty f@{propertyIdentifier, roleExpression, start, end} -> do
                (roleQfd :: QueryFunctionDescription) <- case roleExpression of
                  Nothing -> case mobjectCalculation of
                    Nothing -> throwError $ MissingRoleForPropertyAssignment start end
                    Just objectCalculation -> pure objectCalculation
                  -- delete property PropertyType from <roleExpression>
                  Just e -> ensureRole subject e
                (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
                pure $ UQD currentDomain (QF.DeleteProperty qualifiedProperty) roleQfd currentDomain True True

              PropertyAssignment f@{propertyIdentifier, operator, valueExpression, roleExpression, start, end} -> do
                (roleQfd :: QueryFunctionDescription) <- case roleExpression of
                  Nothing -> case mobjectCalculation of
                    Nothing -> throwError $ MissingRoleForPropertyAssignment start end
                    Just objectCalculation -> pure objectCalculation
                  -- PropertyType =+ 10 for <roleExpression>
                  -- Note that currentDomain now is a RDOM.
                  -- currentDomain is used as starting point in ensureRole.
                  -- Hence this goes wrong.
                  Just e -> do
                    -- currentDomain is an RDOM by construction.
                    -- But we need to compile a querydescription for the roleExpression
                    -- with respect to its context (so we cannot use ensureRole here).
                    contextDomain <- case currentDomain of
                      RDOM adt -> lift2 $ contextOfADT adt
                      otherwise -> throwError $ NotARoleDomain currentDomain (startOf e) (endOf e)
                    qfd <- compileAndDistributeStep (CDOM contextDomain) e (Just subject) stateIdentifier
                    case range qfd of
                      (RDOM _) -> pure qfd
                      otherwise -> throwError $ NotARoleDomain (range qfd) (startOf e) (endOf e)

                (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
                -- Compile the value expression to a QueryFunctionDescription. Its range must comply with the range of the qualifiedProperty. It is compiled relative to the current context; not relative to the object!
                valueQfd <- compileAndDistributeStep currentDomain valueExpression (Just subject) stateIdentifier
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
              ExternalEffect f@{start, end, effectName, arguments} -> do
                case (deconstructModelName effectName) of
                  Nothing -> throwError (NotWellFormedName start effectName)
                  Just modelName -> if isExternalCoreModule modelName
                    then do
                      mexpectedNrOfArgs <- pure $ lookupHiddenFunctionNArgs effectName
                      case mexpectedNrOfArgs of
                        Nothing -> throwError (UnknownExternalFunction start end effectName)
                        Just expectedNrOfArgs -> if expectedNrOfArgs == length arguments
                          then do
                            -- The argument is an expression that can yield a ContextInstance, a RoleInstance or a Value.
                            -- If it yields a Value taken from some Property, then the subject has an implicit Perspective in this State on that PropertyType.
                            compiledArguments <- traverse (\s -> compileAndDistributeStep currentDomain s (Just subject) stateIdentifier) arguments
                            pure $ MQD currentDomain (QF.ExternalEffectFullFunction effectName) compiledArguments currentDomain Unknown Unknown
                          else throwError (WrongNumberOfArguments start end effectName expectedNrOfArgs (length arguments))
                    -- TODO: behandel hier Foreign functions.
                    else throwError (UnknownExternalFunction start end effectName)
              where
                mapName :: String -> String
                mapName s = case CU.uncons (replace (Pattern "$") (Replacement "_") (replace (Pattern "model:") (Replacement "") s)) of
                  (Just {head, tail}) -> CU.fromCharArray [toLower head] <> tail
                  Nothing -> s

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

                qualifyContextTypeWithRespectTo :: String -> QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree ContextType
                qualifyContextTypeWithRespectTo contextIdentifier contextFunctionDescription start end = do
                  (ct :: ADT ContextType) <- case range contextFunctionDescription of
                    (CDOM ct') -> pure ct'
                    otherwise -> throwError $ NotAContextDomain otherwise start end
                  mrt <- lift2 (ct ###> lookForUnqualifiedContextType contextIdentifier)
                  case mrt of
                    Just ctype -> pure ctype
                    -- TODO specialiseer de foutmelding!
                    otherwise -> throwError $ CannotFindContextType start end contextIdentifier

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

                -- | If the name is unqualified, look for an EnumeratedRole with matching local name in the Domain.
                -- | Then, we check whether a candidate's binding type equals the second argument, or is less specialised. In other words: whether the candidate could bind it (the second argument).
                qualifyBinderType :: Maybe String -> ADT EnumeratedRoleType -> ArcPosition -> ArcPosition -> PhaseThree (Maybe EnumeratedRoleType)
                qualifyBinderType Nothing _ _ _ = pure Nothing
                qualifyBinderType (Just ident) bindings start end = if isQualifiedWithDomein ident
                  then pure $ Just $ EnumeratedRoleType ident
                  else do
                    -- EnumeratedRoles in the model with (end)matching name.
                    (nameMatches :: Array EnumeratedRole) <- pure (filter (\(EnumeratedRole{_id:roleId}) -> (unwrap roleId) `endsWithSegments` ident) (values enumeratedRoles))
                    -- EnumeratedRoles that can bind `bindings`.
                    (candidates :: Array EnumeratedRole) <-(filterA (\(EnumeratedRole{binding}) -> lift2 $ lessThanOrEqualTo binding bindings) nameMatches)
                    case head candidates of
                      Nothing -> if null nameMatches
                        then throwError $ UnknownRole start ident
                        else throwError $ LocalRoleDoesNotBind start end ident bindings
                      (Just (EnumeratedRole {_id:candidate})) | length candidates == 1 -> pure $ Just candidate
                      otherwise -> throwError $ NotUniquelyIdentifying start ident (identifier_ <$> candidates)

                ensureContext :: RoleType -> Step -> PhaseThree QueryFunctionDescription
                ensureContext userType stp  = do
                  -- An expression that results in a ContextInstance, in this state, for this usertype.
                  qfd <- compileAndDistributeStep currentDomain stp (Just userType) stateIdentifier
                  case range qfd of
                    (CDOM _) -> pure qfd
                    otherwise -> throwError $ NotAContextDomain (range qfd) (startOf stp) (endOf stp)

                ensureRole :: RoleType -> Step -> PhaseThree QueryFunctionDescription
                ensureRole userType stp = do
                  -- An expression that results in a RoleInstance, in this state, for this usertype.
                  qfd <- compileAndDistributeStep currentDomain stp (Just userType) stateIdentifier
                  case range qfd of
                    (RDOM _) -> pure qfd
                    otherwise -> throwError $ NotARoleDomain (range qfd) (startOf stp) (endOf stp)

ensureFunctional :: Step -> QueryFunctionDescription -> PhaseThree QueryFunctionDescription
ensureFunctional stp qfd = case functional qfd of
  True -> pure qfd
  Unknown -> throwError $ MaybeNotFunctional (startOf stp) (endOf stp) stp
  False -> throwError $ NotFunctional (startOf stp) (endOf stp) stp

transition2stateSpec :: StateTransitionE -> StateSpecification
transition2stateSpec (Entry s) = s
transition2stateSpec (Exit s) = s

stateSpec2ContextType :: StateSpecification -> ContextType
stateSpec2ContextType (ContextState c _) = c
stateSpec2ContextType (SubjectState (ExplicitRole c _ _) _) = c
stateSpec2ContextType (SubjectState (ImplicitRole c _) _) = c
stateSpec2ContextType (ObjectState (ExplicitRole c _ _) _) = c
stateSpec2ContextType (ObjectState (ImplicitRole c _) _) = c

roleSpec2ContextType :: RoleIdentification -> ContextType
roleSpec2ContextType (ExplicitRole c _ _) = c
roleSpec2ContextType (ImplicitRole c _) = c
