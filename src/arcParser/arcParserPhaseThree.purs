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

import Control.Monad.Error.Class (catchError, try)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (gets) as State
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, elemIndex, filter, find, findIndex, foldM, foldl, foldr, fromFoldable, head, index, intercalate, length, nub, null, uncons, union, updateAt)
import Data.Array.Partial (head) as ARRP
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Identity as Identity
import Data.List (List, filter, filterM, fromFoldable) as LIST
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), indexOf)
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert, keys, lookup, singleton, unions)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~~>), MP, MonadPerspectives, forceTypeArray, (###=), (###>>), (###>))
import Perspectives.Data.EncodableMap (EncodableMap, empty, insert, lookup, keys, addAll) as EM
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (modifyEnumeratedRoleInDomeinFile, removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, UpstreamAutomaticEffect(..), UpstreamStateNotification(..), addUpstreamAutomaticEffect, addUpstreamNotification, indexedContexts, indexedRoles)
import Perspectives.Identifiers (Namespace, areLastSegmentsOf, concatenateSegments, isTypeUri, qualifyWith, startsWithSegments, typeUri2ModelUri_, typeUri2typeNameSpace)
import Perspectives.InvertedQuery (RelevantProperties(..))
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ColumnE(..), ContextActionE(..), FormE(..), MarkDownE, NotificationE(..), AuthorOnly(..), PropertyVerbE(..), PropsOrView(..), RoleVerbE(..), RowE(..), ScreenE(..), ScreenElement(..), SelfOnly(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..), TabE(..), TableE(..), WidgetCommonFields) as AST
import Perspectives.Parsing.Arc.AST (ChatE(..), MarkDownE(..), RoleIdentification(..), SegmentedPath, SentenceE(..), SentencePartE(..), StateTransitionE(..), roleIdentification2context)
import Perspectives.Parsing.Arc.AspectInference (inferFromAspectRoles)
import Perspectives.Parsing.Arc.CheckSynchronization (checkSynchronization) as SYNC
import Perspectives.Parsing.Arc.ContextualVariables (addContextualBindingsToExpression, addContextualBindingsToStatements, makeContextStep, makeIdentityStep, makeTypeTimeOnlyContextStep, makeTypeTimeOnlyRoleStep)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..), VarBinding)
import Perspectives.Parsing.Arc.PhaseThree.CheckPerspectivesModifiers (checkPerspectiveModifiers)
import Perspectives.Parsing.Arc.PhaseThree.PerspectiveContextualisation (addAspectsToExternalRoles, contextualisePerspectives)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (setInvertedQueries)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, lift2, modifyDF, runPhaseTwo_', throwError, withDomeinFile, withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition, arcParserStartPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.ExpressionCompiler (compileAndDistributeStep, compileAndSaveProperty, compileAndSaveRole, compileExpression, compileStep, qualifyLocalContextName, qualifyLocalEnumeratedRoleName, qualifyLocalRoleName)
import Perspectives.Query.Kinked (completeInversions)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain, domain2roleInContext, domain2roleType, functional, mandatory, range, replaceContext, roleInContext2Role, sumOfDomains, traverseQfd)
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Query.StatementCompiler (compileStatement)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, transform)
import Perspectives.Representation.Action (AutomaticAction(..), Action(..))
import Perspectives.Representation.CNF (toConjunctiveNormalForm)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getCalculatedProperty, getCalculatedRole, getContext, getEnumeratedRole, tryGetPerspectType)
import Perspectives.Representation.Class.Role (Role(..), allProperties, completeExpandedType, displayName, displayNameOfRoleType, getCalculation, getRole, getRoleType, perspectivesOfRoleType, roleADT, roleADTOfRoleType, roleTypeIsFunctional)
import Perspectives.Representation.Context (Context(..)) as CTXT
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..), addProperty, createModificationSummary, expandPropSet, expandVerbs, isMutatingVerbSet, perspectiveMustBeSynchronized, perspectiveSupportsPropertyForVerb, perspectiveSupportsRoleVerbs, stateSpec2StateIdentifier)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), ScreenMap, TabDef(..), TableDef(..), WidgetCommonFieldsDef)
import Perspectives.Representation.Sentence (Sentence(..)) as Sentence
import Perspectives.Representation.State (Notification(..), State(..), StateDependentPerspective(..), StateFulObject(..), StateRecord, constructState)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and, optimistic, pessimistic)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), ViewType(..), propertytype2string, roletype2string)
import Perspectives.Representation.UserGraph.Build (buildUserGraph)
import Perspectives.Representation.Verbs (PropertyVerb, roleVerbList2Verbs)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (actionStates, automaticStates, contextAspectsClosure, enumeratedRoleContextType, isPerspectiveOnSelf, lookForUnqualifiedPropertyType, lookForUnqualifiedPropertyType_, roleStates, statesPerProperty, string2RoleType)
import Perspectives.Utilities (prettyPrint)
import Prelude (Unit, append, bind, discard, eq, flip, map, not, pure, show, unit, void, ($), (&&), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=))

phaseThree ::
  DomeinFileRecord ->
  LIST.List AST.StateQualifiedPart ->
  LIST.List AST.ScreenE ->
  MP (Either MultiplePerspectivesErrors (Tuple DomeinFileRecord StoredQueries))
phaseThree df@{id} postponedParts screens = do
  -- Store the DomeinFile in cache. If a prefix for the domain is defined in the file,
  -- phaseThree_ will try to retrieve it.
  void $ storeDomeinFileInCache id (DomeinFile df)
  result <- phaseThree_ df postponedParts screens 
  removeDomeinFileFromCache id
  pure result

phaseThree_ ::
  DomeinFileRecord ->
  LIST.List AST.StateQualifiedPart ->
  LIST.List AST.ScreenE ->
  MP (Either MultiplePerspectivesErrors (Tuple DomeinFileRecord StoredQueries))
phaseThree_ df@{id, referredModels} postponedParts screens = do
  -- We don't expect an error on retrieving the DomeinFile, as we've only just put it into cache!
  indexedContexts <- unions <$> traverse (getDomeinFile >=> pure <<< indexedContexts) referredModels
  indexedRoles <- unions <$> traverse (getDomeinFile >=> pure <<< indexedRoles) referredModels
  (Tuple ei {dfr, invertedQueries}) <- runPhaseTwo_' 
    (do
      addAspectsToExternalRoles
      checkAspectRoleReferences
      inferFromAspectRoles
      qualifyBindings
      qualifyStateNames
      compileCalculatedRoles
      requalifyBindingsToCalculatedRoles
      compileCalculatedProperties
      -- As all Calculated roles and Properties are now compiled, we can safely compile public Url calculations.
      compilePublicUrls
      qualifyPropertyReferences
      computeCompleteEnumeratedTypes
      handlePostponedStateQualifiedParts
      compileStateQueries
      contextualisePerspectives
      -- Now all perspectives are available.
      -- Check whether actions are allowed given perspectives.
      handleScreens screens
      checkPerspectiveModifiers
      invertPerspectiveObjects
      -- combinePerspectives
      addUserRoleGraph
      checkSynchronization
      )
    df
    indexedContexts
    indexedRoles
    postponedParts 
  case ei of
    (Left e) -> pure $ Left e
    otherwise -> pure $ Right (Tuple dfr invertedQueries)

getDF :: Unit -> PhaseThree DomeinFileRecord
getDF _ = lift $ State.gets _.dfr

-- | Aspect roles are added by default as Enumerated to their context. However, they may be Calculated and only now can we check.
-- | Switch RoleType for those aspect roles.
checkAspectRoleReferences :: PhaseThree Unit
checkAspectRoleReferences = do
  df@{id} <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df)
    (checkAspectRoles' df)
  where
    checkAspectRoles' :: DomeinFileRecord -> PhaseThree Unit
    checkAspectRoles' {contexts} = do 
      contexts' <- for contexts
        \(CTXT.Context r@{contextRol, rolInContext, gebruikerRol}) -> do
          contextRol' <- traverse check contextRol
          rolInContext' <- traverse check rolInContext
          gebruikerRol' <- traverse check gebruikerRol
          pure $ CTXT.Context $ r {contextRol = contextRol', rolInContext = rolInContext', gebruikerRol = gebruikerRol'}
      modifyDF \dfr -> dfr {contexts = contexts'}
      where
        check :: RoleType -> PhaseThree RoleType
        check rt = lift $ lift $ string2RoleType $ roletype2string rt

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
    qualifyBindings' {enumeratedRoles:eroles, calculatedRoles:croles, contexts} = for_ eroles
      (\(EnumeratedRole rr@{id, binding:mfiller, pos}) -> case mfiller of 
        Nothing -> pure unit
        Just filler -> do
          qfiller <- transform (unsafePartial qualifyBinding pos) filler
          if filler == qfiller
            then pure unit
            else -- change the role in the domain
              modifyDF (\df@{enumeratedRoles} -> df {enumeratedRoles = insert (unwrap id) (EnumeratedRole rr {binding = Just qfiller}) enumeratedRoles}))
      where
        qualifyBinding :: Partial => ArcPosition -> ADT QT.RoleInContext -> PhaseThree (ADT QT.RoleInContext)
        qualifyBinding pos adt = case adt of 
          ST ric -> qualify ric
          UET ric -> qualify ric >>= case _ of 
            ST a -> pure $ UET a
            x -> pure x
          
          where 
            qualify :: QT.RoleInContext -> PhaseThree (ADT QT.RoleInContext)
            qualify (QT.RoleInContext{context,role}) = do
              -- Try to qualify as an EnumeratedRole in the current domain. Notice that we decide on ST/UET below.
              q <- try $ ST <$> qualifyLocalEnumeratedRoleName pos (unwrap role) (keys eroles)
              unsafePartial case q of
                Left (errs :: Array PerspectivesError) -> case unsafePartial fromJust $ head errs of 
                  (UnknownRole _ _) -> do
                    -- Otherwise, try to qualify as a CalculatedRole in the current domain.
                    -- We continue an intentional semantic error introduced while parsing here by attempting to qualify the binding, that we know not
                    -- to be an EnumeratedRole, as an EnumeratedRole. However, with requalifyBindingsToCalculatedRoles we will
                    -- correct that error. We cannot do otherwise because at this state we don't have compiled the expressions
                    -- of the CalculatedRoles yet.
                    (q' :: Either (Array PerspectivesError) (ADT EnumeratedRoleType)) <- try $ ST <$> qualifyLocalEnumeratedRoleName pos (unwrap role) (keys croles)
                    -- By construction we have a Simple ADT or an error.
                    unsafePartial case q' of
                      Left (errs' :: Array PerspectivesError) -> case unsafePartial fromJust $ head errs' of 
                        (UnknownRole _ _) -> do
                          -- If we cannot find the identifier in the Calculated roles, it may be in another namespace.
                          -- But then the name must be fully qualified; we're not going to try just any model.
                          if isTypeUri (unwrap role)
                            then do
                              rl <- lift $ lift $ getRoleType (unwrap role) >>= getRole
                              case rl of
                                E r@(EnumeratedRole{id, context:lexicalContext}) -> do
                                  qualifiedContext <- case context of
                                    -- No context was explicitly declared with the binding. We take the lexical context of the role.
                                    ContextType s | s == "" -> pure lexicalContext
                                    -- A name was specified, fully qualified. Just accept it. TEST!!
                                    -- ContextType contextName -> pure context
                                    -- A name was specified, fully qualified. Check whether it exists, but not as a local name.
                                    ContextType contextName -> catchError
                                      (lift2 $ getContext context *> pure context)
                                      (\e -> throwError $ UnknownContext pos (unwrap context))
                                  lift2 ((flip replaceContext qualifiedContext) <$> roleADT r)
                                C r -> case context of
                                  ContextType empty | empty == "" -> lift2 $ roleADT r
                                  ContextType contextName ->
                                    try (lift2 $ getContext context) >>= case _ of 
                                      Left e -> throwError $ UnknownContext pos (unwrap context)
                                      Right _ -> (flip replaceContext context) <$> (lift2 $ roleADT r)
                            else throwError $ NotWellFormedName pos (unwrap role)
                        e -> throwError e
                        -- qualifiedRoleType is a CalculatedRole defined in the current domain.
                        -- A CalculatedRole cannot be used as an Aspect. Context must be empty.
                      Right (ST qualifiedRoleType) -> case context of
                        ContextType empty | empty == "" -> pure $ UET $ QT.RoleInContext{context, role:qualifiedRoleType}
                          -- Note that this is not yet correct, as the role in the RoleInContext should be
                          -- an EnumeratedRole. But we've not yet compiled the CalculatedRoles so we leave it for now.
                        ContextType contextName -> throwError $ NoCalculatedAspect pos (unwrap qualifiedRoleType)
                  e -> throwError e
                Right (ST qualifiedRoleType) -> case context of
                  ContextType empty | empty == "" -> unsafePartial case lookup (unwrap qualifiedRoleType) eroles of
                    Just r -> lift2 $ roleADT r
                  ContextType contextName -> do
                    qualifiedContext <- qualifyLocalContextName pos contextName (keys contexts)
                    (flip replaceContext qualifiedContext) <$> unsafePartial case lookup (unwrap qualifiedRoleType) eroles of
                      Just r -> lift2 $ roleADT r

-- | States that are constructed out of a SubjectState StateSpecification may have an unqualified name because
-- | an ExplicitRole RoleIdentification may have a RoleType that is not fully qualified.
qualifyStateNames :: PhaseThree Unit
qualifyStateNames = (lift $ State.gets _.dfr) >>= qualifyStateNames'
  where
    qualifyStateNames' :: DomeinFileRecord -> PhaseThree Unit
    qualifyStateNames' {states} = for states (\(State sr@{id}) -> do
      qid <- if isTypeUri (unwrap id)
        then pure $ unwrap id
        -- Note that the validity of this depends on the unqualfied name being a reference to a role!
        -- TODO What about context states?
        else qualifyLocalRoleName arcParserStartPosition (unwrap id) >>= pure <<< roletype2string
      pure $ State sr {id = StateIdentifier qid}) >>=
        \qstates -> modifyDF (\df -> df {states = qstates})

-- | For each (Enumerated) role with a binding to the name of a CalculatedRole (falsely declared to be Enumerated!),
-- | replace that binding with the ADT of the (now compiled) CalculatedRole.
requalifyBindingsToCalculatedRoles :: PhaseThree Unit
requalifyBindingsToCalculatedRoles = (lift $ State.gets _.dfr) >>= qualifyBindings'
  where
    qualifyBindings' :: DomeinFileRecord -> PhaseThree Unit
    qualifyBindings' {enumeratedRoles:eroles, calculatedRoles:croles} = for_ eroles
      (\(EnumeratedRole rr@{id, binding:mfiller, pos}) -> do 
        case mfiller of 
          Nothing -> pure unit
          Just (filler :: ADT QT.RoleInContext) -> do 
            (qbinding :: ADT QT.RoleInContext) <- transform (unsafePartial qualifyBinding pos) filler
            if filler == qbinding
              then pure unit
              else -- change the role in the domain
                modifyDF (\df@{enumeratedRoles} -> df {enumeratedRoles = insert (unwrap id) (EnumeratedRole rr {binding = Just qbinding}) enumeratedRoles}))
      where
        qualifyBinding :: Partial => ArcPosition -> ADT QT.RoleInContext -> PhaseThree (ADT QT.RoleInContext)
        qualifyBinding pos adt = case adt of 
          ST original@(QT.RoleInContext{context,role}) -> case lookup (unwrap role) croles of
            Nothing -> pure $ ST original
            Just crole -> lift2 $ roleADT crole
          UET original@(QT.RoleInContext{context,role}) -> case lookup (unwrap role) croles of
            Nothing -> pure $ UET original
            Just crole -> lift2 $ roleADT crole

-- | Qualify the references to Properties in each View.
-- | Also qualify references to properties in propertyAliases (qualify the destinations).
-- | Note that we need the DomeinFile with qualified bindings in the cache
-- | for this function to work correctly!
qualifyPropertyReferences :: PhaseThree Unit
qualifyPropertyReferences = do
  df@{id} <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df)
    (qualifyPropertyReferences' df)
  where
    qualifyPropertyReferences' :: DomeinFileRecord -> PhaseThree Unit
    qualifyPropertyReferences' df@{id, views, calculatedProperties, enumeratedRoles} = do
      qviews <- traverseWithIndex qualifyView views
      enumeratedRoles' <- traverse (unsafePartial qualifyAliases) enumeratedRoles
      modifyDF \dfr -> dfr {views = qviews, enumeratedRoles = enumeratedRoles'}

      where
        qualifyAliases :: Partial => EnumeratedRole -> PhaseThree EnumeratedRole
        qualifyAliases (EnumeratedRole r@{id:roletype, propertyAliases, pos}) = do
          propertyAliases' <- for propertyAliases
            \(destination) -> do
              (ENP destination') <- qualifyProperty (ENR roletype) pos (ENP destination)
              pure destination'
          pure $ EnumeratedRole r { propertyAliases = propertyAliases'}

        qualifyView :: String -> View -> PhaseThree View
        qualifyView viewName (View vr@{propertyReferences, role, pos}) = do
          qprops <- traverse (qualifyProperty role pos) propertyReferences
          pure $ View $ vr {propertyReferences = qprops}

        qualifyProperty :: RoleType -> ArcPosition -> PropertyType -> PhaseThree PropertyType
        qualifyProperty rtype pos propType = do

          if isTypeUri (propertytype2string propType)
            -- The modeller has provided a qualified property. He cannot say whether it is Calculated, or Enumerated,
            -- however. If it is Calculated, change now.
            then if isJust (lookup (propertytype2string propType) calculatedProperties)
              then pure $ CP $ CalculatedPropertyType (propertytype2string propType)
              else pure propType
            else do
              (candidates :: Array PropertyType) <- lift2 (rtype ###= lookForUnqualifiedPropertyType_ (propertytype2string propType))
              case head candidates of
                Nothing -> throwError $ UnknownProperty pos (propertytype2string propType) (roletype2string rtype)
                (Just t) | length candidates == 1 -> pure t
                otherwise -> throwError $ NotUniquelyIdentifying pos (propertytype2string propType) (map propertytype2string candidates)

-- | For each EnumeratedRole R in the model, add InvertedQueries to make deltas
-- | available to User Roles in the context of R that have a Perspective on R.
-- | For an explanation, see https://joopringelberg.github.io/perspectives-documentation/Perspectives%20on%20bindings.pdf
-- invertedQueriesForLocalRolesAndProperties :: PhaseThree Unit
-- invertedQueriesForLocalRolesAndProperties = do
--   df@{id} <- lift $ State.gets _.dfr
--   withDomeinFile
--     id
--     (DomeinFile df)
--     (invertedQueriesForLocalRolesAndProperties' df)
--   where
--     invertedQueriesForLocalRolesAndProperties' :: DomeinFileRecord -> PhaseThree Unit
--     invertedQueriesForLocalRolesAndProperties' {enumeratedRoles} = do
--       for_ enumeratedRoles
--         \(EnumeratedRole {id, context, mandatory, functional}) -> do
--           (userTypes :: Array RoleType) <- lift $ lift (context ###= unsafePartial localEnumeratedRolesWithPerspectiveOnRole (ENR id))
--           qwk <- pure $ ZQ
--             (Just (SQD (RDOM (ST id)) (QF.DataTypeGetter QF.ContextF) (CDOM (ST context)) True (bool2threeValued mandatory)))
--             Nothing
--           for_ userTypes \userType -> do
--             pv <- lift2 $ unsafePartial relevantPropertiesForObjectRole (ENR id) userType
--             -- Now add those verbs to the inverted query.
--             setInvertedQueriesForUserAndRole userType (ST id) pv true qwk

-- | The calculation of a CalculatedRole, of a CalculatedProperty, the object of a Perspective
-- | and the object and query of a state are all expressions.
-- | This function compiles the parser AST output that represents these expressions to QueryFunctionDescriptions.
-- | All names are qualified in the process.
compileCalculatedRoles :: PhaseThree Unit
compileCalculatedRoles = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (compileCalculatedRoles' df)
  where
    compileCalculatedRoles' :: DomeinFileRecord -> PhaseThree Unit
    compileCalculatedRoles' {id,calculatedRoles, states, enumeratedRoles} = do
      traverse_ compileRolExpr (identifier <$> calculatedRoles)
      -- The objects of perspectives are compiled when we handle the postponedStateQualifiedParts.
      -- Get the DomeinFile out of cache and replace the one in PhaseTwoState with it.
      -- We will not have errors on trying to retrieve the DomeinFile here.
      DomeinFile modifiedDomeinFile <- lift2 $ getDomeinFile id
      modifyDF \dfr -> modifiedDomeinFile
      where
        compileRolExpr :: CalculatedRoleType -> PhaseThree Unit
        compileRolExpr roleType = do
          cr@(CalculatedRole {calculation, context}) <- lift2 $ getCalculatedRole roleType
          case calculation of
            Q _ -> pure unit
            -- Compiles the parsed expression and stores the modified CalculatedRole in
            -- the DomeinCache.
            S stp isFunctional -> void $ compileAndSaveRole (CDOM $ UET context) stp cr isFunctional

compileCalculatedProperties :: PhaseThree Unit
compileCalculatedProperties = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (compileCalculatedProperties' df)
  where
    compileCalculatedProperties' :: DomeinFileRecord -> PhaseThree Unit
    compileCalculatedProperties' {id, calculatedProperties, states, enumeratedRoles} = do
      traverse_ compilePropertyExpr (identifier <$> calculatedProperties)
      -- The objects of perspectives are compiled when we handle the postponedStateQualifiedParts.
      -- Get the DomeinFile out of cache and replace the one in PhaseTwoState with it.
      -- We will not have errors on trying to retrieve the DomeinFile here.
      DomeinFile modifiedDomeinFile <- lift2 $ getDomeinFile id
      modifyDF \dfr -> modifiedDomeinFile
      where
        compilePropertyExpr :: CalculatedPropertyType -> PhaseThree Unit
        compilePropertyExpr propertyType = do
          cp@(CalculatedProperty {calculation, role}) <- lift2 $ getCalculatedProperty propertyType
          r@(EnumeratedRole{context}) <- lift2 $ getEnumeratedRole role
          case calculation of
            Q _ -> pure unit
            -- The CalculatedProperty query is lexically embedded in its property declaration.
            -- That must be lexically embedded in an EnumeratedRole definition,
            -- which in turn is lexically embedded in a Context definition.
            -- Hence we can use the context type for the RoleInContext.
            -- Gebruik hier het extra stuk van S om de range functioneel te maken, indien van toepassing.
            S stp isFunctional -> void $ (lift2 $ roleADT r) >>= \ric -> compileAndSaveProperty (RDOM ric) stp cp isFunctional


compilePublicUrls :: PhaseThree Unit
compilePublicUrls = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (compilePublicUrls' df)
  where
    compilePublicUrls' :: DomeinFileRecord -> PhaseThree Unit
    compilePublicUrls' {id, enumeratedRoles} = do
      traverse_ compilePublicUrlExpr (identifier <$> enumeratedRoles)
      -- The objects of perspectives are compiled when we handle the postponedStateQualifiedParts.
      -- Get the DomeinFile out of cache and replace the one in PhaseTwoState with it.
      -- We will not have errors on trying to retrieve the DomeinFile here.
      DomeinFile modifiedDomeinFile <- lift2 $ getDomeinFile id
      modifyDF \dfr -> modifiedDomeinFile
      where
        compilePublicUrlExpr :: EnumeratedRoleType -> PhaseThree Unit
        compilePublicUrlExpr roleType = do
          cr@(EnumeratedRole er@{publicUrl, context, pos}) <- lift2 $ getEnumeratedRole roleType
          case publicUrl of
            Nothing -> pure unit
            Just (Q _) -> pure unit
            -- Compiles the parsed expression and stores the modified CalculatedRole in
            -- the DomeinCache.
            Just (S stp isFunctional) -> do
              expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                [ makeIdentityStep "currentcontext" (startOf stp)
                , makeIdentityStep "origin" (startOf stp)
                ]
                stp
              compiledExpression <- compileExpression (CDOM $ UET context) expressionWithEnvironment
              -- Check.
              case range compiledExpression of
                -- Save the result in DomeinCache.
                VDOM PString _ -> lift2 $ void $ modifyEnumeratedRoleInDomeinFile id (EnumeratedRole er {publicUrl = Just $ Q compiledExpression})
                ran -> throwError (NotAStringDomain compiledExpression pos pos)


handlePostponedStateQualifiedParts  :: PhaseThree Unit
handlePostponedStateQualifiedParts = do
  postponedStateQualifiedParts <- lift $ State.gets _.postponedStateQualifiedParts
  df@{id} <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df)
    (for_ (LIST.filter isPerspectiveContribution postponedStateQualifiedParts) (unsafePartial handlePart))
  df' <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df')
    (for_ (LIST.filter (not <<< isPerspectiveContribution) postponedStateQualifiedParts) (unsafePartial handlePart))
  where

    isPerspectiveContribution :: AST.StateQualifiedPart -> Boolean
    isPerspectiveContribution (AST.R _) = true
    isPerspectiveContribution (AST.P _) = true
    isPerspectiveContribution (AST.SO _) = true
    isPerspectiveContribution _ = false

    collectRoleInContexts :: RoleIdentification -> PhaseThree (ADT QT.RoleInContext)
    -- A single role type will result from this case, but it may be a calculated role!
    collectRoleInContexts (ExplicitRole ctxt rt pos) = do
      maximallyQualifiedName <- if isTypeUri (roletype2string rt)
        then pure (roletype2string rt)
        else pure $ concatenateSegments (unwrap ctxt) (roletype2string rt)
      (r :: RoleType) <- qualifyLocalRoleName pos maximallyQualifiedName
      lift2 $ roleADTOfRoleType r
    -- Compile the expression s with respect to context ctxt.
    -- This case MUST represent the current object that holds in the body of `perspective on`. Multiple Enumerated role types can result from this case.
    collectRoleInContexts (ImplicitRole ctxt s) = compileExpression (CDOM (UET ctxt)) s >>= \qfd ->
      case range qfd of
        RDOM adt -> pure adt
        otherwise -> throwError $ NotARoleDomain otherwise (startOf s) (endOf s)

    -- | Correctly handles incomplete (not qualified) RoleIdentifications.
    -- | Result contains no double entries.
    collectStates :: (Maybe SegmentedPath) -> RoleIdentification -> PhaseThree (Array StateIdentifier)
    collectStates mpath r = collectRoles r >>= \roles -> do
      -- Don't include aspects.
      roles' <- nub <$> lift2 (roles ###= (forceTypeArray >=> f >=> ArrayT <<< pure <<< allLeavesInADT))      
      case mpath of
        -- For a Calculated role, we should now take the range of its calculation.
        -- This is because a Calculated role has no instances that have state.
        -- It calculates Enumerated role instances - and those have state!
        Nothing -> pure (StateIdentifier <<< unwrap <$> roles')
        Just p -> if isTypeUri p
          then pure [StateIdentifier p]
          else pure (StateIdentifier <<< flip append p <<< flip append "$" <<< unwrap <$> roles')

      where 
        f :: RoleType ~~~> ADT EnumeratedRoleType
        f rt = ArrayT do
          x <- roleADTOfRoleType rt
          pure [roleInContext2Role <$> x]

    -- | Checks whether all states have been declared.
    statesExist :: ArcPosition -> ArcPosition -> Array StateIdentifier -> PhaseThree (Array StateIdentifier)
    statesExist start end states = do 
      for_ states \stateId -> (lift2 $ tryGetPerspectType stateId) >>= case _ of
        Nothing -> throwError $ (StateDoesNotExist stateId start end)
        Just _ -> pure unit
      pure states

    -- | Correctly handles incomplete (not qualified) RoleIdentifications that may occur in the SubjectState case.
    -- | Result contains no double entries.
    -- | State Identifiers are not guaranteed to refer to existing (declared) states!
    stateSpec2States :: AST.StateSpecification -> PhaseThree (Array StateIdentifier)
    stateSpec2States spec = case spec of
      -- Execute the effect for all subjects in the context state.
      -- We add aspect states here in order to accommodate aspect roles when we establish in runtime of InvertedQueries whether
      -- they support a particular property in de context states.
      AST.ContextState ctx mpath -> do 
        (stateName :: String) <- pure (maybe (unwrap ctx) (maybeQualifyWith (unwrap ctx)) mpath)
        -- Now if this is a context type, add all the aspects.
        (mContextType :: Maybe CTXT.Context) <- lift2 $ tryGetPerspectType (ContextType stateName)
        case mContextType of 
          Nothing -> pure [StateIdentifier stateName]
          Just context -> do 
            aspects <- lift2 ((ContextType stateName) ###= contextAspectsClosure)
            pure (StateIdentifier <<< unwrap <$> (cons (ContextType stateName) aspects))
        -- pure [(StateIdentifier $ contextType mpath)]
      -- Execute the effect for each qualifiedUser in each of the subject states.
      -- Note that the subjects in whose states we execute, do not need be the qualified users we execute for:
      -- 'do this automatically for me when you are at home' illustrates this independence.
      AST.SubjectState ridentification mpath -> collectStates mpath ridentification
      -- Execute the effect for all subjects in all object states.
      AST.ObjectState ridentification mpath -> collectStates mpath ridentification

      where
        maybeQualifyWith :: Namespace -> SegmentedPath -> String
        maybeQualifyWith ns path = if isTypeUri path then path else qualifyWith ns path

    stateSpecificationToStateSpec :: AST.StateSpecification -> PhaseThree (Array StateSpec)
    stateSpecificationToStateSpec spec = case spec of
      AST.ContextState _ _ -> map ContextState <$> stateSpec2States spec
      AST.SubjectState _ _ -> map SubjectState <$> stateSpec2States spec
      AST.ObjectState _ _ -> map ObjectState <$> stateSpec2States spec

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart :: Partial => AST.StateQualifiedPart -> PhaseThree Unit

    -- Compiles and distributes all expressions in the automatic effect.
    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.AE (AST.AutomaticEffectE{subject, object, transition, effect, startMoment, endMoment, repeats, start, end})) = do
      -- `originDomain` is the type of the origin, expressed as Domain.
      originDomain <- statespec2Domain (transition2stateSpec transition)
      -- `currentContextDomain` represents the current context, expressed as a Domain.
      currentcontextDomain <- pure (CDOM $ UET $ stateSpec2ContextType (transition2stateSpec transition))
      -- Check whether the state specification is coherent
      isCoherentStateSpecification (transition2stateSpec transition)
      -- `subject` is the current subject of lexical analysis. It is represented by
      -- an ExplicitRole data constructor.
      -- The current subject is always a single role type, but it may be calculated
      -- (and the range of the calculation may be a combination of many Enumerated role
      -- types).
      -- These qualifiedUsers will end up in InvertedQueries.
      (qualifiedUsers :: Array RoleType) <- collectRoles subject
      -- Each of the expressions in the statements is applied to the resource that changes state (the origin),
      -- which can be a role instance or a context instance. Its type can be found from the transition.
      -- origin -> is always included as an identity step.
      -- currentcontext -> For ContextState, equals the origin. For SubjectState or
      -- ObjectState, the role is represented with a RoleIdentification that contains
      -- its current context. We include a computation of type TypeTimeOnly, which instructs the unsafeCompiler to remove them. The value has to be supplied in runtime.
      -- currentactor -> It's type is the qualifiedUser computed above. Again, just a TypeTimeOnly computation.
      (usersInContext :: ADT QT.RoleInContext) <- collectRoleInContexts subject
      effectWithEnvironment <- pure $ addContextualBindingsToStatements
        [ computeOrigin (transition2stateSpec transition) start
        , computeCurrentContext (transition2stateSpec transition) start
        , unsafePartial makeTypeTimeOnlyRoleStep "currentactor" usersInContext start
        ]
        effect
      states <- stateSpec2States (transition2stateSpec transition) >>= statesExist start end
      -- Compile the side effect. Will invert all expressions in the statements, too, including
      -- the object if it is referenced.
      (sideEffect :: QueryFunctionDescription) <- compileStatement
        states
        originDomain
        currentcontextDomain
        qualifiedUsers
        effectWithEnvironment
      -- Compute the currentcontext from the origin.
      currentContextCalculation <- case transition2stateSpec transition of
        -- We do not actually use this result.
        AST.ContextState ct _ -> pure $ SQD (CDOM $ UET ct) (DataTypeGetter IdentityF) (CDOM $ UET ct) True True
        AST.ObjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
        AST.SubjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
      objectQfd <- case object of
        Nothing -> pure Nothing
        Just qfd -> Just <$> roleIdentificationToQueryFunctionDescription qfd start
      objectMustBeRole objectQfd start end
      modifyAllStates
        (case transition2stateSpec transition of
          AST.ContextState _ _ -> ContextAction 
            { effect: sideEffect
            , startMoment
            , endMoment
            , repeats
            }
          otherwise -> RoleAction 
            { currentContextCalculation
            , startMoment
            , endMoment
            , repeats
            , effect: sideEffect
            })
        qualifiedUsers
        states
        originDomain
        objectQfd
      case object, objectQfd of
        Just object', Just objectQfd' -> for_ qualifiedUsers (modifyPerspective objectQfd' object' start
          \(Perspective r) -> Perspective r {automaticStates = union r.automaticStates states})
        _, _ -> pure unit
      where

        -- | Modifies the DomeinFile in PhaseTwoState.
        modifyAllStates :: AutomaticAction -> Array RoleType -> Array StateIdentifier -> Domain -> Maybe QueryFunctionDescription -> PhaseThree Unit
        modifyAllStates automaticAction qualifiedUsers states currentDomain mobjectQfd = for_ states
          \stateId -> isUpstreamModelState stateId >>= if _
          -- TODO. #16 Make the effect conditional on the specialised stateful object, rather than the general object.
            then modifyDF 
              (addUpstreamAutomaticEffect 
                (UpstreamAutomaticEffect
                  { stateId
                  , automaticAction
                  , qualifiedUsers
                  , isOnEntry: 
                    case transition of 
                      AST.Entry _ -> true
                      _ -> false
                  }) stateId)
            else (modifyPartOfState 
              start 
              end
              (\(sr@{automaticOnEntry, automaticOnExit, query}) -> do
                query' <- case query of
                  Q q -> pure $ Q q
                  S stp _ -> do
                    -- This is the state query (condition). It can belong to either a role- or a context state.
                    -- A context state may have:
                    --    * origin, for which we may specify an identity step.
                    --    * currentcontext == origin.
                    -- Role states are only defined in the lexical scope of enumerated roles! Consequently, we'll just
                    -- have to deal with the ExplicitRole constructor of RoleIdentification and we know that its RoleType
                    -- must be enumerated.
                    -- A role state may have:
                    --    * origin, to be derived from the RoleIdentification in either SubjectState or ObjectState,
                    --    * currentcontext = origin >> context but which can be read directly from the RoleIdentification.
                    expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                      [ computeCurrentContext (transition2stateSpec transition) start
                      , computeOrigin (transition2stateSpec transition) start]
                      stp
                    Q <$> compileAndDistributeStep currentDomain expressionWithEnvironment states
                case transition of
                  AST.Entry _ -> pure $ sr
                    { automaticOnEntry = EM.addAll
                        automaticAction                   -- value
                        automaticOnEntry                  -- EM.EncodableMap key value
                        qualifiedUsers                    -- Array Key
                    , query = query'
                    , object = mobjectQfd}
                  AST.Exit _ -> pure $ sr
                    { automaticOnExit = EM.addAll automaticAction automaticOnExit qualifiedUsers
                    , query = query'
                    , object = mobjectQfd})
              stateId)

    -- Compiles and distributes all expressions in the message.
    -- See extensive comments in the case AutomaticEffectE.
    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.N (AST.NotificationE{user, object, transition, message, startMoment, endMoment, repeats, start, end})) = do
      originDomain <- statespec2Domain (transition2stateSpec transition)
      (qualifiedUsers :: Array RoleType) <- collectRoles user
      states <- stateSpec2States (transition2stateSpec transition) >>= statesExist start end
      -- Then compile the parts of the sentence, tacking each compiled part onto that sequence.
      -- The expressions in the Sentence are compiled with respect to the current context.
      (usersInContext :: ADT QT.RoleInContext) <- collectRoleInContexts user
      compiledMessage <- compileSentence originDomain message usersInContext states (transition2stateSpec transition)
      currentContextCalculation <- case transition2stateSpec transition of
        AST.ContextState ct _ -> pure $ SQD (CDOM $ UET ct) (DataTypeGetter IdentityF) (CDOM $ UET ct) True True
        AST.ObjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
        AST.SubjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
      objectQfd <- case object of
        Nothing -> pure Nothing
        Just qfd -> Just <$> roleIdentificationToQueryFunctionDescription qfd start
      objectMustBeRole objectQfd start end
      modifyAllStates
        (case transition2stateSpec transition of
          AST.ContextState _ _ -> ContextNotification 
            { sentence: compiledMessage
            , startMoment
            , endMoment
            , repeats
            , domain: unsafePartial typeUri2ModelUri_ $ unwrap $ roleIdentification2context user
            }
          otherwise -> RoleNotification 
            { currentContextCalculation
            , sentence: compiledMessage
            , startMoment
            , endMoment
            , repeats
            , domain: unsafePartial typeUri2ModelUri_ $ unwrap $ roleIdentification2context user
            })
        qualifiedUsers
        states
        originDomain
        objectQfd
      case object, objectQfd of
        Just object', Just objectQfd' -> for_ qualifiedUsers (modifyPerspective objectQfd' object' start
          \(Perspective r) -> Perspective r {automaticStates = union r.automaticStates states})
        _, _ -> pure unit
      where
          -- | Modifies the DomeinFile in PhaseTwoState.
          modifyAllStates :: Notification -> Array RoleType -> Array StateIdentifier -> Domain -> Maybe QueryFunctionDescription -> PhaseThree Unit
          modifyAllStates notification qualifiedUsers states currentDomain mobjectQfd= for_ states
            \stateId -> isUpstreamModelState stateId >>= if _
              then modifyDF (addUpstreamNotification 
                (UpstreamStateNotification{stateId, notification, qualifiedUsers, isOnEntry: 
                  case transition of 
                    AST.Entry _ -> true
                    _ -> false
                  }) 
                stateId)
              else modifyPartOfState
                start
                end
                (\(sr@{notifyOnEntry, notifyOnExit, query}) -> do
                  -- Compile the query if we've not done it before.
                  query' <- case query of
                    Q q -> pure $ Q q
                    S stp _ -> do
                      expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                        [ computeCurrentContext (transition2stateSpec transition) start
                        , computeOrigin (transition2stateSpec transition) start]
                        stp
                      Q <$> compileAndDistributeStep currentDomain expressionWithEnvironment states
                  case transition of
                    AST.Entry _ -> pure $ sr
                      { notifyOnEntry = EM.addAll notification notifyOnEntry qualifiedUsers
                      , query = query'
                      , object = mobjectQfd
                      }
                    AST.Exit _ -> pure $ sr
                      { notifyOnExit = EM.addAll notification notifyOnExit qualifiedUsers
                      , query = query'
                      , object = mobjectQfd
                      })
                stateId

          compileSentence :: Domain -> SentenceE -> ADT QT.RoleInContext -> Array StateIdentifier -> AST.StateSpecification -> PhaseThree Sentence.Sentence
          compileSentence currentDomain (SentenceE {parts, sentence}) usersInContext states stateSpec = do
            parts' <- traverse compilePart parts
            pure $ Sentence.Sentence {sentence, parts: parts'} 
            where
              compilePart :: SentencePartE -> PhaseThree QueryFunctionDescription
              compilePart (CPpart stp) = do 
                expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                  [ computeOrigin (transition2stateSpec transition) start
                  , computeCurrentContext (transition2stateSpec transition) start
                  , unsafePartial makeTypeTimeOnlyRoleStep "notifieduser" usersInContext start
                  ]
                  stp
                compileExpression currentDomain expressionWithEnvironment

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.AC (AST.ActionE{id, subject, object:syntacticObject, state, effect, start, end})) = do
      -- `currentContextDomain` represents the current context, expressed as a Domain.
      currentcontextDomain <- pure (CDOM $ UET $ stateSpec2ContextType state)
      -- `subject` is the current subject of lexical analysis. It is represented by
      -- an ExplicitRole data constructor.
      -- The current subject is always a single role type, but it may be calculated
      -- (and the range of the calculation may be a combination of many Enumerated role
      -- types).
      -- These qualifiedUsers will end up in InvertedQueries.
      (qualifiedUsers :: Array RoleType) <- collectRoles subject
      -- Each of the expressons in the statements is applied to the object of the perspective, always a role
      -- instance
      -- We include the following standard variables:
      --  origin -> is always included as an identity step, obviously provided as argument on executing the action.
      --  currentcontext -> is provided on executing the action. Its type is computed from the RoleIdentification
      --  that represents the object.
      --  currentactor -> It's type is the qualifiedUser computed above.
      -- The last two VarBindings have a computation of type TypeTimeOnly, which instructs the unsafeCompiler to remove them.
      (usersInContext :: ADT QT.RoleInContext) <- collectRoleInContexts subject
      effectWithEnvironment <- pure $ addContextualBindingsToStatements
        [ makeIdentityStep "origin" start
        , computeCurrentContext state start
        , unsafePartial makeTypeTimeOnlyRoleStep "currentactor" usersInContext start
        ]
        effect
      states <- stateSpec2States state >>= statesExist start end
      -- `syntacticObject` represents the object of the perspective. It allows
      -- for `currentcontext` and `origin`.
      -- currentcontext == origin and this equals the argument that the
      -- queryfunction is applied to, which is a context instance.
      -- We can add both as an Identity step in a letE.
      syntacticObjectWithEnvironment <- pure $ addContextualBindingsToExpression
        [ makeIdentityStep "currentcontext" start
        , makeIdentityStep "origin" start]
        (roleIdentification2Step syntacticObject)
      -- We must compile the perspective object with respect to its current context.
      -- This is contained within the RoleIdentification that represents the object.
      compiledObject <- withFrame
        (compileExpression
          (CDOM $ UET (roleIdentification2Context syntacticObject))
          syntacticObjectWithEnvironment)
      objectMustBeRole (Just compiledObject) start end
      -- The effect starts with the Perspective object, i.e. the syntacticObject.
      (theAction :: QueryFunctionDescription) <- compileStatement
        states
        (range compiledObject)
        currentcontextDomain
        qualifiedUsers
        effectWithEnvironment

      stateSpecs <- stateSpecificationToStateSpec state
      modifyAllSubjectPerspectives
        qualifiedUsers
        compiledObject
        (Action theAction)
        stateSpecs
      where
        -- | Modifies the DomeinFile in PhaseTwoState.
        -- Add the action for all users to their perspective on the object in all states.
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> Action -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction states = for_ qualifiedUsers
          (modifyPerspective
            objectQfd
            syntacticObject
            start
            \(Perspective pr@{actions}) -> Perspective $ pr {actions = (foldr
              (\stateId actionsMap -> case EM.lookup stateId actionsMap of
                Nothing -> EM.insert stateId (singleton id theAction) actionsMap
                Just actionsInState -> EM.insert stateId (insert id theAction actionsInState) actionsMap)
              actions
              states)})

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.R (AST.RoleVerbE{subject, object, state, roleVerbs:rv, start, end})) = do
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      objectMustBeRole (Just objectQfd) start end
      -- ... for these states only...
      stateSpecs <- stateSpecificationToStateSpec state
      -- ... the role verbs.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd stateSpecs
      where
        -- | Modifies the DomeinFile in PhaseTwoState.
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd stateSpecs = for_ qualifiedUsers
          (modifyPerspective objectQfd object start
            -- Add the roleverbs from the RoleVerbE to the map of roleverbs in the perspective, for each of the states (the keys in the map).
            -- Note that the states can apply to the object of the perspective, or to its subject, or to the current context of the lexical position.
            (\(Perspective pr@{roleVerbs}) -> Perspective pr {roleVerbs = EM.addAll rv roleVerbs stateSpecs}))

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.P (AST.PropertyVerbE{subject, object, state, propertyVerbs, propsOrView, start, end})) = do
      -- Add, for all these users...
      (qualifiedUsers :: Array RoleType) <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      objectMustBeRole (Just objectQfd) start end
      propertyTypes <- unsafePartial collectPropertyTypes propsOrView object start
      -- We must expand the propertyTypes.
      allProps <- lift2 $ allProperties (roleInContext2Role <$> (domain2roleInContext (range objectQfd)))
      expandedProps <- pure $ expandPropSet allProps propertyTypes
      if isMutatingVerbSet propertyVerbs
        then case filter (case _ of 
          CP _ -> true
          _ -> false) expandedProps of
          s | null s -> pure unit
          calculatedProps -> throwError $ CannotModifyCalculatedProperty (intercalate "," $ propertytype2string <$> calculatedProps) start end
        else pure unit
      (propertyVerbs' :: PropertyVerbs) <- pure $ PropertyVerbs propertyTypes propertyVerbs
      -- ... for these states only...
      stateSpecs <- stateSpecificationToStateSpec state
      -- ... the action.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' stateSpecs
      -- Add StateDependentPerspectives to the states.
      -- We cannot modify states of Calculated Roles, as these do not exist.
      states <- stateSpec2States state >>= statesExist start end >>= LIST.filterM isEnumeratedRoleState <<< LIST.fromFoldable
      props <- case propertyTypes of
        Universal -> lift2 $ allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range objectQfd))
        Empty -> pure []
        PSet as -> pure as
      for_ states
        (modifyStateDependentPerspectives
          state
          (case _ of
            ContextPerspective r@{properties} -> ContextPerspective r {properties = union properties props}
            RolePerspective r@{properties} -> RolePerspective  r {properties = union properties props})
          qualifiedUsers
          objectQfd
          start
          end)
      where

        -- | Modifies the DomeinFile in PhaseTwoState.
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> PropertyVerbs -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' stateSpecs = for_ qualifiedUsers
          (modifyPerspective
            objectQfd
            object
            start
            -- Add the propertyVerbs from the PropertyVerbE to the propertyVerbs of the perspective, for each of the states (the keys in the map).
            -- Note that the states can apply to the object of the perspective, or to its subject, or to the current context of the lexical position.
            \(Perspective pr@{propertyVerbs:pverbs}) -> Perspective $ pr {propertyVerbs = (foldr
              (\stateId pverbsMap -> case EM.lookup stateId pverbsMap of
                Nothing -> EM.insert stateId [propertyVerbs'] pverbsMap
                Just propertyVerbsArray -> EM.insert stateId (cons propertyVerbs' propertyVerbsArray) pverbsMap)
              pverbs
              stateSpecs)})

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.SO (AST.SelfOnly{subject, object, state, start, end})) = do
      currentDomain <- pure (CDOM $ UET $ stateSpec2ContextType state)
      -- Set, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      -- ... the selfOnly member to true.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd
      -- Modify the selfOnly value of StateDependentPerspectives in states.
      -- We cannot modify states of Calculated Roles, as these do not exist.
      states <- stateSpec2States state >>= statesExist start end >>= LIST.filterM isEnumeratedRoleState <<< LIST.fromFoldable
      for_ states
        (modifyStateDependentPerspectives
          state
          (case _ of
            ContextPerspective r -> ContextPerspective r {selfOnly = true}
            RolePerspective r -> RolePerspective  r {selfOnly = true})
          qualifiedUsers
          objectQfd
          start
          end)

      where

        -- | Modifies the DomeinFile in PhaseTwoState.
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd = for_ qualifiedUsers
          -- If this is not a selfPerspective, generate an error.
          \qualifiedUser -> do 
            isSelfPerspective <- lift $ lift (qualifiedUser ###>> (unsafePartial isPerspectiveOnSelf objectQfd))
            if isSelfPerspective
              then modifyPerspective objectQfd object start (\(Perspective pr) -> Perspective pr {selfOnly = true}) qualifiedUser
              else throwError $ NotASelfPerspective start end

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.PO (AST.AuthorOnly{subject, object, state, start, end})) = do
      currentDomain <- pure (CDOM $ UET $ stateSpec2ContextType state)
      -- Set, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      -- ... the authorOnly member to true.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd
      -- Modify the authorOnly value of StateDependentPerspectives in states.
      -- We cannot modify states of Calculated Roles, as these do not exist.
      states <- stateSpec2States state >>= statesExist start end >>= LIST.filterM isEnumeratedRoleState <<< LIST.fromFoldable
      for_ states
        (modifyStateDependentPerspectives
          state
          (case _ of
            ContextPerspective r -> ContextPerspective r {authorOnly = true}
            RolePerspective r -> RolePerspective  r {authorOnly = true})
          qualifiedUsers
          objectQfd
          start
          end)

      where

        -- | Modifies the DomeinFile in PhaseTwoState.
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd = for_ qualifiedUsers
          (modifyPerspective objectQfd object start
            (\(Perspective pr) -> Perspective pr {authorOnly = true}))

    -- | Modifies the DomeinFile in PhaseTwoState.
    handlePart (AST.CA (AST.ContextActionE{id, subject, object:currentContext, state, effect, start, end})) = do
      -- `currentContextDomain` represents the current context, expressed as a Domain.
      currentcontextDomain <- pure (CDOM $ UET $ currentContext)
      -- `subject` is the current subject of lexical analysis. It is represented by
      -- an ExplicitRole data constructor.
      -- The current subject is always a single role type, but it may be calculated
      -- (and the range of the calculation may be a combination of many Enumerated role
      -- types).
      -- These qualifiedUsers will end up in InvertedQueries.
      (qualifiedUsers :: Array RoleType) <- collectRoles subject
      -- Each of the expressons in the statements is applied to the object, always a context instance.
      -- We include the following standard variables:
      --  origin -> is always included as an identity step, obviously provided as argument on executing the action.
      --  currentcontext -> is provided on executing the action. Equals the origin.
      --  currentactor -> It's type is the qualifiedUser computed above.
      -- The last VarBinding has a computation of type TypeTimeOnly, which instructs the unsafeCompiler to remove it.
      (usersInContext :: ADT QT.RoleInContext) <- collectRoleInContexts subject
      effectWithEnvironment <- pure $ addContextualBindingsToStatements
        [ makeIdentityStep "origin" start
        , makeIdentityStep "currentcontext" start
        ,unsafePartial makeTypeTimeOnlyRoleStep "currentactor" usersInContext start
        ]
        effect
      states <- stateSpec2States state >>= statesExist start end
      -- The effect starts with the Perspective object, i.e. the syntacticObject.
      (theAction :: QueryFunctionDescription) <- compileStatement
        states
        currentcontextDomain
        currentcontextDomain
        qualifiedUsers
        effectWithEnvironment

      stateSpecs <- stateSpecificationToStateSpec state
      modifyAllSubjectRoles
        qualifiedUsers
        id
        (Action theAction)
        stateSpecs
      where
        -- Add the action to the map of StateSpecs to an Object of Action identifier to Action of all user roles.
        -- (Actions are double indexed in user roles: first by StateSpec, then by Action identifier).
        -- | Modifies the DomeinFile in PhaseTwoState.
        modifyAllSubjectRoles :: Array RoleType -> String -> Action -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectRoles qualifiedUsers actionId theAction stateSpecs = for_ qualifiedUsers
          \(userRole :: RoleType) -> case userRole of
            ENR (EnumeratedRoleType r) -> do
              EnumeratedRole er <- getsDF (unsafePartial fromJust <<< lookup r <<< _.enumeratedRoles)
              modifyDF \dfr@{enumeratedRoles} -> dfr {enumeratedRoles = insert
                r
                (EnumeratedRole (addToRoleRecord er))
                enumeratedRoles
                }

            CR (CalculatedRoleType r) -> do
              CalculatedRole cr <- getsDF (unsafePartial fromJust <<< lookup r <<< _.calculatedRoles)
              modifyDF \dfr@{calculatedRoles} -> dfr {calculatedRoles = insert
                r
                (CalculatedRole (addToRoleRecord cr))
                calculatedRoles
                }
          where
            addToRoleRecord :: forall f. {actions :: EM.EncodableMap StateSpec (Object Action) | f} -> {actions :: EM.EncodableMap StateSpec (Object Action) | f}
            addToRoleRecord r@{actions} = r {actions = foldl
              (\accumulatedActions nextState -> case EM.lookup nextState accumulatedActions of
                Nothing -> EM.insert nextState (singleton actionId theAction) accumulatedActions
                Just actionsObject -> EM.insert nextState (insert actionId theAction actionsObject) accumulatedActions
              )
              actions
              stateSpecs}

    -- | Modifies the DomeinFile in PhaseTwoState.
    modifyStateDependentPerspectives ::
      AST.StateSpecification ->
      (StateDependentPerspective -> StateDependentPerspective) ->
      Array RoleType ->
      QueryFunctionDescription ->
      ArcPosition ->
      ArcPosition ->
      StateIdentifier ->
      PhaseThree Unit
    modifyStateDependentPerspectives state modifier qualifiedUsers objectQfd  start end stateId = do
      -- Compute the currentcontext from the origin.
      ecurrentContextCalculation <- try case state of
        AST.ContextState ct _ -> pure $ SQD (CDOM $ UET ct) (DataTypeGetter IdentityF) (CDOM $ UET ct) True True
        AST.ObjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
        AST.SubjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
      case ecurrentContextCalculation of
        -- We probably cannot compute the current context from the roleIdentification, because it identifies
        -- an indexed role.
        Left _ -> pure unit
        Right currentContextCalculation ->
          modifyPartOfState
            start
            end
            (\(sr@{perspectivesOnEntry}) -> do
              extendedPerspectives <- foldM (addStateDependentPerspectiveForUser currentContextCalculation) perspectivesOnEntry qualifiedUsers
              pure $ sr
                { perspectivesOnEntry = extendedPerspectives
                , object = Just objectQfd
                })
            stateId
      where
        -- NOTE that the selfOnly prop is generated by default to be false.
        addStateDependentPerspectiveForUser ::
          QueryFunctionDescription ->
          EM.EncodableMap RoleType StateDependentPerspective ->
          RoleType ->
          PhaseThree (EM.EncodableMap RoleType StateDependentPerspective)
        addStateDependentPerspectiveForUser currentContextCalculation perspectivesOnEntry qualifiedUser = do
          isSelfPerspective <- (lift $ lift (qualifiedUser ###>> (unsafePartial isPerspectiveOnSelf objectQfd)))
          case EM.lookup qualifiedUser perspectivesOnEntry of
            Nothing -> pure $ EM.insert
              qualifiedUser
              case state of
                AST.ContextState _ _ -> modifier $ ContextPerspective
                  { properties: []
                  , selfOnly: false
                  , authorOnly: false
                  , isSelfPerspective
                  }
                otherwise -> modifier $ RolePerspective
                  { currentContextCalculation
                  , properties: []
                  , selfOnly: false
                  , authorOnly: false
                  , isSelfPerspective
                  }
              perspectivesOnEntry
            Just p@(ContextPerspective _) -> pure $ EM.insert
              qualifiedUser
              (modifier p)
              perspectivesOnEntry
            Just p@(RolePerspective _) -> pure $ EM.insert
              qualifiedUser
              (modifier p)
              perspectivesOnEntry

    -- Apply, for this user, the modifier to his perspective on the object (and create a perspective if necessary).
    -- | Modifies the DomeinFile in PhaseTwoState.
    modifyPerspective :: QueryFunctionDescription -> RoleIdentification -> ArcPosition -> (Perspective -> Perspective) -> RoleType -> PhaseThree Unit
    modifyPerspective objectQfd roleSpec start modifier userRole = do
      (roleTypes :: Array RoleType) <- collectRoles roleSpec
      displayName <- lift2 $ intercalate ", " <$> traverse displayNameOfRoleType roleTypes
      isSelfPerspective <- (lift $ lift (userRole ###>> (unsafePartial isPerspectiveOnSelf objectQfd)))
      case userRole of
        ENR (EnumeratedRoleType r) -> do
          EnumeratedRole er@{perspectives} <- getsDF (unsafePartial fromJust <<< lookup r <<< _.enumeratedRoles)
          mi <- pure $ findIndex (\(Perspective{object}) -> object == objectQfd) perspectives
          perspective <- case mi of
            Nothing -> do
              pure $ Perspective
                { id: (roletype2string userRole) <> "_" <> show (length perspectives)
                , object: objectQfd
                , isEnumerated: (isQFDofEnumeratedRole objectQfd)
                , displayName
                , roleTypes
                , roleVerbs: EM.empty
                , propertyVerbs: EM.empty
                , actions: EM.empty
                , selfOnly: false
                , authorOnly: false
                , isSelfPerspective
                , automaticStates: []
                }
            Just i -> pure (unsafePartial $ fromJust $ index perspectives i)
          modifyDF \dfr@{enumeratedRoles} -> dfr {enumeratedRoles = insert
            r
            (EnumeratedRole $ er {perspectives = case mi of
              Nothing -> cons (modifier perspective) perspectives
              Just i -> unsafePartial $ fromJust $ updateAt i (modifier perspective) perspectives })
            enumeratedRoles
            }
        CR (CalculatedRoleType r) -> do
          CalculatedRole er@{perspectives} <- getsDF (unsafePartial fromJust <<< lookup r <<< _.calculatedRoles)
          mi <- pure $ findIndex (\(Perspective{object}) -> object == objectQfd) perspectives
          perspective <- case mi of
            Nothing -> do
              -- mroleName <- lift2 $ roleIdentification2displayName roleSpec
              pure $ Perspective
                { id: (roletype2string userRole) <> "_" <> show (length perspectives)
                , object: objectQfd
                , isEnumerated: (isQFDofEnumeratedRole objectQfd)
                , displayName
                , roleTypes
                , roleVerbs: EM.empty
                , propertyVerbs: EM.empty
                , actions: EM.empty
                , selfOnly: false
                , authorOnly: false
                , isSelfPerspective
                , automaticStates: []
                }
            Just i -> pure (unsafePartial $ fromJust $ index perspectives i)
          modifyDF \dfr@{calculatedRoles} -> dfr {calculatedRoles = insert
            r
            (CalculatedRole $ er {perspectives = case mi of
              Nothing -> cons (modifier perspective) perspectives
              Just i -> unsafePartial $ fromJust $ updateAt i (modifier perspective) perspectives })
            calculatedRoles
            }

    -- | Modifies the DomeinFile in PhaseTwoState.
    -- | If the state is not in the Domain, adds a modification for an upstream DomeinFiloe.
    modifyPartOfState :: ArcPosition -> ArcPosition -> (StateRecord -> PhaseThree StateRecord) -> StateIdentifier -> PhaseThree Unit
    modifyPartOfState start end modifyState stateId = do
      mstate <- State.gets _.dfr >>= pure <<< lookup (unwrap stateId) <<< _.states
      case mstate of
        Nothing -> isContextRootState stateId >>= if _
          then do
            state' <- State <$> modifyState (unwrap $ constructState stateId (Q $ trueCondition (CDOM $ UET (ContextType (unwrap stateId)))) (Cnt (ContextType (unwrap stateId))) [])
            modifyDF \drf@{states} -> drf {states = insert (unwrap stateId) state' states}
          else isRoleRootState stateId >>= if _
            then do
              rk <- unsafePartial $ roleKind stateId
              -- We have established we can interpret stateId as a role root state, and thus also an EnumeratedRoleType.
              context <- lift2 $ enumeratedRoleContextType (EnumeratedRoleType $ unwrap stateId)
              state' <- State <$> modifyState (unwrap $ constructState
                stateId
                (Q $ trueCondition (RDOM (UET $ QT.RoleInContext{context, role: (EnumeratedRoleType (unwrap stateId))})))
                (case rk of
                  UserRole -> (Srole (EnumeratedRoleType (unwrap stateId)))
                  _ -> (Orole (EnumeratedRoleType (unwrap stateId)))) [])
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

        roleKind :: Partial => StateIdentifier -> PhaseThree RoleKind
        roleKind (StateIdentifier s) = State.gets _.dfr >>= \{enumeratedRoles} -> pure $ _.kindOfRole $ unwrap $ fromJust (lookup s enumeratedRoles)

isUpstreamModelState :: StateIdentifier -> PhaseThree Boolean
isUpstreamModelState (StateIdentifier s) = do
  thisModel <- State.gets (_.namespace <<< _.dfr)
  if s `startsWithSegments` thisModel
    then pure false
    else do
      upstreamModels <- State.gets (map unwrap <<< _.referredModels <<< _.dfr)      
      pure $ isJust $ find (startsWithSegments s) upstreamModels

objectMustBeRole :: Maybe QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree Unit
objectMustBeRole qfd start end = case range <$> qfd of
  Just (RDOM _) -> pure unit
  Nothing -> pure unit
  (Just r) -> throwError (NotARoleDomain r start end)

-- | Qualifies incomplete names and changes RoleType constructor to CalculatedRoleType if necessary.
-- | The role type name (parameter `rt`) is always fully qualified, EXCEPT
-- | for the current subject that holds in the body of `perspective of`.
-- | Result contains no double entries.
-- TODO. Nu ook voor perspective on als een enkele identifier is gebruikt!
collectRoles :: RoleIdentification -> PhaseThree (Array RoleType)
-- A single role type will result from this case, but it may be a calculated role!
collectRoles (ExplicitRole ctxt rt pos) = do
  maximallyQualifiedName <- if isTypeUri (roletype2string rt)
    then pure (roletype2string rt)
    else pure $ concatenateSegments (unwrap ctxt) (roletype2string rt)
  r <- qualifyLocalRoleName pos maximallyQualifiedName
  pure [r]
-- Compile the expression s with respect to context ctxt.
-- This case MUST represent the current object that holds in the body of `perspective on`. Multiple Enumerated role types can result from this case.
collectRoles (ImplicitRole ctxt s) = compileExpression (CDOM (UET ctxt)) s >>= \qfd ->
  case range qfd of
    RDOM adt -> pure $ nub $ map ENR (allLeavesInADT $ roleInContext2Role <$> adt)
    otherwise -> throwError $ NotARoleDomain otherwise (startOf s) (endOf s)

-- We lookup the qualified name of these properties here, for the object of the perspective.
-- The (partial) names for properties used here may be defined outside
-- of the model (due to role filling). So we use functions that rely on the
-- model cache and hence we need the current model to be in that cache, too.
-- Hence the Partial constraint.
collectPropertyTypes :: Partial =>
  AST.PropsOrView ->
  RoleIdentification ->
  ArcPosition ->
  PhaseThree (ExplicitSet PropertyType)
collectPropertyTypes AST.AllProperties _ _ = pure Universal
collectPropertyTypes (AST.Properties ps) object start = do
  roleADT <- roleIdentification2rangeADT object
  PSet <$> for (fromFoldable ps)
    \localPropertyName -> do
      candidates <- lift2 (roleADT ###= lookForUnqualifiedPropertyType localPropertyName)
      case head candidates of
        Nothing -> throwError $ UnknownProperty start localPropertyName (show roleADT)
        (Just t) | length candidates == 1 -> pure t
        _ -> throwError $ NotUniquelyIdentifying start localPropertyName (propertytype2string <$> candidates)

collectPropertyTypes (AST.View view) object start = do
  if isTypeUri view
    then do
      mview <- lift2 $ tryGetPerspectType (ViewType view)
      case mview of
        Just (View {propertyReferences}) -> pure $ PSet propertyReferences
        Nothing -> throwError $ UnknownView start view
    else do
      -- If the RoleIdentification is of a single role type (not an expression), add that role
      -- to the types we get from expanding the role specification as an expression.
      -- This causes a calculated role type to be included along with the types of its range.
      -- It does not matter if that added role is still described as Enumerated while it is actually Calculated.
      roles <- map ENR <$> (allLeavesInADT <$> roleIdentification2rangeADT object)
      roles' <- case object of
        ExplicitRole (ContextType ctxt) r pos -> case r of 
          ENR (EnumeratedRoleType er) -> pure $ nub $ cons (ENR $ EnumeratedRoleType (qualifyWith ctxt er)) roles
          CR (CalculatedRoleType er) -> pure $ nub $ cons (CR $ CalculatedRoleType (qualifyWith ctxt er)) roles
        _ -> pure roles
      (views :: Object View) <- getsDF _.views
      -- As we have postponed handling these parse tree fragments after
      -- handling all others, there can be no forward references.
      -- The property references in Views are, by now, qualified.
      case filter (areLastSegmentsOf view) (keys views) of
        noCandidates | null noCandidates -> throwError $ UnknownView start view
        candidates -> case filter (isViewOfObject roles') candidates of
          noCandidates' | null noCandidates' -> throwError $ NotAViewOfObject start view
          candidates' ->
            case length candidates' of
              1 -> unsafePartial case lookup (unsafePartial ARRP.head candidates') views of
                Just (View {propertyReferences}) -> pure $ PSet propertyReferences
              _ -> throwError $ NotUniquelyIdentifying start view candidates'
  where
    isViewOfObject :: Array RoleType -> String -> Boolean
    -- | "Context" `isLocalNameOf` "model:Perspectives$Context"
    isViewOfObject roles viewName = isJust $ findIndex (\rType -> viewName `startsWithSegments` (roletype2string rType)) roles

handleScreens :: LIST.List AST.ScreenE -> PhaseThree Unit
handleScreens screenEs = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    handleScreens'
  where
    handleScreens' :: PhaseThree Unit
    handleScreens' = do
      screenDefs <- foldM screenDefinition EM.empty (fromFoldable screenEs)
      modifyDF \dfr -> dfr {screens = screenDefs}

    -- `screenDefMap` is the accumulating map of screens.
    -- This function adds the ScreenDefinition that we construct from
    -- the ScreenE to that map.
    screenDefinition :: ScreenMap -> AST.ScreenE -> PhaseThree ScreenMap
    screenDefinition screenDefMap (AST.ScreenE{title, tabs, rows, columns, subject, context, start, end}) = do
      -- Add the ScreenDef for each of these roles.
      -- By construction, the subjects are represented with an
      -- RoleIdentification.ExplicitRole data constructor.
      -- This means that a single Enumerated or Calculated role results.
      -- `collectRoles` will throw an error if it fails, so here we are guaranteed
      -- to have a RoleType.
      subjectRoleTypes <- collectRoles subject
      screenDefinition' (unsafePartial ARRP.head subjectRoleTypes)

      where
        -- This is how we get `subjectRoleType` in scope for `widgetCommonFields`.
        screenDefinition' :: RoleType -> PhaseThree ScreenMap
        screenDefinition' subjectRoleType = do
          (tabs' :: Maybe (LIST.List TabDef)) <- case tabs of
            Nothing -> pure Nothing
            Just ts -> Just <$> traverse tab ts
          (rows' :: Maybe (LIST.List ScreenElementDef)) <- case rows of
            Nothing -> pure Nothing
            Just rs -> Just <$> traverse row rs
          columns' <- case columns of
            Nothing -> pure Nothing
            Just cs -> Just <$> traverse column cs
          screenDef <- pure $ ScreenDefinition
            { title
            , tabs: fromFoldable <$> tabs'
            , rows: fromFoldable <$> rows'
            , columns: fromFoldable <$> columns'
            , whoWhatWhereScreen: Nothing
            }
          pure $ EM.insert (ScreenKey context subjectRoleType) screenDef screenDefMap

          where
            tab :: AST.TabE -> PhaseThree TabDef
            tab (AST.TabE tabTitle isDefault screenElements) = do
              screenElementDefs <- traverse screenElementDef screenElements
              pure $ TabDef {title: tabTitle, isDefault, elements: (fromFoldable screenElementDefs)}

            row :: AST.RowE -> PhaseThree ScreenElementDef
            row (AST.RowE screenElements) = do
              screenElementDefs <- traverse screenElementDef screenElements
              pure $ RowElementD $ RowDef (fromFoldable screenElementDefs)

            column :: AST.ColumnE -> PhaseThree ScreenElementDef
            column (AST.ColumnE screenElements) = do
              screenElementDefs <- traverse screenElementDef screenElements
              pure $ ColumnElementD $ ColumnDef (fromFoldable screenElementDefs)

            screenElementDef :: AST.ScreenElement -> PhaseThree ScreenElementDef
            screenElementDef (AST.RowElement rowE) = row rowE
            screenElementDef (AST.ColumnElement colE) = column colE
            screenElementDef (AST.TableElement tableE) = TableElementD <$> table tableE
            screenElementDef (AST.FormElement formE) = FormElementD <$> form formE
            screenElementDef (AST.MarkDownElement markdownE) = MarkDownElementD <$> markdown markdownE
            screenElementDef (AST.ChatElement chatE) = ChatElementD <$> chat chatE

            functionalWidget :: ThreeValuedLogic
            functionalWidget = True

            relationalWidget :: ThreeValuedLogic
            relationalWidget = False

            table :: AST.TableE -> PhaseThree TableDef
            table (AST.TableE fields) = TableDef <$> widgetCommonFields fields relationalWidget

            form :: AST.FormE -> PhaseThree FormDef
            form (AST.FormE fields) = FormDef <$> widgetCommonFields fields functionalWidget

            markdown :: AST.MarkDownE -> PhaseThree MarkDownDef
            markdown (MarkDownConstant { text, condition, context:ctxt}) = do
              text' <- unsafePartial case text of 
                Simple (Value _ _ t) -> pure t
              condition' <- traverse (compileStep (CDOM $ ST ctxt)) condition
              pure $ MarkDownConstantDef {text: text', condition: condition', domain: unsafePartial typeUri2ModelUri_ $ unwrap ctxt} 
            markdown (MarkDownPerspective {widgetFields, condition, start:s, end:e}) = do
              case condition of 
                Nothing -> do 
                  widgetFields' <- widgetCommonFields widgetFields Unknown
                  pure $ MarkDownPerspectiveDef {widgetFields: widgetFields', conditionProperty: Nothing}
                Just conditionProp -> do 
                  (objectRoleType :: RoleType) <- unsafePartial ARRP.head <$> collectRoles widgetFields.perspective
                  mconditionProperty <- lift $ lift (objectRoleType ###> (lookForUnqualifiedPropertyType_ conditionProp))
                  case mconditionProperty of 
                    -- The condition property cannot be recognised as a property of the role with the markdown property.
                    Nothing -> throwError (UnknownMarkDownConditionProperty s e conditionProp objectRoleType)
                    Just conditionProperty -> do 
                      -- add the conditionProperty to the widgetFields!
                      widgetFields' <- widgetCommonFields widgetFields Unknown
                      pure $ MarkDownPerspectiveDef {widgetFields: widgetFields' {propertyVerbs = (flip (unsafePartial addProperty) conditionProperty) <$> widgetFields'.propertyVerbs}, conditionProperty: Just conditionProperty}
            markdown (MarkDownExpression {text, condition, context:ctxt, start:start', end:end'}) = do
              text' <- compileStep (CDOM $ ST ctxt) text
              -- The resulting QueryFunctionDescription should be functional.
              if functional text' `eq` True 
                then do
                  condition' <- traverse (compileStep (CDOM $ ST ctxt)) condition
                  pure $ MarkDownExpressionDef {textQuery: text', condition: condition', text: Nothing}
                else throwError (MarkDownExpressionMustBeFunctional start' end')
            
            chat :: ChatE -> PhaseThree ChatDef
            chat (ChatE {chatRole, messagesProperty, mediaProperty, start:start', end:end'}) = do 
              (chatRoleType :: RoleType) <- unsafePartial ARRP.head <$> collectRoles chatRole
              qualifiedMessageProperty <- qualifyProperty chatRoleType messagesProperty
              qualifiedMediaProperty <- qualifyProperty chatRoleType mediaProperty
              pure $ ChatDef {chatRole: chatRoleType, chatInstance: Nothing, messageProperty: qualifiedMessageProperty, mediaProperty: qualifiedMediaProperty}

              where 
              qualifyProperty ::  RoleType -> String -> PhaseThree EnumeratedPropertyType
              qualifyProperty chatRoleType prop = do 
                candidates <- lift2 (chatRoleType ###= lookForUnqualifiedPropertyType_ prop )
                case head candidates of
                  Nothing -> throwError $ UnknownProperty start' prop (roletype2string chatRoleType)
                  (Just t) | length candidates == 1 -> case t of
                    ENP p -> pure p
                    CP p -> throwError $ PropertyCannotBeCalculated prop start' end'
                  otherwise -> throwError $ NotUniquelyIdentifying start' prop (map propertytype2string candidates)


            widgetCommonFields :: AST.WidgetCommonFields -> ThreeValuedLogic -> PhaseThree WidgetCommonFieldsDef
            widgetCommonFields {title:title', perspective, propsOrView, propertyVerbs, roleVerbs, start:start', end:end'} isFunctionalWidget = do
              -- From a RoleIdentification that represents the object,
              -- find the relevant Perspective.
              -- A ScreenElement can only be defined for a named Enumerated or Calculated Role. This means that `perspective` is constructed with the
              -- RoleIdentification.ExplicitRole data constructor: a single RoleType.
              -- If no role can be found for the given specification, collectRoles throws an error.
              (objectRoleType :: RoleType) <- unsafePartial ARRP.head <$> collectRoles perspective
              -- Check the Cardinality
              (lift2 $ roleTypeIsFunctional objectRoleType) >>= if _
                -- object is functional
                then if optimistic isFunctionalWidget 
                  -- we're ok with either True or Unknown (MarkDownPerspectiveDef).
                  then pure unit
                  -- but not with False.
                  else throwError (WidgetCardinalityMismatch start' end')
                -- object is relational
                else if not $ pessimistic isFunctionalWidget
                  -- we're ok with either True or Unknown (MarkDownPerspectiveDef).
                  then pure unit
                  -- but not with False
                  else throwError (WidgetCardinalityMismatch start' end')
              -- All properties defined on this object role.
              allProps <- lift2 ((roleADTOfRoleType objectRoleType >>= allProperties <<< map roleInContext2Role))
              -- The user must have a perspective on it. This perspective must have that RoleType
              -- in its member roleTypes.
              -- So we fetch the user role, get its Perspectives, and find the one that refers to the objectRoleType.
              perspectives <- lift2 $ perspectivesOfRoleType subjectRoleType
              case find (\(Perspective{roleTypes}) -> isJust $ elemIndex objectRoleType roleTypes) perspectives of
                -- This case is probably that the object and user exist, but the latter
                -- has no perspective on the former!
                Nothing -> throwError (UserHasNoPerspective subjectRoleType objectRoleType start' end')
                Just pspve@(Perspective{id:perspectiveId}) -> do
                  if perspectiveSupportsRoleVerbs pspve (maybe [] roleVerbList2Verbs roleVerbs)
                    then pure unit
                    else throwError (UnauthorizedForRole "Auteur" subjectRoleType objectRoleType (maybe [] roleVerbList2Verbs roleVerbs) (Just start') (Just end'))
                  case propsOrView, propertyVerbs of
                    -- The modeller has provided no restrictions.
                    AST.AllProperties, Universal -> pure
                      { title:title'
                      , perspectiveId
                      , perspective: Nothing
                      , propertyVerbs: Nothing
                      , roleVerbs: maybe Nothing (Just <<< roleVerbList2Verbs) roleVerbs
                      , userRole: subjectRoleType
                      }
                    pOrV, pVerbs -> do
                      (propertyTypes :: ExplicitSet PropertyType) <- unsafePartial collectPropertyTypes pOrV perspective start'
                      checkVerbsAndProps allProps propertyTypes (expandVerbs pVerbs) pspve objectRoleType
                      pure
                        { title:title'
                        , perspectiveId
                        , perspective: Nothing
                        , propertyVerbs: Just $ PropertyVerbs propertyTypes pVerbs
                        , roleVerbs: maybe Nothing (Just <<< roleVerbList2Verbs) roleVerbs
                        , userRole: subjectRoleType
                        }
              where 
                checkVerbsAndProps :: Array PropertyType -> ExplicitSet PropertyType -> Array PropertyVerb -> Perspective -> RoleType -> PhaseThree Unit
                checkVerbsAndProps allProps requiredProps propertyVerbs' perspective' objectRoleType = for_ (expandPropSet allProps requiredProps)
                  \requiredProp -> for propertyVerbs' \requiredVerb ->
                    if perspectiveSupportsPropertyForVerb perspective' requiredProp requiredVerb
                      then pure unit
                      else throwError (UnauthorizedForProperty "Auteur" subjectRoleType objectRoleType requiredProp requiredVerb (Just start') (Just end'))

addUserRoleGraph :: PhaseThree Unit
addUserRoleGraph = do
  df@{id} <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df)
    do
      ugraph <- buildUserGraph
      modifyDF \dfr -> dfr {userGraph = ugraph}

checkSynchronization :: PhaseThree Unit
checkSynchronization = do
  df@{id} <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df)
    SYNC.checkSynchronization

invertPerspectiveObjects :: PhaseThree Unit
invertPerspectiveObjects = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (addInvertedQueries' df)
  where
    addInvertedQueries' :: DomeinFileRecord -> PhaseThree Unit
    addInvertedQueries' {enumeratedRoles, calculatedRoles} = do
      traverse_ perspectivesInEnumeratedRole enumeratedRoles
      traverse_ perspectivesInCalculatedRole calculatedRoles

      where
        perspectivesInEnumeratedRole :: EnumeratedRole -> PhaseThree Unit
        perspectivesInEnumeratedRole (EnumeratedRole{id, perspectives}) = for_ (filter perspectiveMustBeSynchronized perspectives) (addInvertedQueriesForPerspectiveObject (ENR id))

        perspectivesInCalculatedRole :: CalculatedRole -> PhaseThree Unit
        perspectivesInCalculatedRole (CalculatedRole{id, perspectives, kindOfRole}) = if kindOfRole == Public 
          then pure unit 
          else for_ (filter perspectiveMustBeSynchronized perspectives) (addInvertedQueriesForPerspectiveObject (CR id))

        addInvertedQueriesForPerspectiveObject :: RoleType -> Perspective -> PhaseThree Unit
        addInvertedQueriesForPerspectiveObject roleType p@(Perspective {object, propertyVerbs, selfOnly, authorOnly}) = do
          -- Sets the inverted queries directly in the EnumeratedRoles and Properties in the
          -- DomeinFile we keep in PhaseTwoState.
          sPerProp <- lift2 $ statesPerProperty p
          runReaderT
            (setInvertedQueries [roleType] sPerProp ((roleStates p) `union` (automaticStates p) `union` (actionStates p) `union` (stateSpec2StateIdentifier <$> (fromFoldable $ EM.keys propertyVerbs))) object selfOnly authorOnly)
            (unsafePartial createModificationSummary p)

        explicitSet2RelevantProperties :: ExplicitSet PropertyType -> RelevantProperties
        explicitSet2RelevantProperties Universal = All
        explicitSet2RelevantProperties Empty = Properties []
        explicitSet2RelevantProperties (PSet ps) = Properties ps

-- Compile a RoleIdentification to a sequence of bindings for the standard
-- variables `currentcontext` and `origin`.
roleIdentificationToQueryFunctionDescription :: RoleIdentification -> ArcPosition -> PhaseThree QueryFunctionDescription
roleIdentificationToQueryFunctionDescription roleIdentification pos = do
  syntacticObjectWithEnvironment <- pure $ addContextualBindingsToExpression
    [ makeIdentityStep "currentcontext" pos
    , makeIdentityStep "origin" pos]
    (roleIdentification2Step roleIdentification)
    -- Make a QueryFunctionDescription of a function that computes the object.
  withFrame
    (compileExpression
      (CDOM $ UET (roleIdentification2Context roleIdentification))
      syntacticObjectWithEnvironment)


computeCurrentContextFromRoleIdentification :: RoleIdentification -> ArcPosition -> PhaseThree QueryFunctionDescription
computeCurrentContextFromRoleIdentification roleIdentification pos = do
  compiledObject <- roleIdentificationToQueryFunctionDescription roleIdentification pos
  -- Expand Calculated roles to their calculation.
  expandedCompiledObject <- traverseQfd (\qfd -> case qfd of 
    SQD dom (RolGetter (CR r)) ran _ _ -> lift2 $ (getRole >=> getCalculation) (CR r)
    other -> pure other) compiledObject
  -- First simplify by removing WithFrame:
  reducedObject <- pure $ unwrap $ traverseQfd (\qfd -> case qfd of 
    (UQD _ WithFrame qfd1 _ _ _) -> Identity.Identity qfd1
    other -> Identity.Identity other) expandedCompiledObject
  (contextCalculations :: (Array QueryFunctionDescription)) <- completeInversions reducedObject
  case joinQfds contextCalculations of
    Nothing -> throwError (Custom $ "It is not possible to compute the current context in position " <> show pos <> " (is `origin` an indexed role or context?). Change current state at this position, for example by using `in object|subject|context state`." <> " Information for programmers: function computeCurrentContextFromRoleIdentification with compiledObject = " <> prettyPrint compiledObject)
    Just result -> pure result
  where
    joinQfds :: Array QueryFunctionDescription -> Maybe QueryFunctionDescription
    joinQfds contextCalculations = case uncons contextCalculations of
      Just {head, tail} -> Just (foldl makeUnion head tail)
      Nothing -> Nothing

    makeUnion :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
    makeUnion f1 f2 = BQD (domain f1) (BinaryCombinator UnionF) f1 f2 (unsafePartial $ fromJust $ sumOfDomains (range f1)(range f2)) False (and (mandatory f1)(mandatory f2))

computeOrigin :: AST.StateSpecification -> ArcPosition -> VarBinding
computeOrigin sp pos = case sp of
  AST.ContextState _ _ -> makeIdentityStep "origin" pos
  AST.SubjectState _ _ -> makeIdentityStep "origin" pos
  AST.ObjectState _ _ -> makeIdentityStep "origin" pos

-- The current context must be computed in runtime from the
-- object that changes state. Here we must construct a VarBinding
-- with a Step that the StatementCompiler will construct a
-- QueryFunctionDescription for with the right range. It does not
-- actually have to compute anything, since the unsafeCompiler will
-- remove these VarBindings (the actual values are computed by the core).
computeCurrentContext :: AST.StateSpecification -> ArcPosition -> VarBinding
computeCurrentContext t pos = case t of
  AST.ContextState _ _ -> makeIdentityStep "currentcontext" pos
  AST.SubjectState roleIdentification _ -> makeTypeTimeOnlyContextStep "currentcontext"
    (roleIdentification2Context roleIdentification) pos
  AST.ObjectState roleIdentification _ -> makeTypeTimeOnlyContextStep "currentcontext"
    (roleIdentification2Context roleIdentification) pos


-- | Compile any state queries that are not yet compiled. These will be queries of states without any
-- | automatic action or notification.
compileStateQueries :: PhaseThree Unit
compileStateQueries = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (compileStateQueries' df)
  where
    compileStateQueries' :: DomeinFileRecord -> PhaseThree Unit
    compileStateQueries' df@{states} = do
      states' <- for states ensureStateQueryCompiled
      modifyDF \dfr -> dfr { states = states' }
      where
        ensureStateQueryCompiled :: State -> PhaseThree State
        ensureStateQueryCompiled s@(State sr@{id, query, stateFulObject}) = do
          -- Compile the query if we've not done it before.
          compiledQuery <- case query of
            Q q -> pure $ Q q
            S stp _ -> do
              -- currentContext <- case stateFulObject of
              --   Cnt ctype -> pure ctype
              --   Srole rtype -> lift2 ((getEnumeratedRole >=> pure <<< contextOfRepresentation) rtype)
              --   Orole rtype -> lift2 ((getEnumeratedRole >=> pure <<< contextOfRepresentation) rtype)
              expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                [ (case stateFulObject of
                    Cnt _ -> makeIdentityStep "currentcontext" (startOf stp)
                    Srole _ -> makeContextStep "currentcontext" (startOf stp)
                    Orole _ -> makeContextStep "currentcontext" (startOf stp))
                , makeIdentityStep "origin" (startOf stp)]
                stp
              roleInContext <- stateFulObject2Domain stateFulObject
              Q <$> compileAndDistributeStep
                -- The state query (condition) is lexically embedded in the state definition, which is
                -- lexically embedded in a role definition if we have an Orole or Srole StateFulObject,
                -- which is lexically embedded in a context definition.
                -- Hence, we can conclude that for Orole and Srole, the RDOM (ADT RoleInContext) can be
                -- safely formed from the lexical context of the EnumeratedRoleType in the StateFulObject.
                roleInContext
                expressionWithEnvironment
                [id]
          pure $ State sr { query = compiledQuery }

-- True, if sub adds a single segment to super.
-- sub `isDirectSubstateOf` super
-- model:System$PerspectivesSystem$Root `isDirectSubStateOf` model:System$PerspectivesSystem
isDirectSubstateOf :: Array String -> String -> String -> Boolean
isDirectSubstateOf enumeratedRoleNames sub super = maybe false (\ns -> ns `eq` super && (isNothing $ elemIndex sub enumeratedRoleNames)) (typeUri2typeNameSpace sub)
-- The requirement that `sub` is not an EnumeratedRoleType, prevents root states of roles to
-- be interpreted as named substates of contexts.

-- sub `isDirectSuperStateOf` super
-- model:System$PerspectivesSystem `isDirectSuperStateOf` model:System$PerspectivesSystem$Root
isDirectSuperStateOf :: Array String -> String -> String -> Boolean
isDirectSuperStateOf enumeratedRoleNames super sub = isDirectSubstateOf enumeratedRoleNames sub super


transition2stateSpec :: StateTransitionE -> AST.StateSpecification
transition2stateSpec (Entry s) = s
transition2stateSpec (Exit s) = s

-- | Returns the ContextType that represents the current context for the
-- | lexical position of the state.
stateSpec2ContextType :: AST.StateSpecification -> ContextType
stateSpec2ContextType (AST.ContextState c _) = c
stateSpec2ContextType (AST.SubjectState (ExplicitRole c _ _) _) = c
stateSpec2ContextType (AST.SubjectState (ImplicitRole c _) _) = c
stateSpec2ContextType (AST.ObjectState (ExplicitRole c _ _) _) = c
stateSpec2ContextType (AST.ObjectState (ImplicitRole c _) _) = c

-- A ContextState is mapped to a CDOM.
-- A SubjectState and ObjectState are determined from their RoleIdentification.
-- We turn the RoleIdentification in a step, compile that with CompileStep and take its Range.
-- | A domain that represents the type of the resource that changes state: the origin.
statespec2Domain :: Partial => AST.StateSpecification -> PhaseThree Domain
statespec2Domain (AST.ContextState ctype _) = pure $ CDOM (UET ctype)
statespec2Domain (AST.SubjectState roleIdentification _) = range <$> compileStep (CDOM (UET (roleIdentification2Context roleIdentification))) (roleIdentification2Step roleIdentification)
statespec2Domain (AST.ObjectState roleIdentification _) = range <$> compileStep (CDOM (UET (roleIdentification2Context roleIdentification))) (roleIdentification2Step roleIdentification)

roleIdentification2rangeADT :: RoleIdentification -> PhaseThree (ADT EnumeratedRoleType)
roleIdentification2rangeADT roleIdentification = map roleInContext2Role <$> unsafePartial domain2roleType <<< range <$> compileStep
  (CDOM (UET (roleIdentification2Context roleIdentification)))
  (roleIdentification2Step roleIdentification)

-- | Returns a Step that represents an expression that should be evaluated
-- | with respect to the current context (the compiled function should be applied
-- | to an instance of the current context).
roleIdentification2Step :: RoleIdentification -> Step
roleIdentification2Step (ExplicitRole ctxt (ENR (EnumeratedRoleType rt)) pos) = Simple $ ArcIdentifier pos rt
roleIdentification2Step (ExplicitRole ctxt (CR (CalculatedRoleType rt)) pos) = Simple $ ArcIdentifier pos rt
roleIdentification2Step (ImplicitRole ctxt stp) = stp

roleIdentificationIsEnumerated :: RoleIdentification -> Boolean
roleIdentificationIsEnumerated (ExplicitRole _ (ENR _) _) = true
roleIdentificationIsEnumerated _ = false

isQFDofEnumeratedRole :: QueryFunctionDescription -> Boolean
isQFDofEnumeratedRole (SQD _ (DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF _) _ _ _) = true
isQFDofEnumeratedRole (SQD _ (RolGetter rt) _ _ _) = case rt of
  ENR _ -> true
  CR _ -> false
isQFDofEnumeratedRole _ = false

-- | Returns the current context for the RoleIdentification.
-- | This is, for the lexical position of the current subject or object (for which the
-- | RoleIdentification was constructed), the current context.
roleIdentification2Context :: RoleIdentification -> ContextType
roleIdentification2Context (ExplicitRole ctxt _ _) = ctxt
roleIdentification2Context (ImplicitRole ctxt _) = ctxt

roleIdentification2displayName :: RoleIdentification -> MonadPerspectives (Maybe String)
roleIdentification2displayName (ImplicitRole _ _) = pure Nothing
roleIdentification2displayName (ExplicitRole _ (ENR rt) _) = getEnumeratedRole rt >>= pure <<< Just <<< displayName
roleIdentification2displayName (ExplicitRole _ (CR rt) _) = getCalculatedRole rt >>= pure <<< Just <<< displayName

roleIdentification2TypeName :: RoleIdentification -> (Maybe RoleType)
roleIdentification2TypeName (ImplicitRole _ _) = Nothing
roleIdentification2TypeName (ExplicitRole _ rt@(ENR _) _) = Just rt
roleIdentification2TypeName (ExplicitRole _ rt@(CR _) _) = Just rt

-- A QueryFunctionDescription that will compile to const true.
trueCondition :: Domain -> QueryFunctionDescription
trueCondition dom = SQD dom (Constant PBool "true") (VDOM PBool Nothing) True True

-- | The function roleIdentification2Step produces a simple ArcIdentifier step for Explicit roles.
-- | When we want to construct a QueryFunctionDescription that retrieves that role from the role itself,
-- | the identity function suffices. With a Context domain we should construct a role getter.
adaptRoleStepToDomain :: Domain -> Step -> Step
adaptRoleStepToDomain (CDOM _) stp = stp
-- adaptRoleStepToDomain (RDOM _) (Simple (ArcIdentifier pos _)) = Simple $ Identity pos
adaptRoleStepToDomain _ stp = stp

stateFulObject2Domain :: StateFulObject -> PhaseThree Domain
stateFulObject2Domain (Cnt ctxt) = pure $ CDOM (UET ctxt)
stateFulObject2Domain (Orole role) = do
  EnumeratedRole{context} <- lift2 $ getEnumeratedRole role
  pure $ RDOM (UET (QT.RoleInContext{context, role}))
stateFulObject2Domain (Srole role) = do
  EnumeratedRole{context} <- lift2 $ getEnumeratedRole role
  pure $ RDOM (UET (QT.RoleInContext{context, role}))

-- True, iff the identifier is that of a EnumeratedRole
isEnumeratedRoleState :: StateIdentifier -> PhaseThree Boolean
isEnumeratedRoleState (StateIdentifier s) = do
  eroles <- State.gets _.dfr.enumeratedRoles
  pure $ isJust $ findIndex (\erole -> isJust $ indexOf (Pattern erole) s) (keys eroles)

-- TODO: denk aan prefix expansie in het SegmentedPath!
-- If the second part of the StateIdentification is a qualified name, it is the combination of some
-- base type and a segmented path identifying a state for that type.
-- The first part of the StateIdentification identifies some type. The State base type should be compatible with that.
-- ContextState provides the simplest case: here the first part is a ContextType, of which one of the types of its
-- aspect closure should be the base state type.
-- Throws an error if not.
isCoherentStateSpecification :: AST.StateSpecification -> PhaseThree Unit
isCoherentStateSpecification stateSpec = case stateSpec of 
  AST.ContextState cType mident -> case mident of 
    Just ident ->  if isTypeUri ident
      then do
        aspects <- lift2 ((ContextType ident) ###= contextAspectsClosure)
        if isJust $ find (\aspect -> ident `startsWithSegments` (unwrap aspect)) aspects
          then pure unit
          else throwError (Custom "bla")
      else pure unit
    _ -> pure unit
  -- TODO 
  _ ->  pure unit

----------------------------------------------------------------------------------------
------- COMPUTECOMPLETEENUMERATEDTYPES
----------------------------------------------------------------------------------------
-- | Not to be executed before all EnumeratedRoleTypes have been fully constructed.
computeCompleteEnumeratedTypes :: PhaseThree Unit
computeCompleteEnumeratedTypes = do
  df@{id, enumeratedRoles} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (computeCompleteEnumeratedTypes' enumeratedRoles)

  where
    computeCompleteEnumeratedTypes' :: Object EnumeratedRole ->  PhaseThree Unit
    computeCompleteEnumeratedTypes' enumeratedRoles = do
      enumeratedRoles' <- for enumeratedRoles computeCompleteEnumeratedType
      modifyDF \dfr -> dfr {enumeratedRoles = enumeratedRoles'}

    computeCompleteEnumeratedType :: EnumeratedRole -> PhaseThree EnumeratedRole
    computeCompleteEnumeratedType erole@(EnumeratedRole erec) = do
      normalizedType <- lift $ lift (completeExpandedType erole >>= pure <<< toConjunctiveNormalForm)
      pure (EnumeratedRole erec {completeType = normalizedType})

