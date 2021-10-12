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
import Data.Array (catMaybes, cons, elemIndex, filter, findIndex, foldl, foldr, fromFoldable, head, index, length, uncons, updateAt)
import Data.Array.Partial (head) as ARRP
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.List (List)
import Data.Map (Map, empty, insert, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert, keys, lookup, singleton, unions, values, union)
import Foreign.Object (fromFoldable) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MP, MonadPerspectives, (###=))
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..), DomeinFileRecord, indexedContexts, indexedRoles)
import Perspectives.Identifiers (Namespace, concatenateSegments, deconstructNamespace, endsWithSegments, isQualifiedWithDomein)
import Perspectives.InvertedQuery (RelevantProperties(..))
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), NotificationE(..), PropertyVerbE(..), PropsOrView(..), RoleVerbE(..), SelfOnly(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..)) as AST
import Perspectives.Parsing.Arc.AST (RoleIdentification(..), SegmentedPath, StateTransitionE(..))
import Perspectives.Parsing.Arc.ContextualVariables (addContextualBindingsToExpression, addContextualBindingsToStatements, makeContextStep, makeIdentityStep, makeTypeTimeOnlyContextStep, makeTypeTimeOnlyRoleStep)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..), VarBinding)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, lift2, modifyDF, runPhaseTwo_', withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition, arcParserStartPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.ExpressionCompiler (compileAndDistributeStep, compileAndSaveProperty, compileAndSaveRole, compileExpression, compileStep, qualifyLocalEnumeratedRoleName, qualifyLocalRoleName)
import Perspectives.Query.Kinked (completeInversions, setInvertedQueries)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain, domain2roleType, mandatory, range, sumOfDomains)
import Perspectives.Query.StatementCompiler (compileStatement)
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getCalculatedProperty, getCalculatedRole, getEnumeratedRole)
import Perspectives.Representation.Class.Role (Role(..), displayName, displayNameOfRoleType, getRole, getRoleType, roleADT)
import Perspectives.Representation.Context (Context(..)) as REP
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.Sentence (Sentence(..), SentencePart(..)) as Sentence
import Perspectives.Representation.State (Notification(..), State(..), StateFulObject(..), StateRecord, constructState)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), propertytype2string, roletype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType, lookForUnqualifiedPropertyType_, roleStates, statesPerProperty)
import Perspectives.Utilities (prettyPrint)
import Prelude (class Ord, Unit, append, bind, discard, eq, flip, join, map, pure, show, unit, void, ($), (&&), (<#>), (<$>), (<*), (<<<), (==), (>=>), (>>=), (<>))

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
      qualifyStateNames
      compileCalculatedRolesAndProperties
      requalifyBindingsToCalculatedRoles
      qualifyPropertyReferences
      handlePostponedStateQualifiedParts
      createMissingRootStates
      compileStateQueries
      registerStates
      invertPerspectiveObjects
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
            -- We continue an intentional semantic error introduced while parsing here by attempting to qualify the binding, that we know not
            -- to be an EnumeratedRole, as an EnumeratedRole. However, with requalifyBindingsToCalculatedRoles we will
            -- correct that error. We cannot do otherwise because at this state we don't have compiled the expressions
            -- of the CalculatedRoles yet.
            -- If not found in the EnumeratedRoles, try the CalculatedRoles
            Left (UnknownRole _ _) -> do
              q' <- try $ ST <$> qualifyLocalEnumeratedRoleName pos ident (keys croles)
              case q' of
                -- If we cannot find the identifier in the Calculated roles, it may be in another namespace.
                Left (UnknownRole _ _) -> do
                  if isQualifiedWithDomein ident
                    then do
                      rl <- lift $ lift $ getRoleType ident >>= getRole
                      case rl of
                        E (EnumeratedRole{_id}) -> pure $ ST _id
                        C r -> lift2 $ roleADT r
                    else throwError $ NotWellFormedName pos ident
                Left e -> throwError e
                Right adt -> pure adt
            Left e -> throwError e
            Right adt -> pure adt

-- | States that are constructed out of a SubjectState StateSpecification may have an unqualified name because
-- | an ExplicitRole RoleIdentification may have a RoleType that is not fully qualified.
qualifyStateNames :: PhaseThree Unit
qualifyStateNames = (lift $ State.gets _.dfr) >>= qualifyStateNames'
  where
    qualifyStateNames' :: DomeinFileRecord -> PhaseThree Unit
    qualifyStateNames' {states} = for states (\(State sr@{id}) -> do
      qid <- if isQualifiedWithDomein (unwrap id)
        then pure $ unwrap id
        -- Note that the validity of this depends on the unqualfied name being a reference to a role!
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

        qualifyProperty :: RoleType -> ArcPosition -> PropertyType -> PhaseThree PropertyType
        qualifyProperty rtype pos propType = do
          if isQualifiedWithDomein (propertytype2string propType)
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

    -- | Qualifies incomplete names and changes RoleType constructor to CalculatedRoleType if necessary.
    -- | The role type name (parameter `rt`) is always fully qualified, EXCEPT
    -- | for the current subject that holds in the body of `perspective of`.
    -- TODO. Nu ook voor perspective on als een enkele identifier is gebruikt!
    collectRoles :: RoleIdentification -> PhaseThree (Array RoleType)
    -- A single role type will result from this case, but it may be a calculated role!
    collectRoles (ExplicitRole ctxt rt pos) = do
      maximallyQualifiedName <- if isQualifiedWithDomein (roletype2string rt)
        then pure (roletype2string rt)
        else pure $ concatenateSegments (unwrap ctxt) (roletype2string rt)
      r <- (\a -> [a]) <$> qualifyLocalRoleName pos maximallyQualifiedName
      pure r
    -- Compile the expression s with respect to context ctxt.
    -- This case MUST represent the current object that holds in the body of `perspective on`. Multiple Enumerated role types can result from this case.
    collectRoles (ImplicitRole ctxt s) = compileExpression (CDOM (ST ctxt)) s >>= \qfd ->
      case range qfd of
        RDOM adt -> pure $ reduce adt
        otherwise -> throwError $ NotARoleDomain otherwise (startOf s) (endOf s)
      where
        -- Translate the RoleIdentification to an array of EnumeratedRoleTypes.
        -- Notice we do not fail on UNIVERSAL or EMPTY.
        reduce :: ADT EnumeratedRoleType -> Array RoleType
        reduce (ST t) = [ENR t]
        reduce (SUM args) = join $ map reduce args
        reduce (PROD args) = maybe [] reduce (head args)
        reduce UNIVERSAL = []
        reduce EMPTY = []

    -- | Correctly handles incomplete (not qualified) RoleIdentifications.
    collectStates :: (Maybe SegmentedPath) -> RoleIdentification -> PhaseThree (Array StateIdentifier)
    collectStates mpath r = collectRoles r >>= \roles -> case mpath of
      Nothing -> pure (StateIdentifier <<< roletype2string <$> roles)
      Just p ->  pure (StateIdentifier <<< flip append p <<< flip append "$" <<< roletype2string <$> roles)

    -- | Correctly handles incomplete (not qualified) RoleIdentifications that may occur in the SubjectState case.
    stateSpec2States :: AST.StateSpecification -> PhaseThree (Array StateIdentifier)
    stateSpec2States spec = case spec of
      -- Execute the effect for all subjects in the context state.
      AST.ContextState ctx mpath -> pure [(StateIdentifier $ (maybe (unwrap ctx) (append (unwrap ctx) <<< append "$") mpath))]
      -- Execute the effect for each qualifiedUser in each of the subject states.
      -- Note that the subjects in whose states we execute, do not need be the qualified users we execute for:
      -- 'do this automatically for me when you are at home' illustrates this independence.
      AST.SubjectState ridentification mpath -> collectStates mpath ridentification
      -- Execute the effect for all subjects in all object states.
      AST.ObjectState ridentification mpath -> collectStates mpath ridentification

    stateSpecificationToStateSpec :: AST.StateSpecification -> PhaseThree (Array StateSpec)
    stateSpecificationToStateSpec spec = case spec of
      AST.ContextState _ _ -> map ContextState <$> stateSpec2States spec
      AST.SubjectState _ _ -> map SubjectState <$> stateSpec2States spec
      AST.ObjectState _ _ -> map ObjectState <$> stateSpec2States spec

    addAll :: forall key value. Ord key => value -> Map.Map key value -> Array key -> Map.Map key value
    addAll value = foldr (\key map -> Map.insert key value map)

    handlePart :: Partial => AST.StateQualifiedPart -> PhaseThree Unit

    -- Compiles and distributes all expressions in the automatic effect.
    handlePart (AST.AE (AST.AutomaticEffectE{subject, transition, effect, start, end})) = do
      -- `originDomain` is the type of the origin, expressed as Domain.
      originDomain <- statespec2Domain (transition2stateSpec transition)
      -- `currentContextDomain` represents the current context, expressed as a Domain.
      currentcontextDomain <- pure (CDOM $ ST $ stateSpec2ContextType (transition2stateSpec transition))
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
      -- its current context.
      -- currentactor -> It's type is the qualifiedUser computed above.
      -- All these VarBindings have a computation of type TypeTimeOnly, which instructs the unsafeCompiler to remove them.
      effectWithEnvironment <- pure $ addContextualBindingsToStatements
        [ computeOrigin (transition2stateSpec transition) start
        , computeCurrentContext (transition2stateSpec transition) start
        , makeTypeTimeOnlyRoleStep "currentactor" (unsafePartial fromJust $ head qualifiedUsers) start
        ]
        effect
      states <- stateSpec2States (transition2stateSpec transition)
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
        AST.ContextState ct _ -> pure $ SQD (CDOM $ ST ct) (DataTypeGetter IdentityF) (CDOM $ ST ct) True True
        AST.ObjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
        AST.SubjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
      modifyAllStates
        (case transition2stateSpec transition of
          AST.ContextState _ _ -> ContextAction sideEffect
          otherwise -> RoleAction {currentContextCalculation, effect: sideEffect})
        qualifiedUsers
        states
        originDomain
      where

        modifyAllStates :: Action -> Array RoleType -> Array StateIdentifier -> Domain -> PhaseThree Unit
        modifyAllStates automaticAction qualifiedUsers states currentDomain = for_ states
          (modifyPartOfState start end
            \(sr@{automaticOnEntry, automaticOnExit, query}) -> do
              query' <- case query of
                Q q -> pure $ Q q
                S stp -> do
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
                  Q <$> compileAndDistributeStep currentDomain expressionWithEnvironment [] states
              case transition of
                AST.Entry _ -> pure $ sr
                  { automaticOnEntry = EncodableMap $ addAll
                      automaticAction                  -- value
                      (unwrap automaticOnEntry)   -- Map.Map key value
                      qualifiedUsers              -- Array Key
                  , query = query'}
                AST.Exit _ -> pure $ sr {automaticOnExit = EncodableMap $ addAll automaticAction (unwrap automaticOnExit) qualifiedUsers, query = query'})

    -- Compiles and distributes all expressions in the message.
    -- See extensive comments in the case AutomaticEffectE.
    handlePart (AST.N (AST.NotificationE{user, transition, message, start, end})) = do
      originDomain <- statespec2Domain (transition2stateSpec transition)
      (qualifiedUsers :: Array RoleType) <- collectRoles user
      states <- stateSpec2States (transition2stateSpec transition)
      -- Then compile the parts of the sentence, tacking each compiled part onto that sequence.
      -- The expressions in the Sentence are compiled with respect to the current context.
      compiledMessage <- compileSentence originDomain message qualifiedUsers states (transition2stateSpec transition)
      currentContextCalculation <- case transition2stateSpec transition of
        AST.ContextState ct _ -> pure $ SQD (CDOM $ ST ct) (DataTypeGetter IdentityF) (CDOM $ ST ct) True True
        AST.ObjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
        AST.SubjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
      modifyAllStates
        (case transition2stateSpec transition of
          AST.ContextState _ _ -> ContextNotification compiledMessage
          otherwise -> RoleNotification {currentContextCalculation, sentence: compiledMessage})
        qualifiedUsers
        states
        originDomain
      where
          modifyAllStates :: Notification -> Array RoleType -> Array StateIdentifier -> Domain -> PhaseThree Unit
          modifyAllStates notification qualifiedUsers states currentDomain = for_ states
            \stateId -> modifyPartOfState
              start
              end
              (\(sr@{notifyOnEntry, notifyOnExit, object, query}) -> do
                -- Compile the query if we've not done it before.
                query' <- case query of
                  Q q -> pure $ Q q
                  S stp -> do
                    expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                      [ computeCurrentContext (transition2stateSpec transition) start
                      , computeOrigin (transition2stateSpec transition) start]
                      stp
                    Q <$> compileAndDistributeStep currentDomain expressionWithEnvironment [] states
                case transition of
                  AST.Entry _ -> pure $ sr {notifyOnEntry = EncodableMap $ addAll notification (unwrap notifyOnEntry) qualifiedUsers, query = query'}
                  AST.Exit _ -> pure $ sr {notifyOnExit = EncodableMap $ addAll notification (unwrap notifyOnExit) qualifiedUsers, query = query'})
              stateId

          compileSentence :: Domain -> Sentence.Sentence -> Array RoleType -> Array StateIdentifier -> AST.StateSpecification -> PhaseThree Sentence.Sentence
          compileSentence currentDomain (Sentence.Sentence parts) qualifiedUsers' states stateSpec = Sentence.Sentence <$> traverse compilePart parts
            where
              compilePart :: Sentence.SentencePart -> PhaseThree Sentence.SentencePart
              compilePart hr@(Sentence.HR _) = pure hr
              compilePart cp@(Sentence.CP (Q _)) = pure cp
              compilePart (Sentence.CP (S stp)) = do
                expressionWithEnvironment <- pure $ addContextualBindingsToExpression
                  [ makeIdentityStep "currentcontext" start
                  , makeIdentityStep "origin" start
                  , makeTypeTimeOnlyRoleStep "notifieduser" (unsafePartial fromJust $ head qualifiedUsers') start
                  ]
                  stp
                compiledPart <- compileExpression currentDomain expressionWithEnvironment
                pure (Sentence.CP (Q compiledPart))

    handlePart (AST.AC (AST.ActionE{id, subject, object:syntacticObject, state, effect, start})) = do
      -- `originDomain` is the type of the origin, expressed as Domain.
      originDomain <- statespec2Domain state
      -- `currentContextDomain` represents the current context, expressed as a Domain.
      currentcontextDomain <- pure (CDOM $ ST $ stateSpec2ContextType state)
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
      -- its current context.
      -- currentactor -> It's type is the qualifiedUser computed above.
      -- All these VarBindings have a computation of type TypeTimeOnly, which instructs the unsafeCompiler to remove them.
      effectWithEnvironment <- pure $ addContextualBindingsToStatements
        [ computeOrigin state start
        , computeCurrentContext state start
        , makeTypeTimeOnlyRoleStep "currentactor" (unsafePartial fromJust $ head qualifiedUsers) start
        ]
        effect
      states <- stateSpec2States state
      (theAction :: QueryFunctionDescription) <- compileStatement
        states
        originDomain
        currentcontextDomain
        qualifiedUsers
        effectWithEnvironment
      -- Compute the currentcontext from the origin.
      currentContextCalculation <- case state of
        -- We do not actually use this result.
        AST.ContextState ct _ -> pure $ SQD (CDOM $ ST ct) (DataTypeGetter IdentityF) (CDOM $ ST ct) True True
        AST.ObjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
        AST.SubjectState roleIdentification _ -> computeCurrentContextFromRoleIdentification roleIdentification start
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
          (CDOM $ ST (roleIdentification2Context syntacticObject))
          syntacticObjectWithEnvironment)
      stateSpecs <- stateSpecificationToStateSpec state
      modifyAllSubjectPerspectives
        qualifiedUsers
        compiledObject
        (case state of
          AST.ContextState _ _ -> ContextAction theAction
          otherwise -> RoleAction {currentContextCalculation, effect: theAction})
        stateSpecs
      where
        -- Add the action for all users to their perspective on the object in all states.
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> Action -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd theAction states = for_ qualifiedUsers
          (modifyPerspective
            objectQfd
            syntacticObject
            start
            \(Perspective pr@{actions}) -> Perspective $ pr {actions = EncodableMap (foldr
              (\stateId actionsMap -> case Map.lookup stateId actionsMap of
                Nothing -> Map.insert stateId (singleton id theAction) actionsMap
                Just actionsInState -> Map.insert stateId (insert id theAction actionsInState) actionsMap)
              (unwrap actions)
              states)})

    handlePart (AST.R (AST.RoleVerbE{subject, object, state, roleVerbs:rv, start})) = do
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      -- ... for these states only...
      stateSpecs <- stateSpecificationToStateSpec state
      -- ... the role verbs.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd stateSpecs
      where
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd stateSpecs = for_ qualifiedUsers
          (modifyPerspective objectQfd object start
            (\(Perspective pr@{roleVerbs}) -> Perspective pr {roleVerbs = EncodableMap $ addAll rv (unwrap roleVerbs) stateSpecs}))

    handlePart (AST.P (AST.PropertyVerbE{subject, object, state, propertyVerbs, propsOrView, start})) = do
      -- Add, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      propertyTypes <- collectPropertyTypes propsOrView
      (propertyVerbs' :: PropertyVerbs) <- pure $ PropertyVerbs propertyTypes propertyVerbs
      -- ... for these states only...
      stateSpecs <- stateSpecificationToStateSpec state
      -- ... the action.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' stateSpecs
      where
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> PropertyVerbs -> Array StateSpec -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd propertyVerbs' stateSpecs = for_ qualifiedUsers
          (modifyPerspective
            objectQfd
            object
            start
            \(Perspective pr@{propertyVerbs:pverbs}) -> Perspective $ pr {propertyVerbs = EncodableMap (foldr
              (\stateId pverbsMap -> case Map.lookup stateId pverbsMap of
                Nothing -> Map.insert stateId [propertyVerbs'] pverbsMap
                Just propertyVerbsArray -> Map.insert stateId (cons propertyVerbs' propertyVerbsArray) pverbsMap)
              (unwrap pverbs)
              stateSpecs)})

        -- We lookup the qualified name of these properties here, for the object of the perspective.
        collectPropertyTypes :: AST.PropsOrView -> PhaseThree (ExplicitSet PropertyType)
        collectPropertyTypes AST.AllProperties = pure Universal
        collectPropertyTypes (AST.Properties ps) = do
          -- The (partial) names for properties used here may be defined outside
          -- of the model (due to role filling). So we use functions that rely on the
          -- model cache and hence we need the current model to be in that cache, too (which it is, here).
          roleADT <- roleIdentification2rangeADT object
          PSet <$> for (fromFoldable ps)
            \localPropertyName -> do
              candidates <- lift2 (roleADT ###= lookForUnqualifiedPropertyType localPropertyName)
              case head candidates of
                Nothing -> throwError $ UnknownProperty start localPropertyName (show roleADT)
                (Just t) | length candidates == 1 -> pure t
                _ -> throwError $ NotUniquelyIdentifying start localPropertyName (propertytype2string <$> candidates)


          -- pure $ PSet (ENP <<< EnumeratedPropertyType <$> (fromFoldable ps))
        collectPropertyTypes (AST.View view) = do
          (views :: Object View) <- getsDF _.views
          -- As we have postponed handling these parse tree fragments after
          -- handling all others, there can be no forward references.
          -- The property references in Views are, by now, qualified.
          candidates <- pure $ filter (flip endsWithSegments view) (keys views)
          case length candidates of
            0 -> throwError $ UnknownView start view
            1 -> unsafePartial case lookup (unsafePartial ARRP.head candidates) views of
              Just (View {propertyReferences}) -> pure $ PSet propertyReferences
            _ -> throwError $ NotUniquelyIdentifying start view candidates

    handlePart (AST.SO (AST.SelfOnly{subject, object, state, start})) = do
      currentDomain <- pure (CDOM $ ST $ stateSpec2ContextType state)
      -- Set, for all these users...
      qualifiedUsers <- collectRoles subject
      -- ... to their perspective on this object...
      objectQfd <- roleIdentificationToQueryFunctionDescription object start
      -- ... the selfOnly member to true.
      modifyAllSubjectPerspectives qualifiedUsers objectQfd
      where
        modifyAllSubjectPerspectives :: Array RoleType -> QueryFunctionDescription -> PhaseThree Unit
        modifyAllSubjectPerspectives qualifiedUsers objectQfd = for_ qualifiedUsers
          (modifyPerspective objectQfd object start
            (\(Perspective pr) -> Perspective pr {selfOnly = true}))

    -- Apply, for this user, the modifier to his perspective on the object (and create a perspective if necessary).
    modifyPerspective :: QueryFunctionDescription -> RoleIdentification -> ArcPosition -> (Perspective -> Perspective) -> RoleType -> PhaseThree Unit
    modifyPerspective objectQfd roleSpec start modifier userRole = do
      (roleType :: Maybe RoleType) <- head <$> collectRoles roleSpec
      mroleDisplayName <- lift2 $ traverse displayNameOfRoleType roleType
      case userRole of
        ENR (EnumeratedRoleType r) -> do
          EnumeratedRole er@{perspectives} <- getsDF (unsafePartial fromJust <<< lookup r <<< _.enumeratedRoles)
          mi <- pure $ findIndex (\(Perspective{object}) -> object == objectQfd) perspectives
          perspective <- case mi of
            Nothing -> do
              displayName <- case mroleDisplayName of
                Just roleName -> pure roleName
                Nothing -> lift2 $ reduce (getEnumeratedRole >=> pure <<< _.displayName <<< unwrap) (unsafePartial domain2roleType $ range objectQfd)
              pure $ Perspective
                { id: (roletype2string userRole) <> "_" <> show (length perspectives)
                , object: objectQfd
                , isEnumerated: (isQFDofEnumeratedRole objectQfd)
                , displayName
                , roleType
                , roleVerbs: EncodableMap Map.empty
                , propertyVerbs: EncodableMap Map.empty
                , actions: EncodableMap Map.empty
                , selfOnly: false
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
              displayName <- case mroleDisplayName of
                Just roleName -> pure roleName
                Nothing -> lift2 $ reduce (getEnumeratedRole >=> pure <<< _.displayName <<< unwrap) (unsafePartial domain2roleType $ range objectQfd)
              pure $ Perspective
                { id: (roletype2string userRole) <> "_" <> show (length perspectives)
                , object: objectQfd
                , isEnumerated: (isQFDofEnumeratedRole objectQfd)
                , displayName
                , roleType
                , roleVerbs: EncodableMap Map.empty
                , propertyVerbs: EncodableMap Map.empty
                , actions: EncodableMap Map.empty
                , selfOnly: false
                }
            Just i -> pure (unsafePartial $ fromJust $ index perspectives i)
          modifyDF \dfr@{calculatedRoles} -> dfr {calculatedRoles = insert
            r
            (CalculatedRole $ er {perspectives = case mi of
              Nothing -> cons (modifier perspective) perspectives
              Just i -> unsafePartial $ fromJust $ updateAt i (modifier perspective) perspectives })
            calculatedRoles
            }

    modifyPartOfState :: ArcPosition -> ArcPosition -> (StateRecord -> PhaseThree StateRecord) -> StateIdentifier -> PhaseThree Unit
    modifyPartOfState start end modifyState stateId = do
      mstate <- State.gets _.dfr >>= pure <<< lookup (unwrap stateId) <<< _.states
      case mstate of
        Nothing -> isContextRootState stateId >>= if _
          then do
            state' <- State <$> modifyState (unwrap $ constructState stateId (Q $ trueCondition (CDOM $ ST (ContextType (unwrap stateId)))) (Cnt (ContextType (unwrap stateId))) [])
            modifyDF \drf@{states} -> drf {states = insert (unwrap stateId) state' states}
          else isRoleRootState stateId >>= if _
            then do
              rk <- unsafePartial $ roleKind stateId
              state' <- State <$> modifyState (unwrap $ constructState stateId (Q $ trueCondition (RDOM $ ST (EnumeratedRoleType (unwrap stateId))))
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
        addInvertedQueriesForPerspectiveObject roleType p@(Perspective {object, propertyVerbs, selfOnly}) = do
          -- Sets the inverted queries directly in the EnumeratedRoles and Properties in the
          -- DomeinFile we keep in PhaseTwoState.
          sPerProp <- lift2 $ statesPerProperty p
          setInvertedQueries [roleType] sPerProp (roleStates p) object selfOnly

        explicitSet2RelevantProperties :: ExplicitSet PropertyType -> RelevantProperties
        explicitSet2RelevantProperties Universal = All
        explicitSet2RelevantProperties Empty = Properties []
        explicitSet2RelevantProperties (PSet ps) = Properties ps

createMissingRootStates :: PhaseThree Unit
createMissingRootStates = do
  df@{contexts, enumeratedRoles, states} <- lift $ State.gets _.dfr
  (missingRootStates :: Object State) <- pure $ OBJ.fromFoldable $ catMaybes $ values states <#> \(State{id, stateFulObject}) -> case stateFulObject of
    Cnt (ContextType ctype) -> if isDirectSuperStateOf (keys enumeratedRoles) ctype (unwrap id)
      then if isNothing $ lookup ctype states
        then Just $ Tuple ctype $ constructState (StateIdentifier ctype) (Q $ trueCondition (CDOM $ ST (ContextType ctype))) (Cnt $ ContextType ctype) []
        else Nothing
      else Nothing
    Orole (EnumeratedRoleType rtype) -> if isDirectSuperStateOf (keys enumeratedRoles) rtype (unwrap id)
      then if isNothing $ lookup rtype states
        then Just $ Tuple rtype $ constructState (StateIdentifier rtype) (Q $ trueCondition (RDOM $ ST (EnumeratedRoleType rtype))) (Orole $ EnumeratedRoleType rtype) []
        else Nothing
      else Nothing
    Srole (EnumeratedRoleType rtype) -> if isDirectSuperStateOf (keys enumeratedRoles) rtype (unwrap id)
      then if isNothing $ lookup rtype states
        then Just $ Tuple rtype $ constructState (StateIdentifier rtype) (Q $ trueCondition (RDOM $ ST (EnumeratedRoleType rtype))) (Orole $ EnumeratedRoleType rtype) []
        else Nothing
      else Nothing
  modifyDF \dfr -> dfr {states = states `union` missingRootStates}

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
      (CDOM $ ST (roleIdentification2Context roleIdentification))
      syntacticObjectWithEnvironment)


computeCurrentContextFromRoleIdentification :: RoleIdentification -> ArcPosition -> PhaseThree QueryFunctionDescription
computeCurrentContextFromRoleIdentification roleIdentification pos = do
  compiledObject <- roleIdentificationToQueryFunctionDescription roleIdentification pos
  -- NOTE that filters and WithFrame constructs are ignored in the inversion process.
  (contextCalculations :: (Array QueryFunctionDescription)) <- completeInversions compiledObject
  case joinQfds contextCalculations of
    Nothing -> throwError (Custom $ "It is not possible to compute the current context in position " <> show pos <> " (is `origin` an indexed role or context?). Change current state at this position, for example by using `in object|subject|context state`." <> " Information for programmers: " <> prettyPrint compiledObject)
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
  df@{_id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    _id
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
            S stp -> do
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
              Q <$> compileAndDistributeStep
                (stateFulObject2Domain stateFulObject)
                expressionWithEnvironment
                []
                [id]
          pure $ State sr { query = compiledQuery }

registerStates :: PhaseThree Unit
registerStates = do
  df@{contexts, enumeratedRoles, states} <- lift $ State.gets _.dfr
  modifyDF \dfr -> dfr
    { contexts =
      -- Register a root state with all contexts that have one.
        contexts <#> \c@(REP.Context ctxt@{_id}) -> if isJust $ lookup (unwrap _id) states
          then (REP.Context ctxt {rootState = Just $ StateIdentifier (unwrap _id)})
          else c
    , enumeratedRoles =
      -- Register a root state with all enumerated roles that have one.
        enumeratedRoles <#> \e@(EnumeratedRole erole@{_id}) -> if isJust $ lookup (unwrap _id) states
          then (EnumeratedRole erole {rootState = Just $ StateIdentifier (unwrap _id)})
          else e
    , states =
      -- Register all substates with each state.
        states <#> \(State sr@{id}) -> State $ sr { subStates = StateIdentifier <$> filter (isDirectSuperStateOf (keys enumeratedRoles) $ unwrap id) (keys states)}
    }

-- True, if sub adds a single segment to super.
-- sub `isDirectSubstateOf` super
-- model:System$PerspectivesSystem$Root `isDirectSubStateOf` model:System$PerspectivesSystem
isDirectSubstateOf :: Array String -> String -> String -> Boolean
isDirectSubstateOf enumeratedRoleNames sub super = maybe false (\ns -> ns `eq` super && (isNothing $ elemIndex sub enumeratedRoleNames)) (deconstructNamespace sub)
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
statespec2Domain (AST.ContextState ctype _) = pure $ CDOM (ST ctype)
statespec2Domain (AST.SubjectState roleIdentification _) = range <$> compileStep (CDOM (ST (roleIdentification2Context roleIdentification))) (roleIdentification2Step roleIdentification)
statespec2Domain (AST.ObjectState roleIdentification ctype) = range <$> compileStep (CDOM (ST (roleIdentification2Context roleIdentification))) (roleIdentification2Step roleIdentification)

roleIdentification2rangeADT :: RoleIdentification -> PhaseThree (ADT EnumeratedRoleType)
roleIdentification2rangeADT roleIdentification = unsafePartial domain2roleType <<< range <$> compileStep
  (CDOM (ST (roleIdentification2Context roleIdentification)))
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

stateFulObject2Domain :: StateFulObject -> Domain
stateFulObject2Domain (Cnt ctxt) = CDOM (ST ctxt)
stateFulObject2Domain (Orole rle) = RDOM (ST rle)
stateFulObject2Domain (Srole rle) = RDOM (ST rle)
