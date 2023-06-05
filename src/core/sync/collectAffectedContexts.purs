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

module Perspectives.CollectAffectedContexts where

import Control.Monad.AvarMonadAsk (modify) as AA
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Reader (lift)
import Data.Array (concat, cons, difference, filterA, foldM, foldl, head, nub, null, union, filter)
import Data.Array.NonEmpty (fromArray, singleton) as ANE
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (head) as List
import Data.Map (isEmpty)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (error)
import Foreign.Object (Object, empty, lookup, values)
import Foreign.Object (union) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspective.InvertedQuery.Indices (runTimeIndexForRoleQueries, runtimeIndexForContextQueries, runtimeIndexForFilledByQueries, runtimeIndexForFillsQueries, runtimeIndexForPropertyQueries)
import Perspectives.Assignment.SerialiseAsDeltas (getPropertyValues, serialiseDependencies)
import Perspectives.ContextAndRole (isDefaultContextDelta)
import Perspectives.CoreTypes (type (~~>), InformedAssumption(..), MP, MonadPerspectives, MonadPerspectivesTransaction, execMonadPerspectivesQuery, runMonadPerspectivesQuery, (###=), (##=), (##>), (##>>))
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.Data.EncodableMap (values, empty, lookup) as EM
import Perspectives.Deltas (addDelta)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Error.Boundaries (handleDomeinFileError', handlePerspectContextError, handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, contextIsInState, contextType, getFilledRoles, makeChainGetter, notIsMe, roleIsInState)
import Perspectives.Instances.ObjectGetters (roleType, context) as OG
import Perspectives.InvertedQuery (InvertedQuery(..), backwards, backwardsQueryResultsInContext, backwardsQueryResultsInRole, forwards, lookupInvertedQueries, shouldResultInContextStateQuery, shouldResultInRoleStateQuery)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectContext, getPerspectRol, tryGetPerspectEntiteit)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, singletonPath)
import Perspectives.Query.QueryTypes (isRoleDomain, range)
import Perspectives.Query.UnsafeCompiler (getHiddenFunction, getRoleInstances, getterFromPropertyType)
import Perspectives.Representation.Class.PersistentType (StateIdentifier, cacheInDomeinFile, getEnumeratedRole, tryGetState)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole, InvertedQueryMap)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.State (StateFulObject(..))
import Perspectives.Representation.State (StateFulObject(..), State(..)) as State
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), toArray)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (enumeratedRoleContextType, roleAspectsClosure)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..))
import Perspectives.Utilities (findM, prettyPrint)
import Prelude (Unit, bind, const, discard, flip, join, map, not, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- TODO. #12 Check the way state-conditional verbs are combined to establish whether a peer should receive a delta.
-----------------------------------------------------------
-- USERSWITHPERSPECTIVEONROLEINSTANCE
-----------------------------------------------------------
-- | This function returns all users that have a perspective in some context on a role type such that the
-- | given role instance is used for the computation of the perspective object (it may be that object itself or be
-- | on a path to the object).
-- | As a side effect, Deltas are added to the transaction for the continuation of the path beyond the given role
-- | instance. This is conditional on the last argument.
usersWithPerspectiveOnRoleInstance ::  EnumeratedRoleType -> RoleInstance -> Boolean -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleInstance roleType roleInstance runForwards = do
  contextInstance <- lift (roleInstance ##>> OG.context)
  embeddingContextType <- lift (roleInstance ##>> OG.context >=> contextType)
  users1 <- do
    -- Find all InvertedQueryResults, starting from the role instance of the Delta (provided as third argument to this function).
    -- Each context is the start of a path through instance space that corresponds to a query expression in type space.
    -- The path passes through the roleInstance of the Delta.
    -- This means that the query, when re-run from the context, might yield a different result then before the
    -- mutation described by the Delta.
    -- Index the object of InvertedQueries that we find with each type of roleInstance with the type of its
    -- embedding context.

    -- The lines below looks up inverted queries on Aspects, too. However, we will handle that
    -- by contextualizing Aspects in compile time. No need then to reflect on all Aspects of the roleType in runtime.
    -- (contextCalculations :: (Array InvertedQuery)) <- lift (roleType ###=
    --   (roleAspectsClosure >=>
    --     ArrayT <<<
    --       (map (lookupInvertedQueries (unwrap embeddingContextType))
    --         <<< compileContextInvertedQueries)))
    -- (roleCalculations :: (Array InvertedQuery)) <- lift (embeddingContextType ###=
    --   (contextAspectsClosure >=>
    --     ArrayT <<<
    --       (map (lookupInvertedQueries (unwrap embeddingContextType))
    --         <<< compileRoleInvertedQueries)))
    -- The new versions below anticipates on this perspective contextualisation.

    storedContextCalculations <- lift $ compileContextInvertedQueries roleType
    (contextCalculations :: (Array InvertedQuery)) <- concat <$> ((lift $ runtimeIndexForContextQueries contextInstance) >>= traverse \ctxtType' -> pure $ lookupInvertedQueries (unwrap ctxtType') storedContextCalculations)

    -- If iq has the selfOnly modifier, we must apply a new algorithm to the roleInstance and the roleInstance.
    (for contextCalculations \iq -> if isForSelfOnly iq
      then handleSelfOnlyQuery iq roleInstance roleInstance
      else if runForwards
        then handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq
        else handleBackwardQuery roleInstance iq >>= pure <<< join <<< map snd
        ) >>= pure <<< join

  users2 <- do
    storedRoleCalculations <- lift $ compileRoleInvertedQueries embeddingContextType
    (roleCalculations :: (Array InvertedQuery)) <- concat <$> ((lift $ runTimeIndexForRoleQueries roleType) >>= traverse \roleType' -> pure $ lookupInvertedQueries (unwrap roleType') storedRoleCalculations)

      -- Find all InvertedQueryResults, starting from the new role instance of the Delta.
      -- We do not start on the context because the cardinality of the role getting step is larger than one and
      -- we want to make sure that the new binding is in the path for the users.
    (for roleCalculations \iq -> if runForwards
      then handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq
      else handleBackwardQuery roleInstance iq >>= pure <<< join <<< map snd
      ) >>= pure <<< join
  pure $ nub $ union users1 users2

type ContextWithUsers = Tuple ContextInstance (Array RoleInstance)

-- | Handle InvertedQueryies with the selfOnly modifier.
-- For binding the role arguments are filled filler
handleSelfOnlyQuery :: InvertedQuery -> RoleInstance -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
handleSelfOnlyQuery (InvertedQuery{backwardsCompiled, forwardsCompiled, description}) userRoleArgForBackwardsQuery argForForwardsQuery = do
  peers <- computePeers
  -- Compute backwards and collect assumptions.
  case backwardsCompiled of
    Nothing -> pure unit
    Just bw -> do
      (Tuple _ assumptions) <- lift $ runMonadPerspectivesQuery userRoleArgForBackwardsQuery (unsafeCoerce bw)
      -- Turn assumptions into deltas for the peers.
      for_ (unwrap assumptions) (createDeltasFromAssumption peers)
  -- Return peers
  pure peers
  where
    -- Interpret the forwards part. For each DependencyPath, the result is a RoleInstance that is a peer.
    -- Send dependencies in that path to that peer only!
    computePeers :: MonadPerspectivesTransaction (Array RoleInstance)
    computePeers = if isNothing forwardsCompiled
      then pure [userRoleArgForBackwardsQuery]
      else case forwards description of
        Nothing -> pure []
        Just fw -> do
          (peersAndPaths :: Array (DependencyPath)) <- lift ((singletonPath (R argForForwardsQuery)) ##= interpret fw)
          -- Add Deltas

          for 
            (filter (\{head} -> case head of
                R rid -> true
                -- If not a role domain, just return false. This will be a similar case to
                -- computeUsersFromState.computeUsersFromState, case Orole. For example forward queries resulting
                -- from inverted filtered queries end up in a Boolean, not in the object.
                otherwise -> false)
              peersAndPaths)
            -- Each DependencyPath result is a peer.
            \path -> do
              peer <- pure $ unsafePartial roleAtHead path
              for_
                -- Serialise the ordered dependencies in all paths walked to compute that peer, for that peer only.
                -- That is the materialization of the `selfOnly` modifier, for that peer.
                (allPaths path)
                (serialiseDependencies [peer])
              -- Return all peers.
              pure peer

    roleAtHead :: Partial => DependencyPath -> RoleInstance
    roleAtHead {head} = case head of
      R r -> r

isForSelfOnly :: InvertedQuery -> Boolean
isForSelfOnly (InvertedQuery{selfOnly}) = selfOnly
-----------------------------------------------------------
-- HANDLEBACKWARDQUERY
-----------------------------------------------------------
-- | Treats state-transition-triggering inverted queries different from inverted queries created from perspectives
-- | and expressions in statements (implicit perspectives).
-- | For the former, we create InvertedQueryResult-s (either ContextStateQuery or RoleStateQuery) that are further
-- | evaluated when running the transaction. Eventually, these will lead to state transitions and automatic actions etc.
-- | These InvertedQueryResults are added to the transaction, as a side effect of evaluating handleBackwardQuery.
-- |
-- | The latter case results in no side effect in the transaction but merely returns context instances and user roles.
-- | The user role instances have a perspective on roles in their context.
-- | Perspectives are conditional on states (valid in some states and not in others). The users we return are guaranteed
-- | to have at least one valid perspective, even in the case of object state.
-- | INVARIANT TO RESPECT: both the backward- and forward part of the InvertedQuery should have been compiled.
-- TODO. Fix the error that backwardsCompiled can be Nothing.
handleBackwardQuery :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array ContextWithUsers)
handleBackwardQuery roleInstance iq@(InvertedQuery{description, backwardsCompiled, users:userTypes, states, forwardsCompiled}) = catchError
  (do
  case backwardsCompiled of
    Nothing -> do 
      logPerspectivesError (Custom $ "Backwards is not compiled on " <> prettyPrint description) 
      pure []
    _ -> do
      if unsafePartial shouldResultInContextStateQuery iq
        then createContextStateQuery
        else if unsafePartial shouldResultInRoleStateQuery iq
          then createRoleStateQuery
            else usersWithAnActivePerspective)
      (\e -> do
        logPerspectivesError (Custom $ show e)
        pure [])
  where
    -- | The InvertedQuery is based on a Context state condition.
    -- | This function adds, as a side effect, an InvertedQueryResult to the current transaction.
    createContextStateQuery :: MonadPerspectivesTransaction (Array ContextWithUsers)
    createContextStateQuery = do
      (invertedQueryResults :: Array ContextInstance) <- lift (roleInstance ##= (contextInstancesGetter :: RoleInstance ~~> ContextInstance))
      addInvertedQueryResult $ ContextStateQuery invertedQueryResults
      pure []

    -- | The InvertedQuery is based on a Role state condition.
    -- | This function adds, as a side effect, an InvertedQueryResult to the current transaction.
    createRoleStateQuery :: MonadPerspectivesTransaction (Array ContextWithUsers)
    createRoleStateQuery = do
      (affectedRoles :: Array RoleInstance) <- lift (roleInstance ##= (roleInstancesGetter :: RoleInstance ~~> RoleInstance))
      addInvertedQueryResult $ RoleStateQuery affectedRoles
      pure []

    -- | The InvertedQuery is based on an explicit or implicit perspective.
    -- | This function has no side effect but returns, bundled in their respective contexts, users with
    -- | a valid perspective on a role (and properties) in their context.
    -- | They will receive Deltas that describe the change that triggered this InvertedQuery in the first place.
    usersWithAnActivePerspective :: MonadPerspectivesTransaction (Array ContextWithUsers)
    usersWithAnActivePerspective = if unsafePartial $ backwardsQueryResultsInRole iq
      then fromRoleResults
      else if unsafePartial $ backwardsQueryResultsInContext iq
        then fromContextResults
          else throwError (error "Programming error in handleBackwardQuery.usersWithAnActivePerspective: query should result\
          \in a role or a context.")

    -- | The inverted query (its backward part) always leads to a context instance.
    -- | These InvertedQueries have been derived from explicit Perspectives, or from
    -- | 'implicit Perspectives' resulting from expressions in context state (so that the resource the expression is
    -- | applied to is a context instance). A context state condition is an example; another is expression that are in
    -- | scope of an "in context state" clause.
    -- | The object of such a perspective is computed from the context that the user having the perspective is in.
    -- | Hence, it inversion leads back to that context.
    fromContextResults :: MonadPerspectivesTransaction (Array ContextWithUsers)
    fromContextResults = do
      (contextsWithPerspectiveHolders :: Array ContextInstance) <- lift (roleInstance ##= (contextInstancesGetter :: RoleInstance ~~> ContextInstance))
      foldM computeUsersFromContext [] contextsWithPerspectiveHolders

      where
        -- The currently inefficient version accumulates users from each state examined.
        -- The efficient version stops as soon as a context type state is valid.
        computeUsersFromContext :: Array ContextWithUsers -> ContextInstance -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromContext accumulatedUsers cid = foldM (computeUsersFromState cid) accumulatedUsers states

        -- | As explicit Perspectives may be conditional on each type of state, we have to consider them all.
        computeUsersFromState :: ContextInstance -> Array ContextWithUsers -> StateIdentifier -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromState cid accumulatedUsers stateId = (lift $ tryGetState stateId) >>=
          case _ of
            -- No state means: an undefined root state.
            -- But in that situation, there are no conditions at all, so we just compute the users from the context.
            Nothing -> lift $ flip cons accumulatedUsers <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
            Just (State.State{stateFulObject}) ->
              case stateFulObject of
                -- If the state's StateFulObject is a context type (Cnt), and the context is in that state, obtain all user role instances from the context (and we're done)
                State.Cnt _ -> (lift $ contextIsInState stateId cid) >>= if _
                  then lift $ flip cons accumulatedUsers <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                  else pure accumulatedUsers
                -- If the state's StateFulObject is a subject type (Srole), then obtain the user role instances from the context and pass on all that are in that state.
                State.Srole rtype -> lift $ flip cons accumulatedUsers <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA (roleIsInState stateId) >>= filterA notIsMe) userTypes)
                -- Construct an Array of object roles. If there is no forwards computation, it is just the roleInstance.
                -- If the forwards computation results in the object of the original, not inverted, query, run it on the roleInstance. NOTE: instead we use a weaker test: if it results in a role instance.
                -- Only if any of the object roles is in the required state, obtain the user role instances from the context and return them.
                State.Orole rtype -> do
                  objects <- case forwardsCompiled, isRoleDomain <<< range <$> (forwards description) of
                    Just f, Just true -> lift (roleInstance ##= unsafeCoerce f)
                    _, _ -> pure [roleInstance]
                  lift (findM (roleIsInState stateId) objects) >>= \mObject -> if isJust mObject
                    then lift $ flip cons accumulatedUsers <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                    else pure accumulatedUsers

    -- | The inverted query (its backward part) always leads to a role instance.
    -- | These InvertedQueries have been derived from 'implicit Perspectives', that is: expressions in statements
    -- | in AutomaticActions, Actions or Notifications.
    -- | These expressions are applied to a resource that depends on the current state of the lexical position of
    -- | the "do", "action" or "notify" clauses. This can either be a context instance, a user role instance
    -- | (in the case of subject state) or another role instance (in the case of object state).
    -- | Hence, it inversion leads back to that resource.
    -- | However, the context state case is handled in `fromContextResults`, so we only have to consider object and
    -- | subject state.
    fromRoleResults :: MonadPerspectivesTransaction (Array ContextWithUsers)
    fromRoleResults = do
      (rolesExpressionsAreAppliedTo :: Array RoleInstance) <- lift (roleInstance ##= (roleInstancesGetter :: RoleInstance ~~> RoleInstance))
      foldM computeUsersFromRole [] rolesExpressionsAreAppliedTo

      where
        computeUsersFromRole :: Array ContextWithUsers -> RoleInstance -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromRole accumulatedUsers rid = foldM (computeUsersFromState rid) accumulatedUsers states

        -- | We only have to consider Srole and Orole cases.
        computeUsersFromState :: RoleInstance -> Array ContextWithUsers -> StateIdentifier -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromState rid accumulatedUsers stateId = (lift $ tryGetState stateId) >>=
          case _ of
            -- If there is no state, we assume we deal with an undefined root state, meaning there are no conditions at all.
            -- We then compute the users from the context of the role instance
            Nothing -> do
              cid <- lift (rid ##>> OG.context)
              flip cons accumulatedUsers <<< Tuple cid <$> lift (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
            Just (State.State{stateFulObject}) ->
              case stateFulObject of
                State.Cnt _ -> throwError (error $ "fromRoleResults.computeUsersFromState: context states should not occur.")
                -- The role we've been lead back to is a user role. Check whether it is in the required state.
                State.Srole _ -> (lift $ roleIsInState stateId rid) >>= if _
                  then lift (rid ##>> OG.context) >>= \cid -> pure $ cons (Tuple cid [rid]) accumulatedUsers
                  else pure accumulatedUsers
                -- The role we've been lead back to is an object role. If it is in the required state,
                -- compute the users having a perspective from its context.
                State.Orole _ -> (lift $ roleIsInState stateId rid) >>= if _
                  then do
                    cid <- lift (rid ##>> OG.context)
                    flip cons accumulatedUsers <<< Tuple cid <$> lift (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                  else pure accumulatedUsers
    
    -- Coerce the compiled backwards query to a role instance getter. 
    -- The result thus is a function that computes, from a role instance, the backwards query
    -- and returns role instances in context instances from which we have to take the user role instances that have a perspective!
    roleInstancesGetter :: RoleInstance ~~> RoleInstance
    roleInstancesGetter = case backwardsCompiled of
      Nothing -> do 
        \_ -> do 
          logPerspectivesError (Custom $ "Backwards is not compiled on " <> prettyPrint description) 
          ArrayT $ pure []
      Just c -> unsafeCoerce c

    -- Results in a query that computes, from a role instance, the backwards query 
    -- that returns context instances from which we have to take the user role instances that have a perspective.
    contextInstancesGetter :: RoleInstance ~~> ContextInstance
    contextInstancesGetter = case backwardsCompiled of
      Nothing -> do 
        \_ -> do 
          logPerspectivesError (Custom $ "Backwards is not compiled on " <> prettyPrint description) 
          ArrayT $ pure []
      Just c -> unsafeCoerce c

-- | Adds the InvertedQueryResult to the current Transaction, but only if the resource is not marked as (to be) removed.
addInvertedQueryResult :: InvertedQueryResult -> MonadPerspectivesTransaction Unit
addInvertedQueryResult (ContextStateQuery ctxts) = AA.modify \(Transaction r@{invertedQueryResults, untouchableContexts}) -> case difference ctxts untouchableContexts of
  nothing | null nothing -> Transaction r
  ctxts' -> Transaction (r {invertedQueryResults = union [ContextStateQuery ctxts'] invertedQueryResults})
addInvertedQueryResult (RoleStateQuery roles) = AA.modify \(Transaction r@{invertedQueryResults, untouchableRoles}) -> case difference roles untouchableRoles of
  nothing | null nothing -> Transaction r
  roles' -> Transaction (r {invertedQueryResults = union [RoleStateQuery roles'] invertedQueryResults})

-----------------------------------------------------------
-- OBSERVINGCONTEXTS
-----------------------------------------------------------
-- | Computes an InvertedQueryResult for the given RoleInstance and its type.
-- | Just collect and add to the Transaction all contexts that have a role with
-- | a perspective on the RoleType in the context instance.
-- | Guarantees RULE TRIGGERING by adding InvertedQueryResults to the transaction.
-- | This function is used in the singular circumstance of contexts and roles that are read
-- | from file. We want to trigger state changes for each of its roles.
-- | INVARIANT: the ContextType is the type of the ContextInstance.
addRoleObservingContexts :: ContextType -> ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
addRoleObservingContexts ctxtType id roleType roleInstance = do
  -- the inverted queries stored on an EnumeratedRole in the contextInvertedQueries member (`context` step).
  (contextCalculations :: Object (Array InvertedQuery)) <- lift $ compileContextInvertedQueries roleType
  -- the inverted queries stored on a Context in the invertedQueries member (`role` step).
  roleCalculations <- lift $ compileRoleInvertedQueries ctxtType
  -- Index with the embedding context. In _contextInvertedQueries we store (the inverted query that begins with)
  -- the context step away from role instance. We then arrive at context instance id.
  -- Its type we use to index the filledByCalculations.
  (lift $ runtimeIndexForContextQueries id) >>= traverse_ \ctxtType' -> for_ (lookupInvertedQueries (unwrap ctxtType') contextCalculations) (handleBackwardQuery roleInstance)
  (lift $ runTimeIndexForRoleQueries roleType) >>= traverse_ \roleType' -> for_ (lookupInvertedQueries (unwrap roleType') roleCalculations) (handleBackwardQuery roleInstance)

-----------------------------------------------------------
-- FOR ROLEBINDING DELTAS
-----------------------------------------------------------
-- | Computes the InvertedQueryResults (for STATE EVALUATION) and adds them to the Transaction.
-- | Returns all users (for SYNCHRONISATION) that have a perspective that takes them through the binding relation.
-- | A binding represents two types of step: `binding` and `binder <TypeOfBinder>`
-- | This function finds all queries recorded on the types of the roles represented
-- | by binder, the new binding and the old binding, that have such steps.
-- | As a side effect, Deltas are added to the transaction for the continuation of the path beyond the given role
-- | instance. This is conditional on the last argument.
usersWithPerspectiveOnRoleBinding :: RoleBindingDelta -> Boolean -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleBinding delta@(RoleBindingDelta dr@{filled, filler:mbinding, oldFiller, deltaType}) runForwards = do
  filledType <- lift (filled ##>> OG.roleType)
  case mbinding of
    Nothing -> pure []
    Just filler -> do
      -- TODO. The filler may have many types!
      fillerType <- lift (filler ##>> OG.roleType)
      filledContextType <- lift $ enumeratedRoleContextType filledType
      -- Includes calculations from all types of filledType.
      filledByCalculations <- lift $ compileInvertedQueryMap _filledByInvertedQueries filledType
      filledByKeys <- lift $ unsafePartial runtimeIndexForFilledByQueries delta
      -- `fillsKeys` are computed using all types of filled.
      fillsKeys <- lift $ unsafePartial runtimeIndexForFillsQueries delta
      -- We've stored the relevant InvertedQueries with the type of the filled,
      -- so we execute them on any filler that is a specialisation of (or equal to) the required filler type.
      -- Includes calculations from all types of fillerType.
      fillsCalculations <- lift $ compileInvertedQueryMap _fillsInvertedQueries filledType
        -- Index with the embedding context. In filledByInvertedQueries we store (the inverted query that
        -- begins with) the filler step away from filled.
        -- Its context type we use to index the filledByCalculations.
      users1 <- concat <$> (for filledByKeys \filledByKey -> case EM.lookup filledByKey filledByCalculations of
        Nothing -> pure []
        Just calculations -> concat <$> for calculations
          -- Find all affected contexts, starting from the filled instance of the Delta.
          (\iq -> if runForwards
            then (handleBackwardQuery filled iq) >>= runForwardsComputation filled iq
            else handleBackwardQuery filled iq >>= pure <<< join <<< map snd
            ))
      -- All InvertedQueries with a backwards step that is `filledBy <TypeOfBinder>`, iff we actually bind something:
      -- Index with the embedding context. In fillsInvertedQueries we store (the inverted query that would begin with)
      -- the filled step away from filler (filler). However, because of cardinality, we apply these queries to the
      -- filled role instead.
      -- We do use, however, the context type of the filler to index the binderCalculations.
      users2 <- concat <$> (for fillsKeys \fillsKey -> case EM.lookup fillsKey fillsCalculations of
        Nothing -> pure []
        Just calculations -> concat <$> for calculations
          (\iq -> if isForSelfOnly iq
            -- These inverted queries skip the first step and so must be applied to the filled itself.
            -- NOTE/TODO: ik denk dat het tweemaal filled moet zijn. De forwards query wordt toegepast op het tweede argument.
            then handleSelfOnlyQuery iq filled filler
            else if runForwards
              then (handleBackwardQuery filled iq) >>= runForwardsComputation filler iq
              else handleBackwardQuery filled iq >>= pure <<< join <<< map snd
        ))
      -- All InvertedQueries with a backwards step that is `filledBy <TypeOfBinder>`, iff we actually overwrite something:
      users3 <- case oldFiller of
        Just bnd | deltaType == ReplaceBinding ->
          -- We deal here just with the case that we destroy a filler; not with the case that we add one.
          -- Hence, no forward computation. `handleBackwardQuery` just passes on users that have at least one
          -- valid perspective, even if the condition is object state.
          concat <$> map snd <<< concat <$> (for fillsKeys \fillsKey -> case EM.lookup fillsKey fillsCalculations of
            Nothing -> pure []
            Just calculations -> concat <$> for calculations (handleBackwardQuery bnd))
        otherwise -> pure []
      pure (nub $ union users1 (users2 `union` users3))

-- | Runs the forward part of the QueryWithAKink. That is part of the original query. Assumptions collected
-- | during evaluation are turned into Deltas for peers with a perspective and collected in the current transaction.
-- | These peers are provided as user role instances grouped together with their context.
-- | The System User is excluded (no deltas generated for him).
runForwardsComputation ::
  RoleInstance ->
  InvertedQuery ->
  (Array ContextWithUsers) ->
  MonadPerspectivesTransaction (Array RoleInstance)
runForwardsComputation roleInstance (InvertedQuery{description, forwardsCompiled, statesPerProperty, states}) cwus = do
  case forwards description of
    Nothing -> do
      -- This case arises for example for a perspective on an EnumeratedRoleType in the same context.
      -- Such a perspective will have properties (no sense in providing a perspective with just role verbs,
      -- because instances of such a role cannot be shown).
      -- Another case that the roleInstance has just been added as a binding to a role a user has a perspective on.
      -- For each property, get its value from the role instance, if the state condition is met.
      -- When there are no properties, we add the deltas for the role instance anyway.
      -- This covers the case of a new binding for a perspective without properties.
      if (isEmpty (unwrap statesPerProperty))
        then do
          PerspectRol{pspType, context} <- lift $  getPerspectRol roleInstance
          magic context (SerializableNonEmptyArray $ ANE.singleton roleInstance) pspType (join $ snd <$> cwus)
        else pure unit
      forWithIndex_ (unwrap statesPerProperty)
        (\prop propStates -> do
          propGetter <- lift (makeChainGetter <$> (getterFromPropertyType prop))
          for_ propStates
            -- For each state that provides a perspective on the property,
            \stateIdentifier ->
              (lift $ tryGetState stateIdentifier) >>= case _ of
                -- If we deal with a roleInstance of a type for which no state has been defined,
                -- we should carry on as if the state condition was satisfied.
                Nothing -> for_ cwus
                  \(Tuple cid users) -> void $ lift (execMonadPerspectivesQuery roleInstance propGetter)
                    >>= (traverse (createDeltasFromAssumption users))
                Just (State.State{stateFulObject}) ->
                  -- if the stateful object...
                  case stateFulObject of
                    -- ... is the current context, then if it is in that state,
                    Cnt _ -> for_ cwus
                      \(Tuple cid users) -> (lift $ contextIsInState stateIdentifier cid) >>= if _
                        -- then create deltas for all resources visited while retrieving
                        -- the property, for all users;
                        then  void $ lift (execMonadPerspectivesQuery roleInstance propGetter)
                          >>= (traverse (createDeltasFromAssumption users))
                        else pure unit
                    -- ... is the subject role, for each user that is that state,
                    Srole _ -> for_ cwus
                      \(Tuple cid users) -> lift (filterA (roleIsInState stateIdentifier) users) >>=
                        \sanctionedUsers ->
                          -- create deltas for all resources while retrieving the property;
                           void $ lift (execMonadPerspectivesQuery roleInstance propGetter)
                            >>= (traverse (createDeltasFromAssumption sanctionedUsers))
                    -- ... is the object role, then if it is in that state,
                    Orole _ -> lift (roleIsInState stateIdentifier roleInstance) >>= if _
                      -- create deltas for all resources visited by the query while
                      -- retrieving the property, for all users;
                      then void $ lift (execMonadPerspectivesQuery roleInstance propGetter)
                        >>= (traverse (createDeltasFromAssumption (join $ snd <$> cwus)))
                      else pure unit
        )
    -- These are all other cases, where there still is some path to walk to the
    -- role (and its binding) that carries the properties.
    -- NOTE: THE RESULT OF THE FORWARD PART MAY BE A PROPERTY VALUE!
    Just fw -> do
      -- Run the query interpreter on the same role instance as the backwards query,
      -- resulting in all the paths that lead up to a role result.
      (rinstances :: Array (DependencyPath)) <- lift ((singletonPath (R roleInstance)) ##= interpret fw)
      -- There can be zero or multiple state-valid perspectives on the results for each result, context and user.
      -- We analyse each of the possibilities and accumulate the results in the transaction, filtering out doubles when adding.
      for_ states
        \stateIdentifier -> (lift $ tryGetState stateIdentifier) >>= case _ of
          -- If we deal with a roleInstance of a type for which no state has been defined,
          -- we should carry on as if the state condition was satisfied.
          Nothing -> for_ (join (allPaths <$> rinstances)) (serialiseDependencies (join $ snd <$> cwus))
          Just (State.State{stateFulObject}) ->
            case stateFulObject of
              -- if the context is in that state,
              Cnt _ -> for_ cwus
                \(Tuple cid users) -> (lift $ contextIsInState stateIdentifier cid) >>= if _
                  -- then create deltas for all resources visited by the query (as reflected in
                  -- the assumptions), for all users;
                  then for_ (join (allPaths <$> rinstances)) (serialiseDependencies users)
                  else pure unit
              -- otherwise, for each user that is that state,
              Srole _ -> for_ cwus
                \(Tuple cid users) -> lift (filterA (roleIsInState stateIdentifier) users) >>=
                  \sanctionedUsers ->
                    -- create deltas for all resources visited by the query (as reflected in
                    -- the assumptions);
                    for_ (join (allPaths <$> rinstances)) (serialiseDependencies sanctionedUsers)
              -- otherwise, for all paths that end in a role that is in that state,
              Orole _ -> (filterA
                (\{head} -> case head of
                  R rid -> lift (roleIsInState stateIdentifier rid)
                  -- If not a role domain, just return false. This will be a similar case to
                  -- computeUsersFromState.computeUsersFromState, case Orole. For example forward queries resulting
                  -- from inverted filtered queries end up in a Boolean, not in the object.
                  otherwise -> pure false)
                rinstances)
                  -- then create deltas for all resources visited by the query (as reflected in
                  -- the assumptions), for all users;
                  >>= pure <<< join <<< (map allPaths)
                  >>= traverse_ (serialiseDependencies (join $ snd <$> cwus))

      -- For each property, get its value from the role instances found by the query interpreter,
      -- if the state condition is met. This is similar but not equal to the treatment
      -- of the case above where there was no forwards query.
      -- Do this only for inverted queries that result in a role domain.
      if isRoleDomain $ range fw
        then 
          forWithIndex_ (unwrap statesPerProperty)
            (\prop propStates -> for_ propStates
              -- For each state that provides a perspective on the property,
              \stateIdentifier -> (lift $ tryGetState stateIdentifier) >>= case _ of
                -- If we deal with a roleInstance of a type for which no state has been defined,
                -- we should carry on as if the state condition was satisfied.
                Nothing -> for_ (join (allPaths <$> rinstances)) (serialiseDependencies (join $ snd <$> cwus))
                Just (State.State{stateFulObject}) ->
                  -- if the stateful object...
                  case stateFulObject of
                    -- ... is the current context, then if it is in that state,
                    Cnt _ -> for_ cwus
                      \(Tuple cid users) -> (lift $ contextIsInState stateIdentifier cid) >>= if _
                        -- then run the interpreter on the property computation and the head of the dependency paths
                        -- and create deltas for all users
                        then for_ (_.head <$> rinstances)
                          \(dep :: Dependency) -> do
                            (vals :: Array DependencyPath) <- lift ((singletonPath dep) ##= getPropertyValues prop)
                            for_ (join (allPaths <$> vals)) (serialiseDependencies users)
                        else pure unit
                    -- ... is the subject role, collect each user that is that state,
                    Srole _ -> for_ cwus
                      \(Tuple cid users) -> lift (filterA (roleIsInState stateIdentifier) users) >>=
                        \sanctionedUsers ->
                          -- run the interpreter on the property computation and the head of the dependency paths
                          -- and create deltas for all collected users
                          for_ (_.head <$> rinstances)
                            \(dep :: Dependency) -> do
                              (vals :: Array DependencyPath) <- lift ((singletonPath dep) ##= getPropertyValues prop)
                              for_ (join (allPaths <$> vals)) (serialiseDependencies sanctionedUsers)
                    -- ... is the object role, then for all paths that end in a role that is in that state,
                    Orole _ -> (filterA
                      (\{head} -> case head of
                        R rid -> lift (roleIsInState stateIdentifier rid)
                        otherwise -> throwError (error ("runForwardsComputation (states per property) hits on a QueryInterpreter result that is not a role: " <> show otherwise)))
                      rinstances)
                        >>= pure <<< map _.head
                        -- run the interpreter on the property computation and the head of the dependency path
                        -- and create deltas for all users
                        >>= traverse_
                          \(dep :: Dependency) -> do
                            (vals :: Array DependencyPath) <- lift ((singletonPath dep) ##= getPropertyValues prop)
                            for_ (join (allPaths <$> vals)) (serialiseDependencies (join $ snd <$> cwus))
            )
        else pure unit
  pure $ join $ snd <$> cwus

-- | Add deltas for all the users to the current transaction, from the given assumption.
createDeltasFromAssumption :: Array RoleInstance -> InformedAssumption -> MonadPerspectivesTransaction Unit
createDeltasFromAssumption users (RoleAssumption ctxt roleTypeId) = do
  instances <- lift (ctxt ##= getRoleInstances (ENR roleTypeId))
  case SerializableNonEmptyArray <$> ANE.fromArray instances of
    Nothing -> pure unit
    Just instances' -> magic ctxt instances' roleTypeId users

-- The value of me of a context is indexed, and thus private. It never leads to a delta.
createDeltasFromAssumption users (Me _) = pure unit

createDeltasFromAssumption users (Binding roleInstance) = do
  mbnd <- lift (roleInstance ##> binding)
  case mbnd of
    Nothing -> pure unit
    Just bnd -> do
      ctxt <- lift (bnd ##>> OG.context)
      rtype <- lift (bnd ##>> OG.roleType)
      magic ctxt (SerializableNonEmptyArray $ ANE.singleton bnd) rtype users
      (try $ lift $ getPerspectRol roleInstance) >>=
        handlePerspectRolError "createDeltasFromAssumption:Binding"
          \(PerspectRol{bindingDelta}) -> case bindingDelta of
            Nothing -> pure unit
            Just bd -> addDelta $ DeltaInTransaction {users, delta: bd}

-- FilledRolesAssumption fillerId filledContextType filledType
createDeltasFromAssumption users (FilledRolesAssumption fillerId filledContextType filledType) = do
  filledRoles <- lift (fillerId ##= getFilledRoles filledContextType filledType)
  case head filledRoles of
    Nothing -> pure unit
    Just someFilled -> (lift $ tryGetPerspectEntiteit someFilled) >>= (case _ of 
      -- This means that the context of the filler is public.
      Nothing -> pure unit
      Just (PerspectRol{context:filledContext}) -> magic filledContext (SerializableNonEmptyArray $ unsafePartial fromJust $ ANE.fromArray filledRoles) filledType users)
  for_ filledRoles \filledId -> do
    (try $ lift $ getPerspectRol filledId) >>=
      handlePerspectRolError "createDeltasFromAssumption.FilledRolesAssumption"
        \(PerspectRol{bindingDelta}) -> case bindingDelta of
          Nothing -> pure unit
          Just bd -> addDelta $ DeltaInTransaction {users, delta: bd}

createDeltasFromAssumption users (Property roleInstance propertyType) = do
  ctxt <- lift (roleInstance ##>> OG.context)
  rtype <- lift (roleInstance ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton roleInstance) rtype users
  (try $ lift $ getPerspectRol roleInstance) >>=
    handlePerspectRolError "createDeltasFromAssumption.Property"
      \(PerspectRol{propertyDeltas}) -> case lookup (unwrap propertyType) propertyDeltas of
        Nothing -> pure unit
        Just deltas -> for_ deltas \propertyDelta -> addDelta $ DeltaInTransaction {users, delta: propertyDelta}

createDeltasFromAssumption users (Context roleInstance) = do
  ctxt <- lift (roleInstance ##>> OG.context)
  rtype <- lift (roleInstance ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton roleInstance) rtype users

-- The forwards part of a QueryWithAKink in an fillsInvertedQueries or filledByInvertedQueries
-- never starts with 'external', because these queries are applied to respectively the
-- new binding or the role that binds.
-- Whenever 'external' is applied, a 'context' step has been applied before - leading
-- to a UniverseContextDelta that causes the receiver to create both a context and
-- its external role.
createDeltasFromAssumption users (External contextInstance) = pure unit

-- State is private, hence no deltas should be generated.
createDeltasFromAssumption users (State contextInstance) = pure unit

-- State is private, hence no deltas should be generated.
createDeltasFromAssumption users (RoleState _) = pure unit

-- | Add a UniverseContextDelta, UniverseRoleDelta and a ContextDelta.
magic :: ContextInstance -> SerializableNonEmptyArray RoleInstance -> EnumeratedRoleType ->  Array RoleInstance -> MonadPerspectivesTransaction Unit
magic ctxt roleInstances rtype users =  do
  ctype <- lift (ctxt ##>> contextType)
  (try $ lift $ getPerspectContext ctxt) >>=
    handlePerspectContextError "Perspectives.CollectAffectedContexts.magic"
      -- Fetch the UniverseContextDelta from the context instance here.
      \(PerspectContext{universeContextDelta, buitenRol}) -> do
        (try $ lift $ getPerspectRol buitenRol) >>=
          handlePerspectRolError "Perspectives.CollectAffectedContexts.magic"
            \(PerspectRol{universeRoleDelta: externalRoleDelta}) -> do
              addDelta $ DeltaInTransaction {users, delta: externalRoleDelta}
              addDelta $ DeltaInTransaction {users, delta: universeContextDelta}
              for_ (toArray roleInstances) \roleInstance -> do
                (try $ lift $ getPerspectRol roleInstance) >>=
                  handlePerspectRolError "Perspectives.CollectAffectedContexts.magic"
                    \(PerspectRol{universeRoleDelta, contextDelta}) -> do
                      addDelta $ DeltaInTransaction {users, delta: universeRoleDelta}
                      -- Not if the roleInstance is an external role!
                      if (not $ isDefaultContextDelta contextDelta)
                        then addDelta $ DeltaInTransaction {users, delta: contextDelta}
                        else pure unit

-- | Adds users for SYNCHRONISATION, guarantees RULE TRIGGERING.
-- The role instance is the current object; so if a perspective is conditional on object state, we can check
-- this role instance.
-- Note we don't evaluate any forward part; it is not needed, so it may be Nothing.
aisInPropertyDelta :: RoleInstance -> EnumeratedPropertyType -> EnumeratedPropertyType -> EnumeratedRoleType -> MonadPerspectivesTransaction (Array RoleInstance)
aisInPropertyDelta id property replacementProperty eroleType = do
  -- We must handle both the original property type and its replacement (if any).
  calculations <- if property == replacementProperty
    then lift $ compileDescriptions' property
    else do
      calculations <- lift $ compileDescriptions' property
      calculations' <- lift $ compileDescriptions' replacementProperty
      pure (calculations `OBJ.union` calculations')
  -- `handleBackwardQuery` just passes on users that have at least one
  -- valid perspective, even if the condition is object state.
  -- We must use all types of the role to look up calculations.
  allKeys <- lift $ runtimeIndexForPropertyQueries eroleType
  allCalculations <- pure $ foldl
    (\cumulatedCalculations nextKey -> case lookup nextKey calculations of
      Nothing -> cumulatedCalculations
      Just c -> c `union` cumulatedCalculations)
    []
    allKeys
  (cwu :: Array ContextWithUsers) <- join <$> for allCalculations (handleBackwardQuery id)
  lift $ filterA notIsMe (nub $ join $ snd <$> cwu)
  where
    -- Either compile the InvertedQuery or take the compilation from the DomeinFile in cache, if it had been compiled before.
    compileDescriptions' :: EnumeratedPropertyType -> MonadPerspectives (Object (Array InvertedQuery))
    compileDescriptions' rt@(EnumeratedPropertyType ert) =  do
      modelName <- pure $ (unsafePartial $ fromJust $ typeUri2ModelUri ert)
      (try $ retrieveDomeinFile modelName) >>=
        handleDomeinFileError' "aisInPropertyDelta" empty
          \(df :: DomeinFile) -> case preview (onPropertyDelta rt) df of
            Nothing -> pure empty
            Just (iqObj :: Object (Array InvertedQuery)) -> if areCompiled iqObj
              then pure iqObj
              else do
                compiledCalculations <- traverse (traverse compileBoth) iqObj
                -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
                modifyDomeinFileInCache (over (onPropertyDelta rt) (const compiledCalculations)) modelName
                pure compiledCalculations
      where
        onPropertyDelta :: EnumeratedPropertyType -> Traversal' DomeinFile (Object (Array InvertedQuery))
        onPropertyDelta (EnumeratedPropertyType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedProperties") <<< at x <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "onPropertyDelta")

_filledByInvertedQueries :: InvertedQueryMapsLens
_filledByInvertedQueries = _Newtype <<< prop (SProxy :: SProxy "filledByInvertedQueries")

_fillsInvertedQueries :: InvertedQueryMapsLens
_fillsInvertedQueries = _Newtype <<< prop (SProxy :: SProxy "fillsInvertedQueries")

type InvertedQueryMapsLens = Lens' EnumeratedRole InvertedQueryMap

-- | Compiles the two parts of the description for an InvertedQueryMapsLens,
-- | stores the result in cache.
compileInvertedQueryMap :: InvertedQueryMapsLens -> EnumeratedRoleType -> MonadPerspectives InvertedQueryMap
compileInvertedQueryMap lens rt@(EnumeratedRoleType ert) = do
  modelName <- pure $ (unsafePartial $ fromJust $ typeUri2ModelUri ert)
  -- Get the InvertedQueries, for all types of rt.
  (allRoletypes :: Array EnumeratedRoleType) <- (rt ###= roleAspectsClosure)
  foldM (\cumulatedMap roleType -> do
    role <- getEnumeratedRole roleType
    calculations <- pure $ unsafePartial fromJust $ preview lens role
    -- Compile the descriptions.
    if areCompiled' calculations
      then pure (cumulatedMap <> calculations)
      else do
        (compiledCalculations :: InvertedQueryMap) <- EncodableMap <$> traverse (traverse compileBoth) (unwrap calculations)
        cacheInDomeinFile roleType (over lens (const compiledCalculations) role)
        pure (cumulatedMap <> compiledCalculations)
    )
    EM.empty
    allRoletypes
  where
    areCompiled' :: InvertedQueryMap -> Boolean
    areCompiled' ar = case List.head $ EM.values ar of
      Nothing -> true
      Just iqs -> case head iqs of
        Nothing -> true
        Just (InvertedQuery{backwardsCompiled}) -> isJust backwardsCompiled

-- | Compiles the two parts of the description of the inverted queries stored
-- | on an EnumeratedRole in the contextInvertedQueries member.
-- | stores the result in cache.
compileContextInvertedQueries :: EnumeratedRoleType -> MonadPerspectives (Object (Array InvertedQuery))
compileContextInvertedQueries rt@(EnumeratedRoleType ert) = do
  modelName <- pure $ (unsafePartial $ fromJust $ typeUri2ModelUri ert)
  (try $ retrieveDomeinFile modelName) >>=
    handleDomeinFileError' "compileDescriptions_" empty
    \(df :: DomeinFile) -> do
      -- Get the InvertedQueries.
      (calculations :: Object (Array InvertedQuery)) <- pure $ unsafePartial $ fromJust $ preview (onDelta rt) df
      -- Compile the descriptions.
      if areCompiled calculations
        then pure calculations
        else do
          (compiledCalculations :: Object (Array InvertedQuery)) <- traverse (traverse compileBoth) calculations
          -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
          modifyDomeinFileInCache (over (onDelta rt) (const compiledCalculations)) modelName
          pure compiledCalculations
  where
    onDelta :: EnumeratedRoleType -> Traversal' DomeinFile (Object (Array InvertedQuery))
    onDelta (EnumeratedRoleType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedRoles") <<< at x <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "contextInvertedQueries")

-- | Compiles the two parts of the description of the inverted queries stored
-- | on a Context in the invertedQueries member.
-- | stores the result in cache.
compileRoleInvertedQueries :: ContextType -> MonadPerspectives (Object (Array InvertedQuery))
compileRoleInvertedQueries ct@(ContextType ctxtType) = do
  modelName <- pure $ (unsafePartial $ fromJust $ typeUri2ModelUri ctxtType)
  (try $ retrieveDomeinFile modelName) >>=
    handleDomeinFileError' "compileDescriptions_" empty
    \(df :: DomeinFile) -> do
      -- Get the InvertedQueries.
      (calculations :: Object (Array InvertedQuery)) <- pure $ unsafePartial $ fromJust $ preview (onDelta ct) df
      -- Compile the descriptions.
      if areCompiled calculations
        then pure calculations
        else do
          (compiledCalculations :: Object (Array InvertedQuery)) <- traverse (traverse compileBoth) calculations
          -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
          modifyDomeinFileInCache (over (onDelta ct) (const compiledCalculations)) modelName
          pure compiledCalculations
  where
    onDelta :: ContextType -> Traversal' DomeinFile (Object (Array InvertedQuery))
    onDelta (ContextType x) = _Newtype <<< prop (SProxy :: SProxy "contexts") <<< at x <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "invertedQueries")

areCompiled :: Object (Array InvertedQuery) -> Boolean
areCompiled ar = case head $ values ar of
  Nothing -> true
  Just iqs -> case head iqs of
    Nothing -> true
    Just (InvertedQuery{backwardsCompiled}) -> isJust backwardsCompiled

compileBoth :: InvertedQuery -> MP InvertedQuery
compileBoth ac@(InvertedQuery iqr@{description, backwardsCompiled, forwardsCompiled}) = case backwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    backwards' <- traverse getHiddenFunction (backwards description)
    forwards' <- traverse getHiddenFunction (forwards description)
    -- It is an error if backwards' is Nothing.
    if isNothing backwards'
      then logPerspectivesError (Custom $ "compileBoth: backwards is nothing for \n" <> prettyPrint description)
      else pure unit
    pure $ InvertedQuery iqr{backwardsCompiled = (map unsafeCoerce backwards'), forwardsCompiled = (map unsafeCoerce forwards')}

lookupInvertedPropertyQueries :: EnumeratedRoleType -> Object (Array InvertedQuery) -> MonadPerspectivesTransaction (Array InvertedQuery)
lookupInvertedPropertyQueries eroleType calculations = do
  allRoletypes <- lift (eroleType ###= roleAspectsClosure)
  pure $ foldl (\calcs nextEnumeratedType -> case lookup (unwrap nextEnumeratedType) calculations of
    Nothing -> calcs
    Just newCalcs -> calcs <> newCalcs)
    []
    allRoletypes
