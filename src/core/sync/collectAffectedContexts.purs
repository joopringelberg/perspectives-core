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
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Reader (lift)
import Data.Array (filterA, foldM, head, nub, singleton, union)
import Data.Array.NonEmpty (fromArray, singleton, head) as ANE
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (foldM) as LNE
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (error)
import Foreign.Object (Object, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.SerialiseAsDeltas (getPropertyValues, serialiseDependency)
import Perspectives.ContextAndRole (isDefaultContextDelta)
import Perspectives.CoreTypes (type (~~>), InformedAssumption(..), MP, MonadPerspectives, MonadPerspectivesTransaction, execMonadPerspectivesQuery, runMonadPerspectivesQuery, (###=), (##=), (##>), (##>>))
import Perspectives.Deltas (addDelta)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Error.Boundaries (handleDomeinFileError', handlePerspectContextError, handlePerspectRolError)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, contextIsInState, contextType, getRoleBinders, makeChainGetter, notIsMe, roleIsInState)
import Perspectives.Instances.ObjectGetters (roleType, context) as OG
import Perspectives.InvertedQuery (InvertedQuery(..), backwards, backwardsQueryResultsInContext, backwardsQueryResultsInRole, forwards, shouldResultInContextStateQuery, shouldResultInRoleStateQuery)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, singletonPath)
import Perspectives.Query.QueryTypes (isRoleDomain, range)
import Perspectives.Query.UnsafeCompiler (getHiddenFunction, getRoleInstances, getterFromPropertyType)
import Perspectives.Representation.Class.PersistentType (StateIdentifier, tryGetState)
import Perspectives.Representation.EnumeratedRole (EnumeratedRoleRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.State (StateFulObject(..))
import Perspectives.Representation.State (StateFulObject(..), State(..)) as State
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), toArray)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (roleAspectsClosure)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..))
import Perspectives.Utilities (findM)
import Prelude (Unit, bind, const, discard, join, map, not, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- USERSWITHPERSPECTIVEONROLEINSTANCE
-----------------------------------------------------------
-- | This function returns all users that have a perspective in some context on a role type such that the
-- | given role instance is used for the computation of the perspective object (it may be that object itself or be
-- | on a path to the object).
-- | As a side effect, Deltas are added to the transaction for the continuation of the path beyond the given role
-- | instance.
usersWithPerspectiveOnRoleInstance ::  EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleInstance roleType roleInstance = do
  users1 <- do
      -- Find all InvertedQueryResults, starting from the role instance of the Delta (provided as third argument to this function).
      -- Each context is the start of a path through instance space that corresponds to a query expression in type space.
      -- The path passes through the roleInstance of the Delta.
      -- This means that the query, when re-run from the context, might yield a different result then before the
      -- mutation described by the Delta.
    contextCalculations <- lift2 (roleType ###= (roleAspectsClosure >=> ArrayT <<< compileBothFor _onContextDelta_context))
    -- TODO.
    -- If iq has the selfOnly modifier, we must apply a new algorithm to the roleInstance and the roleInstance.
    (for contextCalculations \iq -> if isForSelfOnly iq
      then handleSelfOnlyQuery iq roleInstance roleInstance
      else handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq) >>= pure <<< join
  users2 <- do
    roleCalculations <- lift2 (roleType ###= (roleAspectsClosure >=> ArrayT <<< compileBothFor _onContextDelta_role))
      -- Find all InvertedQueryResults, starting from the new role instance of the Delta.
      -- We do not start on the context because the cardinality of the role getting step is larger than one and
      -- we want to make sure that the new binding is in the path for the users.
    (for roleCalculations \iq ->
      handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq) >>= pure <<< join
  pure $ nub $ union users1 users2

type ContextWithUsers = Tuple ContextInstance (Array RoleInstance)

-- | Handle InvertedQueryies with the selfOnly modifier.
handleSelfOnlyQuery :: InvertedQuery -> RoleInstance -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
handleSelfOnlyQuery (InvertedQuery{backwardsCompiled, forwardsCompiled, description}) argForBackwardsQuery argForForwardsQuery = do
  peers <- computePeers
  -- Compute backwards and collect assumptions.
  case backwardsCompiled of
    Nothing -> pure unit
    Just bw -> do
      (Tuple _ assumptions) <- lift2 $ runMonadPerspectivesQuery argForBackwardsQuery (unsafeCoerce bw)
      -- Turn assumptions into deltas for the peers.
      for_ (unwrap assumptions) (createDeltasFromAssumption peers)
  -- Return peers
  pure peers
  where
    -- Interpret the forwards part. For each DependencyPath, the result is a RoleInstance that is a peer.
    -- Send dependencies in that path to that peer.
    computePeers :: MonadPerspectivesTransaction (Array RoleInstance)
    computePeers = if isNothing forwardsCompiled
      then pure [argForBackwardsQuery]
      else case forwards description of
        Nothing -> pure []
        Just fw -> do
          (peersAndPaths :: Array (DependencyPath)) <- lift2 ((singletonPath (R argForForwardsQuery)) ##= interpret fw)
          -- Add Deltas
          for peersAndPaths
            -- Each DependencyPath result is a peer.
            \path -> do
              peer <- pure $ unsafePartial roleAtHead path
              for_
                -- Serialise the ordered dependencies in all paths walked to compute that peer, for that peer only.
                (allPaths path)
                (LNE.foldM (serialiseDependency [peer]) Nothing)
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
handleBackwardQuery :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array ContextWithUsers)
handleBackwardQuery roleInstance iq@(InvertedQuery{description, backwardsCompiled, users:userTypes, states, forwardsCompiled}) = do
  if unsafePartial shouldResultInContextStateQuery iq
    then createContextStateQuery
    else if unsafePartial shouldResultInRoleStateQuery iq
      then createRoleStateQuery
      else usersWithAnActivePerspective
  where
    -- | The InvertedQuery is based on a Context state condition.
    -- | This function adds, as a side effect, an InvertedQueryResult to the current transaction.
    createContextStateQuery :: MonadPerspectivesTransaction (Array ContextWithUsers)
    createContextStateQuery = do
      (invertedQueryResults :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      addInvertedQueryResult $ ContextStateQuery invertedQueryResults
      pure []

    -- | The InvertedQuery is based on a Role state condition.
    -- | This function adds, as a side effect, an InvertedQueryResult to the current transaction.
    createRoleStateQuery :: MonadPerspectivesTransaction (Array ContextWithUsers)
    createRoleStateQuery = do
      (affectedRoles :: Array RoleInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance)
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
      (contextsWithPerspectiveHolders :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      foldM computeUsersFromContext [] contextsWithPerspectiveHolders

      where
        -- The currently inefficient version accumulates users from each state examined.
        -- The efficient version stops as soon as a context type state is valid.
        computeUsersFromContext :: Array ContextWithUsers -> ContextInstance -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromContext accumulatedUsers cid = foldM (computeUsersFromState cid) [] states

        -- | As explicit Perspectives may be conditional on each type of state, we have to consider them all.
        computeUsersFromState :: ContextInstance -> Array ContextWithUsers -> StateIdentifier -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromState cid accumulatedUsers stateId = (lift2 $ tryGetState stateId) >>=
          case _ of
            -- No state means: an undefined root state.
            -- But in that situation, there are no conditions at all, so we just compute the users from the context.
            Nothing -> lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
            Just (State.State{stateFulObject}) ->
              case stateFulObject of
                -- If the state's StateFulObject is a context type (Cnt), and the context is in that state, obtain all user role instances from the context (and we're done)
                State.Cnt _ -> (lift2 $ contextIsInState stateId cid) >>= if _
                  then lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                  else pure []
                -- If the state's StateFulObject is a subject type (Srole), then obtain the user role instances from the context and pass on all that are in that state.
                State.Srole rtype -> lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA (roleIsInState stateId) >>= filterA notIsMe) userTypes)
                -- Construct an Array of object roles. If there is no forwards computation, it is just the roleInstance.
                -- If the forwards computation results in the object of the original, not inverted, query, run it on the roleInstance. NOTE: instead we use a weaker test: if it results in a role instance.
                -- Only if any of the object roles is in the required state, obtain the user role instances from the context and return them.
                State.Orole rtype -> do
                  objects <- case forwardsCompiled, isRoleDomain <<< range <$> (forwards description) of
                    Just f, Just true -> lift2 (roleInstance ##= unsafeCoerce f)
                    _, _ -> pure [roleInstance]
                  lift2 (findM (roleIsInState stateId) objects) >>= \mObject -> if isJust mObject
                    then lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                    else pure []

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
      (rolesExpressionsAreAppliedTo :: Array RoleInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance)
      foldM computeUsersFromRole [] rolesExpressionsAreAppliedTo

      where
        computeUsersFromRole :: Array ContextWithUsers -> RoleInstance -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromRole accumulatedUsers rid = foldM (computeUsersFromState rid) [] states

        -- | We only have to consider Srole and Orole cases.
        computeUsersFromState :: RoleInstance -> Array ContextWithUsers -> StateIdentifier -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromState rid accumulatedUsers stateId = (lift2 $ tryGetState stateId) >>=
          case _ of
            -- If there is no state, we assume we deal with an undefined root state, meaning there are no conditions at all.
            -- We then compute the users from the context of the role instance
            Nothing -> do
              cid <- lift2 (rid ##>> OG.context)
              singleton <<< Tuple cid <$> lift2 (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
            Just (State.State{stateFulObject}) ->
              case stateFulObject of
                State.Cnt _ -> throwError (error $ "fromRoleResults.computeUsersFromState: context states should not occur.")
                -- The role we've been lead back to is a user role. Check whether it is in the required state.
                State.Srole _ -> (lift2 $ roleIsInState stateId rid) >>= if _
                  then lift2 (rid ##>> OG.context) >>= \cid -> pure $ [Tuple cid [rid]]
                  else pure []
                -- The role we've been lead back to is an object role. If it is in the required state,
                -- compute the users having a perspective from its context.
                State.Orole _ -> (lift2 $ roleIsInState stateId rid) >>= if _
                  then do
                    cid <- lift2 (rid ##>> OG.context)
                    singleton <<< Tuple cid <$> lift2 (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                  else pure []

addInvertedQueryResult :: InvertedQueryResult -> MonadPerspectivesTransaction Unit
addInvertedQueryResult result = lift $ AA.modify \(Transaction r@{invertedQueryResults}) -> Transaction (r {invertedQueryResults = union [result] invertedQueryResults})

-----------------------------------------------------------
-- OBSERVINGCONTEXTS
-----------------------------------------------------------
-- | Computes an InvertedQueryResult for the given RoleInstance and its type.
-- | Just collect and add to the Transaction all contexts that have a role with
-- | a perspective on the RoleType in the context instance.
-- | Guarantees RULE TRIGGERING by adding InvertedQueryResults to the transaction.
-- | This function is used in the singular circumstance of contexts and roles that are read
-- | from file. We want to trigger state changes for each of its roles.
addRoleObservingContexts :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
addRoleObservingContexts id roleType roleInstance = do
  contextCalculations <- lift2 $ compileBothFor _onContextDelta_context roleType
  for_ contextCalculations (handleBackwardQuery roleInstance)
  roleCalculations <- lift2 $ compileBothFor _onContextDelta_role roleType
  for_ roleCalculations (handleBackwardQuery roleInstance)

-----------------------------------------------------------
-- FOR ROLE DELTAS
-----------------------------------------------------------
-- | Computes the AffectedContexts and adds them to the Transaction.
-- | Adds the users that should receive it, to the Delta.
-- | Adds users for SYNCHRONISATION, guarantees RULE TRIGGERING.
-- | Adds deltas for paths beyond the nodes involved in the binding,
-- | for queries that use the binder- or binding step.
-- | A binding represents two types of step: `binding` and `binder <TypeOfBinder>`
-- | This function finds all queries recorded on the types of the roles represented
-- | by id, the new binding and the old binding, that have such steps.
-- Implementation note: because we accept 'dangling' roles (roles with no context) we catch
-- errors when computing affected contexts.
aisInRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction (Array RoleInstance)
aisInRoleDelta (RoleBindingDelta dr@{id:binder, binding, oldBinding, deltaType}) = do
  binderType <- lift2 (binder ##>> OG.roleType)
  bindingCalculations <- lift2 $ compileBothFor _onRoleDelta_binding binderType
  users1 <- join <$> (for bindingCalculations
    -- Find all affected contexts, starting from the binder instance of the Delta.
    (\iq -> (handleBackwardQuery binder iq) >>= runForwardsComputation binder iq))
  -- All InvertedQueries with a backwards step that is `binder <TypeOfBinder>`, iff we actually bind something:
  users2 <- case binding of
    Just bnd -> do
      -- We've stored the relevant InvertedQueries with the type of the binder,
      -- so we execute them on any binding that is a specialisation of (or equal to) the required binding type.
      binderCalculations <- lift2 $ compileBothFor _onRoleDelta_binder binderType
      (for binderCalculations
        (\iq -> if isForSelfOnly iq
          then handleSelfOnlyQuery iq binder bnd
          else (handleBackwardQuery binder iq) >>= runForwardsComputation bnd iq)) >>= pure <<< join
    Nothing -> pure []
  -- All InvertedQueries with a backwards step that is `binder <TypeOfBinder>`, iff we actually overwrite something:
  users3 <- case oldBinding of
    Just bnd | deltaType == SetBinding -> do
      binderCalculations <- lift2 $ compileBothFor _onRoleDelta_binder binderType
      -- We deal here just with the case that we destroy a binding; not with the case that we add one.
      -- Hence, no forward computation. `handleBackwardQuery` just passes on users that have at least one
      -- valid perspective, even if the condition is object state.
      join <$> map snd <<< join <$> for binderCalculations (handleBackwardQuery bnd)
    otherwise -> pure []
  pure (nub $ union users1 (union users2 users3))

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
      forWithIndex_ (unwrap statesPerProperty)
        (\prop propStates -> do
          propGetter <- lift2 (makeChainGetter <$> (getterFromPropertyType prop))
          for_ propStates
            -- For each state that provides a perspective on the property,
            \stateIdentifier ->
              (lift2 $ tryGetState stateIdentifier) >>= case _ of
                -- If we deal with a roleInstance of a type for which no state has been defined,
                -- we should carry on as if the state condition was satisfied.
                Nothing -> for_ cwus
                  \(Tuple cid users) -> void $ lift2 (execMonadPerspectivesQuery roleInstance propGetter)
                    >>= (traverse (createDeltasFromAssumption users))
                Just (State.State{stateFulObject}) ->
                  -- if the stateful object...
                  case stateFulObject of
                    -- ... is the current context, then if it is in that state,
                    Cnt _ -> for_ cwus
                      \(Tuple cid users) -> (lift2 $ contextIsInState stateIdentifier cid) >>= if _
                        -- then create deltas for all resources visited while retrieving
                        -- the property, for all users;
                        then  void $ lift2 (execMonadPerspectivesQuery roleInstance propGetter)
                          >>= (traverse (createDeltasFromAssumption users))
                        else pure unit
                    -- ... is the subject role, for each user that is that state,
                    Srole _ -> for_ cwus
                      \(Tuple cid users) -> lift2 (filterA (roleIsInState stateIdentifier) users) >>=
                        \sanctionedUsers ->
                          -- create deltas for all resources while retrieving the property;
                           void $ lift2 (execMonadPerspectivesQuery roleInstance propGetter)
                            >>= (traverse (createDeltasFromAssumption sanctionedUsers))
                    -- ... is the object role, then if it is in that state,
                    Orole _ -> lift2 (roleIsInState stateIdentifier roleInstance) >>= if _
                      -- create deltas for all resources visited by the query while
                      -- retrieving the property, for all users;
                      then void $ lift2 (execMonadPerspectivesQuery roleInstance propGetter)
                        >>= (traverse (createDeltasFromAssumption (join $ snd <$> cwus)))
                      else pure unit
        )
    -- These are all other cases, where there still is some path to walk to the
    -- role (and its binding) that carries the properties.
    Just fw -> do
      -- Run the query interpreter on the same role instance as the backwards query,
      -- resulting in all the paths that lead up to a role result.
      (rinstances :: Array (DependencyPath)) <- lift2 ((singletonPath (R roleInstance)) ##= interpret fw)
      -- There can be zero or multiple state-valid perspectives on the results for each result, context and user.
      -- We analyse each of the possibilities and accumulate the results in the transaction, filtering out doubles when adding.
      for_ states
        \stateIdentifier -> (lift2 $ tryGetState stateIdentifier) >>= case _ of
          -- If we deal with a roleInstance of a type for which no state has been defined,
          -- we should carry on as if the state condition was satisfied.
          Nothing -> for_ (join (allPaths <$> rinstances)) (LNE.foldM (serialiseDependency (join $ snd <$> cwus)) Nothing)
          Just (State.State{stateFulObject}) ->
            case stateFulObject of
              -- if the context is in that state,
              Cnt _ -> for_ cwus
                \(Tuple cid users) -> (lift2 $ contextIsInState stateIdentifier cid) >>= if _
                  -- then create deltas for all resources visited by the query (as reflected in
                  -- the assumptions), for all users;
                  then for_ (join (allPaths <$> rinstances)) (LNE.foldM (serialiseDependency users) Nothing)
                  else pure unit
              -- otherwise, for each user that is that state,
              Srole _ -> for_ cwus
                \(Tuple cid users) -> lift2 (filterA (roleIsInState stateIdentifier) users) >>=
                  \sanctionedUsers ->
                    -- create deltas for all resources visited by the query (as reflected in
                    -- the assumptions);
                    for_ (join (allPaths <$> rinstances)) (LNE.foldM (serialiseDependency sanctionedUsers) Nothing)
              -- otherwise, for all paths that end in a role that is in that state,
              Orole _ -> (filterA
                (\{head} -> case head of
                  R rid -> lift2 (roleIsInState stateIdentifier rid)
                  -- If not a role domain, just return false. This will be a similar case to
                  -- computeUsersFromState.computeUsersFromState, case Orole. For example forward queries resulting
                  -- from inverted filtered queries end up in a Boolean, not in the object.
                  otherwise -> pure false)
                rinstances)
                  -- then create deltas for all resources visited by the query (as reflected in
                  -- the assumptions), for all users;
                  >>= pure <<< join <<< (map allPaths)
                  >>= traverse_ (LNE.foldM (serialiseDependency (join $ snd <$> cwus)) Nothing)

      -- For each property, get its value from the role instances found by the query interpreter,
      -- if the state condition is met. This is similar but not equal to the treatment
      -- of the case above where there was no forwards query.
      forWithIndex_ (unwrap statesPerProperty)
        (\prop propStates -> for_ propStates
          -- For each state that provides a perspective on the property,
          \stateIdentifier -> (lift2 $ tryGetState stateIdentifier) >>= case _ of
            -- If we deal with a roleInstance of a type for which no state has been defined,
            -- we should carry on as if the state condition was satisfied.
            Nothing -> for_ (join (allPaths <$> rinstances)) (LNE.foldM (serialiseDependency (join $ snd <$> cwus)) Nothing)
            Just (State.State{stateFulObject}) ->
              -- if the stateful object...
              case stateFulObject of
                -- ... is the current context, then if it is in that state,
                Cnt _ -> for_ cwus
                  \(Tuple cid users) -> (lift2 $ contextIsInState stateIdentifier cid) >>= if _
                    -- then run the interpreter on the property computation and the head of the dependency paths
                    -- and create deltas for all users
                    then for_ (_.head <$> rinstances)
                      \(dep :: Dependency) -> do
                        (vals :: Array DependencyPath) <- lift2 ((singletonPath dep) ##= getPropertyValues prop)
                        for_ (join (allPaths <$> vals)) (LNE.foldM (serialiseDependency users) Nothing)
                    else pure unit
                -- ... is the subject role, collect each user that is that state,
                Srole _ -> for_ cwus
                  \(Tuple cid users) -> lift2 (filterA (roleIsInState stateIdentifier) users) >>=
                    \sanctionedUsers ->
                      -- run the interpreter on the property computation and the head of the dependency paths
                      -- and create deltas for all collected users
                      for_ (_.head <$> rinstances)
                        \(dep :: Dependency) -> do
                          (vals :: Array DependencyPath) <- lift2 ((singletonPath dep) ##= getPropertyValues prop)
                          for_ (join (allPaths <$> vals)) (LNE.foldM (serialiseDependency sanctionedUsers) Nothing)
                -- ... is the object role, then for all paths that end in a role that is in that state,
                Orole _ -> (filterA
                  (\{head} -> case head of
                    R rid -> lift2 (roleIsInState stateIdentifier rid)
                    otherwise -> throwError (error ("runForwardsComputation (states per property) hits on a QueryInterpreter result that is not a role: " <> show otherwise)))
                  rinstances)
                    >>= pure <<< map _.head
                    -- run the interpreter on the property computation and the head of the dependency path
                    -- and create deltas for all users
                    >>= traverse_
                      \(dep :: Dependency) -> do
                        (vals :: Array DependencyPath) <- lift2 ((singletonPath dep) ##= getPropertyValues prop)
                        for_ (join (allPaths <$> vals)) (LNE.foldM (serialiseDependency (join $ snd <$> cwus)) Nothing)
        )
  pure $ join $ snd <$> cwus

-- | Add deltas for all the users to the current transaction, from the given assumption.
createDeltasFromAssumption :: Array RoleInstance -> InformedAssumption -> MonadPerspectivesTransaction Unit
createDeltasFromAssumption users (RoleAssumption ctxt roleTypeId) = do
  instances <- lift2 (ctxt ##= getRoleInstances (ENR roleTypeId))
  case SerializableNonEmptyArray <$> ANE.fromArray instances of
    Nothing -> pure unit
    Just instances' -> magic ctxt instances' roleTypeId users

-- The value of me of a context is indexed, and thus private. It never leads to a delta.
createDeltasFromAssumption users (Me _) = pure unit

createDeltasFromAssumption users (Binding roleInstance) = do
  mbnd <- lift2 (roleInstance ##> binding)
  case mbnd of
    Nothing -> pure unit
    Just bnd -> do
      ctxt <- lift2 (bnd ##>> OG.context)
      rtype <- lift2 (bnd ##>> OG.roleType)
      magic ctxt (SerializableNonEmptyArray $ ANE.singleton bnd) rtype users
      (try $ lift2 $ getPerspectRol roleInstance) >>=
        handlePerspectRolError "createDeltasFromAssumption:Binding"
          \(PerspectRol{bindingDelta}) -> case bindingDelta of
            Nothing -> pure unit
            Just bd -> addDelta $ DeltaInTransaction {users, delta: bd}

createDeltasFromAssumption users (Binder roleInstance roleType) = do
  bndrs <- lift2 (roleInstance ##= getRoleBinders roleType)
  -- There may not be a bndr!
  case ANE.fromArray bndrs of
    Nothing -> pure unit
    Just someBndrs -> do
      ctxt <- lift2 (ANE.head someBndrs ##>> OG.context)
      magic ctxt (SerializableNonEmptyArray someBndrs) roleType users
  for_ bndrs \bndr -> do
    (try $ lift2 $ getPerspectRol bndr) >>=
      handlePerspectRolError "createDeltasFromAssumption.Binder"
        \(PerspectRol{bindingDelta}) -> case bindingDelta of
          Nothing -> pure unit
          Just bd -> addDelta $ DeltaInTransaction {users, delta: bd}

createDeltasFromAssumption users (Property roleInstance propertyType) = do
  ctxt <- lift2 (roleInstance ##>> OG.context)
  rtype <- lift2 (roleInstance ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton roleInstance) rtype users
  (try $ lift2 $ getPerspectRol roleInstance) >>=
    handlePerspectRolError "createDeltasFromAssumption.Property"
      \(PerspectRol{propertyDeltas}) -> case lookup (unwrap propertyType) propertyDeltas of
        Nothing -> pure unit
        Just deltas -> for_ deltas \propertyDelta -> addDelta $ DeltaInTransaction {users, delta: propertyDelta}

createDeltasFromAssumption users (Context roleInstance) = do
  ctxt <- lift2 (roleInstance ##>> OG.context)
  rtype <- lift2 (roleInstance ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton roleInstance) rtype users

-- The forwards part of a QueryWithAKink in an onRoleDelta_binder or onRoleDelta_binding
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
  ctype <- lift2 (ctxt ##>> contextType)
  (try $ lift2 $ getPerspectContext ctxt) >>=
    handlePerspectContextError "Perspectives.CollectAffectedContexts.magic"
      -- Fetch the UniverseContextDelta from the context instance here.
      \(PerspectContext{universeContextDelta, buitenRol}) -> do
        (try $ lift2 $ getPerspectRol buitenRol) >>=
          handlePerspectRolError "Perspectives.CollectAffectedContexts.magic"
            \(PerspectRol{universeRoleDelta: externalRoleDelta}) -> do
              addDelta $ DeltaInTransaction {users, delta: externalRoleDelta}
              addDelta $ DeltaInTransaction {users, delta: universeContextDelta}
              for_ (toArray roleInstances) \roleInstance -> do
                (try $ lift2 $ getPerspectRol roleInstance) >>=
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
aisInPropertyDelta :: RoleInstance -> EnumeratedPropertyType -> EnumeratedRoleType -> MonadPerspectivesTransaction (Array RoleInstance)
aisInPropertyDelta id property eroleType = do
  calculations <- lift2 $ compileDescriptions' property
  -- `handleBackwardQuery` just passes on users that have at least one
  -- valid perspective, even if the condition is object state.
  (cwu :: Array ContextWithUsers) <- join <$> for calculations (handleBackwardQuery id)
  --
  lift2 $ filterA notIsMe (nub $ join $ snd <$> cwu)
  where
    compileDescriptions' :: EnumeratedPropertyType -> MonadPerspectives (Array InvertedQuery)
    compileDescriptions' rt@(EnumeratedPropertyType ert) =  do
      modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
      (try $ retrieveDomeinFile modelName) >>=
        handleDomeinFileError' "aisInPropertyDelta" []
          \(df :: DomeinFile) -> case preview (onPropertyDelta rt) df of
            Nothing -> pure []
            Just iqObj -> case lookup (unwrap eroleType) iqObj of
              Nothing -> pure []
              Just calculations -> if areCompiled calculations
                then pure calculations
                else do
                  compiledCalculations <- traverse compileBoth calculations
                  -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
                  modifyDomeinFileInCache (over (onPropertyDelta rt) (const (insert (unwrap eroleType) compiledCalculations iqObj))) modelName
                  pure compiledCalculations
      where
        onPropertyDelta :: EnumeratedPropertyType -> Traversal' DomeinFile (Object (Array InvertedQuery))
        onPropertyDelta (EnumeratedPropertyType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedProperties") <<< at x <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "onPropertyDelta")

-- | Changes the model in cache (but not in Couchdb). For a given lens that retrieves one of onRoleDelta_binder,
-- | onRoleDelta_binding, onContextDelta_role or onContextDelta_context, and an EnumeratedRoleType, compileBackwards the
-- | description in the AffectedContextCalculations.
-- | Compiles the `inverse` queries; not the forwards part.
compileBackwardFor :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array InvertedQuery)
compileBackwardFor onX rt = compileDescriptions_ onX rt false

compileBothFor :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array InvertedQuery)
compileBothFor onX rt = compileDescriptions_ onX rt true

-- | Compiles the backward part of the description and stores it in backwardsCompiled.
-- | If the argument bound to `forward` is true, then the forward part is compiled, too.
compileDescriptions_ :: CalculationsLens -> EnumeratedRoleType -> Boolean -> MonadPerspectives (Array InvertedQuery)
compileDescriptions_ onX rt@(EnumeratedRoleType ert) forward = do
  modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
  (try $ retrieveDomeinFile modelName) >>=
    handleDomeinFileError' "compileDescriptions_" []
    \(df :: DomeinFile) -> do
      -- Get the InvertedQueries in onContextDelta_context.
      (calculations :: Array InvertedQuery) <- pure $ unsafePartial $ fromJust $ preview (onDelta rt) df
      -- Compile the descriptions.
      if forward
        then if areCompiled calculations
          then pure calculations
          else do
            compiledCalculations <- traverse compileBoth calculations
            -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
            modifyDomeinFileInCache (over (onDelta rt) (const compiledCalculations)) modelName
            pure compiledCalculations
        else if areCompiled calculations
          then pure calculations
          else do
            compiledCalculations <- traverse compileBackwards calculations
            -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
            modifyDomeinFileInCache (over (onDelta rt) (const compiledCalculations)) modelName
            pure compiledCalculations
  where
    onDelta :: EnumeratedRoleType -> Traversal' DomeinFile (Array InvertedQuery)
    onDelta (EnumeratedRoleType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedRoles") <<< at x <<< traversed <<< _Newtype <<< onX

areCompiled :: Array InvertedQuery -> Boolean
areCompiled ar = case head ar of
  Nothing -> true
  Just (InvertedQuery{backwardsCompiled}) -> isJust backwardsCompiled

compileForwards :: InvertedQuery -> MP InvertedQuery
compileForwards ac@(InvertedQuery iqr@{description, forwardsCompiled}) = case forwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    c <- traverse getHiddenFunction (forwards description)
    pure $ InvertedQuery iqr {forwardsCompiled = c}

compileBackwards :: InvertedQuery -> MP InvertedQuery
compileBackwards ac@(InvertedQuery iqr@{description, backwardsCompiled}) = case backwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    c <- traverse getHiddenFunction (backwards description)
    pure $ InvertedQuery iqr {backwardsCompiled = (map unsafeCoerce c)}

compileBoth :: InvertedQuery -> MP InvertedQuery
compileBoth ac@(InvertedQuery iqr@{description, backwardsCompiled, forwardsCompiled}) = case backwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    backwards' <- traverse getHiddenFunction (backwards description)
    forwards' <- traverse getHiddenFunction (forwards description)
    pure $ InvertedQuery iqr{backwardsCompiled = (map unsafeCoerce backwards'), forwardsCompiled = (map unsafeCoerce forwards')}

_onContextDelta_context :: CalculationsLens
_onContextDelta_context = prop (SProxy :: SProxy "onContextDelta_context")

_onContextDelta_role :: CalculationsLens
_onContextDelta_role = prop (SProxy :: SProxy "onContextDelta_role")

_onRoleDelta_binding :: CalculationsLens
_onRoleDelta_binding = prop (SProxy :: SProxy "onRoleDelta_binding")

_onRoleDelta_binder :: CalculationsLens
_onRoleDelta_binder = prop (SProxy :: SProxy "onRoleDelta_binder")

type CalculationsLens = Lens' EnumeratedRoleRecord (Array InvertedQuery)

lift2 :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
lift2 = lift <<< lift
