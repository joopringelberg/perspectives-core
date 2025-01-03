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
import Data.Array (concat, cons, difference, elemIndex, filter, filterA, foldM, head, nub, null, union)
import Data.Array.NonEmpty (fromArray, singleton) as ANE
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (isEmpty)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (error)
import Foreign.Object (lookup) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspective.InvertedQuery.Indices (runTimeIndexForRoleQueries, runtimeIndexForContextQueries, runtimeIndexForFilledQueries', runtimeIndexForFillerQueries', runtimeIndexForPropertyQueries)
import Perspectives.ArrayUnions (ArrayUnions(..))
import Perspectives.Assignment.SerialiseAsDeltas (getPropertyValues, serialiseDependencies)
import Perspectives.ContextAndRole (isDefaultContextDelta)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MonadPerspectivesTransaction, runMonadPerspectivesQuery, (##=), (##>), (##>>))
import Perspectives.Data.EncodableMap (EncodableMap, filterKeys, lookup)
import Perspectives.Deltas (addDelta)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Me (notIsMe)
import Perspectives.Instances.ObjectGetters (allFillers, binding, context, context', contextIsInState, contextType, contextType_, getActiveRoleStates_, getFilledRoles, getRecursivelyAllFilledRoles, roleIsInState, roleType_)
import Perspectives.Instances.ObjectGetters (context, contextType, roleType) as OG
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), backwards, backwardsQueryResultsInContext, backwardsQueryResultsInRole, forwards, shouldResultInContextStateQuery, shouldResultInRoleStateQuery)
import Perspectives.InvertedQuery.Storable (getContextQueries, getFilledQueries, getFillerQueries, getPropertyQueries, getRoleQueries)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectContext, getPerspectRol, tryGetPerspectEntiteit)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, singletonPath)
import Perspectives.Query.QueryTypes (Domain(..), RoleInContext(..), domain, isRoleDomain, range)
import Perspectives.Query.UnsafeCompiler (getHiddenFunction, getRoleInstances)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (ContextType, StateIdentifier, tryGetState, getEnumeratedRole)
import Perspectives.Representation.Class.Property (propertyTypeIsAuthorOnly, propertyTypeIsSelfOnly)
import Perspectives.Representation.Class.Role (bindingOfRole, calculationOfRoleType, contextOfRepresentation)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.State (StateFulObject(..))
import Perspectives.Representation.State (StateFulObject(..), State(..)) as State
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), toArray)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (enumeratedRoleContextType, equalsOrSpecialisesRoleInContext)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..))
import Perspectives.Utilities (findM, prettyPrint)
import Prelude (Unit, bind, discard, flip, join, map, not, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (||))
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
usersWithPerspectiveOnRoleInstance ::  EnumeratedRoleType -> RoleInstance -> ContextInstance -> Boolean -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleInstance roleType roleInstance contextInstance runForwards = do
  -- CONTEXT STEP
  cType <- lift $ contextType_ contextInstance
  users1 <- do
    -- Find all InvertedQueryResults, starting from the role instance of the Delta (provided as second argument to this function).
    -- Each context is the start of a path through instance space that corresponds to a query expression in type space.
    -- The path passes through the roleInstance of the Delta.
    -- This means that the query, when re-run from the context, might yield a different result then before the
    -- mutation described by the Delta.
    -- Index the object of InvertedQueries that we find with each type of roleInstance with the type of its
    -- embedding context.
    -- No need to reflect on all Aspects of the roleType in runtime.
    -- We handle that by contextualizing Aspects in compile time. 

    (contextCalculations :: (Array InvertedQuery)) <- lift $ runtimeIndexForContextQueries roleType contextInstance >>= 
      getContextQueries compileBoth <<< unwrap 
        >>= (filterA (invertedQueryHasRoleDomain cType roleType))

    (for contextCalculations \iq -> if isForSelfOnly iq
      -- We now know this is the self-perspective of a multi-user role. Hence, the subject and object of the perspective is the same role type.
      -- If iq has the selfOnly modifier, we must apply another algorithm to the roleInstance and the roleInstance.
      -- An example: Pupil has a perspective on his Grade. However, this is personal. By adding `selfOnly` we ensure that each pupil just receives his own Grading, not that of others.
      then handleSelfOnlyQuery iq roleInstance
      else if runForwards
        then handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq <<< filter (\(Tuple context users) -> not $ null users)
        else handleBackwardQuery roleInstance iq >>= pure <<< join <<< map snd
        ) >>= pure <<< join

  -- ROLE STEP
  users2 <- do
    embeddingContextType <- lift (contextInstance ##>> contextType)
    (roleCalculations :: (Array InvertedQuery)) <- lift (unsafePartial runTimeIndexForRoleQueries roleType contextInstance 
        >>= getRoleQueries compileBoth <<< unwrap
          >>= filterA (invertedQueryHasRoleDomain cType roleType)
        )

      -- Find all InvertedQueryResults, starting from the new role instance of the Delta.
      -- We do not start on the context because the cardinality of the role getting step is larger than one and
      -- we want to make sure that the new binding is in the path for the users.
    (for roleCalculations \iq -> if isForSelfOnly iq 
      then handleSelfOnlyQuery iq roleInstance
      else if runForwards
        then handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq <<< filter (\(Tuple context users) -> not $ null users)
        else handleBackwardQuery roleInstance iq >>= pure <<< join <<< map snd
        ) >>= pure <<< join
  lift $ filterA notIsMe (nub $ union users1 users2)

  where
    -- | Handle InvertedQueryies with the selfOnly (personal) modifier (selfOnly applied to the perspective!).
    -- | The inverted query stems from a self-perspective on a multi(user) role.
    -- | This means that the new role instance can be a new peer. Only that peer should be sent de role and context deltas.
    handleSelfOnlyQuery :: InvertedQuery -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
    handleSelfOnlyQuery (InvertedQuery{backwardsCompiled, forwardsCompiled, description, users:userTypes, statesPerProperty}) newRoleInstance = do
      case backwardsCompiled of
        -- This case should not happen, as we start from a role instance.
        Nothing -> pure []
        Just bw -> do
          -- This computes the context instances that the peers come from.
          (Tuple cids _) <- lift $ runMonadPerspectivesQuery newRoleInstance (unsafeCoerce bw)
          -- Now interpret getting the instances of each userType and for each of them computing its properties.
          concat <<< concat <$> for cids 
            \cid -> for userTypes
              \userType -> do 
                -- The instances (as DependencyPaths) of this user type in this context instance.
                -- Why don't we use the forward part? Well, this InvertedQuery comes from a self-perspective.
                -- Consequently, the perspective is just on the user role (be it Enumerated or Calculated) itself. Hence the 
                -- calculation of that role must equal the forward part.
                -- Neither do we need the backward part to gather dependencies. It will all be in the calculation of the role type.
                (rinstances :: Array (DependencyPath)) <- lift (calculationOfRoleType userType >>= \calc -> singletonPath (C cid) ##= interpret calc)
                concat <$> for rinstances \rinstance -> case rinstance.head of 
                  R peer -> if peer == newRoleInstance
                    -- Regardless of whether the userType is Enumerated or Calculated, only the newRoleInstance may be informed. 
                    -- Notice that this must be a 'new peer' situation.
                    then do 
                      -- For each path that was used to compute this peer: serialise it.
                      for_ (allPaths rinstance) (serialiseDependencies [peer])
                      -- Compute properties for this peer in this perspective and serialise the dependencies.
                      computeProperties [rinstance] statesPerProperty [Tuple cid [peer]]
                      pure [peer]
                    else pure []
                  _ -> pure []

      where
        roleAtHead :: Partial => DependencyPath -> RoleInstance
        roleAtHead {head} = case head of
          R r -> r

    -- For binding the role arguments are userRoleArgForBackwardsQuery=filled, argForForwardsQuery=filler

type ContextWithUsers = Tuple ContextInstance (Array RoleInstance)

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
-- | The 'own' user is not one of them.
-- | INVARIANT TO RESPECT: both the backward- and forward part of the InvertedQuery should have been compiled.
handleBackwardQuery :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array ContextWithUsers)
handleBackwardQuery roleInstance iq@(InvertedQuery{description, backwardsCompiled, users:userTypes, states, forwardsCompiled}) = catchError
  (case backwardsCompiled of
    Nothing -> do 
      logPerspectivesError (Custom $ "Backwards is not compiled on " <> prettyPrint description) 
      pure []
    _ -> do
      if unsafePartial shouldResultInContextStateQuery iq
        then createContextStateQuery *> pure []
        else if unsafePartial shouldResultInRoleStateQuery iq
          then createRoleStateQuery *> pure []
            else usersWithAnActivePerspective roleInstance iq)
      (\e -> do
        logPerspectivesError (Custom $ show e)
        pure [])
  where
    -- | The InvertedQuery is based on a Context state condition.
    -- | This function adds, as a side effect, an InvertedQueryResult to the current transaction.
    createContextStateQuery :: MonadPerspectivesTransaction Unit
    createContextStateQuery = do
      (invertedQueryResults :: Array ContextInstance) <- lift (roleInstance ##= (unsafeCoerce (unsafePartial fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance))
      if null invertedQueryResults
        then pure unit
        else addInvertedQueryResult $ ContextStateQuery invertedQueryResults

    -- | The InvertedQuery is based on a Role state condition.
    -- | This function adds, as a side effect, an InvertedQueryResult to the current transaction.
    createRoleStateQuery :: MonadPerspectivesTransaction Unit
    createRoleStateQuery = do
      -- Apply the compiled backwards query.
      (affectedRoles :: Array RoleInstance) <- lift (roleInstance ##= (unsafeCoerce (unsafePartial fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance))
      addInvertedQueryResult $ RoleStateQuery affectedRoles

-- | The InvertedQuery is based on an explicit or implicit perspective.
-- | This function has no side effect but returns, bundled in their respective contexts, users with
-- | a valid perspective on a role (and properties) in their context.
-- | The contexts are computed by following the backwards query, meaning these are contexts that hold the userTypes in the InvertedQuery.
-- | These may be Enumerated in that context or Calculated starting from that context.
-- | They will receive Deltas that describe the change that triggered this InvertedQuery in the first place.
usersWithAnActivePerspective :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array ContextWithUsers)
usersWithAnActivePerspective roleInstance iq@(InvertedQuery{description, backwardsCompiled, users:userTypes, states, forwardsCompiled}) = if unsafePartial $ backwardsQueryResultsInRole iq
  then fromRoleResults
  else if unsafePartial $ backwardsQueryResultsInContext iq
    then fromContextResults
      else throwError (error "Programming error in handleBackwardQuery.usersWithAnActivePerspective: query should result\
      \in a role or a context.")
  where
    -- | The inverted query (its backward part) always leads to a context instance.
    -- | These InvertedQueries have been derived from explicit Perspectives, or from
    -- | 'implicit Perspectives' resulting from expressions in context state (so that the resource the expression is
    -- | applied to is a context instance). A context state condition is an example; another is expression that are in
    -- | scope of an "in context state" clause.
    -- | The object of such a perspective is computed from the context that the user having the perspective is in.
    -- | Hence, it inversion leads back to that context.
    fromContextResults :: MonadPerspectivesTransaction (Array ContextWithUsers)
    fromContextResults = do
      (contextsWithPerspectiveHolders :: Array ContextInstance) <- lift (roleInstance ##= (unsafeCoerce (unsafePartial fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance))
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
                -- Note that the context is (one of the) result(s) of the backwards query.
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
      (rolesExpressionsAreAppliedTo :: Array RoleInstance) <- lift (roleInstance ##= (unsafeCoerce (unsafePartial fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance))
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
                  then lift (rid ##>> OG.context) >>= (\cid -> pure $ cons (Tuple cid [rid]) accumulatedUsers)
                  else pure accumulatedUsers
                -- The role we've been lead back to is an object role. If it is in the required state,
                -- compute the users having a perspective from its context.
                State.Orole _ -> (lift $ roleIsInState stateId rid) >>= if _
                  then do
                    cid <- lift (rid ##>> OG.context)
                    flip cons accumulatedUsers <<< Tuple cid <$> lift (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                  else pure accumulatedUsers
    
-- | Adds the InvertedQueryResult to the current Transaction, but only if the resource is not marked as (to be) removed.
addInvertedQueryResult :: InvertedQueryResult -> MonadPerspectivesTransaction Unit
addInvertedQueryResult (ContextStateQuery ctxts) = AA.modify \(Transaction r@{invertedQueryResults, untouchableContexts}) -> case difference ctxts untouchableContexts of
  nothing | null nothing -> Transaction r
  ctxts' -> Transaction (r {invertedQueryResults = union [ContextStateQuery ctxts'] invertedQueryResults})
addInvertedQueryResult (RoleStateQuery roles) = AA.modify \(Transaction r@{invertedQueryResults, untouchableRoles}) -> case difference roles untouchableRoles of
  nothing | null nothing -> Transaction r
  roles' -> Transaction (r {invertedQueryResults = union [RoleStateQuery roles'] invertedQueryResults})

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
usersWithPerspectiveOnRoleBinding delta@(RoleBindingDelta dr@{filled, filler:mbinding, oldFiller:moldFiller, deltaType}) runForwards = 
  case mbinding of 
    Nothing -> pure []
    Just filler -> do
      -- We do not just fill `filled`, but also all roles it fills itself.
      allFilleds <- lift $ cons filled <$> getRecursivelyAllFilledRoles filled
      -- Also, we don't just fill with filler, but also with all its fillers.
      allFillers <- lift $ allFillers filled
      join <$> for (allPairs allFilleds allFillers) \(Tuple filled' filler') -> usersWithPerspectiveOnRoleBinding' filled' filler' moldFiller deltaType runForwards
  where
    allPairs :: Array RoleInstance -> Array RoleInstance -> Array (Tuple RoleInstance RoleInstance)
    allPairs filleds fillers = do
      filled' <- filleds
      filler <- fillers
      pure $ Tuple filled' filler

-- | This function SHOULD NOT handle the roles that are filled by the filled role; instead, it should be applied in a 
-- | recursive context that handles that. See usersWithPerspectiveOnRoleBinding, statesAndPeersForRoleInstanceToRemove
usersWithPerspectiveOnRoleBinding' :: RoleInstance -> RoleInstance -> Maybe RoleInstance -> RoleBindingDeltaType -> Boolean -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleBinding' filled filler moldFiller deltaType runForwards = do
  filledType <- lift (filled ##>> OG.roleType)
  fillerType <- lift (filler ##>> OG.roleType)
  filledContextType <- lift $ enumeratedRoleContextType filledType
  fillerContextType <- lift $ enumeratedRoleContextType fillerType
  -- Includes calculations from all types of filledType.
  (ArrayUnions fillerKeys) <- lift $ unsafePartial runtimeIndexForFillerQueries' filledType filledContextType
  fillerCalculations <- lift (getFillerQueries compileBoth fillerKeys
    >>= filterA (invertedQueryHasRoleDomain fillerContextType fillerType))
  -- FILLER step
  -- These are inverted queries that begin with the filler step starting from filled.
  (users1 :: Array RoleInstance) <- concat <$> for fillerCalculations
    -- Find all affected contexts, starting from the filler instance of the Delta (on storing the query, we left out the filler step).
    (\iq -> if isForSelfOnly iq 
      then handleSelfOnlyQuery iq filler filled
      else if runForwards
        -- However, we can skip that step and start the backwards part with the filler instead.
        then (handleBackwardQuery filler iq) >>= runForwardsComputation filled iq <<< filter (\(Tuple context users) -> not $ null users)
        else handleBackwardQuery filler iq >>= pure <<< concat <<< map snd
      )
  -- FILLED step
  -- These are inverted queries that begin with the filled step starting from filler.
  (ArrayUnions filledKeys) <- lift $ unsafePartial runtimeIndexForFilledQueries' filledType filledContextType
  filledCalculations <- lift (getFilledQueries compileBoth filledKeys
    >>= filterA (invertedQueryHasRoleDomain filledContextType filledType))
  (users2 :: Array RoleInstance) <- concat <$> for filledCalculations
      (\iq -> if isForSelfOnly iq
        -- These inverted queries skip the first step and so must be applied to the filled itself.
        -- The inverted query stems from a self-perspective on a multi(user) role.
        -- So a filler has been added that causes a new instance to appear in a (calculated) multi(user)role. 
        -- This in turn implies the user role instance (the filled role) existed prior to this fill modification, so it may have had properties.
        -- These properties must be syncrhonised for the bearer!
        then handleSelfOnlyQuery iq filled filler
        else if runForwards
          -- However, because of cardinality, we apply these queries to the filled role instead.
          then (handleBackwardQuery filled iq) >>= runForwardsComputation filler iq <<< filter (\(Tuple context users) -> not $ null users)
          else handleBackwardQuery filled iq >>= pure <<< concat <<< map snd
    )
  -- FILLER in case of overwriting an existing filler.
  -- All InvertedQueries with a backwards step that is `filler`, iff we actually overwrite something:
  (users3 :: Array RoleInstance) <- case moldFiller of
    Just oldFiller | deltaType == ReplaceBinding ->
      -- We deal here just with the case that we destroy a filler; not with the case that we add one.
      -- Hence, no forward computation. `handleBackwardQuery` just passes on users that have at least one
      -- valid perspective, even if the condition is object state.
      -- TODO: FIRSTONLY
      concat <<< map snd <$> (concat <$> for fillerCalculations (handleBackwardQuery oldFiller))
    otherwise -> pure []
  lift $ filterA notIsMe (nub $ union users1 (users2 `union` users3))
  
  where

    -- | Handle InvertedQueryies with the selfOnly (personal) modifier (selfOnly applied to the perspective!).
    -- | The inverted query stems from a self-perspective on a multi(user) role.
    -- | For each peer, adds the deltas gathered to compute that peer to the transaction for that peer only.
    handleSelfOnlyQuery :: InvertedQuery -> RoleInstance -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
    handleSelfOnlyQuery (InvertedQuery{backwardsCompiled, forwardsCompiled, description, users:userTypes, statesPerProperty}) backwardsArgument forwardsArgument = do
      case backwardsCompiled of
        -- This case should not happen, as we start from a role instance.
        Nothing -> pure []
        Just bw -> do
          -- This computes the context instances that the peers come from.
          (Tuple cids assumptions) <- lift $ runMonadPerspectivesQuery backwardsArgument (unsafeCoerce bw)
          -- Now interpret getting the instances of each userType and for each of them computing its properties.
          concat <<< concat <$> for cids 
            \cid -> for userTypes
              \userType -> case forwards description of
                -- No forwards computation: the backwardsArgument role must be the user role.
                Nothing -> f assumptions cid (singletonPath $ R backwardsArgument) backwardsArgument
                Just fd -> do 
                  -- The instances (as DependencyPaths) computed by the forwards path must represent user role instances, as this is a selfOnly query
                  -- and thus the object perspective is the very user role the perspective is for.
                  (rinstances :: Array (DependencyPath)) <- lift (singletonPath (R forwardsArgument) ##= interpret fd)
                  concat <$> for rinstances \rinstance -> case rinstance.head of 
                    R peer -> f assumptions cid rinstance peer
                    _ -> pure []

      where
        -- Notiche that the role instance in the head of the rinstance equals the peer.
        f :: ArrayWithoutDoubles InformedAssumption ->  ContextInstance -> DependencyPath -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
        f (ArrayWithoutDoubles assumptions) cid rinstance peer = do 
          -- For each path that was used to compute this peer: serialise it.
          for_ (allPaths rinstance) (serialiseDependencies [peer])
          -- Compute properties for this peer in this perspective and serialise the dependencies.
          computeProperties [rinstance] statesPerProperty [Tuple cid [peer]]
          -- turn the assumptions collected on the backwards path into deltas for this peer
          for_ assumptions (createDeltasFromAssumption [peer])
          -- finally return the peer (only this peer should be informed of the new binding).
          pure [peer]

        roleAtHead :: Partial => DependencyPath -> RoleInstance
        roleAtHead {head} = case head of
          R r -> r

-- | If the role instance is the object of a perspective, add deltas to the transaction for the user(s) of that perspective
-- | so that they will receive the data they have access to according to the perspective.
-- | These will be RoleBindingDeltas and PropertyDeltas.
-- | This function is called only from `setFirstBinding`.
addDeltasForPerspectiveObjects :: RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
addDeltasForPerspectiveObjects filled = do
  allFilleds <- lift $ cons filled <$> getRecursivelyAllFilledRoles filled
  -- Now compute all context-user combinations that have a perspective on this object
  nub <<< concat <$> for allFilleds \filled' -> do
    -- Each filled role might be a perspective object.
    contextInstance <- lift $ context' filled
    cType <- lift $ contextType_ contextInstance
    filledType <- lift (filled' ##>> OG.roleType)
    -- For each of them: get and compile perspective object queries for the step from that filled role to its context.
    -- (there may be state queries or calculated object queries that run through this segment, too, but we're not interested in them)
    (contextCalculations :: (Array InvertedQuery)) <- lift $ runtimeIndexForContextQueries filledType contextInstance >>= 
      getContextQueries compileBoth <<< unwrap 
        >>= (filterA (invertedQueryHasRoleDomain cType filledType))
          >>= pure <<< filter isPerspectiveObject
    -- Then for each query: apply it to obtain users that have a perspective on the filled role.
    nub <<< concat <$> for contextCalculations \iq@(InvertedQuery{statesPerProperty}) -> do 
      -- If iq has the selfOnly modifier, we must apply another algorithm to the roleInstance and the roleInstance.
      -- In that case, the filled role itself is also the user role instance (selfOnly can only be applied to self-perspectives).
      -- Consequently, the filled role is the only user that should be informed of the deltas in the calculation of the properties.
      -- Notice that authorOnly queries are not inverted. They cannot turn up here.
      if isForSelfOnly iq
        then do
          ctxt <- lift (context' filled)
          computeProperties [(singletonPath (R filled))] statesPerProperty [Tuple ctxt [filled]]
          pure [filled]
        else do 
          cwus <- handleBackwardQuery filled iq
          -- Then take the properties of the query and, for the users computed, apply computeProperties in order to add filled role deltas and property deltas.
          us <- pure (filter (\(Tuple context users) -> not $ null users) cwus)
          if null us
            then pure unit
            -- This function serialises dependencies from the interpretation result to deltas and adds them to the transaction for the users.
            else computeProperties [(singletonPath (R filled))] statesPerProperty us
          pure $ concat (snd <$> cwus)
  
  where 
    isPerspectiveObject :: InvertedQuery -> Boolean
    isPerspectiveObject (InvertedQuery{forwardsCompiled}) = isNothing forwardsCompiled


-----------------------------------------------------------
-- RE-EVALUATE CONSEQUENCES OF CHANGES TO PUBLIC FILLERS
-----------------------------------------------------------
-- | For a role that is filled by a public role, follow all queries from that filled role
-- | that come from the filler role.
-- | This will add the relevant InvertedQueryResults (for STATE EVALUATION) to the current Transaction.
reEvaluatePublicFillerChanges :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
reEvaluatePublicFillerChanges filled filler = do
  filledType <- lift (filled ##>> OG.roleType)
  filledContextType <- lift ( filled ##>> context >=> OG.contextType)
  -- Don't take the actual filler type; work with the required filler type. That is the type required as binding by the type of the filled.
  mfillerRequirements <- lift $ bindingOfRole $ ENR filledType
  case mfillerRequirements of 
    Nothing -> pure unit
    Just fillerRequirements -> do 
      results <- lift $ for (allLeavesInADT fillerRequirements) \(RoleInContext {context:fillerContextType, role: fillerType'}) -> 
        runtimeIndexForFilledQueries' filledType filledContextType
      (ArrayUnions filledKeys) <- pure $ join $ ArrayUnions results
      -- Fetch and compile these queries.
      filledCalculations <- lift (getFilledQueries compileBoth filledKeys)  
      -- No need to do a handle selfOnly (personal) queries differently, as we're only interested in state changes here
      -- and handleSelfOnlyQuery is for synchronizing.
      for_ filledCalculations (handleBackwardQuery filled)

-- | Runs the forward part of the QueryWithAKink. That is part of the original query. Assumptions collected
-- | during evaluation are turned into Deltas for peers with a perspective and collected in the current transaction.
-- | These peers are provided as user role instances grouped together with their context.
-- | The System User is excluded (no deltas generated for him).
-- | Apply this function for its side effects in state. The functional result is just based on what is passed in as the last parameter.
-- | NOTE: should not be called on a selfOnly query.
runForwardsComputation ::
  RoleInstance ->
  InvertedQuery ->
  (Array ContextWithUsers) ->
  MonadPerspectivesTransaction (Array RoleInstance)
runForwardsComputation roleInstance (InvertedQuery{description, forwardsCompiled, statesPerProperty, states}) cwus = if null (filter (\(Tuple context users) -> not $ null users) cwus)
  then pure []
  else do
    case forwards description of
      Nothing -> do
        -- This case arises for example for a perspective on an EnumeratedRoleType in the same context.
        -- Such a perspective will have properties (no sense in providing a perspective with just role verbs,
        -- because instances of such a role cannot be shown).
        -- Another case that the roleInstance has just been added as a binding to a role a user has a perspective on.
        -- For each property, get its value from the role instance, if the state condition is met.
        -- When there are no properties, we add the deltas for the role instance anyway.
        -- This covers the case of a new binding for a perspective without properties.
        PerspectRol{pspType, context} <- lift $  getPerspectRol roleInstance
        if (isEmpty (unwrap statesPerProperty))
          then magic context (SerializableNonEmptyArray $ ANE.singleton roleInstance) pspType (join $ snd <$> cwus)
          else pure unit
        computeProperties [singletonPath (R roleInstance)] statesPerProperty cwus

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

        if isRoleDomain $ range fw
          then computeProperties rinstances statesPerProperty cwus
          else pure unit
    pure $ join $ snd <$> cwus

-- For each property, get its value from the role instances found by the query interpreter,
-- if the state condition is met. This is similar but not equal to the treatment
-- of the case above where there was no forwards query.
-- Do this only for inverted queries that result in a role domain.
computeProperties :: Array (DependencyPath) -> EncodableMap PropertyType (Array StateIdentifier) -> (Array ContextWithUsers) -> MonadPerspectivesTransaction Unit
computeProperties rinstances statesPerProperty cwus = forWithIndex_ (unwrap statesPerProperty) g
  where 
  g :: PropertyType -> Array StateIdentifier -> MonadPerspectivesTransaction Unit
  g prop propStates = do 
    isAuthorOnly <- lift (propertyTypeIsAuthorOnly prop)
    isSelfOnly <- lift (propertyTypeIsSelfOnly prop)
    if isAuthorOnly
      then pure unit
      else for_ propStates
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
                  then for_ (_.head <$> rinstances) (f users isSelfOnly)
                  else pure unit
              -- ... is the subject role, collect each user that is that state,
              Srole _ -> for_ cwus
                \(Tuple _ users) -> lift (filterA (roleIsInState stateIdentifier) users) >>=
                  \sanctionedUsers ->
                    -- run the interpreter on the property computation and the head of the dependency paths
                    -- and create deltas for all collected users
                    for_ (_.head <$> rinstances) (f sanctionedUsers isSelfOnly)
              -- ... is the object role, then for all paths that end in a role that is in that state,
              Orole _ -> (filterA
                (\{head} -> case head of
                  R rid -> lift (roleIsInState stateIdentifier rid)
                  otherwise -> throwError (error ("computeProperties (states per property) hits on a QueryInterpreter result that is not a role: " <> show otherwise)))
                rinstances)
                  >>= pure <<< map _.head
                  -- run the interpreter on the property computation and the head of the dependency path
                  -- and create deltas for all users
                  >>= traverse_ (f (join $ snd <$> cwus) isSelfOnly)

    where
      f :: Array RoleInstance -> Boolean -> Dependency -> MonadPerspectivesTransaction Unit
      f  users isSelfOnly dep = if isSelfOnly 
        then case dep of
          R user -> do 
            (vals :: Array DependencyPath) <- lift ((singletonPath dep) ##= getPropertyValues prop)
            for_ (join (allPaths <$> vals)) (serialiseDependencies [user])
          _ -> pure unit
        else do
          (vals :: Array DependencyPath) <- lift ((singletonPath dep) ##= getPropertyValues prop)
          for_ (join (allPaths <$> vals)) (serialiseDependencies users)
    

-- | Add deltas for all the users to the current transaction, from the given assumption.
createDeltasFromAssumption :: Array RoleInstance -> InformedAssumption -> MonadPerspectivesTransaction Unit
createDeltasFromAssumption users (RoleAssumption ctxt roleTypeId) = do
  instances <- lift (ctxt ##= getRoleInstances (ENR roleTypeId))
  case SerializableNonEmptyArray <$> ANE.fromArray instances of
    Nothing -> pure unit
    Just instances' -> magic ctxt instances' roleTypeId users

-- The value of me of a context is indexed, and thus private. It never leads to a delta.
createDeltasFromAssumption users (Me _) = pure unit

createDeltasFromAssumption users (Filler roleInstance) = do
  mbnd <- lift (roleInstance ##> binding)
  case mbnd of
    Nothing -> pure unit
    Just bnd -> do
      ctxt <- lift (bnd ##>> OG.context)
      rtype <- lift (bnd ##>> OG.roleType)
      magic ctxt (SerializableNonEmptyArray $ ANE.singleton bnd) rtype users
      (try $ lift $ getPerspectRol roleInstance) >>=
        handlePerspectRolError "createDeltasFromAssumption:Filler"
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
      \(PerspectRol{propertyDeltas}) -> case OBJ.lookup (unwrap propertyType) propertyDeltas of
        Nothing -> pure unit
        Just deltas -> void $ for deltas \propertyDelta -> addDelta $ DeltaInTransaction {users, delta: propertyDelta}

createDeltasFromAssumption users (Context roleInstance) = do
  ctxt <- lift (roleInstance ##>> OG.context)
  rtype <- lift (roleInstance ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton roleInstance) rtype users

-- The forwards part of a QueryWithAKink in an filled InvertedQuery or filler InvertedQuery
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

-- | Add a UniverseContextDelta, UniverseRoleDelta and a ContextDelta to the current Transaction.
magic :: ContextInstance -> SerializableNonEmptyArray RoleInstance -> EnumeratedRoleType ->  Array RoleInstance -> MonadPerspectivesTransaction Unit
magic ctxt roleInstances rtype users =  do
  ctype <- lift (ctxt ##>> contextType)
  (try $ lift $ getPerspectContext ctxt) >>=
    handlePerspectContextError "Perspectives.CollectAffectedContexts.magic"
      -- Fetch the UniverseContextDelta from the context instance here.
      \(PerspectContext{universeContextDelta, buitenRol}) -> do
        (try $ lift $ getPerspectRol buitenRol) >>=
          handlePerspectRolError "Perspectives.CollectAffectedContexts.magic"
            \(PerspectRol{universeRoleDelta: externalRoleDelta, contextDelta: eContextDelta}) -> do
              addDelta $ DeltaInTransaction {users, delta: externalRoleDelta}
              addDelta $ DeltaInTransaction {users, delta: universeContextDelta}
              addDelta $ DeltaInTransaction {users, delta: eContextDelta}
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
aisInPropertyDelta :: RoleInstance -> RoleInstance -> EnumeratedPropertyType -> EnumeratedPropertyType -> EnumeratedRoleType -> MonadPerspectivesTransaction (Array RoleInstance)
aisInPropertyDelta 
  instanceOnPath            -- The role instance that occurs in the query path
  propertyBearingInstance   -- Identifies the role instance that actually bears the property
  property                  -- the name of the property taken from the assignment statement
  replacementProperty       -- the actual name of the property on the role instance that bears it
  propertyBearingType                 -- the type of the role instance that actually bears the property.
  = (lift $ propertyTypeIsAuthorOnly $ ENP replacementProperty) >>= if _ 
    then pure []
    else do
      typeOfInstanceOnpath <- lift $ roleType_ instanceOnPath
      (allKeys :: Array RunTimeInvertedQueryKey) <- lift $ runtimeIndexForPropertyQueries 
        typeOfInstanceOnpath 
        propertyBearingType 
        property 
        replacementProperty
      cType <- lift $ getEnumeratedRole propertyBearingType >>= pure <<< contextOfRepresentation 
      allCalculations' <- lift $ getPropertyQueries compileBoth allKeys
      allCalculations <- lift $ filterA (invertedQueryHasRoleRange cType propertyBearingType) allCalculations'
      -- `handleBackwardQuery` will actually not return any users since we have no property queries for properties in a perspective and a perspective itself is always on a role.
      -- However, there may be state queries that must be re-evaluated. We conveniently capture both role- and context state queries through handleBackwardQuery
      for_ allCalculations (handleBackwardQuery propertyBearingInstance)
      -- The property might fall in a perspective. Compute the users and add deltas to the transaction.
      users <- addDeltasForPropertyChange propertyBearingInstance property replacementProperty
      pure (nub users)

-- | If the role instance or any of the roles it fills is the object of a perspective, add deltas to the transaction for the user(s) of that perspective
-- | so that they will receive deltas that inform them about the property change.
-- | These will be RoleBindingDeltas and PropertyDeltas.
-- | This function has similarity to `addDeltasForPerspectiveObjects`. However, a major difference is that with `addDeltasForPropertyChange`
-- | we have a single property and we also have the role instance that represents a value for that property.
-- | With `addDeltasForPerspectiveObjects` we have multiple properties, take state into account and look for the property on the fillers
-- | of the role instance.
addDeltasForPropertyChange :: RoleInstance -> EnumeratedPropertyType -> EnumeratedPropertyType -> MonadPerspectivesTransaction (Array RoleInstance)
addDeltasForPropertyChange roleWithPropertyValue property replacementProperty = do
  isSelfOnlyProperty <- lift (propertyTypeIsSelfOnly $ ENP replacementProperty)
  allFilleds <- lift $ cons roleWithPropertyValue <$> getRecursivelyAllFilledRoles roleWithPropertyValue
  -- It is as if all these filled roles now have a (changed) value for this property.
  nub <<< concat <$> for allFilleds \roleWithPropertyValue' -> do
    -- Each roleWithPropertyValue' role might be a perspective object.
    contextInstance <- lift $ context' roleWithPropertyValue'
    cType <- lift $ contextType_ contextInstance
    rType <- lift (roleWithPropertyValue' ##>> OG.roleType)
    states <- lift (getActiveRoleStates_ roleWithPropertyValue')
    -- For each of them: get and compile perspective object queries for the step from that roleWithPropertyValue role to its context
    -- (there may be state queries or calculated object queries that run through this segment, too, but we're not interested in them).
    -- These can be Enumerated user role instances from the same context, but also Enumerated user role instance computed by an inverted Calculated Perspective object.
    -- We can understand a calculated perspective object from another context than its users equally well as a calculated user in the context of the perspective object! 
    -- Not all inverted queries are of interest to the current property change. We start by filtering out queries that don't mention the property at all.
    (contextCalculations :: (Array InvertedQuery)) <- lift $ runtimeIndexForContextQueries rType contextInstance 
      >>= getContextQueries compileBoth <<< unwrap
        >>= (filterA (invertedQueryHasRoleDomain cType rType))
          >>= pure <<< filter isPerspectiveObject
    -- Then for each query: apply it to obtain users that have a perspective on the roleWithPropertyValue role. State is being taken into consideration here,
    -- but only for the context calculation itself. Not for the computation of the property!
    nub <<< concat <$> for contextCalculations \iq@(InvertedQuery{statesPerProperty}) -> do 
      cwus <- usersWithAnActivePerspective roleWithPropertyValue' iq
      -- Then restrict the properties of the query to `property` and `replacementProperty` and, for the users computed, apply computeProperties in order to add roleWithPropertyValue role deltas and property deltas.
      -- Do so only when the subject state requirement of the inverted query is met by the actual state of the subjects.
      -- (computeProperties takes care of that)
      cwus' <- pure (filter (\(Tuple context users) -> not $ null users) cwus)
      if null cwus'
        -- For a selfOnly query, this should not happen.
        then pure []
        else if isForSelfOnly iq || isSelfOnlyProperty
          -- If iq has the selfOnly modifier, the perspective object equals the user role that has the perspective.
          -- Only compute the property for that role instance!
          then do 
            computeProperties [(singletonPath (R roleWithPropertyValue'))] (filterKeys (\k -> isJust $ elemIndex k [ENP property, ENP replacementProperty]) statesPerProperty) [(Tuple contextInstance [roleWithPropertyValue'])]
            pure [roleWithPropertyValue']
          else do 
            computeProperties [(singletonPath (R roleWithPropertyValue'))] (filterKeys (\k -> isJust $ elemIndex k [ENP property, ENP replacementProperty]) statesPerProperty) cwus'
            pure $ concat (snd <$> cwus)  -- Should this not be cwus'??
  
  where 
    -- It must be a perspective on the right property! 
    isPerspectiveObject :: InvertedQuery -> Boolean
    isPerspectiveObject (InvertedQuery{forwardsCompiled, statesPerProperty}) = if isNothing forwardsCompiled
      then (isJust $ lookup (ENP property) statesPerProperty) || (isJust $ lookup (ENP replacementProperty) statesPerProperty)
      else false

-- | Compiles both the backwards and forwards functions of an `InvertedQuery`.
-- | If the backwards function is already compiled, it returns the original `InvertedQuery`.
-- | Otherwise, it attempts to compile both the backwards and forwards functions.
-- | If the backwards function cannot be compiled, it logs an error.
-- |
-- | Parameters:
-- | - `ac`: The `InvertedQuery` to be compiled.
-- |
-- | Returns:
-- | - The `InvertedQuery` with the compiled backwards and forwards functions.
-- |
-- | Errors:
-- | - Logs an error if the backwards function cannot be compiled.
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
    pure $ InvertedQuery iqr{backwardsCompiled = backwards', forwardsCompiled = forwards'}

-- The backwards part of the query with a kink should have a compatible domain
invertedQueryHasRoleDomain :: ContextType -> EnumeratedRoleType -> InvertedQuery -> MP Boolean
invertedQueryHasRoleDomain context role (InvertedQuery{description}) = case description of 
  ZQ (Just qfd) _ -> case domain qfd of 
    RDOM adt -> do 
      r <- ST (RoleInContext{context, role}) `equalsOrSpecialisesRoleInContext` adt
      pure r
    _ -> pure false
  _ -> pure false

invertedQueryHasRoleRange :: ContextType -> EnumeratedRoleType -> InvertedQuery -> MP Boolean
invertedQueryHasRoleRange context role (InvertedQuery{description}) = case description of 
  ZQ (Just qfd) _ -> case range qfd of 
    RDOM adt -> do 
      r <- ST (RoleInContext{context, role}) `equalsOrSpecialisesRoleInContext` adt
      pure r
    _ -> pure false
  _ -> pure false
