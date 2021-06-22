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
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (error)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.SerialiseAsDeltas (getPropertyValues, serialiseDependency)
import Perspectives.ContextAndRole (isDefaultContextDelta)
import Perspectives.CoreTypes (type (~~>), InformedAssumption(..), MP, MonadPerspectives, MonadPerspectivesTransaction, execMonadPerspectivesQuery, (###=), (##=), (##>), (##>>))
import Perspectives.Deltas (addDelta)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Error.Boundaries (handleDomeinFileError', handlePerspectContextError, handlePerspectRolError)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, contextIsInState, contextType, getRoleBinders, notIsMe, roleIsInState)
import Perspectives.Instances.ObjectGetters (roleType, context) as OG
import Perspectives.InvertedQuery (InvertedQuery(..), backwards, backwardsQueryResultsInContext, backwardsQueryResultsInRole, forwards, shouldResultInContextStateQuery, shouldResultInRoleStateQuery)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, singletonPath)
import Perspectives.Query.UnsafeCompiler (getHiddenFunction, getRoleInstances, getterFromPropertyType)
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getState)
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
-- Notice that even though we compute the users for a single given RoleInstance, we can use that result
-- for any other instance of the same RoleType. This will no longer hold when we add filtering to the inverted queries
-- (because then the affected contexts found will depend on the properties of the RoleInstance, too).
usersWithPerspectiveOnRoleInstance ::  ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleInstance id roleType roleInstance = do
  users1 <- do
      -- Find all InvertedQueryResults, starting from the role instance of the Delta (provided as third argument to this function).
      -- Each context is the start of a path through instance space that corresponds to a query expression in type space.
      -- The path passes through the roleInstance of the Delta.
      -- This means that the query, when re-run from the context, might yield a different result then before the
      -- mutation described by the Delta.
    contextCalculations <- lift2 (roleType ###= (roleAspectsClosure >=> ArrayT <<< compileBothFor _onContextDelta_context))
    (for contextCalculations \iq -> handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq) >>= pure <<< join
  users2 <- do
    roleCalculations <- lift2 (roleType ###= (roleAspectsClosure >=> ArrayT <<< compileBothFor _onContextDelta_role))
      -- Find all InvertedQueryResults, starting from the new role instance of the Delta.
      -- We do not start on the context because the cardinality of the role getting step is larger than one and
      -- we want to make sure that the new binding is in the path for the users.
    (for roleCalculations \iq ->
      handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq) >>= pure <<< join
  pure $ nub $ union users1 users2

type ContextWithUsers = Tuple ContextInstance (Array RoleInstance)

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
handleBackwardQuery roleInstance iq@(InvertedQuery{backwardsCompiled, users:userTypes, states, forwardsCompiled}) = do
  if unsafePartial shouldResultInContextStateQuery iq
    then createContextStateQuery
    else if unsafePartial shouldResultInRoleStateQuery iq
      then createRoleStateQuery
      else usersWithAnActivePerspective
  where
    createContextStateQuery :: MonadPerspectivesTransaction (Array ContextWithUsers)
    createContextStateQuery = do
      (invertedQueryResults :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      addInvertedQueryResult $ ContextStateQuery invertedQueryResults
      pure []

    createRoleStateQuery :: MonadPerspectivesTransaction (Array ContextWithUsers)
    createRoleStateQuery = do
      (affectedRoles :: Array RoleInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance)
      addInvertedQueryResult $ RoleStateQuery affectedRoles
      pure []

    -- TODO. Het lijkt me dat hier alleen nog maar queries langskomen die een context resultaat opleveren?!
    usersWithAnActivePerspective :: MonadPerspectivesTransaction (Array ContextWithUsers)
    usersWithAnActivePerspective = if unsafePartial $ backwardsQueryResultsInRole iq
      -- then fromRoleResults
      then throwError (error "Programming error in handleBackwardQuery.usersWithAnActivePerspective: query should not result\
      \in a role.")
      else if unsafePartial $ backwardsQueryResultsInContext iq
        then fromContextResults
          else throwError (error "Programming error in handleBackwardQuery.usersWithAnActivePerspective: query should result\
          \in context.")

    fromContextResults :: MonadPerspectivesTransaction (Array ContextWithUsers)
    fromContextResults = do
      (invertedQueryResults :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      foldM computeUsersFromContext [] invertedQueryResults

      where
        -- The currently inefficient version accumulates users from each state examined.
        -- The efficient version stops as soon as a context type state is valid.
        computeUsersFromContext :: Array ContextWithUsers -> ContextInstance -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromContext accumulatedUsers cid = foldM (computeUsersFromState cid) [] states

        computeUsersFromState :: ContextInstance -> Array ContextWithUsers -> StateIdentifier -> MonadPerspectivesTransaction (Array ContextWithUsers)
        computeUsersFromState cid accumulatedUsers stateId = do
          State.State{stateFulObject} <- lift2 $ getState stateId
          case stateFulObject of
            -- If the state's StateFulObject is a context type (Cnt), and the context is in that state, obtain all user role instances from the context (and we're done)
            State.Cnt _ -> (lift2 $ contextIsInState stateId cid) >>= if _
              then lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
              else pure []
            -- If the state's StateFulObject is a subject type (Srole), then obtain the user role instances from the context and pass on all that are in that state.
            State.Srole rtype -> lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA (roleIsInState stateId) >>= filterA notIsMe) userTypes)
            -- Construct an Array of object roles. If there is no forwards computation, it is just the roleInstance.
            -- Otherwise run the forwards computation on the roleInstance.
            -- Only if any of the object roles is in the required state, obtain the user role instances from the context and return them.
            State.Orole rtype -> do
              objects <- case forwardsCompiled of
                Nothing -> pure [roleInstance]
                Just f -> lift2 (roleInstance ##= unsafeCoerce f)
              lift2 (findM (roleIsInState stateId) objects) >>= \mObject -> if isJust mObject
                then lift2 $ singleton <<< Tuple cid <$> (join <$> traverse (\userType -> (cid ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)
                else pure []

    -- TODO. Waarschijnlijk wordt deze functie nooit geevalueerd.
    -- fromRoleResults :: MonadPerspectivesTransaction (Array ContextWithUsers)
    -- fromRoleResults = do
    --   (invertedQueryResults :: Array RoleInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance)
    --   -- Return users only for those roles that are in one of the permissable states.
    --   (allowed :: Array RoleInstance) <- filterA
    --     (\rid -> (lift2 $ getActiveRoleStates_ rid) >>= pure <<< not <<< null <<< intersect states)
    --     invertedQueryResults
    --   join <$> for allowed \ri -> do
    --     -- Remove 'me'
    --     -- If a role cannot be found, we remove it, erring on the safe side (notIsMe has an internal error boundary).
    --     lift2 (join <$> traverse (\userType -> (ri ##= OG.context >=> getRoleInstances userType) >>= filterA notIsMe) userTypes)

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
-- | A binding represents two types of step: `binding` and `binder <TypeOfBinder>`
-- | This function finds all queries recorded on the types of the roles represented
-- | by id, the new binding and the old binding, that have such steps.
-- Implementation note: because we accept 'dangling' roles (roles with no context) we catch
-- errors when computing affected contexts.
aisInRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction (Array RoleInstance)
aisInRoleDelta (RoleBindingDelta dr@{id, binding, oldBinding, deltaType}) = do
  binderType <- lift2 (id ##>> OG.roleType)
  bindingCalculations <- lift2 $ compileBothFor _onRoleDelta_binding binderType
  users1 <- join <$> (for bindingCalculations
    -- Find all affected contexts, starting from the binder (id) instance of the Delta.
    (\iq -> (handleBackwardQuery id iq) >>= runForwardsComputation id iq))
  -- All InvertedQueries with a backwards step that is `binder <TypeOfBinder>`, iff we actually bind something:
  users2 <- case binding of
    Just bnd -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileBothFor _onRoleDelta_binder bindingType
      (for binderCalculations
        (\iq -> (handleBackwardQuery id iq) >>= runForwardsComputation bnd iq)) >>= pure <<< join
    Nothing -> pure []
  -- All InvertedQueries with a backwards step that is `binder <TypeOfBinder>`, iff we actually overwrite something:
  users3 <- case oldBinding of
    Just bnd | deltaType == SetBinding -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileBothFor _onRoleDelta_binder bindingType
      -- We deal here just with the case that we destroy a binding; not with the case that we add one.
      -- Hence, no forward computation. `handleBackwardQuery` just passes on users that have at least one
      -- valid perspective, even if the condition is object state.
      join <$> map snd <<< join <$> for binderCalculations (handleBackwardQuery bnd)
    otherwise -> pure []
  pure (nub $ union users1 (union users2 users3))

-- RunForwardsComputation only uses the users provided to push deltas.
-- This should not be done for the own user!
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
      -- For each property, get its value from the role instance, if the state condition is met.
      forWithIndex_ (unwrap statesPerProperty)
        (\prop propStates -> for_ propStates
          -- For each state that provides a perspective on the property,
          \stateIdentifier -> do
            State.State{stateFulObject} <- lift2 $ getState stateIdentifier
            -- if the stateful object...
            case stateFulObject of
              -- ... is the current context, then if it is in that state,
              Cnt _ -> for_ cwus
                \(Tuple cid users) -> (lift2 $ contextIsInState stateIdentifier cid) >>= if _
                  -- then create deltas for all resources visited while retrieving
                  -- the property, for all users;
                  then lift2 (getterFromPropertyType prop)
                    >>= lift2 <<< (execMonadPerspectivesQuery roleInstance)
                    >>= void <<< (traverse (createDeltasFromAssumption users))
                  else pure unit
              -- ... is the subject role, for each user that is that state,
              Srole _ -> for_ cwus
                \(Tuple cid users) -> lift2 (filterA (roleIsInState stateIdentifier) users) >>=
                  \sanctionedUsers ->
                    -- create deltas for all resources while retrieving the property;
                    lift2 (getterFromPropertyType prop)
                      >>= lift2 <<< (execMonadPerspectivesQuery roleInstance)
                      >>= void <<< (traverse (createDeltasFromAssumption sanctionedUsers))
              -- ... is the object role, then if it is in that state,
              Orole _ -> lift2 (roleIsInState stateIdentifier roleInstance) >>= if _
                -- create deltas for all resources visited by the query while
                -- retrieving the property, for all users;
                then lift2 (getterFromPropertyType prop)
                  >>= lift2 <<< (execMonadPerspectivesQuery roleInstance)
                  >>= void <<< (traverse (createDeltasFromAssumption (join $ snd <$> cwus)))
                else pure unit
        )
    -- These are all other cases, where there still is some path to walk to the
    -- role (and its binding) that carries the properties.
    Just fw -> do
      currentContext <- lift2 (roleInstance ##>> OG.context)
      -- Run the query interpreter on the same role instance as the backwards query,
      -- resulting in all the paths that lead up to a role result.
      (rinstances :: Array (DependencyPath)) <- lift2 ((singletonPath (C currentContext)) ##= interpret fw)
      -- There can be zero or multiple state-valid perspectives on the results for each result, context and user.
      -- We analyse each of the possibilities and accumulate the results in the transaction, filtering out doubles when adding.
      for_ states
        \stateIdentifier -> do
          State.State{stateFulObject} <- lift2 $ getState stateIdentifier
          case stateFulObject of
            -- if the context is in that state,
            Cnt _ -> for_ cwus
              \(Tuple cid users) -> (lift2 $ contextIsInState stateIdentifier cid) >>= if _
                -- then create deltas for all resources visited by the query (as reflected in
                -- the assumptions), for all users;
                then for_ (join (allPaths <$> rinstances)) (LNE.foldM (serialiseDependency (join $ snd <$> cwus)) Nothing)
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
                otherwise -> throwError (error ("runForwardsComputation hits on a QueryInterpreter result that is not a role: " <> show otherwise)))
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
          \stateIdentifier -> do
            State.State{stateFulObject} <- lift2 $ getState stateIdentifier
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
                  otherwise -> throwError (error ("runForwardsComputation hits on a QueryInterpreter result that is not a role: " <> show otherwise)))
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

-- Add deltas for all the users.
createDeltasFromAssumption :: Array RoleInstance -> InformedAssumption -> MonadPerspectivesTransaction Unit
createDeltasFromAssumption users (RoleAssumption ctxt roleTypeId) = do
  instances <- lift2 (ctxt ##= getRoleInstances (ENR roleTypeId))
  case SerializableNonEmptyArray <$> ANE.fromArray instances of
    Nothing -> pure unit
    Just instances' -> magic ctxt instances' roleTypeId users

createDeltasFromAssumption users (Me ctxt mRoleInstance) = case mRoleInstance of
  Nothing -> pure unit
  Just me -> do
    rtype <- lift2 (me ##>> OG.roleType)
    magic ctxt (SerializableNonEmptyArray $ ANE.singleton me) rtype users

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
aisInPropertyDelta :: RoleInstance -> EnumeratedPropertyType -> MonadPerspectivesTransaction (Array RoleInstance)
aisInPropertyDelta id property = do
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
          \(df :: DomeinFile) -> do
            -- Get the AffectedContextCalculations in onPropertyDelta.
            (calculations :: Array InvertedQuery) <- pure $ unsafePartial $ fromJust $ preview (onPropertyDelta rt) df
            -- Compile the descriptions.
            if areCompiled calculations
              then pure calculations
              else do
                compiledCalculations <- traverse compileBoth calculations
                -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
                modifyDomeinFileInCache (over (onPropertyDelta rt) (const compiledCalculations)) modelName
                pure compiledCalculations
      where
        onPropertyDelta :: EnumeratedPropertyType -> Traversal' DomeinFile (Array InvertedQuery)
        onPropertyDelta (EnumeratedPropertyType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedProperties") <<< at x <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "onPropertyDelta")

-- | Changes the model in cache (but not in Couchdb). For a given lens that retrieves one of onRoleDelta_binder,
-- | onRoleDelta_binding, onContextDelta_role or onContextDelta_context, and an EnumeratedRoleType, compileBackwards the
-- | description in the AffectedContextCalculations.
-- | Compiles the `inverse` queries; not the forwards part.
compileBackwardFor :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array InvertedQuery)
compileBackwardFor onX rt = compileDescriptions_ onX rt false

compileBothFor :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array InvertedQuery)
compileBothFor onX rt = compileDescriptions_ onX rt false

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
