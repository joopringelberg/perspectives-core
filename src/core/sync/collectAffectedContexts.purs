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
import Control.Monad.Writer (runWriterT)
import Data.Array (cons, filterA, head, intersect, nub, null, union)
import Data.Array.NonEmpty (fromArray, singleton, head) as ANE
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (isDefaultContextDelta)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles, InformedAssumption(..), MP, MonadPerspectives, MonadPerspectivesTransaction, WithAssumptions, runMonadPerspectivesQuery, (###=), (##=), (##>>), (##>))
import Perspectives.Deltas (addDelta)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Error.Boundaries (handleDomeinFileError', handlePerspectContextError, handlePerspectRolError)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, contextType, getActiveRoleAndContextStates, getActiveRoleStates_, getActiveStates_, getRoleBinders, notIsMe)
import Perspectives.Instances.ObjectGetters (roleType, context) as OG
import Perspectives.InvertedQuery (InvertedQuery(..), backwards, backwardsQueryResultsInContext, backwardsQueryResultsInRole, forwards, shouldResultInContextStateQuery, shouldResultInRoleStateQuery)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.UnsafeCompiler (getHiddenFunction, getRoleInstances, getterFromPropertyType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRoleRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType, RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), toArray)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (roleAspectsClosure)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..))
import Prelude (Unit, bind, const, discard, join, map, not, pure, unit, ($), (<$>), (<<<), (==), (>=>), (>>=))
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
    contextCalculations <- lift2 (roleType ###= (roleAspectsClosure >=> ArrayT <<< compileDescriptions _onContextDelta_context))
    (for contextCalculations \iq -> handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq) >>= pure <<< join
  users2 <- do
    roleCalculations <- lift2 (roleType ###= (roleAspectsClosure >=> ArrayT <<< compileDescriptions _onContextDelta_role))
      -- Find all InvertedQueryResults, starting from the new role instance of the Delta.
      -- We do not start on the context because the cardinality of the role getting step is larger than one and
      -- we want to make sure that the new binding is in the path for the users.
    (for roleCalculations \iq ->
      handleBackwardQuery roleInstance iq >>= runForwardsComputation roleInstance iq) >>= pure <<< join
  pure $ nub $ union users1 users2

handleBackwardQuery :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array RoleInstance)
handleBackwardQuery roleInstance iq@(InvertedQuery{backwardsCompiled, users:userTypes, states}) = do
  if unsafePartial shouldResultInContextStateQuery iq
    then createContextStateQuery
    else if unsafePartial shouldResultInRoleStateQuery iq
      then createRoleStateQuery
      else usersWithAnActivePerspective
  where
    createContextStateQuery :: MonadPerspectivesTransaction (Array RoleInstance)
    createContextStateQuery = do
      (invertedQueryResults :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      addInvertedQueryResult $ ContextStateQuery invertedQueryResults
      pure []

    createRoleStateQuery :: MonadPerspectivesTransaction (Array RoleInstance)
    createRoleStateQuery = do
      (affectedRoles :: Array RoleInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance)
      addInvertedQueryResult $ RoleStateQuery affectedRoles
      pure []

    -- TODO. Dit is gebouwd op de veronderstelling dat backwardsCompiled type RoleInstance ~~> ContextInstance heeft.
    -- Maar het kan ook RoleInstance ~~> RoleInstance zijn!
    usersWithAnActivePerspective :: MonadPerspectivesTransaction (Array RoleInstance)
    usersWithAnActivePerspective = if unsafePartial $ backwardsQueryResultsInRole iq
      then fromRoleResults
      else if unsafePartial $ backwardsQueryResultsInContext iq
        then fromContextResults
          else throwError (error "Programming error in handleBackwardQuery.usersWithAnActivePerspective")

    fromContextResults :: MonadPerspectivesTransaction (Array RoleInstance)
    fromContextResults = do
      (invertedQueryResults :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      -- Return users only for those contexts that are in one of the permissable states.
      allowed <- filterA
        (\cid -> (lift2 $ getActiveStates_ cid) >>= pure <<< not <<< null <<< intersect states)
        invertedQueryResults
      join <$> for allowed \ci -> do
        -- Remove 'me'
        -- If a role cannot be found, we remove it, erring on the safe side (notIsMe has an internal error boundary).
        lift2 (join <$> traverse (\userType -> (ci ##= getRoleInstances userType) >>= filterA notIsMe) userTypes)

    fromRoleResults :: MonadPerspectivesTransaction (Array RoleInstance)
    fromRoleResults = do
      (invertedQueryResults :: Array RoleInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> RoleInstance)
      -- Return users only for those contexts that are in one of the permissable states.
      (allowed :: Array RoleInstance) <- filterA
        (\rid -> (lift2 $ getActiveRoleStates_ rid) >>= pure <<< not <<< null <<< intersect states)
        invertedQueryResults
      join <$> for allowed \ri -> do
        -- Remove 'me'
        -- If a role cannot be found, we remove it, erring on the safe side (notIsMe has an internal error boundary).
        lift2 (join <$> traverse (\userType -> (ri ##= OG.context >=> getRoleInstances userType) >>= filterA notIsMe) userTypes)

addInvertedQueryResult :: InvertedQueryResult -> MonadPerspectivesTransaction Unit
addInvertedQueryResult result = lift $ AA.modify \(Transaction r@{invertedQueryResults}) -> Transaction (r {invertedQueryResults = union [result] invertedQueryResults})

-----------------------------------------------------------
-- OBSERVINGCONTEXTS
-----------------------------------------------------------
-- | Computes an InvertedQueryResult for the given RoleInstance and its type.

-- | Just collect and add to the Transaction all contexts that have a role with
-- | a perspective on the RoleType in the context instance.
-- | Guarantees RULE TRIGGERING.
-- TODO. Wordt alleen gebruikt door saveContextInstance.
-- Ik vermoed dat het hier om onEntry gaat.
-- Misschien moeten we dat niet op deze manier doen.
addRoleObservingContexts :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
addRoleObservingContexts id roleType roleInstance = do
  contextCalculations <- lift2 $ compileDescriptions _onContextDelta_context roleType
  for_ contextCalculations (handleBackwardQuery roleInstance)
  roleCalculations <- lift2 $ compileDescriptions _onContextDelta_role roleType
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
  bindingCalculations <- lift2 $ compileDescriptions_ _onRoleDelta_binding binderType true
  users1 <- join <$> (for bindingCalculations
    -- Find all affected contexts, starting from the binder (id) instance of the Delta.
    (\iq -> (handleBackwardQuery id iq) >>= runForwardsComputation id iq))
  users2 <- case binding of
    Just bnd -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions_ _onRoleDelta_binder bindingType true
      (for binderCalculations
        (\iq -> (handleBackwardQuery id iq) >>= runForwardsComputation bnd iq)) >>= pure <<< join
    Nothing -> pure []
  users3 <- case oldBinding of
    Just bnd | deltaType == SetBinding -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions _onRoleDelta_binder bindingType
      join <$> for binderCalculations (handleBackwardQuery bnd)
    otherwise -> pure []
  pure (nub $ union users1 (union users2 users3))

-- RunForwardsComputation only uses the users provided to push deltas.
-- This should not be done for the own user!
runForwardsComputation ::
  RoleInstance ->
  InvertedQuery ->
  (Array RoleInstance) ->
  MonadPerspectivesTransaction (Array RoleInstance)
runForwardsComputation roleInstance (InvertedQuery{description, forwardsCompiled, statesPerProperty}) users = do
  case forwardsCompiled of
    Nothing -> if isNothing (forwards description)
      then do
        -- Collect those PropertyTypes in statesPerProperty whose states contain an active state.
        activeStates <- lift2 (roleInstance ##= getActiveRoleAndContextStates)
        (arrayOfProperties :: Array PropertyType) <- pure $ foldrWithIndex
          (\prop propStates props -> if null $ intersect propStates activeStates
            then props
            else cons prop props)
          []
          (unwrap statesPerProperty)
        Tuple _ assumptions' <- lift2 $ runWriterT $runArrayT $ for_ arrayOfProperties \prop -> do
          getter <- lift $ lift $ getterFromPropertyType prop
          getter roleInstance
        rtype <- lift2 (roleInstance ##>> OG.roleType)
        -- log $ "Running propertyGetters for InvertedQuery without forward part\n" <>
        --   "\n\t for properties: " <> intercalate "\n\t\t" (show <$> arrayOfProperties) <>
        --   "\n\t on role instance: " <> show roleInstance <> " having type " <> show rtype <>
        --   "\n\t results in: " <> intercalate "\n\t\t" (show <$> unwrap assumptions') <>
        --   "\n\tfor users: " <> show users
        for_ (unwrap assumptions') (createDeltasFromAssumption users)
      else pure unit
    Just f -> do
      -- Apply the forwards query to the same role instance as the backwards query.
      (Tuple instances assumptions :: WithAssumptions RoleInstance) <- lift2 (runMonadPerspectivesQuery roleInstance (unsafeCoerce f))
      -- Now create and push Deltas for each of the assumptions. These Delta's are valid for all users.
      for_ (unwrap assumptions) (createDeltasFromAssumption users)
      -- Then, for each separate user type, run all property getters in a single query for each value returned from the main query. The Assumptions thus gathered apply to that single user type.
      -- TODO. We verzamelen hier alles in één grote Transactie en splitsen hem dan later weer uit. Dat kan veel beter.
        -- Collect those PropertyTypes in statesPerProperty whose states contain an active state.
      activeStates <- lift2 (roleInstance ##= getActiveRoleAndContextStates)
      arrayOfProperties <- pure $ foldrWithIndex
        (\prop propStates props -> if null $ intersect propStates activeStates
          then props
          else cons prop props)
        []
        (unwrap statesPerProperty)
      Tuple _ (assumptions' :: ArrayWithoutDoubles InformedAssumption) <- lift2 $ runWriterT $runArrayT $ for_ arrayOfProperties \prop -> do
        getter <- lift $ lift $ getterFromPropertyType prop
        for_ instances getter
      -- log $ "Running forward part of Inverted Query" <>
      --   "\n\t with result type: " <> show (range <$> forwards description) <>
      --   "\n\t results in: " <> intercalate "\n\t\t" (show <$> unwrap assumptions') <>
      --   "\n\tfor users: " <> show users
      for_ (unwrap assumptions') (createDeltasFromAssumption users)
  pure users

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
aisInPropertyDelta :: RoleInstance -> EnumeratedPropertyType -> MonadPerspectivesTransaction (Array RoleInstance)
aisInPropertyDelta id property = do
  calculations <- lift2 $ compileDescriptions' property
  users <- join <$> for calculations (handleBackwardQuery id)
  lift $ lift $ filterA notIsMe (nub users)
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
                compiledCalculations <- traverse compileBackwards calculations
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
compileDescriptions :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array InvertedQuery)
compileDescriptions onX rt = compileDescriptions_ onX rt false

compileDescriptions_ :: CalculationsLens -> EnumeratedRoleType -> Boolean -> MonadPerspectives (Array InvertedQuery)
compileDescriptions_ onX rt@(EnumeratedRoleType ert) forward = do
  modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
  (try $ retrieveDomeinFile modelName) >>=
    handleDomeinFileError' "compileDescriptions_" []
    \(df :: DomeinFile) -> do
      -- Get the AffectedContextCalculations in onContextDelta_context.
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
    -- TODO. je hoeft hier geen unsafeCoerce te mappen, denk ik.
    pure $ InvertedQuery iqr {forwardsCompiled = (map unsafeCoerce c)}

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
