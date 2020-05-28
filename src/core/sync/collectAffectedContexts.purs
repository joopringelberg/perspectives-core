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

module Perspectives.CollectAffectedContexts where

import Control.Monad.AvarMonadAsk (modify) as AA
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (lift)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Writer (runWriterT)
import Data.Array (filterA, fold, head, nub, union)
import Data.Array.NonEmpty (fromArray, singleton, head) as ANE
import Data.Foldable (traverse_)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (toUnfoldable)
import Data.Map.Internal (Map, keys, values)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (rol_isMe)
import Perspectives.CoreTypes (type (~~>), InformedAssumption(..), MP, MonadPerspectives, MonadPerspectivesTransaction, WithAssumptions, runMonadPerspectivesQuery, (###=), (##=), (##>>))
import Perspectives.Deltas (addContextDelta, addPropertyDelta, addRoleDelta, addUniverseContextDelta, addUniverseRoleDelta)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.Instances.ObjectGetters (binding, bottom, contextType, getEnumeratedRoleInstances, getProperty, getRoleBinders)
import Perspectives.Instances.ObjectGetters (roleType, context) as OG
import Perspectives.InvertedQuery (InvertedQuery(..), RelevantProperties(..), backwards, forwards)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Query.QueryTypes (roleRange)
import Perspectives.Query.UnsafeCompiler (getHiddenFunction, getRoleInstances, getterFromPropertyType)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (allProperties)
import Perspectives.Representation.EnumeratedRole (EnumeratedRoleRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (aspectsClosure)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, const, discard, join, map, not, pure, unit, when, ($), (<<<), (==), (>=>), (>>=), (<$>))
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
    contextCalculations <- lift2 (roleType ###= (aspectsClosure >=> ArrayT <<< compileDescriptions _onContextDelta_context))
    (for contextCalculations \(InvertedQuery{backwardsCompiled, userTypes}) -> do
      -- Find all affected contexts, starting from the role instance of the Delta.
      (affectedContexts :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
      handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  roleCalculations <- lift2 (roleType ###= (aspectsClosure >=> ArrayT <<< compileDescriptions _onContextDelta_role))
  users2 <- for roleCalculations (\(InvertedQuery{backwardsCompiled, userTypes}) -> do
    -- Find all affected contexts, starting from the context instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: ContextInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  -- Remove 'me'
  lift $ lift $ filterA (getPerspectRol >=> pure <<< not <<< rol_isMe) (nub $ union users1 users2)

-- Adds an AffectedContext to the transaction and returns user instances.
handleAffectedContexts :: Array ContextInstance -> Map RoleType RelevantProperties -> MonadPerspectivesTransaction (Array RoleInstance)
handleAffectedContexts affectedContexts userProps = case ANE.fromArray affectedContexts of
  Nothing -> pure []
  Just contextInstances -> do
    userTypes <- pure $ toUnfoldable (keys userProps)
    addAffectedContext $ AffectedContext {contextInstances, userTypes}
    (for userTypes \er -> for affectedContexts \ci -> lift $ lift $ (ci ##= getRoleInstances er)) >>= pure <<< join <<< join

addAffectedContext :: AffectedContext -> MonadPerspectivesTransaction Unit
addAffectedContext as = lift $ AA.modify \(Transaction r@{affectedContexts}) -> Transaction (r {affectedContexts = union [as] affectedContexts})

-----------------------------------------------------------
-- USERSHASPERSPECTIVEONROLEINSTANCE
-----------------------------------------------------------
-- | Parameter `peer` should be bound to an instance of model:System$User (the bottom of a user role chain).
userHasNoPerspectiveOnRoleInstance ::  EnumeratedRoleType -> RoleInstance -> RoleInstance -> MonadPerspectives Boolean
userHasNoPerspectiveOnRoleInstance roleType roleInstance peer = execStateT (userHasPerspectiveOnRoleInstance_ roleType roleInstance peer) true

userHasPerspectiveOnRoleInstance_ ::  EnumeratedRoleType -> RoleInstance -> RoleInstance -> StateT Boolean MonadPerspectives Unit
userHasPerspectiveOnRoleInstance_ roleType roleInstance peer = do
  (lift $ compileDescriptions _onContextDelta_context roleType) >>= traverse_ g
  notFound <- get
  when notFound $ (lift $ compileDescriptions _onContextDelta_role roleType) >>= traverse_ g

  where
    g :: InvertedQuery -> StateT Boolean MonadPerspectives Unit
    g (InvertedQuery{backwardsCompiled, userTypes}) = do
      notFound <- get
      when notFound $
        lift (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance) >>= traverse_
        -- Check if the peer has one of the userType roles in at least one of the context instances.
          \ctxt -> do
            notFound1 <- get
            when notFound1 do
              for_ ((toUnfoldable $ keys userTypes) :: Array RoleType) \ut -> do
                notFound2 <- get
                when notFound2 do
                  roles <- lift (ctxt ##= getRoleInstances ut)
                  for_ roles \role -> do
                    notFound3 <- get
                    when notFound3 do
                      otherPeer <- lift (role ##>> bottom)
                      when (otherPeer == peer) (put false)

-----------------------------------------------------------
-- OBSERVINGCONTEXTS
-----------------------------------------------------------
-- | Just collect and add to the Transaction all contexts that have a role with
-- | a perspective on the RoleType in the context instance.
-- | Guarantees RULE TRIGGERING.
addRoleObservingContexts :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
addRoleObservingContexts id roleType roleInstance = do
  contextCalculations <- lift2 $ compileDescriptions _onContextDelta_context roleType
  for_ contextCalculations \(InvertedQuery{backwardsCompiled, userTypes}) ->
      (lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)) >>= addContexts userTypes
  roleCalculations <- lift2 $ compileDescriptions _onContextDelta_role roleType
  for_ roleCalculations \(InvertedQuery{backwardsCompiled, userTypes}) -> lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: ContextInstance ~~> ContextInstance) >>= addContexts userTypes

addContexts :: Map RoleType RelevantProperties -> Array ContextInstance -> MonadPerspectivesTransaction Unit
addContexts userTypes as = case ANE.fromArray as of
  Nothing -> pure unit
  Just contextInstances -> lift $ AA.modify \(Transaction r@{affectedContexts}) -> Transaction (r {affectedContexts = union [AffectedContext {contextInstances, userTypes: toUnfoldable $ keys userTypes}] affectedContexts})

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
aisInRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction RoleBindingDelta
aisInRoleDelta (RoleBindingDelta dr@{id, binding, oldBinding, deltaType}) = do
  binderType <- lift2 (id ##>> OG.roleType)
  bindingCalculations <- lift2 $ compileDescriptions_ _onRoleDelta_binding binderType true
  users1 <- for bindingCalculations (runInvertedQuery id) >>= pure <<< join
  users2 <- case binding of
    Just bnd -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions _onRoleDelta_binder bindingType
      for binderCalculations (runInvertedQuery bnd) >>= pure <<< join
    Nothing -> pure []

  users3 <- case oldBinding of
    Just bnd | deltaType == SetBinding -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions _onRoleDelta_binder bindingType
      for binderCalculations (\(InvertedQuery{backwardsCompiled, userTypes}) -> do
        -- Find all affected contexts, starting from the role instance of the Delta.
        affectedContexts <- lift2 $ catchError (bnd ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance) (pure <<< const [])
        handleAffectedContexts affectedContexts userTypes) >>= pure <<< join >>= lift2 <<< filterA (getPerspectRol >=> pure <<< not <<< rol_isMe)
    otherwise -> pure []

  pure $ RoleBindingDelta dr {users = (nub $ union users1 (union users2 users3))}

  where

    runInvertedQuery :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array RoleInstance)
    runInvertedQuery roleInstance (InvertedQuery{description, backwardsCompiled, forwardsCompiled, userTypes}) = do
      -- Find all affected contexts, starting from the role instance of the Delta.
      affectedContexts <- lift2 $ catchError (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance) (pure <<< const [])
      users <- handleAffectedContexts affectedContexts userTypes >>= lift2 <<< filterA (getPerspectRol >=> pure <<< not <<< rol_isMe)
      case forwardsCompiled of
        Nothing -> if isNothing (forwards description)
          then do
            arrayOfProperties <- case fold $ values userTypes of
              All -> do
                rtype <- lift2 (roleInstance ##>> OG.roleType)
                lift2 $ allProperties (ST rtype)
              Properties props -> pure props
            Tuple _ assumptions' <- lift2 $ runWriterT $runArrayT $ for_ arrayOfProperties \prop -> do
              getter <- lift $ lift $ getterFromPropertyType prop
              getter roleInstance
            for_ (unwrap assumptions') (createDeltasFromAssumption users)
          else pure unit
        Just f -> do
          -- Apply the forwards query to the same role instance as the backwards query.
          (Tuple instances assumptions :: WithAssumptions RoleInstance) <- lift2 (runMonadPerspectivesQuery roleInstance (unsafeCoerce f))
          -- Now create and push Deltas for each of the assumptions. These Delta's are valid for all users.
          for_ (unwrap assumptions) (createDeltasFromAssumption users)
          -- Then, for each separate user type, run all property getters in a single query for each value returned from the main query. The Assumptions thus gathered apply to that single user type.
          -- TODO. We verzamelen hier alles in één grote Transactie en splitsen hem dan later weer uit. Dat kan veel beter.
          arrayOfProperties <- case fold $ values userTypes of
            All -> case forwards description of
              Nothing -> pure []
              Just qfd -> lift2 $ allProperties (unsafePartial $ roleRange qfd)
            Properties props -> pure props
          Tuple _ assumptions' <- lift2 $ runWriterT $runArrayT $ for_ arrayOfProperties \prop -> do
            getter <- lift $ lift $ getterFromPropertyType prop
            for_ instances getter
          for_ (unwrap assumptions') (createDeltasFromAssumption users)
      pure users

createDeltasFromAssumption :: Array RoleInstance -> InformedAssumption -> MonadPerspectivesTransaction Unit
createDeltasFromAssumption users (RoleAssumption ctxt roleTypeId) = do
  instances <- lift2 (ctxt ##= getEnumeratedRoleInstances roleTypeId)
  case SerializableNonEmptyArray <$> ANE.fromArray instances of
    Nothing -> pure unit
    Just instances' -> magic ctxt instances' roleTypeId users

createDeltasFromAssumption users (Me ctxt mRoleInstance) = case mRoleInstance of
  Nothing -> pure unit
  Just me -> do
    rtype <- lift2 (me ##>> OG.roleType)
    magic ctxt (SerializableNonEmptyArray $ ANE.singleton me) rtype users

createDeltasFromAssumption users (Binding roleInstance) = do
  bnd <- lift2 (roleInstance ##>> binding)
  ctxt <- lift2 (bnd ##>> OG.context)
  rtype <- lift2 (bnd ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton bnd) rtype users
  addRoleDelta $ RoleBindingDelta
    { id: roleInstance
    , binding: Just bnd
    , oldBinding: Nothing
    , roleWillBeRemoved: false
    , deltaType: SetBinding
    , users: users
    , sequenceNumber: 0
    }

createDeltasFromAssumption users (Binder roleInstance roleType) = do
  bndrs <- lift2 (roleInstance ##= getRoleBinders roleType)
  -- There may not be a bndr!
  case ANE.fromArray bndrs of
    Nothing -> pure unit
    Just someBndrs -> do
      ctxt <- lift2 (ANE.head someBndrs ##>> OG.context)
      magic ctxt (SerializableNonEmptyArray someBndrs) roleType users
  for_ bndrs \bndr ->
    addRoleDelta $ RoleBindingDelta
      { id: bndr
      , binding: Just roleInstance
      , oldBinding: Nothing
      , roleWillBeRemoved: false
      , deltaType: SetBinding
      , users: users
      , sequenceNumber: 0
      }

createDeltasFromAssumption users (Property roleInstance propertyType) = do
  ctxt <- lift2 (roleInstance ##>> OG.context)
  rtype <- lift2 (roleInstance ##>> OG.roleType)
  magic ctxt (SerializableNonEmptyArray $ ANE.singleton roleInstance) rtype users
  values <- lift2 (roleInstance ##= getProperty propertyType)
  addPropertyDelta $ RolePropertyDelta
    { id: roleInstance
    , property: propertyType
    , values
    , deltaType: AddProperty
    , users: users
    , sequenceNumber: 0
    }

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

-- | Add a UniverseContextDelta, UniverseRoleDelta and a ContextDelta.
magic :: ContextInstance -> SerializableNonEmptyArray RoleInstance -> EnumeratedRoleType ->  Array RoleInstance -> MonadPerspectivesTransaction Unit
magic ctxt roleInstances rtype users =  do
  ctype <- lift2 (ctxt ##>> contextType)
  addUniverseContextDelta $ UniverseContextDelta
    { id: ctxt
    , contextType: ctype
    , deltaType: ConstructEmptyContext
    , users: users
    , sequenceNumber: 0
    }
  addUniverseRoleDelta $ UniverseRoleDelta
    { id: ctxt
    , roleInstances: roleInstances
    , roleType: rtype
    , deltaType: ConstructEmptyRole
    , users: users
    , sequenceNumber: 0
    }
  addContextDelta $ ContextDelta
    { id: ctxt
    , roleType: rtype
    , roleInstances: roleInstances
    , destinationContext: Nothing
    , deltaType: AddRoleInstancesToContext
    , users: users
    , sequenceNumber: 0
    }


-- | Adds users for SYNCHRONISATION, guarantees RULE TRIGGERING.
aisInPropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction RolePropertyDelta
aisInPropertyDelta (RolePropertyDelta dr@{id, property})= do
  calculations <- lift2 $ compileDescriptions' property
  users <- for calculations (\(InvertedQuery{backwardsCompiled, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust backwardsCompiled) :: RoleInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  otherUsers <- lift $ lift $ filterA (getPerspectRol >=> pure <<< not <<< rol_isMe) (nub users)
  pure $ RolePropertyDelta dr {users = otherUsers}
  where
    compileDescriptions' :: EnumeratedPropertyType -> MonadPerspectives (Array InvertedQuery)
    compileDescriptions' rt@(EnumeratedPropertyType ert) =  do
      modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
      (df :: DomeinFile) <- retrieveDomeinFile modelName
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
  (df :: DomeinFile) <- retrieveDomeinFile modelName
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
compileForwards ac@(InvertedQuery{description, backwardsCompiled, forwardsCompiled, userTypes}) = case backwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    c <- traverse getHiddenFunction (forwards description)
    pure $ InvertedQuery{description, backwardsCompiled, forwardsCompiled: (map unsafeCoerce c), userTypes}

compileBackwards :: InvertedQuery -> MP InvertedQuery
compileBackwards ac@(InvertedQuery{description, backwardsCompiled, forwardsCompiled, userTypes}) = case backwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    c <- traverse getHiddenFunction (backwards description)
    pure $ InvertedQuery{description, backwardsCompiled: (map unsafeCoerce c), forwardsCompiled, userTypes}

compileBoth :: InvertedQuery -> MP InvertedQuery
compileBoth ac@(InvertedQuery{description, backwardsCompiled, forwardsCompiled, userTypes}) = case backwardsCompiled of
  Just c -> pure ac
  Nothing -> do
    backwards' <- traverse getHiddenFunction (backwards description)
    forwards' <- traverse getHiddenFunction (forwards description)
    pure $ InvertedQuery{description, backwardsCompiled: (map unsafeCoerce backwards'), forwardsCompiled: (map unsafeCoerce forwards'), userTypes}

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
