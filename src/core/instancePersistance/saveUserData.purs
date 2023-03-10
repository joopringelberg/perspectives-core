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

-- | The functions in this module save and remove contexts and roles.
-- | These updates fully take care of the basic responsibilities of the PDR:
-- | PERSISTENCE
-- | SYNCHRONISATION
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER
-- | except for saveContextInstance (no CURRENTUSER, nor SYNCHRONISATION)

module Perspectives.SaveUserData
  ( changeRoleBinding
  , handleNewPeer
  , removeAllRoleInstances
  , removeBinding
  , removeContextIfUnbound
  , removeContextInstance
  , removeRoleInstance
  , replaceBinding
  , saveContextInstance
  , scheduleContextRemoval
  , scheduleRoleRemoval
  , setBinding
  , setFirstBinding
  , stateEvaluationAndQueryUpdatesForContext
  , stateEvaluationAndQueryUpdatesForRole
  )
  where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (try)
import Control.Monad.State (lift)
import Data.Array (concat, cons, delete, elemIndex, find, head, nub, union)
import Data.Array.NonEmpty (singleton)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, for_, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Foreign.Generic (encodeJSON)
import Foreign.Object (Object, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (getAuthor, getSubject, cacheAndSave)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (addRoleObservingContexts, usersWithPerspectiveOnRoleBinding, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (changeContext_me, context_buitenRol, context_iedereRolInContext, modifyContext_rolInContext, rol_binding, rol_context, rol_isMe, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater, (##=), (##>), (##>>), (###=))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBindingRequests, findFilledRoleRequests, findMeRequests, findResourceDependencies, findRoleRequests)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Extern.Couchdb (addModelToLocalStore')
import Perspectives.Identifiers (deconstructBuitenRol, typeUri2ModelUri, isExternalRole)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (allRoleBinders, context, contextType, contextType_, getUnlinkedRoleInstances, isMe, roleType_)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol, removeEntiteit, saveEntiteit)
import Perspectives.Query.UnsafeCompiler (getMyType, getRoleInstances)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..), externalRoleType)
import Perspectives.ResourceIdentifiers (stripNonPublicIdentifiers)
import Perspectives.RoleAssignment (filledNoLongerPointsTo, filledPointsTo, fillerNoLongerPointsTo, fillerPointsTo, lookForAlternativeMe, roleIsMe, roleIsNotMe)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (allUnlinkedRoles, isUnlinked_)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..), stripResourceSchemes)
import Prelude (Unit, bind, discard, join, not, pure, unit, void, ($), (&&), (<>), (==), (>>=), (<$>), (<<<), (||))
 
-- | This function takes care of
-- | PERSISTENCE
-- | QUERY UPDATES
-- | RULE TRIGGERING
-- | but nothing else. It is only used by loadAndSaveCrlFile. This means that we only deal with
-- | contexts that have been created from a source file, freshly as it were, fully equipped with
-- | roles.
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt@(PerspectContext{pspType:cType})) <- lift $ saveEntiteit id
  subject <- getSubject
  forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances ->
    case head instances of
      Nothing -> pure unit
      -- STATE CHANGES
      Just i -> addRoleObservingContexts cType id (EnumeratedRoleType roleName) i
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    (PerspectRol{_id, binding, pspType:roleType, filledRoles}) <- lift $ saveEntiteit rol
    case binding of
      Nothing -> pure unit
      Just fillerId -> (lift $ findFilledRoleRequests fillerId cType roleType) >>= addCorrelationIdentifiersToTransactie
    for_ filledRoles \roleMap ->
      for_ roleMap \instances ->
        for_ instances \binder ->
          -- no attempt is made to look up role 'binder'.
          (lift $ findBindingRequests binder) >>= addCorrelationIdentifiersToTransactie
    -- For rule triggering, not for the delta or SYNCHRONISATION:
    case binding of 
      Just b -> do 
        fillerType <- lift $ roleType_ b
        void $ usersWithPerspectiveOnRoleBinding (RoleBindingDelta
          { filled: rol
          , filledType: roleType
          , filler: binding
          , fillerType: Just fillerType
          , oldFiller: Nothing
          , oldFillerType: Nothing
          , deltaType: SetFirstBinding
          , subject
          })
          true
      _ -> pure unit
  (_ :: PerspectRol) <- lift $ saveEntiteit (context_buitenRol ctxt)
  -- For roles with a binding equal to R: detect the binder <RoleType> requests for R
  -- For roles that are bound by role R: detect the binding requests for R.
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Add the role instance to the end of the roles to exit.
-- | Add the actual removal instruction to the end of the scheduledAssignments.
-- | (we build a last-in, last-out stack of destructive effects)
scheduleRoleRemoval :: RoleInstance -> MonadPerspectivesTransaction Unit
scheduleRoleRemoval id = do
  contextOfRole <- lift (id ##>> context)
  -- If the role's context is scheduled to be removed, don't add its removal to the scheduledAssignments.
  contextIsScheduledToBeRemoved <- gets (\(Transaction{scheduledAssignments}) -> isJust $ find
    (case _ of
      ContextRemoval ctxt _ -> ctxt == contextOfRole
      _ -> false)
    scheduledAssignments)
  roleIsUntouchable <- gets (\(Transaction{untouchableRoles}) -> isJust $ elemIndex id untouchableRoles)
  if contextIsScheduledToBeRemoved || roleIsUntouchable
    then pure unit
    else modify (over Transaction \t@{scheduledAssignments, rolesToExit} -> t
      { rolesToExit = rolesToExit `union` [id]
      , scheduledAssignments = scheduledAssignments `union` [RoleRemoval id]
      })

-- | Schedules all roles in the context, including its external role, for removal.
-- | Add the actual removal instruction to the end of the scheduledAssignments.
-- | (we build a last-in, last-out stack of destructive effects)
scheduleContextRemoval :: Maybe RoleType -> ContextInstance -> MonadPerspectivesTransaction Unit
scheduleContextRemoval authorizedRole id = (lift $ try $ getPerspectContext id) >>=
  handlePerspectContextError "removeContextInstance"
  \(ctxt@(PerspectContext{rolInContext, buitenRol, pspType:contextType})) -> do
    unlinkedRoleTypes <- lift (contextType ###= allUnlinkedRoles)
    unlinkedInstances <- lift $ concat <$> (for unlinkedRoleTypes \rt -> id ##= getUnlinkedRoleInstances rt)
    modify (over Transaction \t@{scheduledAssignments, rolesToExit} -> t
      { scheduledAssignments = scheduledAssignments `union` [(ContextRemoval id authorizedRole)]
      , rolesToExit = nub $ rolesToExit <> (cons buitenRol $ unlinkedInstances <> (concat $ values rolInContext))})

-- Only called when the external role is also 'bound' in a DBQ role.
removeContextIfUnbound :: RoleInstance -> Maybe RoleType ->  MonadPerspectivesTransaction Unit
removeContextIfUnbound roleInstance@(RoleInstance rid) rtype = do
  mbinder <- lift (roleInstance ##> allRoleBinders)
  case mbinder of
    Nothing -> scheduleContextRemoval rtype (ContextInstance $ deconstructBuitenRol rid)
    otherwise -> pure unit

-- | Remove the context instance plus roles after detaching all its roles.
-- | Does NOT remove the context role binding the external role.
-- | Logs an error if the context does not exist, but does not break.
-- | PERSISTENCE
removeContextInstance :: ContextInstance -> Maybe RoleType -> MonadPerspectivesTransaction Unit
removeContextInstance id authorizedRole = do
  (lift $ try $ getPerspectContext id) >>=
    handlePerspectContextError "removeContextInstance"
    \(ctxt@(PerspectContext{pspType:contextType, rolInContext, buitenRol})) -> do
      lift $ do
        unlinkedRoleTypes <- contextType ###= allUnlinkedRoles
        unlinkedInstances <- concat <$> (for unlinkedRoleTypes \rt -> id ##= getUnlinkedRoleInstances rt)
        for_ (unlinkedInstances <> (concat $ values rolInContext)) removeEntiteit
        void $ removeEntiteit buitenRol
        removeEntiteit id

-- | STATE EVALUATION
-- | QUERY UPDATES
-- | SYNCHRONISATION
stateEvaluationAndQueryUpdatesForContext :: ContextInstance -> Maybe RoleType -> MonadPerspectivesTransaction Unit
stateEvaluationAndQueryUpdatesForContext id authorizedRole = do
  (lift $ try $ getPerspectContext id) >>=
    handlePerspectContextError "removeContextInstance"
    \(ctxt@(PerspectContext{pspType:contextType, rolInContext, buitenRol})) -> do
      users1 <- concat <$> for (concat $ values rolInContext) handleRoleOnContextRemoval
      users2 <- handleRoleOnContextRemoval buitenRol
      -- SYNCHRONISATION
      subject <- getSubject
      me <- getAuthor
      -- (roleType ###>> hasAspect (EnumeratedRoleType "sys:RootContext$External"))
      addDelta $ DeltaInTransaction
        { users: nub $ users1 <> users2
        , delta: SignedDelta
            { author: stripNonPublicIdentifiers me
            , encryptedDelta: sign $ encodeJSON $ stripResourceSchemes $ UniverseRoleDelta
              { id
              , contextType
              , roleType: externalRoleType contextType
              , authorizedRole
              , roleInstances: SNEA.singleton (context_buitenRol ctxt)
              , deltaType: RemoveExternalRoleInstance
              , subject
              }}}

-- | Modifies the context instance by detaching the given role instances.
-- | PERSISTENCE of the context instance.
removeRoleInstance :: RoleInstance -> MonadPerspectivesTransaction Unit
removeRoleInstance roleId = (lift $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError "removeRoleInstance"
  \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
    -- PERSISTENCE (remove role instance from context).
    removeRoleInstanceFromContext role
    -- PERSISTENCE (severe the binding links of the incoming FILLS relation).
    severeBindingLinks role
    -- PERSISTENCE (finally remove the role instance from cache and database).
    lift $ removeEntiteit roleId

-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | STATE EVALUATION
-- | QUERY UPDATES.
stateEvaluationAndQueryUpdatesForRole :: RoleInstance -> MonadPerspectivesTransaction Unit
stateEvaluationAndQueryUpdatesForRole roleId = (lift $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError "removeRoleInstance"
  \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
    -- STATE EVALUATION
    users <- statesAndPeersForRoleInstanceToRemove role
    -- SYNCHRONISATION
    synchroniseRoleRemoval role users
    -- QUERY UPDATES.
    queryUpdatesForRoleRemoval role

-- | Handles all responsibilities for the given role instance in the dynamic context
-- | that it's context is removed:
-- | STATE EVALUATION
-- | QUERY UPDATES
-- | PERSISTENCE (just severe the binding links).
handleRoleOnContextRemoval :: RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
handleRoleOnContextRemoval roleId = (lift $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError' "removeRoleInstance" []
  \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
    -- STATE EVALUATION
    users <- statesAndPeersForRoleInstanceToRemove role
    -- QUERY UPDATES.
    queryUpdatesForRoleRemoval role
    -- PERSISTENCE (Remove the incoming links on both the incoming and outgoing
    -- binding relations).
    severeBindingLinks role
    pure users

-- | Returns peers with a perspective on the instance.
-- | Adds StateEvaluations to the current transaction for resources whose state
-- | is affected when the role instance is removed.
-- STATE EVALUATION and computing peers with a perspective on the instance.
statesAndPeersForRoleInstanceToRemove :: PerspectRol -> MonadPerspectivesTransaction (Array RoleInstance)
statesAndPeersForRoleInstanceToRemove (PerspectRol{_id:roleId, pspType:roleType, binding, filledRoles}) = do
  -- The last boolean argument prevents usersWithPerspectiveOnRoleInstance from adding Deltas to the transaction
  -- for the continuation of the path beyond the given role instance.
  users1 <- usersWithPerspectiveOnRoleInstance roleType roleId false
  -- Users from the inverted queries on the incoming fills relation (the role that fills this role):
  users2 <- usersWithPerspectiveOnRoleBinding' roleId binding roleType
  -- Users from the inverted queries on the outgoing fills relation (roles that are filled by this role):
  -- The first index is the String representation of the ContextType, the second that of the EnumeratedRoleType.
  users3 <- concat <$> for (values filledRoles)
    \(instancesPerRoleType :: Object (Array RoleInstance)) -> concat <$> values <$> forWithIndex instancesPerRoleType
      \(roleName :: String) (filledRoles' :: Array RoleInstance) -> concat <$> for filledRoles'
        \(filledRole :: RoleInstance) -> usersWithPerspectiveOnRoleBinding' filledRole (Just roleId) (EnumeratedRoleType roleName)
  pure $ nub $ users1 <> users2 <> users3

  where

    usersWithPerspectiveOnRoleBinding' :: RoleInstance -> Maybe RoleInstance -> EnumeratedRoleType -> MonadPerspectivesTransaction (Array RoleInstance)
    usersWithPerspectiveOnRoleBinding' filled filler filledType = usersWithPerspectiveOnRoleBinding
      (RoleBindingDelta
        { filled
        , filledType
        , filler
        , fillerType: Just roleType
        , oldFiller: Nothing
        , oldFillerType: Nothing
        , deltaType: RemoveBinding
        , subject: ENR $ EnumeratedRoleType sysUser -- is not used
      })
      -- Do not run forwards query!
      false

-- | SYNCHRONISATION. Compute a RemoveRoleInstance UniverseRoleDelta 
-- | (but not for external roles: a delta is generated on exiting the context).
synchroniseRoleRemoval :: PerspectRol -> Array RoleInstance -> MonadPerspectivesTransaction Unit
synchroniseRoleRemoval (PerspectRol{_id:roleId, pspType:roleType, context:contextId}) users = if isExternalRole (unwrap roleId)
  then pure unit
  else do
    contextWillBeRemoved <- gets (_.untouchableContexts <<< unwrap) >>= pure <<< isJust <<< elemIndex contextId
    if contextWillBeRemoved
      then pure unit
      else do
        -- SYNCHRONISATION
        contextType <- lift $ contextType_ contextId
        subject <- getSubject
        author <- getAuthor
        addDelta $ DeltaInTransaction
          { users
          , delta: SignedDelta
            { author: stripNonPublicIdentifiers author
            , encryptedDelta: sign $ encodeJSON $ stripResourceSchemes $ UniverseRoleDelta
              { id: contextId
              , contextType
              , roleInstances: (SerializableNonEmptyArray (singleton roleId))
              , roleType
              , authorizedRole: Nothing
              , deltaType: RemoveRoleInstance
              , subject } }}

-- | QUERY UPDATES. Add correlation identifiers to the current transaction for
-- | each request that needs to be updated when the role instance is removed.
queryUpdatesForRoleRemoval :: PerspectRol -> MonadPerspectivesTransaction Unit
queryUpdatesForRoleRemoval role@(PerspectRol{_id:roleId, pspType:roleType, context:contextId}) = do
  cType <- lift (contextId ##>> contextType)
  (lift $ findRoleRequests contextId roleType) >>= addCorrelationIdentifiersToTransactie
  if rol_isMe role
    then (lift $ findMeRequests contextId)
      >>= addCorrelationIdentifiersToTransactie
    else pure unit
  -- All requests where roleId is the resource of the assumption (its first member) should be added to
  -- the transaction. They will be binding requests, filled role requests and state requests.
  (lift $ findResourceDependencies (unwrap roleId)) >>= addCorrelationIdentifiersToTransactie

-- | PERSISTENCE. Remove the role instance from context.
-- | Logs an error if the role doesn't exist but does not break.
removeRoleInstanceFromContext :: PerspectRol -> MonadPerspectivesTransaction Unit
removeRoleInstanceFromContext role@(PerspectRol{_id:roleId, pspType:roleType, context:contextId}) = do
  (lift $ try $ getPerspectContext contextId) >>=
    handlePerspectContextError "removeRoleInstance"
      \(pe :: PerspectContext) -> do
        unlinked <- lift $ isUnlinked_ roleType
        -- Modify the context: remove the role instances from those recorded with the role type.
        changedContext <- if unlinked
          then pure pe
          else lift (modifyContext_rolInContext pe roleType (delete roleId))
        if rol_isMe role
          then do
            -- CURRENTUSER.
            mmyType <- lift (contextId ##> getMyType)
            case mmyType of
              Nothing -> cacheAndSave contextId (changeContext_me changedContext Nothing)
              Just myType -> do
                mme <- lift (contextId ##> getRoleInstances myType)
                cacheAndSave contextId (changeContext_me changedContext mme)
                -- and set isMe of mme!
          else cacheAndSave contextId changedContext

-- | PERSISTENCE. Remove the incoming links on both the incoming and outgoing
-- | binding relations.
severeBindingLinks :: PerspectRol -> MonadPerspectivesTransaction Unit
severeBindingLinks (PerspectRol{_id:roleId, pspType:roleType, binding, filledRoles}) = do
  -- Severe the binding links of the incoming FILLS relation:
  case binding of
    Nothing -> pure unit
    -- (Remove from the filledRoles, in other words: change filler)
    Just b -> b `fillerNoLongerPointsTo` roleId
  -- Severe the binding links of the outgoing FILLS relation:
  for_ filledRoles \roleMap ->
    for_ roleMap (\filledRoles' ->
      for_ filledRoles' \filled -> filled `filledNoLongerPointsTo` roleId)

-- | Remove all instances of EnumeratedRoleType from the context instance.
-- | Removes all instances from cache, from the database and adds then to deletedRoles in the Transaction.
-- | Removes the role instances from their context.
-- | ContextDelta's are not necessary (see removeRoleInstance).
removeAllRoleInstances :: EnumeratedRoleType -> Updater ContextInstance
removeAllRoleInstances et cid = do
  instances <- lift (cid ##= getRoleInstances (ENR et))
  for_ instances removeRoleInstance

--------------------------
-- UPDATE A ROLE (ADD OR REMOVE A BINDING)
-- All mutations on a binding should handle RULE TRIGGERING and QUERY UPDATES
-- for both ways to traverse a binding.
-- They should also take care of PERSISTENCE of the binding role and
-- the bound role. SYNCHRONISATION should be taken care of by a RoleBindingDelta.
-----------------------------------------------------------
setBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setBinding roleId newBindingId msignedDelta = (lift $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "setBinding" []
    \(filled :: PerspectRol) -> if isJust $ rol_binding filled
      then replaceBinding roleId newBindingId msignedDelta
      else setFirstBinding roleId newBindingId msignedDelta


-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | If the new binding is in fact bound to the role instance before the operation, this
-- | is a no-op without effect.
-- | PERSISTENCE of binding role, old binding and new binding.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for both the new and the old binding.
-- | QUERY UPDATES for `binding <roleId`, `binder <TypeOfRoleId>` for both the old binding and the new binding.
-- | CURRENTUSER for roleId and its context.
replaceBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
replaceBinding roleId (newBindingId :: RoleInstance) msignedDelta = (lift $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "replaceBinding" []
    \(originalRole :: PerspectRol) -> do
      if (rol_binding originalRole == Just newBindingId)
        then pure []
        else do
          users <- removeBinding_ roleId (Just newBindingId) msignedDelta
          -- PERSISTENCE
          -- Schedule the replacement of the binding; add it to the end of the stack of destructive effects.
          modify (\t -> over Transaction (\tr -> tr {scheduledAssignments = tr.scheduledAssignments `union` [RoleUnbinding roleId (Just newBindingId) msignedDelta]}) t)
          pure users

-- | PERSISTENCE
-- | QUERY EVALUATION
-- | CURRENTUSER
-- | SYNCHRONISATION
-- | STATE EVALUATION
-- | This function is idempotent: if the role is already filled with the filler, nothing changes.
setFirstBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setFirstBinding filled filler msignedDelta = (lift $ try $ getPerspectEntiteit filled) >>=
  handlePerspectRolError' "setFirstBinding, filled" []
    \(filledRole :: PerspectRol) -> if rol_binding filledRole == Just filler
      then pure []
      else  (lift $ try $ getPerspectEntiteit filler) >>=
        handlePerspectRolError' "setFirstBinding, filler" []
          \fillerRole@(PerspectRol{isMe, pspType:fillerType}) -> do
            loadModel fillerType

            -- PERSISTENCE
            filled `filledPointsTo` filler
            filler `fillerPointsTo` filled

            -- QUERY EVALUATION
            filledContextType <- lift (rol_context filledRole ##>> contextType)
            (lift $ findFilledRoleRequests filler filledContextType (rol_pspType filledRole)) >>= addCorrelationIdentifiersToTransactie
            (lift $ findBindingRequests filled) >>= addCorrelationIdentifiersToTransactie

            -- ISME, ME
            (EnumeratedRole{kindOfRole}) <- lift $ getEnumeratedRole (rol_pspType filledRole)
            if isMe && kindOfRole == UserRole then roleIsMe filled (rol_context filledRole) else pure unit

            subject <- getSubject
            delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
              { filled : filled
              , filledType: rol_pspType filledRole
              , filler: Just filler
              , fillerType: Just fillerType
              , oldFiller: Nothing
              , oldFillerType: Nothing
              , deltaType: SetFirstBinding
              , subject
              }

            -- STATE EVALUATION
            -- Adds deltas for paths beyond the nodes involved in the binding,
            -- for queries that use the binder- or binding step.
            users <- usersWithPerspectiveOnRoleBinding delta true
            author <- getAuthor
            signedDelta <-  case msignedDelta of
              Nothing -> pure $ SignedDelta
                { author: stripNonPublicIdentifiers author
                , encryptedDelta: sign $ encodeJSON $ stripResourceSchemes $ delta}
              Just signedDelta -> pure signedDelta

            -- SYNCHRONISATION
            handleNewPeer filled
            addDelta (DeltaInTransaction { users, delta: signedDelta })
            -- Save the SignedDelta as the bindingDelta in the role. Re-fetch filled as it has been changed!

            -- PERSISTENCE
            (modifiedFilled :: PerspectRol) <- lift $ getPerspectEntiteit filled
            cacheAndSave filled (over PerspectRol (\rl -> rl {bindingDelta = Just signedDelta}) modifiedFilled)

            pure users

  where
    -- If the type of the new binding is unknown, load the model. There is only one
    -- circumstance that we can have a RoleInstance of an unknown type and that is when
    -- it is the external role of a sys:Model context. This role has a property Url that
    -- we can fetch the model from.
    loadModel :: EnumeratedRoleType -> MonadPerspectivesTransaction Unit
    loadModel fillerType = do
      mDomeinFile <- lift $ traverse tryRetrieveDomeinFile (DomeinFileId <$> (typeUri2ModelUri $ unwrap fillerType))
      if (isNothing mDomeinFile)
        then addModelToLocalStore' (DomeinFileId $ unsafePartial fromJust (typeUri2ModelUri $ unwrap fillerType))
        else pure unit

-- | If the type of the role has kind UserRole and is not the `me` role for its context,
-- | we add a new user to the context. This user should have access to
-- | this context. We will generate Deltas so his PDR can build it from scratch,
-- | according to his perspective.
-- | Notice that in order to establish whether this role represents `usr:Me`,
-- | it needs a binding!
handleNewPeer :: RoleInstance -> MonadPerspectivesTransaction Unit
handleNewPeer roleInstance = (lift $ try$ getPerspectRol roleInstance) >>=
  handlePerspectRolError "handleNewPeer"
    \(PerspectRol{context, pspType}) -> do
      (EnumeratedRole{kindOfRole}) <- lift $ getEnumeratedRole pspType
      me <- lift $ isMe roleInstance
      if kindOfRole == UserRole && not me
        then context `serialisedAsDeltasFor` roleInstance
        else pure unit

-- | PERSISTENCE of binding role and old binding, through the call to `changeRoleBinding`.
-- | CURRENTUSER for roleId and its context, through the call to `changeRoleBinding`.
-- | STATE CHANGE for `binding <roleId`, `binder <TypeOfRoleId>` for the old binding.
-- | QUERY UPDATES for `binding <roleId>` and `binder <TypeOfRoleId>`.
-- | SYNCHRONISATION by RoleBindingDelta.
removeBinding :: RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding roleId = do
  users <- removeBinding_ roleId Nothing Nothing
  -- PERSISTENCE
  -- Schedule the removal of the binding. Add it to the end of the scheduled destructive effects.
  modify (\t -> over Transaction (\tr -> tr {scheduledAssignments = tr.scheduledAssignments `union` [RoleUnbinding roleId Nothing Nothing]}) t)
  pure users

-- | QUERY EVALUATION
-- | STATE EVALUATION
-- | SYNCHRONISATION
removeBinding_ :: RoleInstance -> Maybe RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding_ filled mFillerId msignedDelta = (lift $ try $ getPerspectEntiteit filled) >>=
  handlePerspectRolError' "removeBinding_" []
    \(originalFilled :: PerspectRol) -> case rol_binding originalFilled of
      Nothing -> pure []
      Just oldFillerId -> do
        filledContextType <- lift (rol_context originalFilled ##>> contextType)
        -- QUERY EVALUATION
        (lift $ findFilledRoleRequests oldFillerId filledContextType (rol_pspType originalFilled)) >>= addCorrelationIdentifiersToTransactie
        (lift $ findBindingRequests filled) >>= addCorrelationIdentifiersToTransactie

        -- STATE EVALUATION
        subject <- getSubject
        fillerType <- lift $ traverse roleType_ mFillerId
        oldFillerType <- lift $ traverse roleType_ (rol_binding originalFilled)
        delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
                      { filled
                      , filledType: rol_pspType originalFilled
                      , filler: case mFillerId of
                        Nothing -> (rol_binding originalFilled)
                        otherwise -> mFillerId
                      , fillerType: case mFillerId of
                        Nothing -> oldFillerType
                        _ -> fillerType
                      , oldFiller: (rol_binding originalFilled)
                      , oldFillerType
                      -- TODO. Dit moet ook een ReplaceBinding kunnen zijn!
                      , deltaType: case mFillerId of
                        Nothing -> RemoveBinding
                        otherwise -> ReplaceBinding
                      , subject
                      }
        users <- usersWithPerspectiveOnRoleBinding delta true
        roleWillBeRemoved <- gets \(Transaction{untouchableRoles}) -> isJust $ elemIndex filled untouchableRoles
        if roleWillBeRemoved
          then pure unit
          else do
            -- SYNCHRONISATION
            -- Only push a RoleBindingDelta if the role will not be removed.
            author <- getAuthor
            signedDelta <- case msignedDelta of
              Nothing ->  pure $ SignedDelta
                { author: stripNonPublicIdentifiers author
                , encryptedDelta: sign $ encodeJSON $ stripResourceSchemes $ delta}
              Just signedDelta -> pure signedDelta
            handleNewPeer filled
            addDelta (DeltaInTransaction { users, delta: signedDelta})

        pure users

-- | Removes the link between the role and its filler, in both directions (unless the filler is removed anyway:
-- | then we just handle the link from role to filler, i.e. the FILLEDBY link).
-- | Removes the binding R of the rol, if any.
-- | Removes the role as value of 'filledRoles' for psp:Rol$binding from the binding R.
-- | Modifies the Role instance.
-- | If the the role instance has in fact no binding before the operation and no filler is given as parameter,
-- | this is a no-op without effect.
-- | If a filler is given as parameter, it will be inserted into the filled role.
-- | PERSISTENCE of binding role and old binding.
-- | CURRENTUSER for roleId and its context.
changeRoleBinding :: RoleInstance -> (Maybe RoleInstance) -> MonadPerspectivesTransaction Unit
changeRoleBinding filledId mNewFiller = (lift $ try $ getPerspectEntiteit filledId) >>=
  handlePerspectRolError' "changeRoleBinding" unit
    \(filled :: PerspectRol) -> do
      roleWillBeRemoved <- gets \(Transaction{untouchableRoles}) -> isJust $ elemIndex filledId untouchableRoles
      -- If the role will be removed, we don't modify it (we don't remove its binding).
      -- However, regardless of that we'll have to modify the filler.
      -- If the role will not be removed and there is a new filler, we fill it with that filler
      -- and record with the filler the new filled role.

      -- Regardless of the filled will be removed, we'll have to remove the pointer from the filler to it.
      case rol_binding filled of
        Just filler -> filler `fillerNoLongerPointsTo` filledId
        _ -> pure unit

      if not roleWillBeRemoved
        -- Remove the link from filler to filled (= the current role).
        then do
          case mNewFiller of
            Nothing -> case rol_binding filled of
              Nothing -> pure unit
              Just filler -> filledId `filledNoLongerPointsTo` filler
            Just newFiller -> do
              filledId `filledPointsTo` newFiller
              newFiller `fillerPointsTo` filledId
        else pure unit

      -- CURRENTUSER
      -- True, iff filled is bound the User of this System.
      filledIsMe <- lift $ isMe filledId
      if roleWillBeRemoved
        then if filledIsMe
          -- We leave filled as it is.
          -- Tries to find a new value for Me
          -- in the context of filled and will set its isMe to true.
          then lookForAlternativeMe filledId (rol_context filled)
          else pure unit
        else if filledIsMe
          then case mNewFiller of
            Nothing -> do
              -- If there is no filler, isMe of the filled must be false.
              roleIsNotMe filledId -- Changes isMe of the role to false.
              -- Tries to find a new value for Me
              -- in the context of filled and will set its isMe to true.
              lookForAlternativeMe filledId (rol_context filled)
            Just newFiller -> do
              newFillerIsMe <- lift $ isMe newFiller
              if newFillerIsMe
                -- Filled was me and is me again. No change.
                then pure unit
                else do
                  roleIsNotMe filledId
                  lookForAlternativeMe filledId (rol_context filled)
          else case mNewFiller of
            Nothing -> pure unit
            Just newFiller -> do
              (EnumeratedRole{kindOfRole}) <- lift $ getEnumeratedRole (rol_pspType filled)
              if kindOfRole == UserRole 
                then do 
                  newFillerIsMe <- lift $ isMe newFiller
                  if newFillerIsMe
                    then roleIsMe filledId (rol_context filled) -- Set isMe of the role and set the role to Me of the context.
                    else pure unit
                else pure unit

