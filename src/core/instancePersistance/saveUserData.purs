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

module Perspectives.SaveUserData
  ( changeRoleBinding
  , doNotSynchronise
  , handleNewPeer
  , queryUpdatesForRole
  , removeAllRoleInstances
  , removeBinding
  , removeContextIfUnbound
  , removeContextInstance
  , removeRoleInstance
  , replaceBinding
  , scheduleContextRemoval
  , scheduleRoleRemoval
  , setBinding
  , setFirstBinding
  , severeBindingLinks
  , stateEvaluationAndQueryUpdatesForContext
  , synchronise
  )
  where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (try)
import Control.Monad.State (lift)
import Data.Array (concat, cons, delete, elemIndex, find, nub, union)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, for_, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Foreign.Object (Object, values)
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (cacheAndSave, deleteProperty, getSubject)
import Perspectives.Authenticate (signDelta)
import Perspectives.CollectAffectedContexts (addDeltasForPerspectiveObjects, usersWithPerspectiveOnRoleBinding, usersWithPerspectiveOnRoleBinding', usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (changeContext_me, context_buitenRol, context_pspType, modifyContext_rolInContext, rol_binding, rol_context, rol_isMe, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater, MonadPerspectives, (###=), (##=), (##>), (##>>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBindingRequests, findFilledRoleRequests, findMeRequests, findResourceDependencies, findRoleRequests)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, isExternalRole, typeUri2ModelUri)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Me (getMyType, isMe)
import Perspectives.Instances.ObjectGetters (allRoleBinders, context, contextType, contextType_, getRoleOnClipboard, getUnlinkedRoleInstances, roleType_)
import Perspectives.ModelDependencies (cardClipBoard)
import Perspectives.Names (findIndexedContextName, findIndexedRoleName, getMySystem, removeIndexedContext, removeIndexedRole)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, removeEntiteit, tryGetPerspectContext, tryGetPerspectRol)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Context (userRole)
import Perspectives.Representation.Class.PersistentType (DomeinFileId(..), getContext, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType, RoleKind(..), RoleType(..), externalRoleType)
import Perspectives.ResourceIdentifiers (isInPublicScheme)
import Perspectives.RoleAssignment (filledNoLongerPointsTo, filledPointsTo, fillerNoLongerPointsTo, fillerPointsTo, lookForAlternativeMe, roleIsMe, roleIsNotMe)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (allUnlinkedRoles, isUnlinked_)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, not, pure, unit, void, ($), (&&), (<$>), (<<<), (<>), (==), (>>=), (||))
import Simple.JSON (writeJSON)
 
synchronise :: Boolean
synchronise = true

doNotSynchronise :: Boolean
doNotSynchronise = false

-- | Add the role instance to the end of the roles to exit.
-- | Add the actual removal instruction to the end of the scheduledAssignments.
-- | (we build a last-in, last-out stack of destructive effects)
-- | Returns the user role instances that should be informed of the removal.
scheduleRoleRemoval :: Boolean -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
scheduleRoleRemoval sync id = do
  -- It may happen that two public roles with the same address both have a perspective on this role instance.
  -- In that case, we remove the instance for the first public role (in a random order). 
  -- We should check whether it is still there before we try to remove it.
  if isInPublicScheme (unwrap id)
    then (lift $ tryGetPerspectRol id) >>= case _ of 
      Nothing -> pure []
      Just _ -> f
    else f
  where 
    f :: MonadPerspectivesTransaction (Array RoleInstance)
    f = do
      contextOfRole <- lift (id ##>> context)
      -- If the role's context is scheduled to be removed, don't add its removal to the scheduledAssignments.
      contextIsScheduledToBeRemoved <- gets (\(Transaction{scheduledAssignments}) -> isJust $ find
        (case _ of
          ContextRemoval ctxt _ -> ctxt == contextOfRole
          _ -> false)
        scheduledAssignments)
      roleIsUntouchable <- gets (\(Transaction{untouchableRoles}) -> isJust $ elemIndex id untouchableRoles)
      if contextIsScheduledToBeRemoved || roleIsUntouchable
        then pure []
        else do 
          modify (over Transaction \t@{scheduledAssignments, rolesToExit} -> t
            { rolesToExit = rolesToExit `union` [id]
            , scheduledAssignments = scheduledAssignments `union` [RoleRemoval id]
            })
          clipboard <- lift getRoleOnClipboard
          case clipboard of 
            Nothing -> pure unit
            Just r -> if r == id
              then do 
                s <- lift $ getMySystem
                deleteProperty [RoleInstance $ buitenRol s] (EnumeratedPropertyType cardClipBoard) Nothing
              else pure unit
          if sync
            then (lift $ try $ (getPerspectRol id)) >>= handlePerspectRolError' "scheduleRoleRemoval" []
              \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
                -- STATE EVALUATION
                users <- statesAndPeersForRoleInstanceToRemove role
                -- SYNCHRONISATION ADDS DELTAS
                synchroniseRoleRemoval role users
                pure users
            else pure []

-- | Schedules all roles in the context, including its external role, for removal.
-- | Add the actual removal instruction to the end of the scheduledAssignments.
-- | (we build a last-in, last-out stack of destructive effects)
scheduleContextRemoval :: Maybe RoleType -> Array RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
scheduleContextRemoval authorizedRole usersWithPerspectiveOnEmbeddingRole id = 
  -- It may happen that two public roles with the same address both have a perspective on this context instance.
  -- In that case, we remove the instance for the first public role (in a random order). 
  -- We should check whether it is still there before we try to remove it.
  if isInPublicScheme (unwrap id)
    then (lift $ tryGetPerspectContext id) >>= case _ of 
      Nothing -> pure unit
      Just _ -> f
    else f
  where 
    f :: MonadPerspectivesTransaction Unit
    f = (lift $ try $ getPerspectContext id) >>=
      handlePerspectContextError "scheduleContextRemoval"
      \(ctxt@(PerspectContext{rolInContext, buitenRol:br, pspType:contextType})) -> do
        unlinkedRoleTypes <- lift (contextType ###= allUnlinkedRoles)
        unlinkedInstances <- lift $ concat <$> (for unlinkedRoleTypes \rt -> id ##= getUnlinkedRoleInstances rt)
        allRoleInstances <- pure $ (cons br $ unlinkedInstances <> (concat $ values rolInContext))

        subject <- getSubject
        signedDelta <- signDelta $ writeJSON $ stripResourceSchemes $ UniverseRoleDelta 
          { id
          , contextType: context_pspType ctxt
          , roleType: over ContextType buitenRol (context_pspType ctxt)
          , authorizedRole
          -- Note that in this case, the authorizedRole is NOT the type of the roleInstances.
          , roleInstances: SNEA.singleton $ context_buitenRol ctxt
          , deltaType: RemoveExternalRoleInstance
          , subject
          }
        userRoleTypes <- lift (getContext (context_pspType ctxt) >>= pure <<< userRole)
        -- TODO: we missen de users die een perspectief hebben op de contextrol waar de context in is ingebed.
        (users :: Array RoleInstance) <- lift (concat <$> (traverse (\(userRole :: RoleType) -> id ##= getRoleInstances userRole) userRoleTypes ))
        -- Remove the own user
        addDelta $ DeltaInTransaction {users: users <> usersWithPerspectiveOnEmbeddingRole, delta: signedDelta}

        clipboard <- lift getRoleOnClipboard
        case clipboard of 
          Just roleOnClipboard -> if isJust $ elemIndex roleOnClipboard allRoleInstances
            then do 
              s <- lift $ getMySystem
              deleteProperty [RoleInstance (buitenRol s)] (EnumeratedPropertyType cardClipBoard) Nothing
            else pure unit
          _ -> pure unit
        modify (over Transaction \t@{scheduledAssignments, rolesToExit} -> t
          { scheduledAssignments = scheduledAssignments `union` [(ContextRemoval id authorizedRole)]
          , rolesToExit = nub $ rolesToExit <> allRoleInstances}) 

-- Only called when the external role is also 'bound' in a DBQ role.
removeContextIfUnbound :: RoleInstance -> Maybe RoleType ->  MonadPerspectivesTransaction Unit
removeContextIfUnbound roleInstance@(RoleInstance rid) rtype = do
  mbinder <- lift (roleInstance ##> allRoleBinders)
  case mbinder of
    Nothing -> scheduleContextRemoval rtype [] (ContextInstance $ deconstructBuitenRol rid)
    otherwise -> pure unit

-- | Remove the context instance plus roles after detaching all its roles.
-- | Does NOT remove the context role binding the external role.
-- | Logs an error if the context does not exist, but does not break.
-- | PERSISTENCE ()
removeContextInstance :: ContextInstance -> Maybe RoleType -> MonadPerspectives Unit
removeContextInstance id authorizedRole = do
  (try $ getPerspectContext id) >>=
    handlePerspectContextError "removeContextInstance"
    \(ctxt@(PerspectContext{pspType:contextType, rolInContext, buitenRol})) -> do
        unlinkedRoleTypes <- contextType ###= allUnlinkedRoles
        unlinkedInstances <- concat <$> (for unlinkedRoleTypes \rt -> id ##= getUnlinkedRoleInstances rt)
        for_ (unlinkedInstances <> (concat $ values rolInContext)) removeEntiteit
        -- Clean up the indexed role administration in state.
        findIndexedContextName id >>= case _ of 
          Just indexedName -> removeIndexedContext indexedName
          Nothing -> pure unit
        void $ removeEntiteit buitenRol
        removeEntiteit id

-- | STATE EVALUATION
-- | QUERY UPDATES
-- | SYNCHRONISATION
-- | Adds a RemoveExternalRoleInstance delta to the transaction.
stateEvaluationAndQueryUpdatesForContext :: ContextInstance -> Maybe RoleType -> MonadPerspectivesTransaction Unit
stateEvaluationAndQueryUpdatesForContext id authorizedRole = do
  (lift $ try $ getPerspectContext id) >>=
    handlePerspectContextError "stateEvaluationAndQueryUpdatesForContext"
    \(ctxt@(PerspectContext{pspType:contextType, rolInContext, buitenRol})) -> do
      users1 <- concat <$> for (concat $ values rolInContext) handleRoleOnContextRemoval
      users2 <- handleRoleOnContextRemoval buitenRol
      -- SYNCHRONISATION
      subject <- getSubject
      -- (roleType ###>> hasAspect (EnumeratedRoleType "sys:RootContext$External"))
      delta <- signDelta 
        (writeJSON $ stripResourceSchemes $ UniverseRoleDelta
              { id
              , contextType
              , roleType: externalRoleType contextType
              , authorizedRole
              , roleInstances: SNEA.singleton (context_buitenRol ctxt)
              , deltaType: RemoveExternalRoleInstance
              , subject
              })
      addDelta $ DeltaInTransaction
        { users: nub $ users1 <> users2
        , delta }

-- | Modifies the context instance by detaching the given role instances.
-- | PERSISTENCE of the context instance.
removeRoleInstance :: RoleInstance -> MonadPerspectives Unit
removeRoleInstance roleId = (try $ (getPerspectRol roleId)) >>= handlePerspectRolError "removeRoleInstance"
  \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
    -- PERSISTENCE (remove role instance from context).
    removeRoleInstanceFromContext role
    -- PERSISTENCE (severe the binding links of the incoming FILLS relation).
    severeBindingLinks role
    -- Clean up the indexed role administration in state.
    findIndexedRoleName roleId >>= case _ of 
      Just indexedName -> removeIndexedRole indexedName
      Nothing -> pure unit
    -- PERSISTENCE (finally remove the role instance from cache and database).
    removeEntiteit roleId

-- | QUERY UPDATES.
queryUpdatesForRole :: RoleInstance -> MonadPerspectivesTransaction Unit
queryUpdatesForRole roleId = (lift $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError "removeRoleInstance"
  \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
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
    lift $ severeBindingLinks role
    pure users

-- | Returns peers with a perspective on the instance.
-- | Adds StateEvaluations to the current transaction for resources whose state
-- | is affected when the role instance is removed.
-- | STATE EVALUATION and computing peers with a perspective on the instance.
-- | NO SYNCHRONIZATION: of the removal itself; this must be handled in the environment of the calling functions.
-- | Furthermore, adds no deltas to the transaction.
statesAndPeersForRoleInstanceToRemove :: PerspectRol -> MonadPerspectivesTransaction (Array RoleInstance)
statesAndPeersForRoleInstanceToRemove (PerspectRol{id:roleId, context, pspType:roleType, binding, filledRoles}) = do
  -- The last boolean argument prevents usersWithPerspectiveOnRoleInstance from adding Deltas to the transaction
  -- for the continuation of the path beyond the given role instance.
  users1 <- usersWithPerspectiveOnRoleInstance roleType roleId context false
  users2 <-  case binding of 
      -- Users from the inverted queries on the outgoing fills relation (roles that are filled by this role):
      -- The first index is the String representation of the ContextType, the second that of the EnumeratedRoleType.
      Nothing -> concat <$> for (values filledRoles)
        \(instancesPerRoleType :: Object (Array RoleInstance)) -> concat <$> values <$> forWithIndex instancesPerRoleType
          \(roleName :: String) (filledRoles' :: Array RoleInstance) -> concat <$> for filledRoles'
            \(filledRole :: RoleInstance) -> usersWithPerspectiveOnRoleBinding' filledRole roleId Nothing RemoveBinding false
      -- Users from the inverted queries on the incoming fills relation (the role that fills this role).
      -- This function takes into consideration all filleds and all fillers.
      Just filler -> usersWithPerspectiveOnRoleBinding' roleId filler Nothing RemoveBinding false
  pure $ nub $ users1 <> users2

-- | SYNCHRONISATION. Compute a RemoveRoleInstance UniverseRoleDelta 
-- | (but not for external roles: a delta is generated on exiting the context).
synchroniseRoleRemoval :: PerspectRol -> Array RoleInstance -> MonadPerspectivesTransaction Unit
synchroniseRoleRemoval (PerspectRol{id:roleId, pspType:roleType, context:contextId}) users = if isExternalRole (unwrap roleId)
  then pure unit
  else do
    contextWillBeRemoved <- gets (_.untouchableContexts <<< unwrap) >>= pure <<< isJust <<< elemIndex contextId
    if contextWillBeRemoved
      then pure unit
      else do
        -- SYNCHRONISATION
        contextType <- lift $ contextType_ contextId
        subject <- getSubject
        delta <- signDelta
          (writeJSON $ stripResourceSchemes $ UniverseRoleDelta
            { id: contextId
            , contextType
            , roleInstances: (SerializableNonEmptyArray (singleton roleId))
            , roleType
            , authorizedRole: Nothing
            , deltaType: RemoveRoleInstance
            , subject } )
        addDelta $ DeltaInTransaction { users, delta }

-- | QUERY UPDATES. Add correlation identifiers to the current transaction for
-- | each request that needs to be updated when the role instance is removed.
queryUpdatesForRoleRemoval :: PerspectRol -> MonadPerspectivesTransaction Unit
queryUpdatesForRoleRemoval role@(PerspectRol{id:roleId, pspType:roleType, context:contextId}) = do
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
removeRoleInstanceFromContext :: PerspectRol -> MonadPerspectives Unit
removeRoleInstanceFromContext role@(PerspectRol{id:roleId, pspType:roleType, context:contextId}) = do
  (try $ getPerspectContext contextId) >>=
    handlePerspectContextError "removeRoleInstance"
      \(pe :: PerspectContext) -> do
        unlinked <- isUnlinked_ roleType
        -- Modify the context: remove the role instances from those recorded with the role type.
        changedContext <- if unlinked
          then pure pe
          else modifyContext_rolInContext pe roleType (delete roleId)
        if rol_isMe role
          then do
            -- CURRENTUSER.
            mmyType <- contextId ##> getMyType
            case mmyType of
              Nothing -> cacheAndSave contextId (changeContext_me changedContext Nothing)
              Just myType -> do
                mme <- contextId ##> getRoleInstances myType
                cacheAndSave contextId (changeContext_me changedContext mme)
                -- and set isMe of mme!
          else cacheAndSave contextId changedContext

-- | PERSISTENCE. Remove the incoming links on both the incoming and outgoing
-- | binding relations.
severeBindingLinks :: PerspectRol -> MonadPerspectives Unit
severeBindingLinks (PerspectRol{id:roleId, pspType:roleType, binding, filledRoles}) = do
  -- Severe the binding links of the incoming FILLS relation:
  case binding of
    Nothing -> pure unit
    -- (Remove from the filledRoles, in other words: change filler)
    Just b -> if isInPublicScheme (unwrap b) && not isInPublicScheme (unwrap roleId)
      then pure unit
      else b `fillerNoLongerPointsTo` roleId
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
  lift $ for_ instances removeRoleInstance

--------------------------
-- UPDATE A ROLE (ADD OR REMOVE A BINDING)
-- All mutations on a binding should handle RULE TRIGGERING and QUERY UPDATES
-- for both ways to traverse a binding.
-- They should also take care of PERSISTENCE of the binding role and
-- the bound role. SYNCHRONISATION should be taken care of by a RoleBindingDelta.
-----------------------------------------------------------
setBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setBinding roleId newBindingId msignedDelta = (lift $ try $ getPerspectRol roleId) >>=
  handlePerspectRolError' "setBinding" []
    \(filled :: PerspectRol) -> if isJust $ rol_binding filled
      then replaceBinding roleId newBindingId msignedDelta
      -- Since in all cases that setBinding is called, the role instances involved do already exist, we can just
      -- insert all context serialisation deltas for a new peer right at the end of the array of deltas.
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
replaceBinding roleId (newBindingId :: RoleInstance) msignedDelta = (lift $ try $ getPerspectRol roleId) >>=
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
setFirstBinding filled filler msignedDelta = (lift $ try $ getPerspectRol filled) >>=
  handlePerspectRolError' "setFirstBinding, filled" []
    \(filledRole :: PerspectRol) -> if rol_binding filledRole == Just filler
      then pure []
      else  (lift $ try $ getPerspectRol filler) >>=
        handlePerspectRolError' "setFirstBinding, filler" []
          \fillerRole@(PerspectRol{isMe, pspType:fillerType}) -> do
            loadModel fillerType

            -- PERSISTENCE
            lift (filled `filledPointsTo` filler)
            if isInPublicScheme (unwrap filler) && not isInPublicScheme (unwrap filled)
              then pure unit
              else lift (filler `fillerPointsTo` filled)

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
            signedDelta <-  case msignedDelta of
              Nothing -> signDelta
                (writeJSON $ stripResourceSchemes $ delta)
              Just signedDelta -> pure signedDelta

            -- PERSISTENCE
            -- Save the SignedDelta as the bindingDelta in the role. Re-fetch filled as it has been changed!
            (modifiedFilled :: PerspectRol) <- lift $ getPerspectRol filled
            lift $ cacheAndSave filled (over PerspectRol (\rl -> rl {bindingDelta = Just signedDelta}) modifiedFilled)

            -- SYNCHRONISATION
            -- Only now can we compute the deltas that must be sent to other users in case the filled role is a 
            -- perspective object. The bindingdelta we've just added is a vital part of that.
            handleNewPeer filled
            addDelta (DeltaInTransaction { users, delta: signedDelta })

            void $ addDeltasForPerspectiveObjects filled

            pure users

  where
    loadModel :: EnumeratedRoleType -> MonadPerspectivesTransaction Unit
    loadModel fillerType = case (typeUri2ModelUri $ unwrap fillerType) of
      Nothing -> pure unit
      Just modelUri -> lift $ void $ retrieveDomeinFile (DomeinFileId modelUri)

-- | If the type of the role has kind UserRole and is not the `me` role for its context,
-- | we add a new user to the context. This user should have access to
-- | this context. We will generate Deltas so his PDR can build it from scratch,
-- | according to his perspective.
-- | Notice that in order to establish whether this role represents `sys:SocialMe`,
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
removeBinding_ filled mFillerId msignedDelta = (lift $ try $ getPerspectRol filled) >>=
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
            signedDelta <- case msignedDelta of
              Nothing -> signDelta
                (writeJSON $ stripResourceSchemes $ delta)
              Just signedDelta -> pure signedDelta
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
changeRoleBinding filledId mNewFiller = (lift $ try $ getPerspectRol filledId) >>=
  handlePerspectRolError' "changeRoleBinding" unit
    \(filled :: PerspectRol) -> do
      roleWillBeRemoved <- gets \(Transaction{untouchableRoles}) -> isJust $ elemIndex filledId untouchableRoles
      -- If the role will be removed, we don't modify it (we don't remove its binding).
      -- However, regardless of that we'll have to modify the filler.
      -- If the role will not be removed and there is a new filler, we fill it with that filler
      -- and record with the filler the new filled role.

      -- Regardless of the filled will be removed, we'll have to remove the pointer from the filler to it.
      case rol_binding filled of
        Just filler -> if isInPublicScheme (unwrap filler) && not isInPublicScheme (unwrap filledId)
          then pure unit
          else lift (filler `fillerNoLongerPointsTo` filledId)
        _ -> pure unit

      if not roleWillBeRemoved
        -- Remove the link from filler to filled (= the current role).
        then do
          case mNewFiller of
            Nothing -> case rol_binding filled of
              Nothing -> pure unit
              Just filler -> lift (filledId `filledNoLongerPointsTo` filler)
            Just newFiller -> do
              lift (filledId `filledPointsTo` newFiller)
              -- lift (newFiller `fillerPointsTo` filledId)
              if isInPublicScheme (unwrap newFiller) && not isInPublicScheme (unwrap filledId)
                then pure unit
                else lift (newFiller `fillerPointsTo` filledId)

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

