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
  ( saveContextInstance
  , removeContextInstance
  , removeRoleInstance
  , removeAllRoleInstances
  , removeContextIfUnbound
  , removeBinding
  , setBinding
  , setFirstBinding
  , replaceBinding
  , handleNewPeer
  , changeRoleBinding
  )

  where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.State (lift)
import Data.Array (concat, delete, elemIndex, head, nub, union)
import Data.Array.NonEmpty (singleton)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, for_, traverse)
import Effect.Exception (error)
import Foreign.Generic (encodeJSON)
import Foreign.Object (values)
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (getAuthor, getSubject, cacheAndSave)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (addRoleObservingContexts, usersWithPerspectiveOnRoleBinding, lift2, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_binding, changeRol_isMe, context_buitenRol, context_iedereRolInContext, modifyContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, rol_binding, rol_context, rol_isMe, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater, (##=), (##>), (##>>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBindingRequests, findFilledRoleRequests, findResourceDependencies, findRoleRequests)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.Identifiers (deconstructBuitenRol, deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (allRoleBinders, contextType, getProperty, isMe)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol, removeEntiteit, saveEntiteit)
import Perspectives.Query.UnsafeCompiler (getMyType, getRoleInstances)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..), externalRoleType)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (isUnlinked_)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), SubjectOfAction(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, join, not, pure, unit, void, ($), (&&), (<>), (==), (>>=), (<$>))

-- | This function takes care of
-- | PERSISTENCE
-- | QUERY UPDATES
-- | RULE TRIGGERING
-- | but nothing else. It is only used by loadAndSaveCrlFile. This means that we only deal with
-- | contexts that have been created from a source file, freshly as it were, fully equipped with
-- | roles.
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt@(PerspectContext{pspType:cType})) <- lift2 $ saveEntiteit id
  subject <- getSubject
  forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances ->
    case head instances of
      Nothing -> pure unit
      -- STATE CHANGES
      Just i -> addRoleObservingContexts cType id (EnumeratedRoleType roleName) i
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    (PerspectRol{_id, binding, pspType:roleType, filledRoles}) <- lift2 $ saveEntiteit rol
    case binding of
      Nothing -> pure unit
      Just fillerId -> (lift2 $ findFilledRoleRequests fillerId cType roleType) >>= addCorrelationIdentifiersToTransactie
    for_ filledRoles \roleMap ->
      for_ roleMap \instances ->
        for_ instances \binder ->
          (lift2 $ findBindingRequests binder) >>= addCorrelationIdentifiersToTransactie
    -- For rule triggering, not for the delta or SYNCHRONISATION:
    if isJust binding
      then void $ usersWithPerspectiveOnRoleBinding (RoleBindingDelta
        { filled: rol
        , filler: binding
        , oldFiller: Nothing
        , deltaType: SetFirstBinding
        , subject
        })
        true
      else pure unit
  (_ :: PerspectRol) <- lift2 $ saveEntiteit (context_buitenRol ctxt)
  -- For roles with a binding equal to R: detect the binder <RoleType> requests for R
  -- For roles that are bound by role R: detect the binding requests for R.
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- Only called when the external role is also 'bound' in a DBQ role.
removeContextIfUnbound :: RoleInstance -> Maybe RoleType ->  MonadPerspectivesTransaction Unit
removeContextIfUnbound roleInstance@(RoleInstance rid) rtype = do
  mbinder <- lift (lift (roleInstance ##> allRoleBinders))
  case mbinder of
    Nothing -> removeContextInstance (ContextInstance $ deconstructBuitenRol rid) rtype
    otherwise -> pure unit

-- | Remove the context instance plus roles after detaching all its roles.
-- | Does NOT remove the context role binding the external role.
removeContextInstance :: ContextInstance -> Maybe RoleType -> MonadPerspectivesTransaction Unit
removeContextInstance id authorizedRole = do
  (lift $ lift $ try $ getPerspectContext id) >>=
    handlePerspectContextError "removeContextInstance"
    \(ctxt@(PerspectContext{pspType, rolInContext, buitenRol})) -> do
      -- | STATE EVALUATION
      -- | QUERY UPDATES
      -- | PERSISTENCE (for role instances).
      users1 <- concat <$> for (concat $ values rolInContext) handleRoleOnContextRemoval
      users2 <- handleRoleOnContextRemoval buitenRol
      -- SYNCHRONISATION
      subject <- getSubject
      me <- getAuthor
      -- (roleType ###>> hasAspect (EnumeratedRoleType "sys:RootContext$External"))
      addDelta $ DeltaInTransaction
        { users: nub $ users1 <> users2
        , delta: SignedDelta
            { author: me
            , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
              { id
              , roleType: externalRoleType pspType
              , authorizedRole
              , roleInstances: SNEA.singleton (context_buitenRol ctxt)
              , deltaType: RemoveExternalRoleInstance
              , subject
              }}}
      -- Only now remove all resources.
      lift2 $ do
        for_ (concat $ values rolInContext) removeEntiteit
        void $ removeEntiteit buitenRol
        removeEntiteit id

-- | Modifies the context instance by detaching the given role instances.
-- | Notice that this function does neither uncache nor unsave the rolInstances
-- | themselves. Instead, use removeRoleInstance.
-- | Does not touch the binding of any of the role instances.
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | STATE EVALUATION
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
removeRoleInstance :: RoleInstance -> MonadPerspectivesTransaction Unit
removeRoleInstance roleId = (lift2 $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError "removeRoleInstance"
  \role@(PerspectRol{pspType:roleType, context:contextId, binding}) -> do
    -- STATE EVALUATION
    users <- statesAndPeersForRoleInstanceToRemove role
    -- SYNCHRONISATION
    synchroniseRoleRemoval role users
    -- QUERY UPDATES.
    queryUpdatesForRoleRemoval role
    -- PERSISTENCE (remove role instance from context).
    removeRoleInstanceFromContext role
    -- PERSISTENCE (severe the binding links of the incoming FILLS relation).
    severeBindingLinks role
    -- PERSISTENCE (finally remove the role instance from cache and database).
    lift2 $ removeEntiteit roleId

-- | Handles all responsibilities for the given role instance in the dynamic context
-- | that it's context is removed:
-- | STATE EVALUATION
-- | QUERY UPDATES
-- | PERSISTENCE (just severe the binding links).
handleRoleOnContextRemoval :: RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
handleRoleOnContextRemoval roleId = (lift2 $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError' "removeRoleInstance" []
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
statesAndPeersForRoleInstanceToRemove (PerspectRol{_id:roleId, pspType:roleType, binding}) = do
  -- The last boolean argument prevents usersWithPerspectiveOnRoleInstance from adding Deltas to the transaction
  -- for the continuation of the path beyond the given role instance.
  users1 <- usersWithPerspectiveOnRoleInstance roleType roleId false
  -- Users from the inverted queries on the incoming fills relation:
  users2 <- usersWithPerspectiveOnRoleBinding' roleId binding
  pure $ nub $ users1 <> users2

  where
    usersWithPerspectiveOnRoleBinding' :: RoleInstance -> Maybe RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
    usersWithPerspectiveOnRoleBinding' filled filler = usersWithPerspectiveOnRoleBinding
      (RoleBindingDelta
        { filled
        , filler
        , oldFiller: Nothing
        , deltaType: RemoveBinding
        , subject: UserInstance (RoleInstance "") -- is not used
      })
      -- Do not run forwards query!
      false

-- | SYNCHRONISATION. Compute a RemoveRoleInstance UniverseRoleDelta.
synchroniseRoleRemoval :: PerspectRol -> Array RoleInstance -> MonadPerspectivesTransaction Unit
synchroniseRoleRemoval (PerspectRol{_id:roleId, pspType:roleType, context:contextId}) users = do
  -- SYNCHRONISATION
  subject <- getSubject
  author <- getAuthor
  addDelta $ DeltaInTransaction
    { users
    , delta: SignedDelta
      { author
      , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
        { id: contextId
        , roleInstances: (SerializableNonEmptyArray (singleton roleId))
        , roleType
        , authorizedRole: Nothing
        , deltaType: RemoveRoleInstance
        , subject } }}

-- | QUERY UPDATES. Add correlation identifiers to the current transaction for
-- | each request that needs to be updated when the role instance is removed.
queryUpdatesForRoleRemoval :: PerspectRol -> MonadPerspectivesTransaction Unit
queryUpdatesForRoleRemoval role@(PerspectRol{_id:roleId, pspType:roleType, context:contextId}) = do
  cType <- lift2 (contextId ##>> contextType)
  (lift2 $ findRoleRequests contextId roleType) >>= addCorrelationIdentifiersToTransactie
  if rol_isMe role
    then (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me"))
      >>= addCorrelationIdentifiersToTransactie
    else pure unit
  -- All requests where roleId is the resource of the assumption (its first member) should be added to
  -- the transaction. They will be binding requests, filled role requests and state requests.
  (lift2 $ findResourceDependencies (unwrap roleId)) >>= addCorrelationIdentifiersToTransactie

-- | PERSISTENCE. Remove the role instance from context.
removeRoleInstanceFromContext :: PerspectRol -> MonadPerspectivesTransaction Unit
removeRoleInstanceFromContext role@(PerspectRol{_id:roleId, pspType:roleType, context:contextId}) = do
  (lift2 $ try $ getPerspectContext contextId) >>=
    handlePerspectContextError "removeRoleInstance"
      \(pe :: PerspectContext) -> do
        unlinked <- lift2 $ isUnlinked_ roleType
        -- Modify the context: remove the role instances from those recorded with the role type.
        changedContext <- if unlinked
          then pure pe
          else lift2 (modifyContext_rolInContext pe roleType (delete roleId))
        if rol_isMe role
          then do
            -- CURRENTUSER.
            mmyType <- lift2 (contextId ##> getMyType)
            case mmyType of
              Nothing -> cacheAndSave contextId (changeContext_me changedContext Nothing)
              Just myType -> do
                mme <- lift2 (contextId ##> getRoleInstances myType)
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
  instances <- lift2 (cid ##= getRoleInstances (ENR et))
  for_ instances removeRoleInstance

--------------------------
-- UPDATE A ROLE (ADD OR REMOVE A BINDING)
-- All mutations on a binding should handle RULE TRIGGERING and QUERY UPDATES
-- for both ways to traverse a binding.
-- They should also take care of PERSISTENCE of the binding role and
-- the bound role. SYNCHRONISATION should be taken care of by a RoleBindingDelta.
-----------------------------------------------------------
setBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setBinding roleId newBindingId msignedDelta = (lift2 $ try $ getPerspectEntiteit roleId) >>=
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
replaceBinding roleId (newBindingId :: RoleInstance) msignedDelta = (lift2 $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "replaceBinding" []
    \(originalRole :: PerspectRol) -> do
      if (rol_binding originalRole == Just newBindingId)
        then pure []
        else do
          users <- removeBinding_ roleId (Just newBindingId) msignedDelta
          -- PERSISTENCE
          -- Schedule the replacement of the binding.
          lift $ modify (\t -> over Transaction (\tr -> tr {scheduledAssignments = tr.scheduledAssignments `union` [RoleUnbinding roleId (Just newBindingId) msignedDelta]}) t)
          pure users

-- | PERSISTENCE
-- | QUERY EVALUATION
-- | CURRENTUSER
-- | SYNCHRONISATION
-- | STATE EVALUATION
setFirstBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setFirstBinding filled filler msignedDelta = (lift2 $ try $ getPerspectEntiteit filled) >>=
  handlePerspectRolError' "setFirstBinding, filled" []
    \(filledRole :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit filler) >>=
      handlePerspectRolError' "setFirstBinding, filler" []
        \fillerRole@(PerspectRol{isMe, pspType:fillerType}) -> do
          loadModel fillerType

          -- PERSISTENCE
          filled `filledPointsTo` filler
          filler `fillerPointsTo` filled

          -- QUERY EVALUATION
          filledContextType <- lift2 (rol_context filledRole ##>> contextType)
          (lift2 $ findFilledRoleRequests filler filledContextType (rol_pspType filledRole)) >>= addCorrelationIdentifiersToTransactie
          (lift2 $ findBindingRequests filled) >>= addCorrelationIdentifiersToTransactie

          -- ISME, ME
          if isMe then roleIsMe filled (rol_context filledRole) else pure unit

          subject <- getSubject
          delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
            { filled : filled
            , filler: Just filler
            , oldFiller: Nothing
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
              { author
              , encryptedDelta: sign $ encodeJSON $ delta}
            Just signedDelta -> pure signedDelta

          -- SYNCHRONISATION
          addDelta (DeltaInTransaction { users, delta: signedDelta })
          -- Save the SignedDelta as the bindingDelta in the role. Re-fetch filled as it has been changed!

          -- PERSISTENCE
          (modifiedFilled :: PerspectRol) <- lift2 $ getPerspectEntiteit filled
          cacheAndSave filled (over PerspectRol (\rl -> rl {bindingDelta = Just signedDelta}) modifiedFilled)

          pure users

  where
    -- If the type of the new binding is unknown, load the model. There is only one
    -- circumstance that we can have a RoleInstance of an unknown type and that is when
    -- it is the external role of a sys:Model context. This role has a property Url that
    -- we can fetch the model from.
    loadModel :: EnumeratedRoleType -> MonadPerspectivesTransaction Unit
    loadModel fillerType = do
      mDomeinFile <- lift2 $ traverse tryRetrieveDomeinFile (deconstructModelName $ unwrap fillerType)
      if (isNothing mDomeinFile)
        then do
          murl <- lift2 (filler ##> getProperty (EnumeratedPropertyType "model:System$Model$External$Url"))
          case murl of
            Nothing -> throwError (error $ "System error: no url found to load model for unknown type " <> (unwrap fillerType))
            Just (Value url) -> addModelToLocalStore [url] filled
        else pure unit

-- | If the type of the role has kind UserRole and is not the `me` role for its context,
-- | we add a new user to the context. This user should have access to
-- | this context. We will generate Deltas so his PDR can build it from scratch,
-- | according to his perspective.
-- | Notice that in order to establish whether this role represents `usr:Me`,
-- | it needs a binding!
handleNewPeer :: RoleInstance -> MonadPerspectivesTransaction Unit
handleNewPeer roleInstance = (lift2 $ try$ getPerspectRol roleInstance) >>=
  handlePerspectRolError "handleNewPeer"
    \(PerspectRol{context, pspType}) -> do
      (EnumeratedRole{kindOfRole}) <- lift2 $ getEnumeratedRole pspType
      me <- lift2 $ isMe roleInstance
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
  -- Schedule the removal of the binding.
  lift $ modify (\t -> over Transaction (\tr -> tr {scheduledAssignments = tr.scheduledAssignments `union` [RoleUnbinding roleId Nothing Nothing]}) t)
  pure users

-- | QUERY EVALUATION
-- | STATE EVALUATION
-- | SYNCHRONISATION
removeBinding_ :: RoleInstance -> Maybe RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding_ filled mFillerId msignedDelta = (lift2 $ try $ getPerspectEntiteit filled) >>=
  handlePerspectRolError' "removeBinding_" []
    \(originalFilled :: PerspectRol) -> case rol_binding originalFilled of
      Nothing -> pure []
      Just oldFillerId -> do
        filledContextType <- lift2 (rol_context originalFilled ##>> contextType)
        -- QUERY EVALUATION
        (lift2 $ findFilledRoleRequests oldFillerId filledContextType (rol_pspType originalFilled)) >>= addCorrelationIdentifiersToTransactie
        (lift2 $ findBindingRequests filled) >>= addCorrelationIdentifiersToTransactie

        -- STATE EVALUATION
        subject <- getSubject
        delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
                      { filled
                      , filler: case mFillerId of
                          Nothing -> (rol_binding originalFilled)
                          otherwise -> mFillerId
                      , oldFiller: (rol_binding originalFilled)
                      -- TODO. Dit moet ook een ReplaceBinding kunnen zijn!
                      , deltaType: case mFillerId of
                        Nothing -> RemoveBinding
                        otherwise -> ReplaceBinding
                      , subject
                      }
        users <- usersWithPerspectiveOnRoleBinding delta true
        roleWillBeRemoved <- lift $ gets \(Transaction{untouchableRoles}) -> isJust $ elemIndex filled untouchableRoles
        if roleWillBeRemoved
          then pure unit
          else do
            -- SYNCHRONISATION
            -- Only push a RoleBindingDelta if the role will not be removed.
            author <- getAuthor
            signedDelta <- case msignedDelta of
              Nothing ->  pure $ SignedDelta
                { author
                , encryptedDelta: sign $ encodeJSON $ delta}
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
changeRoleBinding filledId mNewFiller = (lift2 $ try $ getPerspectEntiteit filledId) >>=
  handlePerspectRolError "changeRoleBinding"
    \(filled :: PerspectRol) -> do
      roleWillBeRemoved <- lift $ gets \(Transaction{untouchableRoles}) -> isJust $ elemIndex filledId untouchableRoles
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
      -- True, iff filled bound the User of this System.
      filledIsMe <- lift2 $ isMe filledId
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
              newFillerIsMe <- lift2 $ isMe newFiller
              if newFillerIsMe
                -- Filled was me and is me again. No change.
                then pure unit
                else do
                  roleIsNotMe filledId
                  lookForAlternativeMe filledId (rol_context filled)
          else case mNewFiller of
            Nothing -> pure unit
            Just newFiller -> do
              newFillerIsMe <- lift2 $ isMe newFiller
              if newFillerIsMe
                then roleIsMe filledId (rol_context filled) -- Set isMe of the role and set the role to Me of the context.
                else pure unit

-- | In the context of the roleInstance, find the role that is me and set its isMe to true.
-- | Set the me of the context to that role.
-- | If not found, set me of the context to Nothing.
-- | Invariant: roleId was the previous value of me of contextId at the time of calling.
lookForAlternativeMe :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
lookForAlternativeMe roleId contextId = (lift2 $ try $ getPerspectContext contextId) >>=
  handlePerspectContextError "lookForAlternativeMe"
    \ctxt -> do
      mmyType <- lift2 (contextId ##> getMyType)
      case mmyType of
        Nothing -> cacheAndSave contextId (changeContext_me ctxt Nothing)
        Just myType -> do
          mme <- lift2 (contextId ##> getRoleInstances myType)
          case mme of
            Nothing -> cacheAndSave contextId (changeContext_me ctxt Nothing)
            Just me -> roleIsMe me contextId

-- | Set isMe of the roleInstance to false.
roleIsNotMe :: RoleInstance -> MonadPerspectivesTransaction Unit
roleIsNotMe roleId = (lift2 $ try $ getPerspectRol roleId) >>=
  handlePerspectRolError "roleIsNotMe"
    \role -> do
      cacheAndSave roleId (changeRol_isMe role false)

-- | Set isMe of the roleInstance to true.
-- | Set me of the context to the roleInstance.
roleIsMe :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
roleIsMe roleId contextId = (lift2 $ try $ getPerspectContext contextId) >>=
  handlePerspectContextError "roleIsMe"
    \ctxt -> (lift2 $ try $ getPerspectRol roleId) >>=
      handlePerspectRolError "roleIsMe"
        \role -> do
          cacheAndSave roleId (changeRol_isMe role true)
          cacheAndSave contextId (changeContext_me ctxt (Just roleId))

-- | <fillerId> `fillerNoLongerPointsTo` <filledId>
-- | Break the link from filler to filled (FILLS link)
-- | (Remove from the filledRoles, in other words: change filler)
-- | Not the other way round!
-- | Removes filled from filledRoles of filler (because filler no longer fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
fillerNoLongerPointsTo fillerId filledId = (lift2 $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError "fillerNoLongerPointsTo"
    \(filler :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError "fillerNoLongerPointsTo"
      \(filled :: PerspectRol) -> do
        filledContextType <- lift2 (rol_context filled ##>> contextType)
        filler' <- pure $ (removeRol_gevuldeRollen filler filledContextType (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <fillerId> `fillerPointsTo` <filledId>
-- | Add the link from filler to filled (FILLS link)
-- | (Add to the filledRoles, in other words: change filler)
-- | Not the other way round!
-- | Adds filled from filledRoles of filler (because filler now fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
fillerPointsTo fillerId filledId = (lift2 $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError "fillerNoLongerPointsTo"
    \(filler :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError "fillerNoLongerPointsTo"
      \(filled :: PerspectRol) -> do
        filledContextType <- lift2 (rol_context filled ##>> contextType)
        filler' <- pure $ (addRol_gevuldeRollen filler filledContextType (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <filledId> `filledNoLongerPointsTo` <fillerId>
-- | Break the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Remove the binding, in other words: change filled. Also removes the bindingDelta.)
-- | NOTE: the second argument is currently useless, but we anticipate with it on multiple fillers.
filledNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
filledNoLongerPointsTo filledId fillerId = (lift2 $ try $ getPerspectEntiteit filledId) >>=
  handlePerspectRolError "filledNoLongerPointsTo"
    \(filled :: PerspectRol) -> cacheAndSave filledId (removeRol_binding filled)

-- | <filledId> `filledPointsTo` <fillerId>
-- | Add the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Insert the binding, in other words: change filled)
filledPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
filledPointsTo filledId fillerId = (lift2 $ try $ getPerspectEntiteit filledId) >>=
  handlePerspectRolError "filledNoLongerPointsTo"
    \(filled :: PerspectRol) -> cacheAndSave filledId (changeRol_binding fillerId filled)
