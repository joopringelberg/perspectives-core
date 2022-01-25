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
  , setBinding_
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
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Generic (encodeJSON)
import Foreign.Object (isEmpty, values)
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (getAuthor, getSubject, setMe, cacheAndSave)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (addRoleObservingContexts, usersWithPerspectiveOnRoleBinding, lift2, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_binding, changeRol_isMe, context_buitenRol, context_iedereRolInContext, modifyContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, rol_binding, rol_context, rol_isMe, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater, (##=), (##>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests, findBindingRequests, findRoleRequests, findRoleStateRequests)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.Identifiers (deconstructBuitenRol, deconstructModelName, isExternalRole)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (allRoleBinders, getProperty, isMe)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol, removeEntiteit, saveEntiteit)
import Perspectives.Query.UnsafeCompiler (getMyType, getRoleInstances)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..), externalRoleType)
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
  (ctxt :: PerspectContext) <- lift2 $ saveEntiteit id
  subject <- getSubject
  forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances ->
    case head instances of
      Nothing -> pure unit
      -- RULE TRIGGERING
      Just i -> addRoleObservingContexts id (EnumeratedRoleType roleName) i
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    (PerspectRol{_id, binding, pspType, gevuldeRollen}) <- lift2 $ saveEntiteit rol
    case binding of
      Nothing -> pure unit
      Just b -> (lift2 $ findBinderRequests b pspType) >>= addCorrelationIdentifiersToTransactie
    forWithIndex_ gevuldeRollen \_ instances -> for_ instances \binder ->
      (lift2 $ findBindingRequests binder) >>= addCorrelationIdentifiersToTransactie
    -- For rule triggering, not for the delta or SYNCHRONISATION:
    if isJust binding
      then void $ usersWithPerspectiveOnRoleBinding (RoleBindingDelta
        { id: rol
        , binding: binding
        , oldBinding: Nothing
        , deltaType: SetBinding
        , roleWillBeRemoved: false
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
  \role@(PerspectRol{pspType:roleType, context:contextId, binding, gevuldeRollen:filledRoles}) -> do
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
  \role@(PerspectRol{pspType:roleType, context:contextId, binding, gevuldeRollen:filledRoles}) -> do
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
        { id: filled
        , binding: filler
        , oldBinding: Nothing
        , deltaType: RemoveBinding
        , roleWillBeRemoved: true -- is not used
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
  (lift2 $ findRoleRequests contextId roleType) >>= addCorrelationIdentifiersToTransactie
  (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie
  (lift2 $ findBinderRequests roleId roleType) >>= addCorrelationIdentifiersToTransactie
  (lift2 $ findRoleStateRequests roleId) >>= addCorrelationIdentifiersToTransactie
  if rol_isMe role
    then (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me"))
      >>= addCorrelationIdentifiersToTransactie
    else pure unit

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
severeBindingLinks (PerspectRol{_id:roleId, pspType:roleType, binding, gevuldeRollen:filledRoles}) = do
  -- Severe the binding links of the incoming FILLS relation:
  case binding of
    Nothing -> pure unit
    -- (Remove from the gevuldeRollen, in other words: change filler)
    Just b -> b `fillerNoLongerPointsTo` roleId
  -- Severe the binding links of the outgoing FILLS relation:
  for_ filledRoles (\filledRoles' -> for_ filledRoles' \filled -> filled `filledNoLongerPointsTo` roleId)

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
-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | If the new binding is in fact bound to the role instance before the operation, this is a no-op without effect.
-- | function, because of createAndAddRoleInstance.
-- | PERSISTENCE of binding role, old binding and new binding.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for both the new and the old binding.
-- | QUERY UPDATES for `binding <roleId`, `binder <TypeOfRoleId>` for both the old binding and the new binding.
-- | CURRENTUSER for roleId and its context.
setBinding :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setBinding roleId (newBindingId :: RoleInstance) msignedDelta = (lift2 $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "setBinding" []
    \(originalRole :: PerspectRol) -> do
      if (rol_binding originalRole == Just newBindingId)
        then pure []
        else setBinding_ roleId newBindingId msignedDelta

-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | If the new binding is in fact bound to the role instance before the operation, we still have to execute the
-- | function, because of createAndAddRoleInstance.
-- | PERSISTENCE of binding role, old binding and new binding.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for both the new and the old binding.
-- | QUERY UPDATES for `binding <roleId`, `binder <TypeOfRoleId>` for both the old binding and the new binding.
-- | CURRENTUSER for roleId and its context.
setBinding_ :: RoleInstance -> RoleInstance -> Maybe SignedDelta -> MonadPerspectivesTransaction (Array RoleInstance)
setBinding_ roleId (newBindingId :: RoleInstance) msignedDelta = (lift2 $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "setBinding_, originalRole" []
    \(originalRole :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit newBindingId) >>=
      handlePerspectRolError' "setBinding_, new binding" []
      \newBinding@(PerspectRol{isMe, pspType}) -> do
        -- NOTE. If the role is a user role, we have to give it its new binding *before* we compute the users
        -- that should receive the delta.

        cacheAndSave roleId (changeRol_binding newBindingId originalRole)

        (lift2 $ findBinderRequests newBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
        (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie

        -- If the type of the new binding is unknown, load the model. There is only one
        -- circumstance that we can have a RoleInstance of an unknown type and that is when
        -- it is the external role of a sys:Model context. This role has a property Url that
        -- we can fetch the model from.
        mDomeinFile <- lift2 $ traverse tryRetrieveDomeinFile (deconstructModelName $ unwrap pspType)
        if (isNothing mDomeinFile)
          then do
            murl <- lift2 (newBindingId ##> getProperty (EnumeratedPropertyType "model:System$Model$External$Url"))
            case murl of
              Nothing -> throwError (error $ "System error: no url found to load model for unknown type " <> (unwrap pspType))
              Just (Value url) -> addModelToLocalStore [url] newBindingId
          else pure unit
        -- Handle isMe (on the binding role) and me (on its context).
        if isMe
          then do
            -- Since we're here, we can safely assume roleId can be exchanged for a PerspectRol.
            modifiedRole <- lift2 $ getPerspectEntiteit roleId
            cacheAndSave roleId (changeRol_isMe modifiedRole isMe)
            -- set roleId to be the value of Me of its context.
            setMe (rol_context modifiedRole) (Just roleId)
          else pure unit

        -- Handle inverse binding.
        case rol_binding originalRole of
          Nothing -> pure unit
          (Just (oldBindingId :: RoleInstance)) -> do
            (lift2 $ findBinderRequests oldBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
            -- Remove this roleinstance as a binding role from the old binding.
            (lift2 $ try $ getPerspectEntiteit oldBindingId) >>=
              handlePerspectRolError "setBinding_, old binding"
                \(oldBinding@(PerspectRol{isMe:oldIsMe}) :: PerspectRol) -> do
              oldBinding'@(PerspectRol{gevuldeRollen}) <- pure $ (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)
              if (isEmpty gevuldeRollen && isExternalRole (unwrap oldBindingId))
                -- Pass on the contextrole type or Nothing. It is an external role, so the binding role is a contextrole.
                -- So we pass on the type
                then removeContextInstance (ContextInstance $ deconstructBuitenRol (unwrap oldBindingId)) (Just $ ENR (rol_pspType originalRole))
                else cacheAndSave oldBindingId oldBinding'

              -- if the oldBinding has isMe and the new binding has not, than remove me from the context.
              if oldIsMe && not isMe
                then do
                  -- Since we're here, we can safely assume roleId can be exchanged for a PerspectRol.
                  modifiedRole <- lift2 $ getPerspectEntiteit roleId
                  setMe (rol_context modifiedRole) Nothing
                else pure unit

        -- Add this roleinstance as a binding role for the new binding.
        cacheAndSave newBindingId (addRol_gevuldeRollen newBinding (rol_pspType originalRole) roleId)

        subject <- getSubject
        delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
          { id : roleId
          , binding: Just newBindingId
          , oldBinding: rol_binding originalRole
          , deltaType: SetBinding
          , roleWillBeRemoved: false
          , subject
          }
        -- Adds deltas for paths beyond the nodes involved in the binding,
        -- for queries that use the binder- or binding step.
        users <- usersWithPerspectiveOnRoleBinding delta true
        author <- getAuthor
        signedDelta <-  case msignedDelta of
          Nothing -> pure $ SignedDelta
            { author
            , encryptedDelta: sign $ encodeJSON $ delta}
          Just signedDelta -> pure signedDelta
        addDelta (DeltaInTransaction { users, delta: signedDelta })
        -- Save the SignedDelta as the bindingDelta in the role.
        -- Since we're here, we can safely assume roleId can be exchanged for a PerspectRol.
        (modifiedRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
        cacheAndSave roleId (over PerspectRol (\rl -> rl {bindingDelta = Just signedDelta}) modifiedRole)

        pure users

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

-- | Removes the link between the role and its filler, in both directions (unless the filler is removed anyway:
-- | then we just handle the link from role to filler, i.e. the FILLEDBY link).
-- | Removes the binding R of the rol, if any.
-- | Removes the role as value of 'gevuldeRollen' for psp:Rol$binding from the binding R.
-- | Modifies the Role instance.
-- | If the the role instance has in fact no binding before the operation, this is a no-op without effect.
-- | Parameter `bindingRemoved` is true iff the role that is the binding of the role will be removed.
-- | In that case we need not add a RoleBindingDelta, because peers will work out from the UniverseRoleDelta
-- | what to do with the binding.
-- | PERSISTENCE of binding role and old binding.
-- | STATE CHANGE for `binding <roleId`, `binder <TypeOfRoleId>` for the old binding.
-- | QUERY UPDATES for `binding <roleId>` and `binder <TypeOfRoleId>`.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | CURRENTUSER for roleId and its context.
removeBinding :: Boolean -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding bindingRemoved roleId = (lift2 $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "removeBinding" []
    \(originalRole :: PerspectRol) -> case rol_binding originalRole of
      Nothing -> pure []
      Just bindingId -> do
        -- QUERY EVALUATION
        (lift2 $ findBinderRequests bindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
        (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie

        -- STATE EVALUATION
        subject <- getSubject
        roleWillBeRemoved <- lift $ gets \(Transaction{untouchableRoles}) -> isJust $ elemIndex roleId untouchableRoles
        delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
                      { id : roleId
                      , binding: (rol_binding originalRole)
                      , oldBinding: (rol_binding originalRole)
                      , deltaType: RemoveBinding
                      , roleWillBeRemoved
                      , subject
                      }
        users <- usersWithPerspectiveOnRoleBinding delta true
        if roleWillBeRemoved
          then pure unit
          else do
            -- SYNCHRONISATION
            -- Only push a RoleBindingDelta if the role will not be removed.
            author <- getAuthor
            signedDelta <- pure $ SignedDelta
              { author
              , encryptedDelta: sign $ encodeJSON $ delta}
            addDelta (DeltaInTransaction { users, delta: signedDelta})

        -- PERSISTENCE
        -- Schedule the removal of the binding.
        lift $ modify (\t -> over Transaction (\tr -> tr {rolesToUnbind = union [Tuple roleId Nothing] tr.rolesToUnbind}) t)

        pure users

changeRoleBinding :: Tuple RoleInstance (Maybe RoleInstance) -> MonadPerspectivesTransaction Unit
changeRoleBinding (Tuple filledId mNewFiller) = (lift2 $ try $ getPerspectEntiteit filledId) >>=
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
-- | (Remove from the gevuldeRollen, in other words: change filler)
-- | Not the other way round!
-- | Removes filled from gevuldeRollen of filler (because filler no longer fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerNoLongerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
fillerNoLongerPointsTo fillerId filledId = (lift2 $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError "fillerNoLongerPointsTo"
    \(filler :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError "fillerNoLongerPointsTo"
      \(filled :: PerspectRol) -> do
        filler'@(PerspectRol{gevuldeRollen}) <- pure $ (removeRol_gevuldeRollen filler (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <fillerId> `fillerPointsTo` <filledId>
-- | Add the link from filler to filled (FILLS link)
-- | (Add to= the gevuldeRollen, in other words: change filler)
-- | Not the other way round!
-- | Adds filled from gevuldeRollen of filler (because filler now fills filled).
-- | This function takes care of
-- | PERSISTENCE
fillerPointsTo :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
fillerPointsTo fillerId filledId = (lift2 $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError "fillerNoLongerPointsTo"
    \(filler :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError "fillerNoLongerPointsTo"
      \(filled :: PerspectRol) -> do
        filler'@(PerspectRol{gevuldeRollen}) <- pure $ (addRol_gevuldeRollen filler (rol_pspType filled) filledId)
        cacheAndSave fillerId filler'

-- | <filledId> `filledNoLongerPointsTo` <fillerId>
-- | Break the link from Filled to Filler (FILLEDBY link).
-- | Not the other way round!
-- | (Remove the binding, in other words: change filled)
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
