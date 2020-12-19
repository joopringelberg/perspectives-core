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
  )

  where

import Control.Monad.AvarMonadAsk (modify) as AA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.State (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (deleteAt, findIndex, index, modifyAt, nub, cons)
import Data.Array.NonEmpty (elemIndex, fromArray, head, singleton, filter)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Generic (encodeJSON)
import Foreign.Object (isEmpty, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (getAuthor, getSubject, removeRoleInstancesFromContext, setMe, cacheAndSave)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (addRoleObservingContexts, aisInRoleDelta, lift2, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeRol_binding, changeRol_isMe, context_buitenRol, context_iedereRolInContext, context_me, removeRol_binding, removeRol_gevuldeRollen, rol_binding, rol_context, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater, (##=), (##>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests, findBindingRequests, findRoleRequests)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.Error.Boundaries (handlePerspectRolError, handlePerspectRolError')
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.Identifiers (deconstructBuitenRol, deconstructModelName, isExternalRole)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (allRoleBinders, getEnumeratedRoleInstances, getProperty, isMe)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol, saveEntiteit)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, eq, join, pure, unit, void, ($), (<>), (>>=), (&&), (==), not)

-- | This function takes care of
-- | PERSISTENCE
-- | QUERY UPDATES
-- | RULE TRIGGERING
-- | but nothing else.
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt :: PerspectContext) <- lift2 $ saveEntiteit id
  subject <- getSubject
  forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances' ->
    case fromArray instances' of
      Nothing -> pure unit
      -- RULE TRIGGERING
      Just instances -> addRoleObservingContexts id (EnumeratedRoleType roleName) (head instances)
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    (PerspectRol{_id, binding, pspType, gevuldeRollen}) <- lift2 $ saveEntiteit rol
    case binding of
      Nothing -> pure unit
      Just b -> (lift2 $ findBinderRequests b pspType) >>= addCorrelationIdentifiersToTransactie
    forWithIndex_ gevuldeRollen \_ instances -> for_ instances \binder ->
      (lift2 $ findBindingRequests binder) >>= addCorrelationIdentifiersToTransactie
    -- For rule triggering, not for the delta or SYNCRONISATION:
    if isJust binding
      then void $ aisInRoleDelta $ RoleBindingDelta
        { id: rol
        , binding: binding
        , oldBinding: Nothing
        , deltaType: SetBinding
        , roleWillBeRemoved: false
        , subject
        }
      else pure unit
  (_ :: PerspectRol) <- lift2 $ saveEntiteit (context_buitenRol ctxt)
  -- For roles with a binding equal to R: detect the binder <RoleType> requests for R
  -- For roles that are bound by role R: detect the binding requests for R.
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- Only called when the external role is also 'bound' in a DBQ role.
removeContextIfUnbound :: RoleInstance -> Maybe RoleType ->  MonadPerspectivesTransaction Unit
removeContextIfUnbound roleInstance@(RoleInstance rid) ctype = do
  mbinder <- lift (lift (roleInstance ##> allRoleBinders))
  case mbinder of
    Nothing -> removeContextInstance (ContextInstance $ deconstructBuitenRol rid) ctype
    otherwise -> pure unit

-- | Removes the ContextInstance both from the cache and from the database.
-- | This function is complete w.r.t. the five responsibilities (ignoring CURRENTUSER).
removeContextInstance :: ContextInstance -> Maybe RoleType -> MonadPerspectivesTransaction Unit
removeContextInstance id authorizedRole = do
  (ctxt@(PerspectContext{pspType})) <- lift $ lift $ getPerspectContext id
  externalRoleType <- pure (EnumeratedRoleType ((unwrap pspType) <> "$External"))
  -- RULE TRIGGERING and QUERY UPDATES.
  (Tuple _ users) <- runWriterT $ do
    forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances' -> remove (EnumeratedRoleType roleName) instances'
    remove externalRoleType [(context_buitenRol ctxt)]
  -- PERSISTENCE
  _ <- scheduleContextRemoval id
  -- SYNCHRONISATION
  subject <- getSubject
  me <- getAuthor
  -- (roleType ###>> hasAspect (EnumeratedRoleType "sys:RootContext$External"))
  addDelta $ DeltaInTransaction
    { users
    , delta: SignedDelta
        { author: me
        , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
          { id
          , roleType: externalRoleType
          , authorizedRole
          , roleInstances: SNEA.singleton (context_buitenRol ctxt)
          , deltaType: if isJust authorizedRole then RemoveUnboundExternalRoleInstance else RemoveExternalRoleInstance
          , subject
          }}}

  -- now remove id from the affectedContexts in the Transaction.
  removeAffectedContext id

  where
    -- No need to take care of SYNCHRONISATION for individual role instances.
    -- The receiving PDR will recompute the role instances to remove.
    -- Takes care of RULE TRIGGERING and QUERY UPDATES.
    -- TODO. Maak hier een set van om dubbele te vermijden.
    remove :: EnumeratedRoleType -> Array RoleInstance -> WriterT (Array RoleInstance) MonadPerspectivesTransaction Unit
    remove roleType instances' =
      case fromArray instances' of
        Nothing -> pure unit
        Just instances -> do
          -- Also adds contexts that 'have a vantage point' on the removed roleInstances
          -- in the context, for RULE TRIGGERING (adds them as AffectedContexts).
          users <- lift $ usersWithPerspectiveOnRoleInstance id roleType (head instances)
          tell users
          -- Add correlation identifiers for outstanding Api requests, in order to update
          -- their query results, for QUERY UPDATES.
          lift ((lift2 $ findRoleRequests id roleType) >>= addCorrelationIdentifiersToTransactie)
          -- PERSISTENCE
          for_ instances removeRoleInstance_

    removeAffectedContext :: ContextInstance -> MonadPerspectivesTransaction Unit
    removeAffectedContext cinst = lift $ AA.modify \(Transaction r@{affectedContexts}) -> Transaction (r {affectedContexts = let
      i = findIndex
        (\(AffectedContext{contextInstances}) -> isJust $ elemIndex cinst contextInstances)
        affectedContexts
      in
        case i of
          Nothing -> affectedContexts
          Just n -> let
            AffectedContext {contextInstances, userTypes} = unsafePartial $ fromJust $ index affectedContexts n
            mcontextInstances' = fromArray $ filter (eq cinst) contextInstances
            in
              case mcontextInstances' of
                Nothing -> unsafePartial $ fromJust $ deleteAt n affectedContexts
                Just contextInstances' -> unsafePartial $ fromJust $ modifyAt n
                  (\_ -> AffectedContext{contextInstances: contextInstances', userTypes})
                  affectedContexts
        })

-- | Collects the union of the user role instances that occurr in the bindings.
-- | Takes care of PERSISTENCE for the role instance that is removed.
-- | Takes care of the five responsibilities wrt the binding and the  binders
-- | (the roles that bind the instance that is removed).
removeRoleInstance_ :: RoleInstance -> WriterT (Array RoleInstance) MonadPerspectivesTransaction Unit
removeRoleInstance_ roleId = (lift $ lift2 $ try (getPerspectRol roleId)) >>=
  handlePerspectRolError "removeRoleInstance_"
    \originalRole@(PerspectRol{gevuldeRollen, binding}) -> do
    -- Remove the role instance from all roles that have it as their binding. This will push Deltas.
    forWithIndex_ gevuldeRollen \_ filledRollen ->
      for_ filledRollen \filledRolId -> do
        (lift $ removeBinding true filledRolId) >>= tell
    -- Remove the inverse binding administration: bnd is no longer filled by roleId.
    case binding of
      Just bnd -> (lift $ (bnd `removedAsFilledFrom` roleId))
      otherwise -> pure unit
    -- Remove from couchdb, remove from the cache.
    void $ lift $ (scheduleRoleRemoval roleId)

-- | Calls removeBinding for the role instance prior to removing it.
-- | Calls removeBinding on all role instances that have this role as their binding.
-- | Calls removeRoleInstancesFromContext.
-- | This function is complete w.r.t. the five responsibilities.
-- | The opposite of this function creates a role instance first and then adds it to a context: [createAndAddRoleInstance](Perspectives.Instances.Builders.html#t:createAndAddRoleInstance).
removeRoleInstance :: RoleInstance -> MonadPerspectivesTransaction Unit
removeRoleInstance roleId@(RoleInstance id) = (lift2 $ try $ (getPerspectRol roleId)) >>= handlePerspectRolError "removeRoleInstance"
  \(PerspectRol{pspType, context}) -> do
    removeRoleInstancesFromContext context pspType (singleton roleId)
    void $ runWriterT $ removeRoleInstance_ roleId

-- | Remove all instances of EnumeratedRoleType from the context instance.
-- | Removes all instances from cache, from the database and adds then to deletedRoles in the Transaction.
-- | Removes the role instances from their context.
-- | ContextDelta's are not necessary (see removeRoleInstance).
removeAllRoleInstances :: EnumeratedRoleType -> Updater ContextInstance
removeAllRoleInstances et cid = do
  instances <- lift2 (cid ##= getEnumeratedRoleInstances et)
  case fromArray instances of
    Nothing -> pure unit
    Just instances' -> do
      removeRoleInstancesFromContext cid et instances'
      void $ runWriterT $ for_ instances removeRoleInstance_

scheduleRoleRemoval :: RoleInstance -> MonadPerspectivesTransaction Unit
scheduleRoleRemoval id = lift $ AA.modify (over Transaction \t@{rolesToBeRemoved} -> t {rolesToBeRemoved = cons id rolesToBeRemoved})

scheduleContextRemoval :: ContextInstance -> MonadPerspectivesTransaction Unit
scheduleContextRemoval id = lift $ AA.modify (over Transaction \t@{contextsToBeRemoved} -> t {contextsToBeRemoved = cons id contextsToBeRemoved})

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
        users <- aisInRoleDelta delta
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

-- | Removes the binding R of the rol, if any.
-- | If the binding is an external role, checks if it should be removed.
-- | Removes the rol as value of 'gevuldeRollen' for psp:Rol$binding from the binding R.
-- | Modifies the Role instance.
-- | If the the role instance has in fact no binding before the operation, this is a no-op without effect.
-- | Parameter `bindingRemoved` is true iff the role that is the binding of the role will be removed.
-- | PERSISTENCE of binding role and old binding.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for the old binding.
-- | QUERY UPDATES for `binding <roleId>` and `binder <TypeOfRoleId>`.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | CURRENTUSER for roleId and its context.
removeBinding :: Boolean -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding bindingRemoved roleId = (lift2 $ try $ getPerspectEntiteit roleId) >>=
  handlePerspectRolError' "removeBinding" []
    \(originalRole :: PerspectRol) -> case rol_binding originalRole of
      Nothing -> pure []
      Just bindingId -> do
        (lift2 $ findBinderRequests bindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
        subject <- getSubject
        delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
                      { id : roleId
                      , binding: (rol_binding originalRole)
                      , oldBinding: (rol_binding originalRole)
                      , deltaType: RemoveBinding
                      , roleWillBeRemoved: bindingRemoved
                      , subject
                      }
        users <- aisInRoleDelta delta
        author <- getAuthor
        signedDelta <- pure $ SignedDelta
          { author
          , encryptedDelta: sign $ encodeJSON $ delta}
        addDelta (DeltaInTransaction { users, delta: signedDelta})

        if not bindingRemoved
          then do
            (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie
            -- Remove the inverse binding administration: roleId is no longer filled by bindingId.
            roleId `removedAsFilledFrom` bindingId
          -- the role that has the binding will be removed anyway.
          else pure unit

        cacheAndSave roleId (over PerspectRol (\rl -> rl {bindingDelta = Nothing})
          (changeRol_isMe (removeRol_binding originalRole) false))

        ctxt <- lift2 $ getPerspectContext $ rol_context originalRole
        if (context_me ctxt == (Just roleId))
          then setMe (rol_context originalRole) Nothing
          else pure unit

        pure users

-- | <filledId> `removedAsFilledFrom` <fillerId>
-- | Removes filled from gevuldeRollen of filler (because filled no longer has filler as binding).
-- | If gevuldeRollen becomes empty, and filler is an external role, removes the context of filler (cascade delete).
removedAsFilledFrom :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
removedAsFilledFrom fillerId filledId = (lift2 $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError "removeAsFilledFrom"
    \(filler :: PerspectRol) -> (lift2 $ try $ getPerspectEntiteit filledId) >>=
      handlePerspectRolError "removeAsFilledFrom"
      \(filled :: PerspectRol) -> do
        -- If, after removing the binder, no binders (filled roles) are left on the filler  AND it is an external role,
        -- remove the context! This is because we then have a context whose external role fills no other role (is not
        -- bound anywhere).
        filler'@(PerspectRol{gevuldeRollen}) <- pure $ (removeRol_gevuldeRollen filler (rol_pspType filled) filledId)
        -- if (isEmpty gevuldeRollen && isExternalRole (unwrap fillerId))
        --   then removeContextInstance (ContextInstance $ deconstructBuitenRol (unwrap fillerId)) false
        --   else cacheAndSave fillerId filler'
        cacheAndSave fillerId filler'
