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

-- | The functions in this module modify contexts and roles. All these functions
-- |  * cache the results
-- |  * save the results
-- |  * add Delta's to the current Transaction
-- | They fall in three categories:
-- |  * modification of a Context, by changing its roles.
-- |  * modification of a Role by changing its binding
-- |  * modification of a Role by changing its property values
-- | The two binding-changing functions recompute the special `isMe` property (a Boolean value indicating whether the role represents the user).
-- | The context-changing functions recompute the special `me` role (it is the role instance that represents the user).
-- | IMPORTANT: the functions that change the context never save nor cache the role instances that are involved in the
-- | change.

module Perspectives.Assignment.Update where
 
import Prelude

import Control.Monad.AvarMonadAsk (get, gets, modify)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.State (StateT, evalStateT, put, get) as ST
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, cons, difference, elemIndex, filter, filterA, find, foldM, foldMap, nub, null, snoc)
import Data.Array (head) as ARR
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.MediaType (MediaType(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, over, unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Foreign.Object (Object, empty, filterKeys, fromFoldable, insert, lookup)
import Foreign.Object (union) as OBJ
import Partial.Unsafe (unsafePartial)
import Persistence.Attachment (class Attachment)
import Perspectives.Authenticate (signDelta)
import Perspectives.CollectAffectedContexts (aisInPropertyDelta, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (addRol_property, changeContext_me, changeContext_preferredUserRoleType, context_pspType, context_rolInContext, deleteRol_property, isDefaultContextDelta, modifyContext_rolInContext, popContext_state, popRol_state, pushContext_state, pushRol_state, removeRol_property, rol_context, rol_isMe, rol_pspType, setRol_property)
import Perspectives.CoreTypes (class Persistent, InformedAssumption(..), MonadPerspectives, Updater, MonadPerspectivesTransaction, (###=), (##=), (##>), (##>>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (findContextStateRequests, findMeRequests, findPropertyRequests, findRoleRequests, findRoleStateRequests)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Identifiers (startsWithSegments, typeUri2LocalName_, typeUri2couchdbFilename)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding_, contextType, getProperty, roleType, roleType_)
import Perspectives.Instances.Values (parsePerspectivesFile, writePerspectivesFile)
import Perspectives.Persistence.API (toFile)
import Perspectives.Persistent (addAttachment, getPerspectContext, getPerspectRol, saveEntiteit_)
import Perspectives.Persistent (saveEntiteit) as Instances
import Perspectives.Query.UnsafeCompiler (getPropertyFromTelescope)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (EnumeratedPropertyType, EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType, StateIdentifier(..))
import Perspectives.ResourceIdentifiers (databaseLocation, resourceIdentifier2DocLocator)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (getRoleAspectSpecialisations, hasPerspectiveOnRole, isUnlinked_, propertyAliases)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Simple.JSON (class WriteForeign, writeJSON)

-----------------------------------------------------------
-- UPDATE A CONTEXT (SET THE PREFERRED USER ROLE TYPE)
-----------------------------------------------------------
-- | Modifies the context instance by setting the value of preferredUserRoleType (a Maybe value!).
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION (none, because this is a private value (it is indexed for every participant in the context)).
-- | RULE TRIGGERING (STATE EVALUATION) (none, because there is no construct in the perspectives language for this member).
-- | QUERY UPDATES
-- | CURRENTUSER (none, because it cannot change by this operator).
setPreferredUserRoleType :: ContextInstance -> (Updater (Array RoleType))
setPreferredUserRoleType contextId userRoleTypes = (lift $ try $ getPerspectContext contextId) >>=
  handlePerspectContextError "setPreferredUserRoleType"
    \(pe :: PerspectContext) -> do
      case ARR.head userRoleTypes of
        -- PERSISTENCE
        Nothing -> lift $ cacheAndSave contextId $ changeContext_preferredUserRoleType pe Nothing
        Just ut -> lift $ cacheAndSave contextId $ changeContext_preferredUserRoleType pe (Just ut)
      -- QUERY UPDATES.
      (lift $ findMeRequests contextId) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- UPDATE A CONTEXT (ADDING OR REMOVING ROLE INSTANCES)
-----------------------------------------------------------
type RoleUpdater = ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))

-- | Modifies the context instance by adding the given role instance.
-- | If the rolInstance is part of the context before the operation, this is a no-op without any effects.
-- | PERSISTENCE of the context instance and of the role instance (a ContextDelta is added and saved).
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and the roleInstance.
-- | To handle an incoming ContextDelta, we include it optionally in the last argument.
addRoleInstanceToContext :: ContextInstance -> EnumeratedRoleType -> (Updater (Tuple RoleInstance (Maybe SignedDelta)))
addRoleInstanceToContext contextId rolName (Tuple roleId receivedDelta) = do
  (lift $ try $ getPerspectContext contextId) >>=
    handlePerspectContextError "addRoleInstancesToContext1"
      \(pe :: PerspectContext) -> do
        unlinked <- lift $ isUnlinked_ rolName
        (lift $ try $ getPerspectRol roleId) >>= handlePerspectRolError "addRoleInstancesToContext2"
          \(role@(PerspectRol{contextDelta}) :: PerspectRol) ->
            -- Do not add a roleinstance a second time.
            if unlinked

              then if isDefaultContextDelta contextDelta
                then f role pe unlinked 
                -- Apparently we've constructed a real ContextDelta before, so do not add a second time.
                else void $ lift $ saveEntiteit_ roleId role

              else (lift $ context_rolInContext pe rolName) >>= \(Tuple _ roles) -> 
                if isJust $ elemIndex roleId roles
                  then void $ lift $ saveEntiteit_ roleId role
                  else f role pe unlinked

  where
    f :: PerspectRol -> PerspectContext -> Boolean -> MonadPerspectivesTransaction Unit
    f (PerspectRol r@{id, universeRoleDelta, isMe}) pe unlinked = do
      changedContext <- if not unlinked
        -- Add the new instance only when the role type is enumerated in the context; hence not for unlinked role types.
        then lift (modifyContext_rolInContext pe rolName (flip snoc roleId))
        else pure pe
      -- CONTEXT PERSISTENCE
      -- Is the new role instance filled by me?
      if isMe
        then do
          (lift $ findMeRequests contextId) >>= addCorrelationIdentifiersToTransactie
          -- CURRENTUSER
          lift $ cacheAndSave contextId (changeContext_me changedContext (Just roleId))
        else lift $ cacheAndSave contextId changedContext

      subject <- getSubject
      delta <- case receivedDelta of
        Just d -> pure d
        _ -> signDelta 
              (writeJSON $ stripResourceSchemes $ ContextDelta
                { contextInstance : contextId
                , contextType: context_pspType pe
                , roleType: rolName
                , deltaType: AddRoleInstancesToContext
                , roleInstance: id
                , destinationContext: Nothing
                , destinationContextType: Nothing
                , subject
                })
      -- Add the delta to the role before computing the users, as that might involve serializing deltas and adding them
      -- to the transaction. We cannot have the default contextDelta, then.
      lift $ cacheAndSave id $ PerspectRol r { contextDelta = delta }

      -- Guarantees RULE TRIGGERING because contexts with a vantage point are added to
      -- the transaction, too.
      -- Also performs SYNCHRONISATION for paths that lie beyond the roleInstance provided to usersWithPerspectiveOnRoleInstance.
      -- Notice that even though we compute the users for a single given RoleInstance, we can use that result
      -- for any other instance of the same RoleType. This will no longer hold when we add filtering to the inverted queries
      -- (because then the affected contexts found will depend on the properties of the RoleInstance, too).
      users <- usersWithPerspectiveOnRoleInstance rolName roleId contextId true
      -- SYNCHRONISATION
      addDelta (DeltaInTransaction { users, delta: universeRoleDelta})
      addDelta $ DeltaInTransaction {users, delta}
      -- QUERY UPDATES
      (lift $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie

-- OBSOLETE!!
-- | Modifies the context instance by detaching the given role instances.
-- | Notice that this function does neither uncache nor unsave the rolInstances
-- | themselves. Instead, use removeRoleInstance.
-- | Does not touch the binding of any of the role instances.
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | STATE EVALUATION
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
removeRoleInstancesFromContext :: ContextInstance -> EnumeratedRoleType -> (Updater (NonEmptyArray RoleInstance))
removeRoleInstancesFromContext contextId rolName rolInstances = (lift $ try $ getPerspectContext contextId) >>=
    handlePerspectContextError "removeRoleInstancesFromContext"
      \(pe :: PerspectContext) -> do
        -- Guarantees STATE EVALUATION because contexts with a vantage point are added to
        -- the transaction, too.
        -- As a side effect, usersWithPerspectiveOnRoleInstance adds Deltas to the transaction for the continuation of the
        -- path beyond the given role instance.
        -- The last boolean argument prevents usersWithPerspectiveOnRoleInstance from doing this.
        users <- usersWithPerspectiveOnRoleInstance rolName (head rolInstances) contextId false
        subject <- getSubject
      -- SYNCHRONISATION
        delta <- signDelta 
          (writeJSON $ stripResourceSchemes $ UniverseRoleDelta
            { id: contextId
            , contextType: context_pspType pe
            , roleInstances: (SerializableNonEmptyArray rolInstances)
            , roleType: rolName
            , authorizedRole: Nothing
            , deltaType: RemoveRoleInstance
            , subject })
        addDelta $ DeltaInTransaction { users, delta}
        -- QUERY UPDATES.
        (lift $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie
        -- Modify the context: remove the role instances from those recorded with the role type.
        (roles :: Array PerspectRol) <- foldM
          (\roles roleId -> (lift $ try $ getPerspectRol roleId) >>= (handlePerspectRolError' "removeRoleInstancesFromContext" roles (pure <<< (flip cons roles))))
          []
          (toArray rolInstances)
        unlinked <- lift $ isUnlinked_ rolName
        changedContext <- if unlinked
          then pure pe
          else lift (modifyContext_rolInContext pe rolName (flip difference (toArray rolInstances)))
        -- PERSISTENCE.
        case find rol_isMe roles of
          Nothing -> lift $ cacheAndSave contextId changedContext
          Just _ -> do
            (lift $ findMeRequests contextId) >>= addCorrelationIdentifiersToTransactie
            -- CURRENTUSER.
            lift $ cacheAndSave contextId (changeContext_me changedContext Nothing)

-- | Detach the role instances from their current context and attach them to the new context.
-- | This is not just a convenience function. The combination of removeRoleInstancesFromContext and addRoleInstanceToContext would add UniverseRoleDeltas, which we don't need here.
-- | If all rolInstances are part of the destination context before the operation, this is a no-op without any effects.
-- | PERSISTENCE of both context instances.
-- | SYNCHRONISATION by two ContextDeltas (no UniverseRoleDeltas needed!).
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
-- TODO. De enige manier om deze functie aan te passen lijkt een nieuwe ContextDelta te maken en mee te nemen in de ContextDelta met MoveRoleInstancesToAnotherContext. Aan de ontvangende kant moet die nieuwe ContextDelta dan in de verplaatste rol worden gezet op de plek van 'contextDelta'.
moveRoleInstanceToAnotherContext :: ContextInstance -> ContextInstance -> EnumeratedRoleType -> Maybe SignedDelta -> (Updater RoleInstance)
moveRoleInstanceToAnotherContext _ _ _ _ _ = pure unit
-- moveRoleInstanceToAnotherContext originContextId destinationContextId rolName rolInstance = pure unit
-- moveRoleInstanceToAnotherContext originContextId destinationContextId rolName rolInstances = do
--   roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
--   me <- pure $ rol_id <$> find rol_isMe roles
--   -- me <- pure $ Just $ RoleInstance ""
--   origin <- lift $ lift $ getPerspectContext originContextId
--   destination <- lift $ lift $ getPerspectContext destinationContextId
--   subject <- subjectForRoleInstance (head rolInstances)
--   when (not null (toArray rolInstances `difference` context_rolInContext destination rolName))
--     do
--       case me of
--         Nothing -> do
--           (lift $ modifyContext_rolInContext destination rolName (append (toArray rolInstances))) >>= cacheAndSave destinationContextId
--           (lift $ modifyContext_rolInContext origin rolName (flip difference (toArray rolInstances))) >>= cacheAndSave originContextId
--         Just m -> do
--           destination' <- lift (modifyContext_rolInContext destination rolName (append (toArray rolInstances)))
--           cacheAndSave destinationContextId (changeContext_me destination' me)
--           (lift $ findRoleRequests destinationContextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
--           origin' <- lift (modifyContext_rolInContext origin rolName (flip difference (toArray rolInstances)))
--           cacheAndSave originContextId (changeContext_me origin' Nothing)
--           (lift $ findRoleRequests originContextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
--       -- Guarantees RULE TRIGGERING because contexts with a vantage point are added to
--       -- the transaction, too.
--       users <- usersWithPerspectiveOnRoleInstance rolName (head rolInstances)
--       -- SYNCHRONISATION
--       addContextDelta $ ContextDelta
--             { id : originContextId
--             , roleType: rolName
--             , deltaType: MoveRoleInstancesToAnotherContext
--             , roleInstances: SerializableNonEmptyArray rolInstances
--             , users
--             , sequenceNumber: 0
--             , destinationContext: Just destinationContextId
--             , subject
--             }
--       -- QUERY UPDATES
--       (lift $ findRoleRequests destinationContextId rolName) >>= addCorrelationIdentifiersToTransactie
--       (lift $ findRoleRequests originContextId rolName) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- UPDATE A ROLE (ADD OR REMOVE PROPERTY VALUES)
-----------------------------------------------------------
type PropertyUpdater = Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))

-- | Modify the role instance with the new property values.
-- | When all values are already in the list of values of the property for the role instance, this is a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
addProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array (Tuple Value (Maybe SignedDelta))))
addProperty rids propertyName valuesAndDeltas = case ARR.head rids of
  Nothing -> pure unit
  Just _ -> do
    values <- pure $ fst <$> valuesAndDeltas
    subject <- getSubject
    for_ rids \roleInstanceOnpath -> do
      -- Look for the contextualised property first: if we find a replacement for the requested property on a role instance,
      -- that is the instance we will work with - and the (replacing) local property we will work with!
      mrid <- lift $ getPropertyBearingRoleInstance propertyName roleInstanceOnpath
      case mrid of
        Nothing -> pure unit
        Just (RoleProp propertyBearingInstance replacementProperty) -> (lift $ try $ getPerspectRol propertyBearingInstance) >>= handlePerspectRolError "addProperty"
          \(propertyBearingInstanceRole :: PerspectRol) -> do
            -- TODO: Only when the property is not Local!
            deltas <- for valuesAndDeltas \(Tuple value msignedDelta) -> do
                delta <- case msignedDelta of
                  Nothing -> do
                    -- Create a delta for each value. The delta is in terms of the 
                    -- replacement property (if any), because that describes the structural change.
                    delta <- pure $ RolePropertyDelta
                      { id : propertyBearingInstance
                      , roleType: rol_pspType propertyBearingInstanceRole
                      , property: replacementProperty
                      , deltaType: AddProperty
                      , values: [value]
                      , subject
                      }
                    signDelta (writeJSON $ stripResourceSchemes delta)
                  Just signedDelta -> pure signedDelta
                pure (Tuple (unwrap value) delta)
            -- Save the property values in the role instance. Do this now because it will affect computing the users.
            -- Apply all changes to the role and then save it:
            --  - change the property values in one go
            --  - add all propertyDeltas.
            lift $ cacheAndSave 
              propertyBearingInstance 
              (over PerspectRol 
                (\r@{propertyDeltas} -> 
                  r {propertyDeltas = setDeltasForProperty replacementProperty (OBJ.union (fromFoldable deltas)) propertyDeltas}) 
                (addRol_property propertyBearingInstanceRole replacementProperty values))
            -- Compute the users for this role (the value has no effect). As a side effect, contexts are added to the transaction.
            -- Even when the property is Private or Local, we should do this, as the computation also triggers actions.
            users <- aisInPropertyDelta 
              roleInstanceOnpath 
              propertyBearingInstance 
              propertyName 
              replacementProperty 
              (rol_pspType propertyBearingInstanceRole)
            void $ for deltas \(Tuple _ delta) ->
                -- TODO: For a Private property, remove users that are not another installation of the own user.
                -- NOTICE: probably self-synchronisation (keeping installations of the same user up to date) currently doesn't work.
                addDelta (DeltaInTransaction { users, delta: delta })
            -- Look for requests for the original property AND the replacement property (if any).
            (lift $ findPropertyRequests propertyBearingInstance propertyName) >>= addCorrelationIdentifiersToTransactie
            (lift $ findPropertyRequests propertyBearingInstance replacementProperty) >>= addCorrelationIdentifiersToTransactie

-- | Get the property bearing role individual in the chain.
-- | If the property is defined on role instance's type (either directly or by Aspect), return it; otherwise
-- | recursively apply this function to its binding.
-- | Takes (aspect) property replacements into consideration.
getPropertyBearingRoleInstance :: EnumeratedPropertyType -> RoleInstance -> MonadPerspectives (Maybe RoleProp)
getPropertyBearingRoleInstance prop roleId = do
  tp <- roleId ##>> roleType
  -- NOTE. UP TILL version v0.20.0 we have a very specific problem with the external roles of the specialisations
  -- of model:System$Model. These roles are fetched before the models themselves are fetched; indeed, we don't mean to 
  -- get the models until the end user explicitly asks for it.
  -- This situation will go away in the next version.
  aliases <- catchError (propertyAliases tp)
    \e -> pure empty
  case lookup (unwrap prop) aliases of
    Just destination -> pure $ Just $ RoleProp roleId destination 
    Nothing -> do 
      allProps <- allLocallyRepresentedProperties (ST tp)
      if isJust $ elemIndex (ENP prop) allProps
        then pure $ Just $ RoleProp roleId prop
        else binding_ roleId >>= maybe (pure Nothing) (getPropertyBearingRoleInstance prop)
  
data RoleProp = RoleProp RoleInstance EnumeratedPropertyType
roleProp_roleinstance :: RoleProp -> RoleInstance
roleProp_roleinstance (RoleProp ri _) = ri
roleProp_property :: RoleProp -> EnumeratedPropertyType
roleProp_property (RoleProp _ pt) = pt

setDeltasForProperty :: EnumeratedPropertyType -> (Object SignedDelta -> Object SignedDelta) -> (Object (Object SignedDelta)) -> (Object (Object SignedDelta))
setDeltasForProperty propertyName modifier allDeltas = case lookup (unwrap propertyName) allDeltas of
  Nothing -> insert (unwrap propertyName) (modifier empty) allDeltas
  Just oldDeltas -> insert (unwrap propertyName) (modifier oldDeltas) allDeltas

-- | Remove the values from the property's values for the role instance.
-- | When none of the values are in the list of values of the property for the role instance, this is a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
removeProperty :: Array RoleInstance -> EnumeratedPropertyType -> Maybe SignedDelta -> (Updater (Array Value))
removeProperty rids propertyName mdelta values = case ARR.head rids of
  Nothing -> pure unit
  Just _ -> do
    subject <- getSubject
    for_ rids \rid' -> do
      mrid <- lift $ getPropertyBearingRoleInstance propertyName rid'
      case mrid of
        Nothing -> pure unit
        Just (RoleProp rid replacementProperty) -> (lift $ try $ getPerspectRol rid) >>=
          handlePerspectRolError "removeProperty"
          \(pe :: PerspectRol) -> do
            -- Compute the peers first, because otherwise the query interpreter won't find the property values and thus won't find the peers.
            users <- aisInPropertyDelta rid' rid propertyName replacementProperty (rol_pspType pe)
            -- Create a delta for all values at once.
            signedDelta <- case mdelta of 
              Just d -> pure d
              Nothing -> signDelta (writeJSON $ stripResourceSchemes (RolePropertyDelta
                { id : rid
                , roleType: rol_pspType pe
                , property: replacementProperty
                , deltaType: RemoveProperty
                , values: values
                , subject
                }))
            addDelta (DeltaInTransaction { users, delta: signedDelta})
            (lift $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie
            (lift $ findPropertyRequests rid replacementProperty) >>= addCorrelationIdentifiersToTransactie
            -- Apply all changes to the role and then save it:
            --  - change the property values in one go
            --  - remove all propertyDeltas.
            -- filterKeys: Filter out those key/value pairs of a map for which a predicate on the key fails to hold. So: if predicate holds, the key-value pair is added to the result. 
            -- This translates here to: if the key is one of the values to remove, the predicate should fail!
            lift $ cacheAndSave rid (over PerspectRol (\r@{propertyDeltas} -> r {propertyDeltas = setDeltasForProperty replacementProperty (filterKeys (\key -> not $ isJust $ elemIndex (Value key) values)) propertyDeltas}) (removeRol_property pe replacementProperty values))

-- | Delete all property values from the role for the EnumeratedPropertyType.
-- | If there are no values for the property on the role instance, this is a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by PropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
deleteProperty :: Array RoleInstance -> EnumeratedPropertyType -> Maybe SignedDelta -> MonadPerspectivesTransaction Unit
deleteProperty rids propertyName mdelta = case ARR.head rids of
  Nothing -> pure unit
  Just _ -> do
    subject <- getSubject
    for_ rids \rid' -> do
      mrid <- lift $ getPropertyBearingRoleInstance propertyName rid'
      case mrid of
        Nothing -> pure unit
        Just (RoleProp rid replacementProperty) -> (lift $ try $ getPerspectRol rid) >>=
            handlePerspectRolError
            "deleteProperty"
            \(pe@(PerspectRol{properties, pspType})) -> do
              -- Compute the peers first, because otherwise the query interpreter won't find the property values and thus won't find the peers.
              users <- aisInPropertyDelta rid' rid propertyName replacementProperty pspType
              -- Create a delta for all values.
              -- We must add the current values, otherwise a Transaction can only have a single 
              -- DeleteProperty delta.
              signedDelta <- case mdelta of 
                Just d -> pure d
                Nothing -> signDelta (writeJSON $ stripResourceSchemes (RolePropertyDelta
                  { id : rid
                  , roleType: pspType
                  , property: replacementProperty
                  , deltaType: DeleteProperty
                  , values: maybe [] identity (lookup (unwrap replacementProperty) properties)
                  , subject
                  }))
              addDelta (DeltaInTransaction { users, delta: signedDelta})
              (lift $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie
              (lift $ findPropertyRequests rid replacementProperty) >>= addCorrelationIdentifiersToTransactie
              -- Apply all changes to the role and then save it:
              --  - change the property values in one go
              --  - remove all propertyDeltas for this property.
              lift $ cacheAndSave rid (over PerspectRol (\r@{propertyDeltas} -> r {propertyDeltas = setDeltasForProperty replacementProperty (const empty) propertyDeltas}) (deleteRol_property pe replacementProperty))

-- | Modify the role instance with the new property values.
-- | When all new values are in fact already in the set of values for the property of the role instance, this is
-- | a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
setProperty :: Array RoleInstance -> EnumeratedPropertyType -> Maybe SignedDelta -> (Updater (Array Value))
setProperty rids propertyName mdelta values = do
  rids' <- filterA hasDifferentValues rids
  setProperty' rids'
  where
    hasDifferentValues :: RoleInstance -> MonadPerspectivesTransaction Boolean
    hasDifferentValues rid = do
      rtype <- lift $ roleType_ rid
      vals <- lift (rid ##= getPropertyFromTelescope propertyName)
      pure $ (not $ null (difference values vals)) || (not $ null (difference vals values))
    setProperty' ::Array RoleInstance -> MonadPerspectivesTransaction Unit
    setProperty' rids' = case ARR.head rids' of
      Nothing -> pure unit
      Just _ -> do
        subject <- getSubject
        for_ rids' \rid' -> do
          mrid <- lift $ getPropertyBearingRoleInstance propertyName rid'
          case mrid of
            Nothing -> pure unit
            Just (RoleProp rid replacementProperty) -> (lift $ try $ getPerspectRol rid) >>=
                handlePerspectRolError
                "setProperty"
                \(pe@(PerspectRol{properties, pspType})) -> do
                  -- Create a delta for all values.
                  -- We must add the current values, otherwise a Transaction can only have a single 
                  -- DeleteProperty delta.
                  signedDelta <- case mdelta of 
                    Just d -> pure d
                    Nothing -> signDelta (writeJSON $ stripResourceSchemes (RolePropertyDelta
                      { id : rid
                      , roleType: pspType
                      , property: replacementProperty
                      , deltaType: SetProperty
                      , values
                      , subject
                      }))
                  -- Save the property values in the role instance. Do this now because it will affect computing the users.
                  lift $ void $ cacheAndSave rid (setRol_property pe replacementProperty values signedDelta)
                  users <- aisInPropertyDelta rid' rid propertyName replacementProperty pspType
                  addDelta (DeltaInTransaction { users, delta: signedDelta})
                  (lift $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie
                  (lift $ findPropertyRequests rid replacementProperty) >>= addCorrelationIdentifiersToTransactie


-----------------------------------------------------------
-- SAVEFILE
-----------------------------------------------------------
-- | From a Foreign value that represents an ArrayBuffer or String, create a File and save it with a role instance document.
-- | Updates the revision of the role instance.
saveFile :: RoleInstance -> EnumeratedPropertyType -> Foreign -> String -> MonadPerspectivesTransaction String
saveFile r property arrayBuf mimeType = do
  -- Look for the contextualised property first: if we find a replacement for the requested property on a role instance,
  -- that is the instance we will work with - and the (replacing) local property we will work with!
  mrid <- lift $ getPropertyBearingRoleInstance property r
  RoleProp rid replacementProperty <- case mrid of 
    -- Notice that we now assume the property is indeed represented on instances of this type.
    -- This may go wrong when we actually have no property value yet but it should NOT be represented on the given instance.
    Nothing -> pure $ RoleProp r property
    Just x -> pure x
  roleInstance :: PerspectRol <- lift $ getPerspectRol rid
  subject <- getSubject
  dbLoc <- lift $ databaseLocation $ unwrap rid
  {database, documentName} <- lift $ resourceIdentifier2DocLocator (unwrap rid)
  mval <- lift (rid ##> getProperty replacementProperty)
  usedVal <- case mval of 
    Nothing -> pure $ writePerspectivesFile
      { fileName: (typeUri2LocalName_ $ unwrap replacementProperty) -- As the property value is unavailable, we'll use the local prop name as client side name, too.
      , propertyType: replacementProperty
      , mimeType
      , database: dbLoc
      , roleFileName: documentName
      }
    Just val -> case parsePerspectivesFile $ unwrap val of
      Left e -> pure $ writePerspectivesFile
        { fileName: (typeUri2LocalName_ $ unwrap replacementProperty) -- As the property value is unavailable, we'll use the local prop name as client side name, too.
        , propertyType: replacementProperty
        , mimeType
        , database: dbLoc
        , roleFileName: documentName
        }
      Right rec ->  pure $ writePerspectivesFile $ rec {database = dbLoc, roleFileName = documentName}
  case parsePerspectivesFile usedVal of
    Left e -> throwError $ error ("Could not parse '" <> usedVal <> "' trying to save file:" <> show e)
    Right rec -> do
      theFile <- liftEffect $ toFile (typeUri2LocalName_ (unwrap replacementProperty)) rec.mimeType arrayBuf
      success <- lift $ addAttachment r (typeUri2couchdbFilename (unwrap replacementProperty)) theFile (MediaType rec.mimeType)
      if success
        then do
          -- Add an UploadFile delta.
          -- TODO. Deze delta zit NIET in de rol zelf!
          delta <- pure $ RolePropertyDelta
            { id : rid
            , roleType: rol_pspType roleInstance
            , property: replacementProperty
            , deltaType: UploadFile
            , values: [Value usedVal]
            , subject
            }
          signedDelta <- signDelta (writeJSON $ stripResourceSchemes delta)
          setProperty [rid] replacementProperty Nothing [Value usedVal]
          -- Compute the users for this role. As a side effect, contexts are added to the transaction.
          users <- aisInPropertyDelta r rid property replacementProperty (rol_pspType roleInstance)
          addDelta (DeltaInTransaction { users, delta: signedDelta })
          pure usedVal
        else throwError (error ("Could not save file in the database"))

 
-----------------------------------------------------------
-- CACHEANDSAVE
-----------------------------------------------------------
-- Save the entity in cache and in couchdb.
cacheAndSave :: forall a i. Attachment a => WriteForeign a => Persistent a i => i -> a -> MonadPerspectives Unit
cacheAndSave rid rol = void $ cacheAndSave_ rid rol

cacheAndSave_ :: forall a i. Attachment a => WriteForeign a => Persistent a i => i -> a -> MonadPerspectives a
cacheAndSave_ rid rol = do
  void $ cacheEntity rid rol
  Instances.saveEntiteit rid

-----------------------------------------------------------
-- SET ME
-----------------------------------------------------------
-- | Even though we modify the ContextInstance, we do not push a Delta.
-- | This is because the value of Me is indexed and never communicated with other users.
setMe :: ContextInstance -> Maybe RoleInstance -> MonadPerspectivesTransaction Unit
setMe cid me = do
  (lift $ try $ getPerspectContext cid) >>=
    handlePerspectContextError "setMe"
      \ctxt -> do
        lift $ cacheAndSave cid (changeContext_me ctxt me)
        (lift $ findMeRequests cid) >>= addCorrelationIdentifiersToTransactie

-- | Returns the role instance having the perspective that allows resource modification 
-- | (not necessarily and usually not an instance of sys:PerspectivesSystem$User)
getSubject :: MonadPerspectivesTransaction RoleType
getSubject = gets (_.authoringRole <<< unwrap)

-----------------------------------------------------------
-- SETACTIVECONTEXTSTATE
-----------------------------------------------------------
-- | Add the state identifier as the last state in the array of state identifiers in the context instance.
-- | The five responsibilities are adressed as follows:
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION is not applicable: state is not synchronised between participants but recomputed by each PDR.
-- | RULE TRIGGERING is not applicable. State change is a *consequence* of assignment (and therefore of triggered
-- |  rules) but not a direct cause. The rule triggering mechanism depends on building a list of affected contexts
-- |  during assignment. Changing state is not affecting contexts; it is a representation of such changes.
-- | QUERY UPDATES This is handled by adding correlation identifiers for requests that depend on the State Assumption
-- |  for the context instance to Transaction State.
-- | CURRENTUSER is not applicable.
setActiveContextState :: StateIdentifier -> ContextInstance -> MonadPerspectivesTransaction Unit
setActiveContextState stateId contextId = (lift $ try $ getPerspectContext contextId) >>=
    handlePerspectContextError "setActiveContextState"
      \(pe :: PerspectContext) -> do
        lift $ cacheAndSave contextId $ pushContext_state pe stateId
        (lift $ findContextStateRequests contextId) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- SETDEFAULTCONTEXTROOTSTATE
-----------------------------------------------------------
setDefaultContextRootState :: ContextInstance -> MonadPerspectivesTransaction Unit
setDefaultContextRootState cid = (lift (cid ##>> contextType)) >>= \ctype -> setActiveContextState (StateIdentifier (unwrap ctype)) cid

-----------------------------------------------------------
-- SETINACTIVECONTEXTSTATE
-----------------------------------------------------------
-- | Remove the state identifier from the array of state identifiers in the context instance.
-- | The five responsibilities are adressed as follows:
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION is not applicable: state is not synchronised between participants but recomputed by each PDR.
-- | RULE TRIGGERING is not applicable. State change is a *consequence* of assignment (and therefore of triggered
-- |  rules) but not a direct cause. The rule triggering mechanism depends on building a list of affected contexts
-- |  during assignment. Changing state is not affecting contexts; it is a representation of such changes.
-- | QUERY UPDATES This is handled by adding correlation identifiers for requests that depend on the State Assumption
-- |  for the context instance to Transaction State.
-- | CURRENTUSER is not applicable.
setInActiveContextState :: StateIdentifier -> ContextInstance -> MonadPerspectivesTransaction Unit
setInActiveContextState stateId contextId = (lift $ try $ getPerspectContext contextId) >>=
    handlePerspectContextError "setInActiveContextState"
      \(pe :: PerspectContext) -> do
        lift$ cacheAndSave contextId $ popContext_state pe stateId
        (lift $ findContextStateRequests contextId) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- SETACTIVEROLESTATE
-----------------------------------------------------------
-- | Add the state identifier as the last state in the array of state identifiers in the role instance.
-- | The five responsibilities are adressed as follows:
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION is not applicable: state is not synchronised between participants but recomputed by each PDR.
-- | RULE TRIGGERING is not applicable. State change is a *consequence* of assignment (and therefore of triggered
-- |  rules) but not a direct cause. The rule triggering mechanism depends on building a list of affected contexts
-- |  during assignment. Changing state is not affecting contexts; it is a representation of such changes.
-- | QUERY UPDATES This is handled by adding correlation identifiers for requests that depend on the State Assumption
-- |  for the role instance to Transaction State.
-- | CURRENTUSER is not applicable.
setActiveRoleState :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction Unit
setActiveRoleState stateId roleId = (lift $ try $ getPerspectRol roleId) >>=
    handlePerspectContextError "setActiveRoleState"
      \(pe :: PerspectRol) -> do
        lift $ cacheAndSave roleId $ pushRol_state pe stateId
        (lift $ findRoleStateRequests roleId) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- SETDEFAULTROLEROOTSTATE
-----------------------------------------------------------
setDefaultRoleRootState :: RoleInstance -> MonadPerspectivesTransaction Unit
setDefaultRoleRootState rid = (lift (rid ##>> roleType)) >>= \rtype -> setActiveRoleState (StateIdentifier (unwrap rtype)) rid

-----------------------------------------------------------
-- SETINACTIVEROLESTATE
-----------------------------------------------------------
-- | Remove the state identifier from the array of state identifiers in the context instance.
-- | The five responsibilities are adressed as follows:
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION is not applicable: state is not synchronised between participants but recomputed by each PDR.
-- | RULE TRIGGERING is not applicable. State change is a *consequence* of assignment (and therefore of triggered
-- |  rules) but not a direct cause. The rule triggering mechanism depends on building a list of affected contexts
-- |  during assignment. Changing state is not affecting contexts; it is a representation of such changes.
-- | QUERY UPDATES This is handled by adding correlation identifiers for requests that depend on the State Assumption
-- |  for the context instance to Transaction State.
-- | CURRENTUSER is not applicable.
setInActiveRoleState :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction Unit
setInActiveRoleState stateId roleId = (lift $ try $ getPerspectRol roleId) >>=
    handlePerspectContextError "setInActiveRoleState"
      \(pe :: PerspectRol) -> do
        lift $ cacheAndSave roleId $ popRol_state pe stateId
        (lift $ findRoleStateRequests roleId) >>= addCorrelationIdentifiersToTransactie

-- | Set the the authoringRole in the Transaction for the duration of the monadic action.
withAuthoringRole :: forall a. RoleType -> MonadPerspectivesTransaction a -> MonadPerspectivesTransaction a
withAuthoringRole authoringRole mp = do
  originalAuthor <- gets (_.authoringRole <<< unwrap)
  modify (over Transaction \t -> t {authoringRole = authoringRole})
  a <- mp
  modify (over Transaction \t -> t {authoringRole = originalAuthor})
  pure a


-----------------------------------------------------------
-- ROLECONTEXTUALISATIONS
-- Get the contextualised versions of a role type in a specific context,
-- under the additional condition that the authoring user (in the Transaction)
-- has a perspective on it.
-- If there are no specialisations, just the EnumeratedRoleType passed in will be returned.
-- If there are specialisations, it will be left out.
-----------------------------------------------------------
roleContextualisations :: ContextInstance -> EnumeratedRoleType -> MonadPerspectivesTransaction (Array EnumeratedRoleType)
roleContextualisations ctxt qualifiedRoleIdentifier = do 
  -- Get the context type.
  cType <- lift (ctxt ##>> contextType)
  -- If the qualifiedRoleIdentifier is defined in this context type, create an instance of just that role type.
  roleTypesToCreate <- if (unwrap qualifiedRoleIdentifier) `startsWithSegments` (unwrap cType)
    then pure [qualifiedRoleIdentifier]
    -- Else just get the specialisations of qualifiedRoleIdentifier.
    else lift (qualifiedRoleIdentifier ###= getRoleAspectSpecialisations) 
      >>= pure <<< filter \(EnumeratedRoleType r) -> r `startsWithSegments` (unwrap cType)
  -- Then, if we have no type, use the qualifiedRoleIdentifier anyway.
  roleTypesToCreate' <- if null roleTypesToCreate
    then pure [qualifiedRoleIdentifier]
    else pure $ nub roleTypesToCreate
  -- Get the user role type that is executing.
  (user :: RoleType) <- gets (_.authoringRole <<< unwrap)
  -- Filter the object role types, keeping only those that the user role type has a perspective on.
  lift $ concat <$> runArrayT (filterA (unsafePartial hasPerspectiveOnRole user) roleTypesToCreate')

-- | A state condition may be fully determined, or it may not yet be determinable due to 
-- | resource removal.
data ConditionResult = Undetermined | Determined Boolean

isUntouchable :: Array ContextInstance -> Array RoleInstance -> InformedAssumption -> MonadPerspectives Boolean
-- There should not be a role in rls that has type r and context c.
isUntouchable ctxts rls (RoleAssumption c r) =  ST.evalStateT ((for_ rls f) *> ST.get) false
  where
    f :: RoleInstance -> ST.StateT Boolean MonadPerspectives Unit
    f rl = do
      isUntouchable_ <- ST.get
      if not isUntouchable_
        then do 
          rle <- lift $ getPerspectRol rl
          if c == rol_context rle && r == rol_pspType rle
            then ST.put true
            else pure unit
        else pure unit

isUntouchable ctxts rls (Me c) = pure $ isJust $ elemIndex c ctxts
isUntouchable ctxts rls (Filler rl) = pure $ isJust $ elemIndex rl rls
isUntouchable ctxts rls (FilledRolesAssumption filler _ _) = pure $ isJust $ elemIndex filler rls
isUntouchable ctxts rls (Property rl _) = pure $ isJust $ elemIndex rl rls
isUntouchable ctxts rls (Context rl) = pure $ isJust $ elemIndex rl rls
isUntouchable ctxts rls (External ct) = pure $ isJust $ elemIndex ct ctxts
isUntouchable ctxts rls (State ct) = pure $ isJust $ elemIndex ct ctxts
isUntouchable ctxts rls (RoleState rl) = pure $ isJust $ elemIndex rl rls

isUndetermined :: Array InformedAssumption -> MonadPerspectivesTransaction Boolean
isUndetermined assumptions = do
  Transaction{untouchableContexts, untouchableRoles} <- get
  bools <- lift (for assumptions (isUntouchable untouchableContexts untouchableRoles))
  pure $ ala Disj foldMap bools
  