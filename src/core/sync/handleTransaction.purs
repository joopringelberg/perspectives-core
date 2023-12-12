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

module Perspectives.Sync.HandleTransaction where

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (lift, runExcept)
import Data.Array (catMaybes)
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Data.Ordering (Ordering(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.Update (addProperty, addRoleInstanceToContext, deleteProperty, moveRoleInstanceToAnotherContext, removeProperty)
import Perspectives.Authenticate (authenticate)
import Perspectives.Checking.Authorization (roleHasPerspectiveOnExternalRoleWithVerbs, roleHasPerspectiveOnPropertyWithVerb, roleHasPerspectiveOnRoleWithVerb)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, removeInternally, (###=), (###>>), (##=), (##>), (##>>))
import Perspectives.Couchdb (DeleteCouchdbDocument(..))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedContextToTransaction, addCreatedRoleToTransaction)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, typeUri2LocalName_, typeUri2ModelUri_)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getProperty, roleType)
import Perspectives.Instances.Values (parsePerspectivesFile)
import Perspectives.ModelDependencies (rootContext)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment, getAttachment, getDocument)
import Perspectives.Persistent (entityExists, getPerspectRol, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity, overwriteEntity, rev)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), ResourceType(..), RoleType(..), StateIdentifier(..), externalRoleType)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..)) as Verbs
import Perspectives.ResourceIdentifiers (addSchemeToResourceIdentifier, isInPublicScheme, resourceIdentifier2DocLocator, resourceIdentifier2WriteDocLocator, takeGuid)
import Perspectives.SaveUserData (removeBinding, removeContextIfUnbound, replaceBinding, scheduleContextRemoval, scheduleRoleRemoval, setFirstBinding)
import Perspectives.SerializableNonEmptyArray (toArray, toNonEmptyArray)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (hasAspect, isPublicRole, roleAspectsClosure)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), DeltaRecord, RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..), addPublicResourceScheme, addResourceSchemes)
import Prelude (class Eq, class Ord, Unit, bind, compare, discard, flip, pure, show, unit, void, ($), (*>), (+), (<$>), (<<<), (<>), (==), (>>=))
import Simple.JSON (readJSON')

-- TODO. Each of the executing functions must catch errors that arise from unknown types.
-- Inspect the model version of an unknown type and establish whether the resident model is newer or older than
-- the arriving type. If newer, check whether the type is backwards compatible with the arriving older type.
-- If older, retrieve the newer version of the model and check whether the arriving type is backwards compatible with
-- the type in the resident model.
-- If the types are truly incompatible, either ignore the incoming delta (best solution if the resident model is newer)
-- or update to the newer model version (have the end user consent).

executeContextDelta :: ContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeContextDelta (ContextDelta{deltaType, contextInstance, contextType, roleType, roleInstance, destinationContext, subject} ) signedDelta = do
  log (show deltaType <> " to/from " <> show contextInstance <> " and " <> show roleInstance)
  case deltaType of
    -- The subject must be allowed to change the role: they must have a perspective on it that includes:
    --  * the verb CreateAndFill, in case a context role is created;
    --  * the verb Create, in case another role is created.
    AddRoleInstancesToContext -> (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Create, Verbs.CreateAndFill]) >>= case _ of
      Left e -> handleError e
      Right _ -> addRoleInstanceToContext contextInstance roleType (Tuple roleInstance (Just signedDelta))
    -- NOTE: the perspective should always include the Delete verb.
    MoveRoleInstancesToAnotherContext -> (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Create, Verbs.CreateAndFill]) >>= case _ of
      Left e -> handleError e
      Right _ -> moveRoleInstanceToAnotherContext contextInstance (unsafePartial $ fromJust destinationContext) roleType roleInstance

executeRoleBindingDelta :: RoleBindingDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeRoleBindingDelta (RoleBindingDelta{filled, filler, deltaType, subject}) signedDelta = do
  log (show deltaType <> " of " <> show filled <> " (to) " <> show filler)
  roleType' <- lift (filled ##>> roleType)
  (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType' [Verbs.Fill, Verbs.CreateAndFill]) >>= case _ of
    Left e -> handleError e
    Right _ -> case deltaType of
      SetFirstBinding -> void $ setFirstBinding filled (unsafePartial $ fromJust filler) (Just signedDelta)
      RemoveBinding -> void $ removeBinding filled
      ReplaceBinding -> void $ replaceBinding filled (unsafePartial $ fromJust filler) (Just signedDelta)

-- TODO. Wat met SetPropertyValue?
executeRolePropertyDelta :: RolePropertyDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeRolePropertyDelta d@(RolePropertyDelta{id, roleType, deltaType, values, property, subject}) signedDelta = do
  log (show deltaType <> " for " <> show id <> " and property " <> show property)
  case deltaType of
    AddProperty -> do
      -- we need not check whether the model is known if we assume valid transactions:
      -- a role instance creation delta must have preceded the property delta.
      (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.AddPropertyValue) >>= case _ of
        Left e -> handleError e
        Right _ -> addProperty [id] property (flip Tuple (Just signedDelta) <$> values)
    RemoveProperty -> (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.RemovePropertyValue) >>= case _ of
      Left e -> handleError e
      Right _ -> removeProperty [id] property values
    DeleteProperty -> (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.DeleteProperty) >>= case _ of
      Left e -> handleError e
      Right _ -> deleteProperty [id] property
    UploadFile -> do
      -- Do this only when we're executing the Delta for a public role. 
      -- As this is a RolePropertyDelta, it was created when a file was added to the role for a property with a File range. 
      -- The only 'user' whose resource identifiers are all in the public scheme, is the virtual user for whom we make things public.
      -- This 'user' never changes a resource himself - he is not an active actor.
      -- Consequently, if we find, on executing this Delta, that its id is a public resource, we're acting for the public role.
      -- NOT for the 'real' user who uploaded the file.
      -- So, testing whether the id is in the public scheme is an adequate test to detect this case.
      if isInPublicScheme (unwrap id)
        then do
          -- Currently, the id is in the public scheme. Transform to the local scheme, so we can retrieve the attachment locally.
          storageSchemes <- lift $ gets _.typeToStorage
          {database, documentName} <- lift $ resourceIdentifier2DocLocator $ (addSchemeToResourceIdentifier storageSchemes (RType roleType)) (takeGuid (unwrap id))
          -- Retrieve the attachment from the local version of the role. 
          -- Use the last segment of the property type as the attachments name.
          matt <- lift $ getAttachment database documentName (typeUri2LocalName_ (unwrap property))
          case matt of 
            Nothing -> pure unit
            Just att -> do 
              -- Add the attachment to the public version of the role.
              {database:pubDatabase} <- lift $ resourceIdentifier2WriteDocLocator (unwrap id)
              prol <- lift $ getPerspectRol id
              mvalue <- lift (id ##> getProperty property)
              case mvalue of 
                Nothing -> pure unit
                Just val -> case parsePerspectivesFile (unwrap val) of
                  Left e -> pure unit
                  Right rec -> do 
                    DeleteCouchdbDocument{ok, rev} <- lift $ addAttachment pubDatabase documentName (rev prol) (typeUri2LocalName_ (unwrap property)) att (MediaType rec.mimeType)
                    case ok of 
                      Just true -> do
                        -- The revision on the cached version is no longer valid.
                        -- Neither does it have the new attachment info.
                        void $ lift $ removeInternally id
                        -- Retrieve again so it includes the new attachment info and revision.
                        -- But, because replication from the write database to the read database takes some time,
                        -- if we were to retrieve from the write database, we are likely to retrieve the previous version,
                        -- with neither attachments nor updated revision.
                        -- So we read from the write database.
                        {database:rdb, documentName:rdn} <- lift $ resourceIdentifier2WriteDocLocator (unwrap id)
                        roleInstance :: PerspectRol <- lift $ getDocument rdb rdn
                        -- save version in cache. If we use cacheEntity, the revision will not be overwritten!
                        lift $ void $ overwriteEntity id roleInstance
                      _ -> throwError (error ("Could not save file in the database"))

        else pure unit

-- TODO. Wat we nodig hebben, is een secundair kanaal naar de client waarin we
-- fouten en waarschuwingen kunnen sturen.
-- Totdat we dat hebben, zetten we een waarschuwing op de console.
handleError :: PerspectivesError -> MonadPerspectivesTransaction Unit
handleError e = liftEffect $ log (show e)

-- | Retrieves from the repository the model that holds the ContextType, if necessary.
-- | The ConstructExternalRole always precedes the UniverseContextDelta for the context it is the external
-- | role for. Hence we only have to check whether the external role exists.
executeUniverseContextDelta :: UniverseContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseContextDelta (UniverseContextDelta{id, contextType, deltaType, subject}) signedDelta = do
  log (show deltaType <> " with id " <> show id <> " and with type " <> show contextType)
  externalRoleExists <- lift $ entityExists (RoleInstance $ buitenRol $ unwrap id)
  if externalRoleExists
    then case deltaType of
      ConstructEmptyContext -> do
        (exists :: Maybe PerspectContext) <- lift $ tryGetPerspectEntiteit id
        -- This is where we make the operation idempotent. Nothing happens if it already exists.
        if isNothing exists
          then do
            contextInstance <- pure
              (PerspectContext defaultContextRecord
                { id = id
                , displayName = unwrap id 
                , pspType = contextType
                , buitenRol = RoleInstance $ buitenRol $ unwrap id
                , universeContextDelta = signedDelta
                , states = [StateIdentifier $ unwrap contextType]
                })
            lift $ void $ cacheEntity id contextInstance
            (lift $ findRoleRequests (ContextInstance "def:AnyContext") (externalRoleType contextType)) >>= addCorrelationIdentifiersToTransactie
            addCreatedContextToTransaction id
          else pure unit
    else logPerspectivesError $ UnauthorizedForContext "auteur" subject contextType

-- | Retrieves from the repository the model that holds the RoleType, if necessary.
executeUniverseRoleDelta :: UniverseRoleDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseRoleDelta (UniverseRoleDelta{id, roleType, roleInstances, authorizedRole, deltaType, subject}) s = do
  log (show deltaType <> " for/from " <> show id <> " with ids " <> show roleInstances <> " with type " <> show roleType)
  void $ lift $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ $ unwrap roleType)
  case deltaType of
    ConstructEmptyRole -> do
      -- We have to check this case: a user is allowed to create himself.
      if userCreatesThemselves
        then constructAnotherRole_
        else do
          -- Check if the author has a perspective on the role to be created that includes
          -- the verb Create.
          (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Create, Verbs.CreateAndFill]) >>= case _ of
            Left e -> handleError e
            Right _ -> constructAnotherRole_
    ConstructExternalRole -> do
      -- Notice that merely constructing a role has no consequences for the responsibilities
      -- QUERY UPDATES, RULE TRIGGERING, PERSISTENCE or CURRENTUSER.
      -- If roleType has the aspect model:System$RootContext$External, it can stand alone. These roles can
      -- be created by any user and usually will be created by parsing a CRL file.
      lift (roleType ###>> hasAspect (EnumeratedRoleType rootContext)) >>= if _
        then constructExternalRole
        else (lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [Verbs.CreateAndFill]) >>= case _ of
          Left e -> handleError e
          Right _ -> constructExternalRole
    RemoveRoleInstance -> do
      (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Remove]) >>= case _ of
        Left e -> handleError e
        -- Right _ -> for_ (toNonEmptyArray roleInstances) removeRoleInstance
        Right _ -> for_ (toNonEmptyArray roleInstances) scheduleRoleRemoval

    -- TODO Het lijkt niet nuttig om beide cases te behouden.
    RemoveUnboundExternalRoleInstance -> do
      (lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [Verbs.Delete, Verbs.Remove]) >>= case _ of
        Left e -> handleError e
        Right _ -> for_ (toArray roleInstances) (flip removeContextIfUnbound authorizedRole)
    RemoveExternalRoleInstance -> do
      (lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [Verbs.Delete, Verbs.Remove]) >>= case _ of
        Left e -> handleError e
        -- As external roles are always stored the same as their contexts, we can reliably retrieve the context instance id from the role id.
        Right _ -> for_ (ContextInstance <<< deconstructBuitenRol <<< unwrap <$> toArray roleInstances) (scheduleContextRemoval authorizedRole)
    where
      userCreatesThemselves :: Boolean
      userCreatesThemselves = case subject of
        ENR r -> r == roleType
        _ -> false

      constructAnotherRole_ :: MonadPerspectivesTransaction Unit
      constructAnotherRole_ = do
        -- find the number of roleinstances in the context.
        offset <- (lift (id ##= getRoleInstances (ENR roleType))) >>= pure <<< getNextRolIndex
        forWithIndex_ (toNonEmptyArray roleInstances) \i roleInstance -> do
          (exists :: Maybe PerspectRol) <- lift $ tryGetPerspectEntiteit roleInstance
          -- Here we make constructing another role idempotent. Nothing happens if it already exists.
          if isNothing exists
            then void $ constructEmptyRole_ id (offset + i) roleInstance
            else pure unit

      constructEmptyRole_ :: ContextInstance -> Int -> RoleInstance -> MonadPerspectivesTransaction Boolean
      constructEmptyRole_ contextInstance i rolInstanceId = do
        (exists :: Maybe PerspectRol) <- lift $ tryGetPerspectEntiteit rolInstanceId
        if isNothing exists
          then do
            allTypes <- lift (roleType ###= roleAspectsClosure)
            role <- pure (PerspectRol defaultRolRecord
                  { id = rolInstanceId
                  , pspType = roleType
                  , allTypes = allTypes
                  , context = contextInstance
                  , occurrence = i
                  , universeRoleDelta = s
                  , states = [StateIdentifier $ unwrap roleType]
                  })
            addCreatedRoleToTransaction rolInstanceId
            void $ lift $ cacheEntity rolInstanceId role
            pure true
          else pure false

      constructExternalRole  :: MonadPerspectivesTransaction Unit
      constructExternalRole = do
        externalRole <- pure (head $ toNonEmptyArray roleInstances)
        log ("ConstructExternalRole in " <> show id)
        -- Here we make constructing an external role idempotent. Nothing happens if it already exists.
        constructEmptyRole_ id 0 externalRole >>= if _
          then lift $ void $ saveEntiteit externalRole
          else pure unit

-- | Execute all Deltas in a run that does not distribute.
-- executeTransaction :: TransactionForPeer -> MonadPerspectives Unit
-- executeTransaction t@(TransactionForPeer{deltas}) = void $ (runMonadPerspectivesTransaction'
--     false
--     (ENR $ EnumeratedRoleType sysUser)
--     (for_ deltas f))

-- TODO #29 Refactor delta decoding for performance
executeTransaction :: TransactionForPeer -> MonadPerspectivesTransaction Unit
executeTransaction t@(TransactionForPeer{deltas}) = for_ deltas f
  where
    f :: SignedDelta -> MonadPerspectivesTransaction Unit
    f s@(SignedDelta{author, encryptedDelta}) = executeDelta $ authenticate (RoleInstance author) encryptedDelta
      where
        executeDelta :: Maybe String -> MonadPerspectivesTransaction Unit
        -- For now, we fail silently on deltas that cannot be authenticated.
        executeDelta Nothing = pure unit
        executeDelta (Just stringifiedDelta) = do 
          storageSchemes <- lift $ gets _.typeToStorage
          catchError
            (case runExcept $ readJSON' stringifiedDelta of
              Right d1 -> executeRolePropertyDelta (addResourceSchemes storageSchemes d1) s
              Left _ -> case runExcept $ readJSON' stringifiedDelta of
                Right d2 -> executeRoleBindingDelta (addResourceSchemes storageSchemes d2) s
                Left _ -> case runExcept $ readJSON' stringifiedDelta of
                  Right d3 -> executeContextDelta (addResourceSchemes storageSchemes d3) s
                  Left _ -> case runExcept $ readJSON' stringifiedDelta of
                    Right d4 -> executeUniverseRoleDelta ((addResourceSchemes storageSchemes d4)) s
                    Left _ -> case runExcept $ readJSON' stringifiedDelta of
                      Right d5 -> executeUniverseContextDelta (addResourceSchemes storageSchemes d5) s
                      Left _ -> log ("Failing to parse and execute: " <> stringifiedDelta))
            (\e -> liftEffect $ log (show e))

-- | All identifiers in deltas in a transaction have been stripped from their storage schemes, except for those with the pub: scheme.
-- | This function adds public resource schemes for the given storageUrl or, when a different publishing point is found for an identifier,
-- | that specific url.
expandDeltas :: TransactionForPeer -> String -> MonadPerspectivesTransaction (Array Delta)
expandDeltas t@(TransactionForPeer{deltas}) storageUrl = catMaybes <$> (for deltas expandDelta)
  where

  expandDelta :: SignedDelta -> MonadPerspectivesTransaction (Maybe Delta)
  expandDelta s@(SignedDelta{author, encryptedDelta}) = expandDelta' $ authenticate (RoleInstance author) encryptedDelta
    where
      expandDelta' :: Maybe String -> MonadPerspectivesTransaction (Maybe Delta)
      expandDelta' Nothing = pure Nothing
      expandDelta' (Just stringifiedDelta) = do 
        case runExcept $ readJSON' stringifiedDelta of
          Right (d1 :: RolePropertyDelta) -> notWhenPublicSubject (unwrap d1) (lift $ (Just <<< RPD s <$> addPublicResourceScheme storageUrl d1))
          Left _ -> case runExcept $ readJSON' stringifiedDelta of
            Right (d2 :: RoleBindingDelta) -> notWhenPublicSubject (unwrap d2) (lift $ (Just <<< RBD s <$> addPublicResourceScheme storageUrl d2))
            Left _ -> case runExcept $ readJSON' stringifiedDelta of
              Right (d3 :: ContextDelta) -> notWhenPublicSubject (unwrap d3) (lift $ (Just <<< CDD s <$> addPublicResourceScheme storageUrl d3))
              Left _ -> case runExcept $ readJSON' stringifiedDelta of
                Right (d4 :: UniverseRoleDelta) -> notWhenPublicSubject (unwrap d4) (lift $ (Just <<< URD s <$> addPublicResourceScheme storageUrl d4))
                Left _ -> case runExcept $ readJSON' stringifiedDelta of
                  Right (d5 :: UniverseContextDelta) -> notWhenPublicSubject (unwrap d5) (lift $ (Just <<< UCD s <$> addPublicResourceScheme storageUrl d5))
                  Left _ -> log ("Failing to parse and execute: " <> stringifiedDelta) *> pure Nothing

      notWhenPublicSubject :: forall f. DeltaRecord f -> MonadPerspectivesTransaction (Maybe Delta) -> MonadPerspectivesTransaction (Maybe Delta)
      notWhenPublicSubject rec a = (lift $ isPublicRole rec.subject) >>= if _ then pure Nothing else a

executeDeltas :: Array Delta -> MonadPerspectivesTransaction Unit
executeDeltas deltas = for_ deltas case _ of 
  UCD s d -> executeUniverseContextDelta d s
  URD s d -> executeUniverseRoleDelta d s
  CDD s d -> executeContextDelta d s
  RBD s d -> executeRoleBindingDelta d s
  RPD s d -> executeRolePropertyDelta d s

-----------------------------------------------------------
-- COLLECTING DELTAS
-----------------------------------------------------------
data Delta = UCD SignedDelta UniverseContextDelta
  | URD SignedDelta UniverseRoleDelta
  | CDD SignedDelta ContextDelta
  | RBD SignedDelta RoleBindingDelta
  | RPD SignedDelta RolePropertyDelta

derive instance Generic Delta _
derive instance Eq Delta
instance Ord Delta where 
  compare (UCD _ u1) (UCD _ u2) = compare u1 u2
  compare (URD _ u1) (URD _ u2) = compare u1 u2
  compare (CDD _ u1) (CDD _ u2) = compare u1 u2
  compare (RBD _ u1) (RBD _ u2) = compare u1 u2
  compare (RPD _ u1) (RPD _ u2) = compare u1 u2
  compare _ _ = LT
