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

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Couchdb where

import Affjax (Request, URL, printResponseFormatError, request)
import Affjax.RequestBody (string) as RequestBody
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (StateT, execStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (elemIndex, foldl, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Class (class GenericEncode)
import Foreign.Object (Object, insert, lookup)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_isMe, rol_binding, rol_pspType)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MP, MPQ, MonadPerspectivesTransaction, MonadPerspectives, assumption)
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (addAttachment, defaultPerspectRequest, getViewOnDatabase, retrieveDocumentVersion, version, documentNamesInDatabase)
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription, hiddenFunctionInsert)
import Perspectives.Guid (guid)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.ObjectGetters (isMe)
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (class Persistent, entitiesDatabaseName, getPerspectEntiteit, saveEntiteit, saveEntiteit_, tryGetPerspectEntiteit, updateRevision)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity, removeInternally)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.User (getSystemIdentifier)
import Prelude (class Monad, Unit, bind, discard, map, pure, show, unit, void, ($), (<<<), (<>), (>>=), (||), (==))
import Unsafe.Coerce (unsafeCoerce)

-- | Retrieve from the repository the external roles of instances of sys:Model.
-- | These are kept in the field "modelDescription" of DomeinFile.
-- | TODO: provide a repository parameter, so the URL is taken from the repository rather than hardcoded.
models :: MPQ RoleInstance
models = ArrayT do
  sysId <- lift getSystemIdentifier
  tell [assumption sysId ophaalTellerName]
  lift $ getExternalRoles

  where
    getListOfModels :: MP (Array RoleInstance)
    getListOfModels = catchError (modelsDatabaseName >>= documentNamesInDatabase >>= pure <<< map (RoleInstance <<< (_ <> "$External"))) \_ -> pure []

    ophaalTellerName :: String
    ophaalTellerName = "model:System$PerspectivesSystem$External$ModelOphaalTeller"

    getExternalRoles :: MP (Array RoleInstance)
    getExternalRoles = do
      (roles :: Array PerspectRol) <- getViewOnDatabase "repository" "defaultViews" "modeldescriptions" Nothing
      for roles \r@(PerspectRol{_id}) -> do
        -- If the model is already in use, this role has been saved before.
        (savedRole :: Maybe PerspectRol) <- tryGetPerspectEntiteit _id
        case savedRole of
          Nothing -> void $ cacheEntity _id r
          Just _ -> pure unit
        pure _id

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_models/")

-- | Retrieves all instances of a particular role type from Couchdb.
-- | For example: `user: Users = callExternal cdb:RoleInstances("model:System$PerspectivesSystem$User") returns: model:System$PerspectivesSystem$User`
-- | Notice that only the first element of the array argument is actually used.
roleInstances :: Array String -> MPQ RoleInstance
roleInstances roleTypes = ArrayT $ lift $ do
  (roles :: Array PerspectRol) <- entitiesDatabaseName >>= \db -> getViewOnDatabase db "defaultViews" "roleView" (head roleTypes)
  for roles \r@(PerspectRol{_id}) -> do
    void $ cacheEntity _id r
    pure _id

-- | Retrieve the model(s) from the url(s) and add them to the local couchdb installation.
-- | Load the acompanying instances, too.
-- | Notice that the urls should be the full path to the relevant documents.
-- TODO. Authentication at the repository urls.
addModelToLocalStore :: Array String -> MonadPerspectivesTransaction Unit
addModelToLocalStore urls = do
  for_ urls addModelToLocalStore'
  where
    addModelToLocalStore' :: String -> MonadPerspectivesTransaction Unit
    addModelToLocalStore' url = do
        -- Retrieve the DomeinFile from the URL.
      (rq :: (Request String)) <- lift $ lift $ defaultPerspectRequest
      res <- liftAff $ request $ rq {url = url}
      df@(DomeinFile{_id, modelDescription, crl, indexedNames}) <- liftAff $ onAccepted res.status [200, 304] "addModelToLocalStore"
        (onCorrectCallAndResponse "addModelToLocalStore" res.body \a -> pure unit)

      rev <- version res.headers
      -- Store the model in Couchdb. Remove the revision: it belongs to the repository,
      -- not the local perspect_models.
      save (identifier df :: DomeinFileId) (changeRevision Nothing df)
      -- Copy the attachment
      lift $ lift $ addA url _id

      -- sys:MijnSysteem is a special case, as we have an identifier for that already.
      -- It is created on creating a new local user.
      crl' <- lift2 $ replaceSystemIdentifier crl
      -- For all indexed names, generate a guid and replace the occurrences of
      -- that indexed name in the CRL source file. Notice that even for model:System
      -- we can replace all indexed names now, because sys:MijnSysteem does not occur
      -- any more.
      crl'' <- pure $ foldl (\(crl_ :: String) iname -> replaceAll (Pattern iname) (Replacement $ "model:User$" <> show (guid unit)) crl_) crl' indexedNames
      -- The modeller may have used indexed names of each of the referred models.
      -- We should be able to look them up in state.
      -- TODO: look up the indexed names and apply them to the crl'' text.

      -- Remove the modelDescription first from cache: it will have been retrieved before the user decided to start using this model.
      case modelDescription of
        Nothing -> pure unit
        Just m -> void $ lift2 $ removeInternally (identifier (m :: PerspectRol))

      -- Parse the CRL. This will cache all roleInstances.
      parseResult <- lift2 $ parseAndCache crl''
      case parseResult of
        Left e -> throwError (error (show e))
        Right (Tuple contextInstances roleInstances') -> do
          -- Save role- and contextinstances.
          cis <- lift2 $ execStateT (saveRoleInstances roleInstances') contextInstances
          forWithIndex_ cis \i a -> lift2 $ saveEntiteit (ContextInstance i)
          -- For each role instance with a binding that is not one of the other imported role instances,
          -- set the inverse binding administration on that binding.
          forWithIndex_ roleInstances' \i a -> case rol_binding a of
            Nothing -> pure unit
            Just newBindingId -> if (isJust $ lookup (unwrap newBindingId) roleInstances')
              then pure unit
              else do
                -- set the inverse binding
                newBinding <- lift2 $ getPerspectEntiteit newBindingId
                lift2 $ void $ cacheEntity newBindingId (addRol_gevuldeRollen newBinding (rol_pspType a) (RoleInstance i))
                lift2 $ void $ saveEntiteit newBindingId
                -- There can be no queries that use binder <type of a> on newBindingId, since the model is new.
                -- So we need no action for QUERY UPDATES or RULE TRIGGERING.

    -- Replace all occurrences of "model:User$MijnSysteem" in the text with an identifier of the form "model:User$<guid>"
    replaceSystemIdentifier :: String -> MonadPerspectives String
    replaceSystemIdentifier c = do
      sysId <- getMySystem
      pure $ replaceAll (Pattern "model:System$MijnSysteem") (Replacement sysId) c

    -- Prefer an earlier version of the Context instance.
    cacheRoleInstance :: RoleInstance -> PerspectRol -> MP Unit
    cacheRoleInstance roleId role = do
      mexistingRole <- tryGetPerspectEntiteit roleId
      case mexistingRole of
        Nothing -> void $ cacheEntity roleId role
        Just _ -> pure unit

    saveRoleInstances :: Object PerspectRol -> StateT (Object PerspectContext) MonadPerspectives Unit
    saveRoleInstances ris = forWithIndex_ ris \i a@(PerspectRol{context, pspType}) -> do
      me <- lift $ isMe (RoleInstance i)
      if me || pspType == (EnumeratedRoleType "model:System$PerspectivesSystem$User")
        then do
          void $ lift $ saveEntiteit_ (RoleInstance i) (changeRol_isMe a true)
          void $ modify \cis -> case lookup (unwrap context) cis of
            Nothing -> cis
            Just c -> insert (unwrap context) (changeContext_me c (Just (RoleInstance i))) cis
        else void $ lift $ saveEntiteit_ (RoleInstance i) a

    -- Prefer an earlier version of the Context instance.
    save :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Unit
    save i a = lift $ lift do
      mexistingContext <- tryGetPerspectEntiteit i
      case mexistingContext of
        Nothing -> do
          void $ cacheEntity i a
          updateRevision i
          void $ saveEntiteit i
        otherwise -> pure unit

    -- url is the path to the document in the repository.
    addA :: String -> String -> MP Unit
    addA url modelName = do
      (rq :: (Request String)) <-  defaultPerspectRequest
      res <- liftAff $ request $ rq {url = url <> "/screens.js"}
      case elemIndex res.status [StatusCode 200, StatusCode 304] of
        Nothing -> pure unit
        Just _ -> do
          void $ case res.body of
            Left e -> throwError $ error ("addModelToLocalStore: Errors on retrieving attachment: " <> (printResponseFormatError e))
            -- Left e -> pure unit
            Right attachment -> do
              perspect_models <- modelsDatabaseName
              void $ addAttachment (perspect_models <> modelName) "screens.js" attachment (MediaType "text/ecmascript")

-- | Take a DomeinFile from the local perspect_models database and upload it to the repository database at url.
-- | Notice that url should include the name of the repository database within the couchdb installation. We do
-- | not assume anything about that name here.
uploadToRepository :: DomeinFileId -> URL -> MPQ Unit
uploadToRepository dfId url = do
  df <- lift $ lift $ getPerspectEntiteit dfId
  uploadToRepository_ dfId url df

-- | As uploadToRepository, but provide the DomeinFile as argument.
uploadToRepository_ :: DomeinFileId -> URL -> DomeinFile -> MPQ Unit
uploadToRepository_ dfId url df = lift $ lift $ do
  docUrl <- pure (url <> "/" <> (show dfId))
  (rq :: (Request String)) <- defaultPerspectRequest
  -- Try to get the revision, so we can overwrite.
  mVersion <- retrieveDocumentVersion docUrl
  case mVersion of
    Nothing -> do
    -- If not available, store without revision
      res <- liftAff $ request $ rq {method = Left PUT, url = docUrl, content = Just $ RequestBody.string (encodeJSON df)}
      void $ onAccepted res.status [200, 201] "uploadToRepository_"
        (onCorrectCallAndResponse "uploadToRepository_" res.body (\(a :: PutCouchdbDocument) -> pure unit))
      -- Now add the attachment.
    -- If available, store with revision
    Just rev -> do
      res <- liftAff $ request $ rq {method = Left PUT, url = (docUrl <> "?rev=" <> rev), content = Just $ RequestBody.string  (encodeJSON df)}
      void $ onAccepted res.status [200, 201] "uploadToRepository_"
        (onCorrectCallAndResponse "uploadToRepository_" res.body (\(a :: PutCouchdbDocument) -> pure unit))

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "couchdb_Models" {func: unsafeCoerce models, nArgs: 0}
  , Tuple "couchdb_AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore, nArgs: 1}
  , Tuple "couchdb_UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2}
  , Tuple "couchdb_RoleInstances" {func: unsafeCoerce roleInstances, nArgs: 1}
]

addExternalFunctions :: forall m. Monad m => m Unit
addExternalFunctions = for_ externalFunctions \(Tuple n f) -> pure $ hiddenFunctionInsert n f.func f.nArgs
