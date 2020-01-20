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

import Affjax (Request, URL, request)
import Affjax.RequestBody as RequestBody
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Foreign.Generic (defaultOptions, genericEncodeJSON)
import Foreign.Generic.Class (class GenericEncode)
import Perspectives.CoreTypes (MP, MPQ, MonadPerspectivesTransaction, assumption)
import Perspectives.Couchdb (PutCouchdbDocument, ViewResult(..), onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (defaultPerspectRequest, getViewOnDatabase, retrieveDocumentVersion, version)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription, hiddenFunctionInsert)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Persistent (class Persistent, getPerspectEntiteit, saveEntiteit, saveEntiteit_)
import Perspectives.Representation.Class.Cacheable (cacheInitially, cachePreservingRevision)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.Revision (changeRevision)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Prelude (class Monad, Unit, bind, discard, map, pure, show, unit, void, ($), (<<<), (<>), (>>=), (<$>))
import Unsafe.Coerce (unsafeCoerce)

-- | Retrieve from the repository the external roles of instances of sys:Model.
-- | These are kept in the field "modelDescription" of DomeinFile.
-- | TODO: provide a repository parameter, so the URL is taken from the repository rather than hardcoded.
models :: MPQ RoleInstance
models = ArrayT do
  tell [assumption "model:User$MijnSysteem" ophaalTellerName]
  lift $ getExternalRoles

  where
    getListOfModels :: MP (Array RoleInstance)
    getListOfModels = catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map (RoleInstance <<< (_ <> "$External"))) \_ -> pure []

    ophaalTellerName :: String
    ophaalTellerName = "model:System$PerspectivesSystem$External$ModelOphaalTeller"

    getExternalRoles :: MP (Array RoleInstance)
    getExternalRoles = do
      (ViewResult{rows} :: ViewResult PerspectRol) <- getViewOnDatabase "repository" "defaultViews" "modeldescriptions"
      for (_.value <<< unwrap <$> rows) \r@(PerspectRol{_id}) -> do
        void $ cachePreservingRevision _id r
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
      (df@(DomeinFile{contextInstances, roleInstances}) :: DomeinFile) <- liftAff $ onAccepted res.status [200, 304] "addModelToLocalStore"
        (onCorrectCallAndResponse "addModelToLocalStore" res.body \a -> pure unit)
      rev <- version res.headers
      -- Store the model in Couchdb. Remove the revision: it belongs to the repository,
      -- not the local perspect_models.
      save (identifier df :: DomeinFileId) (changeRevision Nothing df)
      -- Take the role- and contextinstances from it and add them to the user (instances) database.
      forWithIndex_ contextInstances \i a -> save (ContextInstance i) a
      -- TODO. Dit gaat niet op voor alle rollen, maar wel voor de model representatie.
      forWithIndex_ roleInstances \i a -> void $ lift $ lift $ saveEntiteit_ (RoleInstance i) a

    save :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Unit
    save i a = do
      void $ lift $ lift $ cacheInitially i a
      void $ lift $ lift $ saveEntiteit i

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
  -- Try to get the revision
  mVersion <- retrieveDocumentVersion docUrl
  case mVersion of
    Nothing -> do
    -- If not available, store without revision
      res <- liftAff $ request $ rq {method = Left PUT, url = docUrl, content = Just $ RequestBody.string (genericEncodeJSON defaultOptions df)}
      void $ onAccepted res.status [200, 201] "saveUnversionedEntiteit"
        (onCorrectCallAndResponse "saveUnversionedEntiteit" res.body (\(a :: PutCouchdbDocument) -> pure unit))
    -- If available, store with revision
    Just rev -> do
      res <- liftAff $ request $ rq {method = Left PUT, url = (docUrl <> "?rev=" <> rev), content = Just $ RequestBody.string  (genericEncodeJSON defaultOptions df)}
      void $ onAccepted res.status [200, 201] "saveVersionedEntiteit"
        (onCorrectCallAndResponse "saveVersionedEntiteit" res.body (\(a :: PutCouchdbDocument) -> pure unit))

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "couchdb_Models" {func: unsafeCoerce models, nArgs: 0}
  , Tuple "couchdb_AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore, nArgs: 1}
  , Tuple "couchdb_UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2}
]

addExternalFunctions :: forall m. Monad m => m Unit
addExternalFunctions = for_ externalFunctions \(Tuple n f) -> pure $ hiddenFunctionInsert n f.func f.nArgs
