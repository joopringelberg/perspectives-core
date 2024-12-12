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

module Main.RecompileBasicModels where

-- | Recompiles a number of essential models. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. The essential models include model:ModelManagement, so that
-- | using this account, all models can be recompiled from the client.

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (execState)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, difference, elemIndex, filter, head, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (unsafeToForeign)
import Perspectives.ContextAndRole (rol_property)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.Couchdb (DeleteCouchdbDocument(..))
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), addDownStreamAutomaticEffect, addDownStreamNotification, setRevision)
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder) as TOP
import Perspectives.InvertedQuery.Storable (saveInvertedQueries)
import Perspectives.ModelDependencies (domeinFileName, modelManifest, versionToInstall)
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)
import Perspectives.Persistence.API (Keys(..), addAttachment, addDocument, documentsInDatabase, getAttachment, getViewOnDatabase, includeDocs, toFile)
import Perspectives.Persistence.Types (Url)
import Perspectives.Persistent (getDomeinFile, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedPropertyType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_)
import Simple.JSON (class ReadForeign, read, read', writeJSON)

-- | Parse and compile the versions to install of all models found at the URL, e.g. https://perspectives.domains/models_perspectives_domains
recompileModelsAtUrl :: Url -> Url -> MonadPerspectivesTransaction Unit
recompileModelsAtUrl modelsDb manifestsDb = do
  manifests <- lift $ getViewOnDatabase manifestsDb "defaultViews/roleView" (Key modelManifest)
  versionsToCompile <- traverse getVersionedDomeinFileName manifests >>= pure <<< catMaybes
  {rows:allModels} <- lift $ documentsInDatabase modelsDb includeDocs
  uninterpretedDomeinFiles <- for (filter (isJust <<< (flip elemIndex versionsToCompile) <<< _.id) allModels) \({id, doc}) -> case read <$> doc of
    Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
    Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
    Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
  r <- runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModelAtUrl)
  case r of 
    Left errors -> logPerspectivesError (Custom ("recompileModelsAtUrl: " <> show errors)) 
    _ -> pure unit
  where
    recompileModelAtUrl :: UninterpretedDomeinFile -> ExceptT MultiplePerspectivesErrors MonadPerspectivesTransaction UninterpretedDomeinFile
    recompileModelAtUrl model@(UninterpretedDomeinFile{namespace, _id, _rev, arc}) =
      do
        log ("Recompiling " <> namespace)
        r <- lift $ loadAndCompileArcFile_ (DomeinFileId namespace) arc
        case r of
          Left m -> logPerspectivesError $ Custom ("recompileModelsAtUrl: " <> show m)
          Right (Tuple df@(DomeinFile dfr@{id}) invertedQueries) -> lift $ lift do
            log $  "Recompiled '" <> namespace <> "' succesfully (" <> namespace <> ")!"

            mscreensAttachment <- getAttachment modelsDb _id "screens.js"
            _rev' <- addDocument modelsDb (setRevision _rev (DomeinFile dfr {_id = _id})) namespace
            case mscreensAttachment of 
              Nothing -> do 
                theFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
                void $ addAttachment modelsDb _id _rev' "storedQueries.json" theFile (MediaType "application/json")
              Just screensAttachment -> do
                DeleteCouchdbDocument {rev} <- addAttachment modelsDb _id _rev' "screens.js" screensAttachment (MediaType "text/exmascript")
                theFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
                void $ addAttachment modelsDb _id rev "storedQueries.json" theFile (MediaType "application/json")
        pure model
    getVersionedDomeinFileName :: String -> MonadPerspectivesTransaction (Maybe String)
    getVersionedDomeinFileName rid = do 
      r <- lift $ getPerspectRol (RoleInstance rid)
      case head $ rol_property r (EnumeratedPropertyType domeinFileName), head $ rol_property r (EnumeratedPropertyType  versionToInstall) of
        Just (Value dfName), Just (Value version) -> pure $ Just $ (replace (Pattern ".json") (Replacement "") dfName) <> "@" <> version <> ".json"
        _, _ -> pure Nothing


recompileModel :: UninterpretedDomeinFile -> ExceptT MultiplePerspectivesErrors MonadPerspectivesTransaction UninterpretedDomeinFile
recompileModel model@(UninterpretedDomeinFile{_rev, _id, namespace, arc}) =
  do
    log ("Recompiling " <> namespace)
    r <- lift $ loadAndCompileArcFile_ (DomeinFileId namespace) arc
    case r of
      Left m -> logPerspectivesError $ Custom ("recompileModel: " <> show m)
      Right (Tuple df@(DomeinFile drf@{invertedQueriesInOtherDomains, upstreamStateNotifications, upstreamAutomaticEffects}) invertedQueries) -> lift $ lift do
        log $  "Recompiled '" <> namespace <> "' succesfully!"
        -- We have to add the _id here manually.
        df' <- pure $ DomeinFile drf { _id = _id}
        storeDomeinFileInCouchdbPreservingAttachments df'
        saveInvertedQueries invertedQueries

        -- Distribute upstream state notifications over the other domains.
        forWithIndex_ upstreamStateNotifications
          \domainName notifications -> do
            (try $ getDomeinFile (DomeinFileId domainName)) >>=
              handleDomeinFileError "addModelToLocalStore'"
              \(DomeinFile dfr) -> do
                (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications addDownStreamNotification) dfr))
        -- Distribute upstream automatic effects over the other domains.
        forWithIndex_ upstreamAutomaticEffects
          \domainName automaticEffects -> do
            (try $ getDomeinFile (DomeinFileId domainName)) >>=
              handleDomeinFileError "addModelToLocalStore'"
              \(DomeinFile dfr) -> do
                (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ automaticEffects addDownStreamAutomaticEffect) dfr))
    pure model


--------------------------------------------------------------------------------------------
-- TOPOLOGICAL SORTING
-- Even though we can sort UninterpretedDomeinFile-s using an instance of Ord where we judge
-- two models to be equal if they don't use each other, the resulting order does not respect
-- the basic requirement that a model should be to the right of its used models.
-- The models form a directed graph.
--------------------------------------------------------------------------------------------
type ToSort = Array UninterpretedDomeinFile
type SortedLabels = Array String
type Skipped = Array UninterpretedDomeinFile

executeInTopologicalOrder :: forall m. MonadThrow MultiplePerspectivesErrors m =>
  ToSort ->
  (UninterpretedDomeinFile -> m UninterpretedDomeinFile) ->
  m Boolean
executeInTopologicalOrder toSort action = TOP.executeInTopologicalOrder
  (\(UninterpretedDomeinFile{namespace}) -> namespace)
  (\(UninterpretedDomeinFile{namespace, referredModels}) -> difference referredModels [namespace])
  toSort
  action
    >>=
    \sorted -> pure $ null (toSort `difference` sorted)

newtype UninterpretedDomeinFile = UninterpretedDomeinFile
  { _rev :: String
  , _id :: String
  , namespace :: String
  , referredModels :: Array String
  , arc :: String
  }

instance readForeignUninterpretedDomeinFile :: ReadForeign UninterpretedDomeinFile where
  readImpl f = UninterpretedDomeinFile <$> (read' f)

derive instance genericUninterpretedDomeinFile :: Generic UninterpretedDomeinFile _
instance showUninterpretedDomeinFiles :: Show UninterpretedDomeinFile where
  show (UninterpretedDomeinFile {namespace, referredModels}) = namespace <> " <- " <> show referredModels

instance Eq UninterpretedDomeinFile where
  eq (UninterpretedDomeinFile {namespace:id1}) (UninterpretedDomeinFile {namespace:id2}) = id1 == id2