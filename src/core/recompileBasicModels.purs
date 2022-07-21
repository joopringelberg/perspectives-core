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
import Data.Array (catMaybes, filter)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Traversable (for)
import Effect.Aff (try)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), addDownStreamAutomaticEffect, addDownStreamNotification, setRevision)
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder) as TOP
import Perspectives.Extern.Couchdb (addInvertedQuery)
import Perspectives.Identifiers (isModelName)
import Perspectives.ModelDependencies (modelManagementDescription)
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)
import Perspectives.Persistence.API (addAttachment, addDocument, documentsInDatabase, getAttachment, includeDocs)
import Perspectives.Persistence.Types (Url)
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadArcAndCrl')
import Simple.JSON (class ReadForeign, read, read')

modelDescription :: EnumeratedRoleType
modelDescription = EnumeratedRoleType modelManagementDescription

recompileModelsAtUrl :: Url -> MonadPerspectivesTransaction Unit
recompileModelsAtUrl repository = do
  {rows:allModels} <- lift $ documentsInDatabase repository includeDocs
  uninterpretedDomeinFiles <- for (filter (isModelName <<< _.id) allModels) \({id, doc}) -> case read <$> doc of
    Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
    Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
    Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
  r <- runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModelAtUrl)
  case r of 
    Left errors -> logPerspectivesError (Custom ("recompileModelsAtUrl: " <> show errors)) 
    _ -> pure unit
  where
    recompileModelAtUrl :: UninterpretedDomeinFile -> ExceptT MultiplePerspectivesErrors MonadPerspectivesTransaction UninterpretedDomeinFile
    recompileModelAtUrl model@(UninterpretedDomeinFile{_id, _rev, contents}) =
      do
        log ("Recompiling " <> _id)
        r <- lift $ loadArcAndCrl' contents.arc contents.crl
        case r of
          Left m -> logPerspectivesError $ Custom ("recompileModel: " <> show m)
          Right df@(DomeinFile dfr) -> lift $ lift do
            log $  "Recompiled '" <> _id <> "' succesfully!"
            -- storeDomeinFileInCouchdbPreservingAttachments df
            mattachment <- getAttachment repository _id "screens.js"
            _rev' <- addDocument repository (setRevision _rev df) _id
            case mattachment of 
              Nothing -> pure unit
              Just attachment -> void $ addAttachment repository _id _rev' "screens.js" attachment (MediaType "text/exmascript")
        pure model

recompileModel :: UninterpretedDomeinFile -> ExceptT MultiplePerspectivesErrors MonadPerspectivesTransaction UninterpretedDomeinFile
recompileModel model@(UninterpretedDomeinFile{_id, _rev, contents}) =
  do
    log ("Recompiling " <> _id)
    r <- lift $ loadArcAndCrl' contents.arc contents.crl
    case r of
      Left m -> logPerspectivesError $ Custom ("recompileModel: " <> show m)
      Right df@(DomeinFile drf@{invertedQueriesInOtherDomains, upstreamStateNotifications, upstreamAutomaticEffects}) -> lift $ lift do
        log $  "Recompiled '" <> _id <> "' succesfully!"
        storeDomeinFileInCouchdbPreservingAttachments df

        -- Distribute the SeparateInvertedQueries over the other domains.
        forWithIndex_ invertedQueriesInOtherDomains
          \domainName queries -> do
            (try $ getDomeinFile (DomeinFileId domainName)) >>=
              handleDomeinFileError "addModelToLocalStore'"
              \(DomeinFile dfr) -> do
                -- Here we must take care to preserve the screens.js attachment.
                (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ queries addInvertedQuery) dfr))
        -- Distribute upstream state notifications over the other domains.
        forWithIndex_ upstreamStateNotifications
          \domainName notifications -> do
            (try $ getDomeinFile (DomeinFileId domainName)) >>=
              handleDomeinFileError "addModelToLocalStore'"
              \(DomeinFile dfr) -> do
                -- Here we must take care to preserve the screens.js attachment.
                (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications addDownStreamNotification) dfr))
        -- Distribute upstream automatic effects over the other domains.
        forWithIndex_ upstreamAutomaticEffects
          \domainName automaticEffects -> do
            (try $ getDomeinFile (DomeinFileId domainName)) >>=
              handleDomeinFileError "addModelToLocalStore'"
              \(DomeinFile dfr) -> do
                -- Here we must take care to preserve the screens.js attachment.
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
  m Unit
executeInTopologicalOrder toSort action = void $ TOP.executeInTopologicalOrder
  (\(UninterpretedDomeinFile{_id}) -> _id)
  (\(UninterpretedDomeinFile{contents}) -> contents.referredModels)
  toSort
  action

newtype UninterpretedDomeinFile = UninterpretedDomeinFile
  { _id :: String
  , _rev :: String
  , contents ::
    { referredModels :: Array String
    , crl :: String
    , arc :: String
    }
  }

instance readForeignUninterpretedDomeinFile :: ReadForeign UninterpretedDomeinFile where
  readImpl f = UninterpretedDomeinFile <$> (read' f)

derive instance genericUninterpretedDomeinFile :: Generic UninterpretedDomeinFile _
instance showUninterpretedDomeinFiles :: Show UninterpretedDomeinFile where
  show (UninterpretedDomeinFile {_id, contents}) = _id <> " <- " <> show contents.referredModels
