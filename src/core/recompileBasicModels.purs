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

import Control.Monad.State (execState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.Array (catMaybes, cons, difference, foldM, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Error, error, runAff, throwError, try)
import Effect.Aff.AVar (new)
import Effect.Class.Console (log)
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (addInvertedQuery, modelsDatabaseName)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (documentsInDatabase, includeDocs)
import Perspectives.Persistence.Types (Url, PouchdbUser, decodePouchdbUser')
import Perspectives.Persistent (getDomeinFile)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..), DomeinFileId(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.TypePersistence.LoadArc (loadArcAndCrl')
import Simple.JSON (class ReadForeign, read, read')

modelDescription :: EnumeratedRoleType
modelDescription = EnumeratedRoleType "model:ModelManagement$ManagedModel$ModelDescription"

modelsInUse :: EnumeratedRoleType
modelsInUse = EnumeratedRoleType "model:System$PerspectivesSystem$ModelsInUse"

-- | Recompiles a number of essential models. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. The essential models include model:ModelManagement, so that
-- | using this account, all models can be recompiled from the client.
-- | These functions depend heavily on the Perspectives types in model:System and
-- | model:ModelManagement.
recompileBasicModels :: Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
recompileBasicModels rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        transactionFlag <- new true
        state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag
        runPerspectivesWithState
          (do
            addAllExternalFunctions
            modelsDb <- modelsDatabaseName
            {rows:allModels} <- documentsInDatabase modelsDb includeDocs
            uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case read <$> doc of
              Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
              Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
              Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
            executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel
          )
          state
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in recompileBasicModels: " <> (show e)
      callback false
    handler (Right e) = do
      logPerspectivesError $ Custom $ "Basic models recompiled!"
      callback true

    recompileModel :: UninterpretedDomeinFile -> MonadPerspectives Unit
    recompileModel (UninterpretedDomeinFile{_id, _rev, contents}) = void $ runMonadPerspectivesTransaction'
      false
      (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
      do
        log ("Recompiling " <> _id)
        -- TODO. We moeten de inverse queries verwerken in de andere modellen!
        r <- lift $ loadArcAndCrl' contents.arc contents.crl
        case r of
          Left m -> logPerspectivesError $ Custom ("recompileModel: " <> show m)
          Right df@(DomeinFile drf@{invertedQueriesInOtherDomains}) -> lift do
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


--------------------------------------------------------------------------------------------
-- TOPOLOGICAL SORTING
-- Even though we can sort UninterpretedDomeinFile-s using an instance of Ord where we judge
-- two models to be equal if they don't use each other, the resulting order does not respect
-- the basic requirement that a model should be to the right of its used models.
-- The models form a directed graph.
-- A topological sort (see e.g. https://www.interviewcake.com/concept/java/topological-sort) of such
-- a graph produces the required order.
-- There is a module Graph in Pursuit, but we cannot use it because our package set is too old.
--------------------------------------------------------------------------------------------
type ToSort = Array UninterpretedDomeinFile
type SortedLabels = Array String
type Skipped = Array UninterpretedDomeinFile

executeInTopologicalOrder :: forall m. Monad m =>
  ToSort ->
  (UninterpretedDomeinFile -> m Unit) ->
  m Unit
executeInTopologicalOrder toSort action = void $ execWriterT (executeInTopologicalOrder' toSort [])
  where
    executeInTopologicalOrder' ::
      ToSort ->
      SortedLabels ->
      WriterT Skipped m SortedLabels
    executeInTopologicalOrder' toSort' sorted = do
      Tuple sortedLabels skipped <- lift $ runWriterT (foldM executeInTopologicalOrder'' sorted toSort')
      if null skipped
        then pure sortedLabels
        else executeInTopologicalOrder' skipped sortedLabels

    executeInTopologicalOrder'' ::
      SortedLabels ->
      UninterpretedDomeinFile ->
      WriterT Skipped m SortedLabels
    executeInTopologicalOrder'' sortedLabels df@(UninterpretedDomeinFile{_id}) = if zeroInDegrees df
      then do
        lift $ action df
        pure $ cons _id sortedLabels
      else do
        tell [df]
        pure sortedLabels
      where
        zeroInDegrees :: UninterpretedDomeinFile -> Boolean
        zeroInDegrees (UninterpretedDomeinFile{contents})= null $ contents.referredModels `difference` sortedLabels

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
