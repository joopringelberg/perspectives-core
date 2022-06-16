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
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Aff (try)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder) as TOP
import Perspectives.Extern.Couchdb (addInvertedQuery)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (documentsInDatabase, includeDocs)
import Perspectives.Persistence.Types (Url)
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Representation.Class.Identifiable (identifier_)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadArcAndCrl')
import Simple.JSON (class ReadForeign, read, read')

modelDescription :: EnumeratedRoleType
modelDescription = EnumeratedRoleType "model:ModelManagement$ManagedModel$ModelDescription"

recompileModelsAtUrl :: Url -> MonadPerspectivesTransaction Unit
recompileModelsAtUrl repository = do
  {rows:allModels} <- lift $ documentsInDatabase repository includeDocs
  uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case read <$> doc of
    Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
    Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
    Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
  executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel

recompileModel :: UninterpretedDomeinFile -> MonadPerspectivesTransaction UninterpretedDomeinFile
recompileModel model@(UninterpretedDomeinFile{_id, _rev, contents}) =
  do
    log ("Recompiling " <> _id)
    -- TODO. We moeten de inverse queries verwerken in de andere modellen!
    r <- loadArcAndCrl' contents.arc contents.crl
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

executeInTopologicalOrder :: forall m. Monad m =>
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
