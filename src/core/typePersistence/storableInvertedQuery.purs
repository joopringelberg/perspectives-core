-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.InvertedQuery.Storable  where


import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.InvertedQueryKey (ContextKeyFields, FillerKeyFields, PropertyKeyFields, RoleKeyFields, RunTimeInvertedQueryKey(..), FilledKeyFields)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (Keys(..), addDocuments, createDatabase, deleteDatabase, deleteDocuments, getAttachment, getViewWithDocs)
import Perspectives.Persistent (invertedQueryDatabaseName)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (resourceIdentifier2DocLocator)
import Simple.JSON (read, writeJSON)

type StorableInvertedQuery = { keys :: Array RunTimeInvertedQueryKey, query :: InvertedQuery, model :: DomeinFileId }

type StoredQueries = Array StorableInvertedQuery

clearInvertedQueriesDatabase :: MonadPerspectives Unit
clearInvertedQueriesDatabase = do
  iqDatabaseName <- invertedQueryDatabaseName 
  catchError (deleteDatabase iqDatabaseName)
    \_ -> createDatabase iqDatabaseName
  createDatabase iqDatabaseName

getPropertyQueries :: Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getPropertyQueries qs = do 
  db <- invertedQueryDatabaseName
  (storedQueries :: StoredQueries) <- getViewWithDocs db "defaultViews/RTPropertyKeyView" (Keys (qs <#> writeJSON <<< unsafePartial f))
  pure (storedQueries <#> _.query)
  where 
    f :: Partial => RunTimeInvertedQueryKey -> PropertyKeyFields
    f (RTPropertyKey fields) = fields

getRoleQueries :: Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getRoleQueries qs = do 
  db <- invertedQueryDatabaseName
  (storedQueries :: StoredQueries) <- getViewWithDocs db "defaultViews/RTRoleKeyView" (Keys (qs <#> unsafePartial f))
  pure (storedQueries <#> _.query)
  where 
    f :: Partial => RunTimeInvertedQueryKey -> RoleKeyFields
    f (RTRoleKey fields) = fields

getContextQueries :: Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getContextQueries qs = do 
  db <- invertedQueryDatabaseName
  (storedQueries :: StoredQueries) <- getViewWithDocs db "defaultViews/RTContextKeyView" (Keys (qs <#> unsafePartial f))
  pure (storedQueries <#> _.query)
  where 
    f :: Partial => RunTimeInvertedQueryKey -> ContextKeyFields
    f (RTContextKey fields) = fields

getFillerQueries :: Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getFillerQueries qs = do 
  db <- invertedQueryDatabaseName
  (storedQueries :: StoredQueries) <- getViewWithDocs db "defaultViews/RTFillerKeyView" (Keys (qs <#> unsafePartial f))
  pure (storedQueries <#> _.query)
  where 
    f :: Partial => RunTimeInvertedQueryKey -> FillerKeyFields
    f (RTFillerKey fields) = fields

getFilledQueries :: Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getFilledQueries qs = do 
  db <- invertedQueryDatabaseName
  (storedQueries :: StoredQueries) <- getViewWithDocs db "defaultViews/RTFilledKeyView" (Keys (qs <#> unsafePartial f))
  pure (storedQueries <#> _.query)
  where 
    f :: Partial => RunTimeInvertedQueryKey -> FilledKeyFields
    f (RTFilledKey fields) = fields

saveInvertedQueries :: StoredQueries -> MonadPerspectives Unit
saveInvertedQueries queries = do 
  db <- invertedQueryDatabaseName
  void $ addDocuments db queries

-- | Retrieve the inverted queries for a model from the Repository.
getInvertedQueriesOfModel :: DomeinFileId -> MonadPerspectives StoredQueries
getInvertedQueriesOfModel (DomeinFileId dfid) = do 
  {database, documentName} <- resourceIdentifier2DocLocator dfid
  getAttachment database documentName "storedQueries.json" >>= case _ of 
    Just f -> case read f of
      Left e -> logPerspectivesError (Custom $ "getInvertedQueriesOfModel" <> show e) *> pure []
      Right sq -> pure sq
    Nothing -> pure []

-- | Remove the inverted queries contributed from the local database.
removeInvertedQueriesContributedByModel :: DomeinFileId -> MonadPerspectives Unit
removeInvertedQueriesContributedByModel dfid = do 
  db <- invertedQueryDatabaseName
  -- First retrieve all inverted queries from the local database contributed by the current model.
  (modelQueries :: Array {_id :: String, _rev :: String }) <- getViewWithDocs db "defaultViews/modelView" (Key dfid)
  void $ deleteDocuments db modelQueries
