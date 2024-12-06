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

module Perspectives.InvertedQuery.Storable
  ( StorableInvertedQuery
  , StoredQueries
  , clearInvertedQueriesDatabase
  , getContextQueries
  , getFilledQueries
  , getFillerQueries
  , getInvertedQueriesOfModel
  , getPropertyQueries
  , getRoleQueries
  , removeInvertedQueriesContributedByModel
  , saveInvertedQueries
  )
  where


import Prelude

import Data.Array (concat, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import LRUCache (defaultGetOptions, get, set)
import Perspectives.CoreTypes (MonadPerspectives, QueryInstances)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (unversionedModelUri)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey, serializeInvertedQueryKey)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (Keys(..), addDocuments, deleteDocuments, documentsInDatabase, excludeDocs, fromBlob, getAttachment, getViewWithDocs)
import Perspectives.Persistent (invertedQueryDatabaseName)
import Perspectives.PerspectivesState (queryCache)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Simple.JSON (readJSON)

type StorableInvertedQuery = { queryType :: String, keys :: Array String, query :: InvertedQuery, model :: DomeinFileId }

type StoredQueries = Array StorableInvertedQuery

clearInvertedQueriesDatabase :: MonadPerspectives Unit
clearInvertedQueriesDatabase = do
  iqDatabaseName <- invertedQueryDatabaseName 
  -- Actually delete all documents, because deleting the database will destroy the design document with queries too
  -- and we cannot import the function to recreate them here from SetupUser.
  {rows} <- documentsInDatabase iqDatabaseName excludeDocs
  -- Don't delete the design document!
  documents :: Array {_id :: String, _rev :: String} <- pure $ (\{id, value} -> {_id: id, _rev: value.rev}) <$> (filter (\{id} -> id /= "_design/defaultViews") rows)
  void $ deleteDocuments iqDatabaseName documents

type QueryGetter = Array RunTimeInvertedQueryKey -> MonadPerspectives StoredQueries
type QueryCompiler = InvertedQuery -> MonadPerspectives InvertedQuery

getQueries :: String -> QueryCompiler -> Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getQueries viewName queryCompiler = getQueriesFromCache queryCompiler getQueries'
  where
  getQueries' :: Array RunTimeInvertedQueryKey -> MonadPerspectives StoredQueries
  getQueries' qs = do 
    db <- invertedQueryDatabaseName
    getViewWithDocs db viewName (Keys (qs <#> serializeInvertedQueryKey))

getPropertyQueries :: QueryCompiler -> Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getPropertyQueries = getQueries "defaultViews/RTPropertyKeyView"

getRoleQueries :: QueryCompiler -> Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getRoleQueries = getQueries "defaultViews/RTRoleKeyView"

getContextQueries :: QueryCompiler -> Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getContextQueries = getQueries "defaultViews/RTContextKeyView"

getFillerQueries ::  QueryCompiler ->Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getFillerQueries = getQueries "defaultViews/RTFillerKeyView"

getFilledQueries ::  QueryCompiler -> Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getFilledQueries = getQueries "defaultViews/RTFilledKeyView"

storeQueryInCache :: String -> (Array InvertedQuery) -> MonadPerspectives QueryInstances
storeQueryInCache key queryInstance = queryCache >>= liftEffect <<< (set key queryInstance Nothing)

getQueryFromCache :: RunTimeInvertedQueryKey -> MonadPerspectives (Maybe (Array InvertedQuery))
getQueryFromCache key = do
  cache <- queryCache
  liftEffect $ get (serializeInvertedQueryKey key) defaultGetOptions cache

getQueriesFromCache :: QueryCompiler -> QueryGetter -> Array RunTimeInvertedQueryKey -> MonadPerspectives (Array InvertedQuery)
getQueriesFromCache queryCompiler queryGetter ks = concat <$> 
  for ks
    \key -> do 
      mQueries <- getQueryFromCache key
      case mQueries of 
        Just queries -> pure queries
        Nothing -> do 
          (storedQueries :: Array StorableInvertedQuery) <- queryGetter [key]
          queries <- for storedQueries \{query} -> queryCompiler query
          void $ storeQueryInCache (serializeInvertedQueryKey key) queries
          pure queries

saveInvertedQueries :: StoredQueries -> MonadPerspectives Unit
saveInvertedQueries queries = do 
  db <- invertedQueryDatabaseName
  void $ addDocuments db queries

-- | Retrieve the inverted queries for a model from the Repository.
getInvertedQueriesOfModel :: String -> String -> MonadPerspectives StoredQueries
getInvertedQueriesOfModel database documentName = do 
  getAttachment database documentName "storedQueries.json" >>= case _ of 
    Just f -> do 
      x <- liftAff $ fromBlob f 
      case readJSON x of
        Left e -> logPerspectivesError (Custom $ "getInvertedQueriesOfModel" <> show e) *> pure []
        Right sq -> pure $ sq
    Nothing -> pure []

-- | Remove the inverted queries contributed from the local database.
removeInvertedQueriesContributedByModel :: DomeinFileId -> MonadPerspectives Unit
removeInvertedQueriesContributedByModel (DomeinFileId dfid) = do 
  db <- invertedQueryDatabaseName
  -- First retrieve all inverted queries from the local database contributed by the current model.
  (modelQueries :: Array {_id :: String, _rev :: String }) <- getViewWithDocs db "defaultViews/modelView" (Key $ unversionedModelUri dfid)
  void $ deleteDocuments db modelQueries
