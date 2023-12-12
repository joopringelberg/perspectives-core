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

-- | This module defines two `Producer`s (see Control.Coroutine):
-- |
-- | *  of descriptions of a change (`CouchdbChange`)
-- | *  of documents that have changed
-- |
-- | of changes to a Couchdb database.
-- | The changes feed produced by Couchdb is the source of changes.
-- | The individual results are created by decoding the foreign results and can be
-- | either a list of multiple errors, or a `CouchdbChange` or a document
-- | (the type of which needs to be an instance of Decode).
-- | Construct a Producer with an EventSource object created with `createEventSource`.

module Perspectives.Persistent.ChangesFeed

  ( createEventSource
  , closeEventSource
  , docProducer
  , changeProducer
  , EventSource
  , CouchdbChange(..)
  , DocProducer
  , ChangeProducer
  , DecodedCouchdbChange
  )

  where

import Prelude

import Control.Coroutine (Producer, transform, ($~))
import Control.Coroutine.Aff (Emitter, Step(..), produce')
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)
import Foreign (Foreign, MultipleErrors)
import Perspectives.Persistence.Types (MonadPouchdb)
import Simple.JSON (class ReadForeign, read, read')

-----------------------------------------------------------
-- FEEDURL, QUERYPARAMS
-----------------------------------------------------------
-- The database we want the changes feed from, not terminated in a slash.
type FeedUrl = String

-- | A string that will be appended to the url that is formed from the FeedUrl and the
-- | string "/_changes?feed=eventsource". Should be well formed, e.g. "&myparam=1"
-- |  See http://127.0.0.1:5984/_utils/docs/api/database/changes.html for the
-- | possible query arguments.
type QueryParams = String

-- | Include the associated document with each result. If there are conflicts,
-- | only the winning revision is returned
-- | (taken from: http://127.0.0.1:5984/_utils/docs/api/database/changes.html).
includeDocs :: QueryParams
includeDocs = "&include_docs=true"

-----------------------------------------------------------
-- EVENTSOURCE
-----------------------------------------------------------
-- | Represents the type of instances of the (foreign) Javascript EventSource object.
-- | Use an EventSource to create a Producer of changes to a Couchdb database.
-- | Apply the function `closeEventSource` to finish the listener on the Javascript side.
foreign import data EventSource :: Type

foreign import createEventSourceImpl :: EffectFn2 FeedUrl QueryParams EventSource

-- | From a URL (not a database name!) (string, not terminated on a slash) that identifies a Couchdb database,
-- | and a string with extra query parameters, create an EventSource that listens to changes in the database.
-- | The Boolean parameter switches whether docs should be included.
-- | If necessary, include credentials in the url in the format http://user:password@{domain}/etc.
createEventSource :: FeedUrl -> Maybe QueryParams -> Boolean -> Effect EventSource
createEventSource feedUrl mqueryParams withDocs = case mqueryParams of
  Nothing -> if withDocs
    then runEffectFn2 createEventSourceImpl feedUrl includeDocs
    else runEffectFn2 createEventSourceImpl feedUrl ""
  Just queryParams -> if withDocs
    then runEffectFn2 createEventSourceImpl feedUrl (includeDocs <> queryParams)
    else runEffectFn2 createEventSourceImpl feedUrl queryParams

-----------------------------------------------------------
-- CLOSEEVENTSOURCE
-----------------------------------------------------------
foreign import closeEventSourceImpl :: EffectFn1 EventSource Unit

-- | Terminate the Couchdb change event stream. Also terminates the Producer created with `docProducer`.
closeEventSource :: EventSource -> Effect Unit
closeEventSource es = runEffectFn1 closeEventSourceImpl es

-----------------------------------------------------------
-- FOREIGNVALUEPRODUCER
-----------------------------------------------------------
foreign import createChangeEmitterImpl :: EffectFn4
  EventSource
  (Foreign -> Step Foreign Unit)
  (Unit -> Step Foreign Unit)
  (Emitter Effect Foreign Unit)
  Unit

-- Hernoem naar createForeignEmitter
-- | Takes an EventSource and produces a function that takes an Emitter.
-- | Apply `produce` or `produce'` to it to create a Producer of Foreign.
createChangeEmitter ::
  EventSource ->
  (Emitter Effect Foreign Unit) ->
  Effect Unit
createChangeEmitter eventSource = runEffectFn4 createChangeEmitterImpl
  eventSource
  Emit
  Finish

-- | A Producer of Foreign values. Each change to the database is emitted by the Producer.
foreignValueProducer :: forall f. EventSource -> Producer Foreign (MonadPouchdb f) Unit
foreignValueProducer eventSource = produce' (createChangeEmitter eventSource)

-----------------------------------------------------------
-- CHANGEPRODUCER
-----------------------------------------------------------
type ChangeProducer f docType = Producer (DecodedCouchdbChange docType) (MonadPouchdb f) Unit

-- | DecodedCouchdbChange is a CouchdbChange if it could be decoded correctly,
-- | a list of errors otherwise.
type DecodedCouchdbChange docType = Either MultipleErrors (CouchdbChange docType)

-- | A producer of `CouchdbChange`s.
changeProducer :: forall f docType. ReadForeign docType => EventSource -> ChangeProducer f docType
changeProducer eventSource = (foreignValueProducer eventSource) $~ (forever (transform decodeCouchdbChange'))
  where
    -- | The Foreign value is the encoded value of a CouchdbChange instance.
    decodeCouchdbChange' :: Foreign -> DecodedCouchdbChange docType
    decodeCouchdbChange' f = read f

-----------------------------------------------------------
-- DOCPRODUCER
-----------------------------------------------------------
-- | By decoding the document, we create a producer of documents.
type DocProducer f docType = Producer (Either MultipleErrors (Tuple String (Maybe docType))) (MonadPouchdb f) Unit

-- | A producer of Maybe documents, returning Nothing if the document is deleted
-- | or if no documents are included in the changes (because they were not asked).
docProducer :: forall f docType. ReadForeign docType => EventSource -> DocProducer f docType
docProducer eventSource = (foreignValueProducer eventSource) $~ (forever (transform decodeDoc))
  where
    decodeDoc :: Foreign -> Either MultipleErrors (Tuple String (Maybe docType))
    decodeDoc f = case read f of
      Left e -> Left e
      Right (CouchdbChange{id, doc}) -> do
        Right $ Tuple id doc


-----------------------------------------------------------
-- COUCHDBCHANGE
-----------------------------------------------------------
-- Change for a deleted document:
-- {
--   "seq": "22...",
--   "id": "emptyTransaction",
--   "changes": [
--     {
--       "rev": "2-f4769a3879540448d335a41418bd919d"
--     }
--   ],
--   "deleted": true,
--   "doc": {
--     "_id": "emptyTransaction",
--     "_rev": "2-f4769a3879540448d335a41418bd919d",
--     "_deleted": true
--   }
-- }
-- Change with a document:
-- {
--    "seq": "7-"
--    ,"id": "test1"
--    ,"changes": [{"rev":"7-dcb195a59c23f1df8c30774dfa2cc910"}]
--    ,"doc":
--      { "_id": "test1"
--      ,"_rev": "7-dcb195a59c23f1df8c30774dfa2cc910"
--      ,"contents": {"test":"Hello world!"}
--      ,"tag": "TestDoc"
--      }
-- }
-- | Represents a change in Couchdb. If deleted, the doc is not conform
-- | its type and should not be decoded!
newtype CouchdbChange docType = CouchdbChange
  { id :: String
  , seq :: String
  , changes :: Array {rev :: String}
  , deleted :: Maybe Boolean
  , doc :: Maybe docType
  }

derive instance genericCouchdbChange :: Generic (CouchdbChange docType) _

derive instance newTypeCouchdbChange :: Newtype (CouchdbChange docType) _

instance showCouchdbChange :: Show docType => Show (CouchdbChange docType) where
  -- show (CouchdbChange{id}) = "CouchdbChange: " <> id
  show (CouchdbChange{id, deleted}) = "CouchdbChange: " <> id <> " (deleted=" <> show deleted <> ")"

instance decodeCouchdbChange :: ReadForeign docType => ReadForeign (CouchdbChange docType) where
  -- decode = genericDecode defaultOptions
  readImpl f = do
    inter <- read' f
    doc <- case inter.doc of
      Nothing -> pure Nothing
      Just (fdoc :: Foreign) -> case inter.deleted of
        Nothing -> read' fdoc
        Just false -> read' fdoc
        -- A deleted document is not produced in full. It just has an _id, _rev
        -- and _deleted field.
        otherwise -> pure Nothing
    pure $ CouchdbChange $ inter {doc = doc}

