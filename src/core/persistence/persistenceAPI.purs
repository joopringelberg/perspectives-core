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


module Perspectives.Persistence.API

( module Perspectives.Persistence.Types
, module Perspectives.Persistence.Authentication
, module Perspectives.Persistence.API)

where

import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (lift)
import Control.Promise as Promise
import Data.Array.NonEmpty (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, catchError, error, throwError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn6, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Foreign (Foreign, unsafeFromForeign, F)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Object (Object, delete, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Persistence.Attachment (class Attachment, getAttachments, getRawAttachments, setAttachment)
import Perspectives.Couchdb (DeleteCouchdbDocument(..), PutCouchdbDocument(..), ViewResult(..), ViewResultRow(..))
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, getRev, rev)
import Perspectives.Persistence.Authentication (AuthoritySource(..), ensureAuthentication)
import Perspectives.Persistence.Errors (handlePouchError, handleNotFound)
import Perspectives.Persistence.RunEffectAff (runEffectFnAff2, runEffectFnAff3, runEffectFnAff6)
import Perspectives.Persistence.State (getCouchdbBaseURL)
import Perspectives.Persistence.Types (AttachmentName, CouchdbUrl, DatabaseName, DocumentName, DocumentWithRevision, MonadPouchdb, Password, PouchError, PouchdbDatabase, PouchdbExtraState, PouchdbState, PouchdbUser, SystemIdentifier, UserName, ViewName, Url, decodePouchdbUser', encodePouchdbUser', readPouchError, runMonadPouchdb)
import Simple.JSON (read, readImpl, write)

-----------------------------------------------------------
-- CREATE DATABASE
-----------------------------------------------------------
-- | Creates a new Pouchdb db object and adds it to PouchdbState with the given name as key.
-- | If PouchdbState contains a couchdbUrl, creates a database on that endpoint.
-- | If dbname is a full url, creates a database on that endpoint.
-- | Ensures authentication for non-pouchdb databases.
-- | Database names must comply to rules given in https://docs.couchdb.org/en/stable/api/database/common.html#db
createDatabase :: forall f. DatabaseName -> MonadPouchdb f Unit
createDatabase dbname = if startsWithDatabaseEndpoint dbname
  then do
    -- A remote database is actually created immediately if it does not exist.
    -- If the server indicates we're not authenticated, ensureAuthentication requests a session and then repeats the action.
    pdb <- ensureAuthentication (Url dbname) (\_ -> liftEffect $ runEffectFn1 createDatabaseImpl dbname)
    -- Make sure the database is created.
    catchError
      do
        f <- lift $ fromEffectFnAff $ databaseInfoImpl pdb
        case (read f) of
          Left e -> throwError $ error ("databaseInfo: error in decoding result: " <> show e)
          (Right (dbInfo :: DatabaseInfo)) -> pure unit
      -- Convert the incoming message to a PouchError type.
      (handlePouchError "databaseInfo" dbname)
    modify \(s@{databases}) -> s {databases = insert dbname pdb databases}
  else do
    mprefix <- getCouchdbBaseURL
    case mprefix of
      Nothing -> do
        pdb <- liftEffect $ runEffectFn1 createDatabaseImpl dbname
        modify \(s@{databases}) -> s {databases = insert dbname pdb databases}
      Just prefix -> do
        -- A remote database is actually created immediately if it does not exist.
        -- If the server indicates we're not authenticated, ensureAuthentication requests a session and then repeats the action.
        pdb <- ensureAuthentication (Authority prefix) (\_ -> liftEffect $ runEffectFn2 createRemoteDatabaseImpl dbname prefix)
        modify \(s@{databases}) -> s {databases = insert dbname pdb databases}

endpointRegex :: Regex
endpointRegex = unsafeRegex "^https?" noFlags

startsWithDatabaseEndpoint :: DatabaseName -> Boolean
startsWithDatabaseEndpoint = test endpointRegex

-- | Creates a remote database or a local database, depending on whether the first argument is an Url or not.
foreign import createDatabaseImpl :: EffectFn1
  String
  PouchdbDatabase

foreign import createRemoteDatabaseImpl :: EffectFn2
  DatabaseName
  CouchdbUrl
  PouchdbDatabase

-----------------------------------------------------------
-- DELETE DATABASE
-----------------------------------------------------------
-- | Deletes the database, if it exists.
-- | NOTE that due to the way Pouchdb is implemented, if the database does not yet exist
-- | this function will create it first and then delete it.
deleteDatabase :: forall f. DatabaseName -> MonadPouchdb f Unit
deleteDatabase dbName = withDatabase dbName
  \db -> do
    modify \(s@{databases}) -> s {databases = delete dbName databases}
    catchError
      do
        f <- lift $ fromEffectFnAff $ deleteDatabaseImpl db
        case (read f) of
          Left e -> throwError $ error ("deleteDatabase: error in decoding result: " <> show e)
          Right ({ok} :: {ok :: Boolean}) -> if ok
            then pure unit
            else throwError $ error ("deleteDatabase did not succeed in deleting '" <> dbName <> "'.")
      -- Convert the incoming message to a PouchError type.
      (handlePouchError "deleteDatabase" dbName)

foreign import deleteDatabaseImpl :: PouchdbDatabase -> EffectFnAff Foreign
-----------------------------------------------------------
-- ENSUREDATABASE
-----------------------------------------------------------

-----------------------------------------------------------
-- WITHDATABASE
-----------------------------------------------------------
-- | Provides the PouchdbDatabase to the function in MonadPouchdb.
-- | Ensures authentication.
withDatabase :: forall f a.
  DatabaseName ->
  (PouchdbDatabase -> MonadPouchdb f a) ->
  MonadPouchdb f a
withDatabase dbName fun = do
  if startsWithDatabaseEndpoint dbName
    then ensureAuthentication (Url dbName) 
      (\_ -> do
        db <- ensureDatabase
        fun db)
    else do 
      mprefix <- getCouchdbBaseURL
      case mprefix of
        Nothing -> do 
          db <- ensureDatabase
          fun db
        Just prefix -> ensureAuthentication (Authority prefix) 
          (\_ -> do 
            db <- ensureDatabase
            fun db)

  where

  -- | Creates the database if it does not exist.
  -- | Makes a default request, so databases are actually created remotely and authentication is triggered.
  -- Only used in withDatabase.
  ensureDatabase :: MonadPouchdb f PouchdbDatabase
  ensureDatabase = do
    mdb <- gets \{databases} -> lookup dbName databases
    case mdb of
      Nothing -> do
        -- actually creates a remote database and authenticates if necessary.
        -- If a Pouchdb database, doesn't create and doesn't authenticate. 
        -- However, neither are necessary.
        createDatabase dbName
        db <- gets \{databases} -> unsafePartial $ fromJust $ lookup dbName databases
        -- Access the database to trigger authentication.
        -- lift $ void $ fromEffectFnAff $ databaseInfoImpl db
        pure db
      Just db -> do 
        -- We have accessed this database before, but we don't know whether
        -- we still have a valid session.
        -- Access the database and throw error if unauthorized.
        f <- lift $ fromEffectFnAff $ databaseInfoImpl db 
        case read f of 
          Left e -> throwError (error "unauthorized")
          (Right (i :: DatabaseInfo)) -> pure unit
        pure db


-----------------------------------------------------------
-- DATABASEINFO
-----------------------------------------------------------
type DatabaseInfo =
  { db_name :: DatabaseName
  , doc_count :: Int
  }

-- This is only used in Main when clearing the local models database, in setupPerspectivesInCouchdb
-- and in createUserDatabases.
databaseInfo :: forall f. DatabaseName -> MonadPouchdb f DatabaseInfo
databaseInfo dbName = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ databaseInfoImpl db
      case (read f) of
        Left e -> throwError $ error ("databaseInfo: error in decoding result: " <> show e)
        Right info -> pure info
    -- Convert the incoming message to a PouchError type.
    (handlePouchError "databaseInfo" dbName)

foreign import databaseInfoImpl :: PouchdbDatabase -> EffectFnAff Foreign

-----------------------------------------------------------
-- DOCUMENTSINDATABASE
-----------------------------------------------------------
includeDocs :: Boolean
includeDocs = true

excludeDocs :: Boolean
excludeDocs = false

-- | Argument `dbName` may be an url identifying a Couchdb database.
documentsInDatabase :: forall f. DatabaseName -> Boolean -> MonadPouchdb f PouchdbAllDocs
documentsInDatabase dbName include_docs = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ runEffectFnAff2 documentsInDatabaseImpl db {include_docs}
      case (read f) of
        Left e -> throwError $ error ("documentsInDatabase: error in decoding result: " <> show e)
        Right r -> pure r
    -- Convert the incoming message to a PouchError type.
    (handlePouchError "documentsInDatabase" dbName)

foreign import documentsInDatabaseImpl :: EffectFn2 PouchdbDatabase {include_docs :: Boolean} Foreign

type Rev = {rev :: String}
type PouchdbAllDocs =
  { offset :: Int
  , rows :: Array { id :: String, value :: Rev, doc :: Maybe Foreign}
  , total_rows :: Int
  , update_seq :: Maybe Int
  }

-----------------------------------------------------------
-- ADD DOCUMENT
-----------------------------------------------------------
-- | Creates the database if it does not exist.
-- | Creates the document in the database.
-- | Returns the new revision.
-- | Authentication ensured.
-- | The semantics of this operation depends critically on using generic encode.
-- | This allows us to control the value of the _id member of the json representation independently of 
-- | that of the ContextRecord or the RolRecord.
addDocument :: forall d f. Attachment d => Encode d => Revision d => DatabaseName -> d -> DocumentName -> MonadPouchdb f Revision_
addDocument dbName doc docName = withDatabase dbName
  \db -> do
    -- Because generic encoding results in a json document having this shape:
    -- {
    --   "contents": {
    --     ...
    --     },
    --   "tag": "PerspectRol"
    -- }
    -- but Couchdb needs the "id_" and "rev_" members, we have to add them here.
    -- The same holds for attachments, if any.
    doc' <- liftEffect (addNameVersionAndAttachments (encode doc) docName (maybe "" identity (rev doc)) (maybe (write "") unwrap (getAttachments doc)))
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff2 addDocumentImpl db doc'
        -- NOTE we do not use the Decode class instance based on genericDecode here anymore.
        case PutCouchdbDocument <$> (read f) of
          Left e -> throwError $ error ("addDocument: error in decoding result: " <> show e)
          Right (PutCouchdbDocument {rev}) -> pure rev
      -- Promise rejected. Convert the incoming message to a PouchError type.
      (handlePouchError "addDocument" docName)

-- | Similar to addDocument but without the requirement that d is an instance of Revision.
addDocument_ :: forall d f. Encode d => DatabaseName -> d -> DocumentName -> MonadPouchdb f Revision_
addDocument_ dbName doc docName = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ runEffectFnAff2 addDocumentImpl db (encode doc)
      -- NOTE we do not use the Decode class instance based on genericDecode here anymore.
      case PutCouchdbDocument <$> (read f) of
        Left e -> throwError $ error ("addDocument: error in decoding result: " <> show e)
        Right (PutCouchdbDocument {rev}) -> pure rev
    -- Promise rejected. Convert the incoming message to a PouchError type.
    (handlePouchError "addDocument_" docName)

foreign import addDocumentImpl :: EffectFn2 PouchdbDatabase Foreign Foreign

foreign import addNameAndVersionHack :: EffectFn4 Foreign DocumentName String Foreign Foreign

-- TODO. Zodra we een encoding toepassen waarbij _rev en _id bewaard blijven, is deze functie overbodig.
addNameVersionAndAttachments :: Foreign -> DocumentName -> String -> Foreign -> Effect Foreign
addNameVersionAndAttachments = runEffectFn4 addNameAndVersionHack

-----------------------------------------------------------
-- DELETE DOCUMENT
-----------------------------------------------------------
foreign import deleteDocumentImpl :: EffectFn3
  PouchdbDatabase
  DocumentName
  String
  Foreign

-- | Deletes the document.
-- | Returns true if no revision is supplied, or if the document is deleted correctly.
-- | May return false if the document did not exist to start with!
-- | Note: if the database did not exist, it will exist after calling this function!
-- | Authentication ensured.
deleteDocument :: forall f. DatabaseName -> DocumentName -> Revision_ -> MonadPouchdb f Boolean
deleteDocument dbName docName mrev = case mrev of
  Just rev -> withDatabase dbName
    \db -> do
      catchError
        do
          f <- lift $ fromEffectFnAff $ runEffectFnAff3 deleteDocumentImpl db docName rev
          case DeleteCouchdbDocument <$> (read f) of
            Left e -> pure false
            Right (DeleteCouchdbDocument {ok}) -> case ok of
              Just b -> pure b
              Nothing -> pure false
        -- Promise rejected. Convert the incoming message to a PouchError type.
        (handlePouchError "deleteDocument" docName)
  Nothing -> retrieveDocumentVersion dbName docName >>= case _ of 
    -- This takes care of a situation where we do not have the revision in cache.
    -- Without this analysis, we get an infinite loop if the document doesn't exist.
    Nothing -> pure false
    Just rev -> deleteDocument dbName docName (Just rev)

-----------------------------------------------------------
-- GET DOCUMENT
-----------------------------------------------------------
-- | Get the document.
-- | Authentication ensured.
-- TODO. Zolang we de generic encode gebruiken die een tagged versie oplevert zonder _id en _rev members,
-- moeten we de _rev van de entiteit overschrijven met die van de envelope.
-- Zodra dat niet meer nodig is, kunnen we getDocument vervangen door getDocument_
getDocument :: forall d f. Attachment d => Revision d => Decode d => DatabaseName -> DocumentName -> MonadPouchdb f d
getDocument dbName docName = withDatabase dbName
  \db -> do
    f <- catchError
      (lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName)
      (handlePouchError "getDocument" docName)
    case (runExcept do
        -- Get the _rev from the envelope.
        rev <- getRev f
        ma <- getRawAttachments f
        a <- decode f
        -- Set the obtained rev in the inner value, the entity.
        pure (setAttachment (changeRevision rev a) ma)) of
      Left e -> throwError $ error ("getDocument : error in decoding result: " <> show e)
      Right result -> pure result

tryGetDocument :: forall d f. Attachment d => Revision d => Decode d => DatabaseName -> DocumentName -> MonadPouchdb f (Maybe d)
tryGetDocument dbName docName = withDatabase dbName
  \db -> do
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName
        case (runExcept do
            -- Get the _rev from the envelope.
            rev <- getRev f
            ma <- getRawAttachments f
            (a :: d) <- decode f
            -- Set the obtained rev in the inner value, the entity.
            pure (setAttachment (changeRevision rev a) ma)) of
          Left e -> throwError $ error ("tryGetDocument : error in decoding result: " <> show e)
          Right result -> pure $ Just result
      -- Returns Nothing if the document is not in the database; re-throws other errors.
      (handleNotFound "tryGetDocument" docName)

-- | Similar to getDocument but without the requirement that the resulting document is an instance of Revision or Attachment.
-- NOTE: this function replaces getDocument as soon as we have an encoding that preserves _rev.
getDocument_ :: forall d f. Decode d => DatabaseName -> DocumentName -> MonadPouchdb f d
getDocument_ dbName docName = withDatabase dbName
  \db -> do
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName
        case runExcept $ decode f of
          Left e -> throwError $ error ("getDocument_ : error in decoding result: " <> show e)
          Right result -> pure result
      (handlePouchError "getDocument_" docName)

-- | Similar to tryGetDocument but without the requirement that the resulting document is an instance of Revision or Attachment.
-- NOTE: this function replaces tryGetDocument as soon as we have an encoding that preserves _rev.
tryGetDocument_ :: forall d f. Decode d => DatabaseName -> DocumentName -> MonadPouchdb f (Maybe d)
tryGetDocument_ dbName docName = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName
      case runExcept $ decode f of
        Left e -> throwError $ error ("tryGetDocument_ : error in decoding result: " <> show e)
        Right result -> pure result
    (handleNotFound "tryGetDocument_" docName)

-- | Similar to getDocument but without the requirement that the resulting document is an instance of Revision
-- | or Decode or Attachment.
getDocument__ :: forall f. DatabaseName -> DocumentName -> MonadPouchdb f Foreign
getDocument__ dbName docName = withDatabase dbName
  \db -> do
    catchError
      (lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName)
      (handlePouchError "getDocument_" docName)

foreign import getDocumentImpl :: EffectFn2
  PouchdbDatabase
  DocumentName
  Foreign

-----------------------------------------------------------
-- DOCUMENT VERSION
-----------------------------------------------------------
retrieveDocumentVersion :: forall f. DatabaseName -> DocumentName -> MonadPouchdb f (Maybe String)
retrieveDocumentVersion dbName docName = withDatabase dbName
  \db -> do
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName
        case read f of
          Left e -> throwError $ error ("retrieveDocumentVersion : error in decoding result: " <> show e)
          Right (a :: DocumentWithRevision) -> pure a._rev
      (handleNotFound "retrieveDocumentVersion" docName)

-----------------------------------------------------------
-- ADDATTACHMENT
-----------------------------------------------------------
-- | Requires a revision if the document exists prior to adding the attachment.
-- | Notice that the revision of the document changes if an attachment is added succesfully!
addAttachment :: forall f attachmentType. DatabaseName -> DocumentName -> Revision_ -> AttachmentName -> attachmentType -> MediaType -> MonadPouchdb f DeleteCouchdbDocument
addAttachment dbName docName rev attachmentName attachment mimetype = withDatabase dbName
  \db -> do
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff6 addAttachmentImpl db docName attachmentName (toNullable rev) attachment mimetype
        case runExcept $ decode f of
          Left e -> throwError $ error ("addAttachment : error in decoding result: " <> show e)
          Right d -> pure d
      (handlePouchError "addAttachment" docName)

foreign import addAttachmentImpl :: forall attachmentType. EffectFn6
  PouchdbDatabase
  DocumentName
  AttachmentName
  (Nullable String)
  attachmentType
  MediaType
  Foreign

-----------------------------------------------------------
-- GETATTACHMENT
-----------------------------------------------------------
getAttachment :: forall f. DatabaseName -> DocumentName -> AttachmentName -> MonadPouchdb f (Maybe Foreign)
getAttachment dbName docName attachmentName = withDatabase dbName
  \db -> catchError
    (lift $ Just <$> (fromEffectFnAff $ runEffectFnAff3 getAttachmentImpl db docName attachmentName))
    -- do
      -- f <- lift $ fromEffectFnAff $ runEffectFnAff3 getAttachmentImpl db docName attachmentName
      -- case runExcept $ decode f of
      --   Left e -> throwError $ error ("addAttachment : error in decoding result: " <> show e)
      --   Right d -> pure $ Just d
    (handleNotFound "getAttachment" docName)

foreign import getAttachmentImpl :: EffectFn3
  PouchdbDatabase
  DocumentName
  AttachmentName
  Foreign

-----------------------------------------------------------
-- DESIGN DOCUMENT
-----------------------------------------------------------
newtype DesignDocument = DesignDocument
  { _id :: String
  , _rev :: Maybe String
  , views :: Object View
  , validate_doc_update :: Maybe String
}

instance decodeDesignDocument :: Decode DesignDocument where
  decode = (map DesignDocument) <<< readImpl <<< unsafeFromForeign
instance encodeDesignDocument :: Encode DesignDocument where
  encode (DesignDocument d) = write d

-----------------------------------------------------------
-- ADDVIEWTODATABASE
-----------------------------------------------------------
addViewToDatabase :: forall f. DatabaseName -> DocumentName -> ViewName -> View -> MonadPouchdb f Revision_
addViewToDatabase dbName docname viewname view = do
  (mddoc :: Maybe DesignDocument) <- tryGetDocument_ dbName ("_design/" <> docname)
  case mddoc of
    Nothing -> addDocument_ dbName (addView (defaultDesignDocumentWithViewsSection docname) viewname view) ("_design/" <> docname)
    Just ddoc -> addDocument_ dbName (addView ddoc viewname view) ("_design/" <> docname)

type View =
  { map :: String
  , reduce :: Maybe String
}

defaultDesignDocumentWithViewsSection :: String -> DesignDocument
defaultDesignDocumentWithViewsSection n = DesignDocument
  { _id: "_design/" <> n
  , _rev: Nothing
  , views: empty
  , validate_doc_update: Nothing
}

addView :: DesignDocument -> ViewName -> View -> DesignDocument
addView (DesignDocument r@{views}) name view = DesignDocument r {views = insert name view views}

-----------------------------------------------------------
-- GETVIEWONDATABASE
-----------------------------------------------------------
-- | Get the view on the database. Notice that the type of the value in the result
-- | is parameterised and must be an instance of Decode.
getViewOnDatabase :: forall f value key.
  Show key =>
  Decode key =>
  Encode key =>
  Decode value =>
  DatabaseName -> ViewName -> Maybe key -> MonadPouchdb f (Array value)
getViewOnDatabase dbName viewname mkey = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ runEffectFnAff3 getViewOnDatabaseImpl db viewname (toNullable (encode <$> mkey))
      case runExcept $ decode f of
        Left e -> throwError $ error ("getViewOnDatabase : error in decoding result: " <> show e)
        Right ((ViewResult{rows}) :: (ViewResult Foreign key)) -> do
          (r :: F (Array value)) <- pure $ traverse
            (\(ViewResultRow{value}) -> decode value)
            rows
          case runExcept r of
            Left e -> throwError (error ("getViewOnDatabase: multiple errors: " <> show e))
            Right results -> pure results
    (handlePouchError "getViewOnDatabase" viewname)

foreign import getViewOnDatabaseImpl ::
  EffectFn3
    PouchdbDatabase
    ViewName
    (Nullable Foreign)
    Foreign

-----------------------------------------------------------
-- SPLIT AN URL INTO A COUCHDB DATABASE URL AND A DOCUMENT NAME
-----------------------------------------------------------
-- | This code is for the use case of files stored in Couchdb databases that are used as 
-- |    * Perspectives Repositories
-- |    * or instances databases.
-- | We consequently make strong assumptions here:
-- |    * that the URL is about a Couchdb installation;
-- |    * that the scheme is https (this is because all repositories)
-- | Consequently, we split the URL on the last forward slash. E.g.:
-- |    https://perspectives.domains/models_perspectives_domains/perspectives_domains-System.json
-- | will be split into
-- |    {database: https://perspectives.domains/models_perspectives_domains/, document: perspectives_domains-System.json}

repositoryFileRegex :: Regex
repositoryFileRegex = unsafeRegex "^https://([^/]+/)(.*)$" noFlags

splitRepositoryFileUrl :: String -> Maybe {database :: String, document :: String}
splitRepositoryFileUrl s = case match repositoryFileRegex s of
  (Just matches) -> case index matches 1, index matches 2 of
    Just (Just database), Just (Just document) -> Just {database, document}
    _, _ -> Nothing
  _ -> Nothing

-----------------------------------------------------------
-- FILE
-----------------------------------------------------------
type FileName = String
type MimeType = String

foreign import data File :: Type

foreign import toFileImpl :: EffectFn3
  FileName
  MimeType
  Foreign -- ArrayBuffer argument to be turned into a file
  File

toFile :: FileName -> MimeType -> Foreign -> Effect File
toFile = runEffectFn3 toFileImpl

fromBlob :: Foreign -> Aff String
fromBlob = fromBlob_ >>> Promise.toAffE

fromBlob_ :: Foreign -> Effect (Promise.Promise String)
fromBlob_ = runEffectFn1 fromBlobImpl

foreign import fromBlobImpl :: EffectFn1
  Foreign
  (Promise.Promise String)
