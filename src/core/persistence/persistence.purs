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


module Perspectives.Persistence.API where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Except (class MonadError, runExcept)
import Control.Monad.Reader (ReaderT, lift, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.MediaType (MediaType)
import Data.Nullable (Nullable, toNullable)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, Error, catchError, error, message, throwError)
import Effect.Aff.AVar (new)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn6, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn6)
import Foreign (Foreign, MultipleErrors)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Object (Object, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Couchdb (DatabaseName, DeleteCouchdbDocument(..), PutCouchdbDocument(..), handleError)
import Perspectives.Couchdb.Databases as CDB
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, getRev, rev)
import Perspectives.CouchdbState (UserName(..)) as CDBstate
import Simple.JSON (read, readImpl, readJSON', write, E)

-----------------------------------------------------------
-- POUCHDBDATABASE
-----------------------------------------------------------
foreign import data PouchdbDatabase :: Type

-----------------------------------------------------------
-- ALIASES
-----------------------------------------------------------
type UserName = String
type Password = String
type SystemIdentifier = String
type Url = String
type DocumentName = String
type AttachmentName = String

-----------------------------------------------------------
-- POUCHDBSTATE
-----------------------------------------------------------
type PouchdbState f =
  { userInfo :: PouchdbUser
  , databases :: Object PouchdbDatabase

	-- TODO. De volgende drie kunnen weg zodra Perspectives.Persistence.API alles heeft overgenomen:
  , couchdbPassword :: String
  , couchdbHost :: String
  , couchdbPort :: Int
  | f}


-- TODO. Te verwijderen zodra Perspectives.Persistence.API alles heeft overgenomen.
-- We do not need the UserName value in the core, as long as we have the systemIdentifier.
type PouchdbUser = PouchdbUser' (userName :: CDBstate.UserName)
type PouchdbUser' f =
  { _rev :: Maybe String
  , systemIdentifier :: String
  , password :: String -- Maybe String om met IndexedDB rekening te houden?
  , couchdbUrl :: Maybe String
  | f
  }

type PouchdbExtraState f =
  ( databases :: Object PouchdbDatabase
  | f)

type CouchdbUrl = String

decodePouchdbUser' :: Foreign -> E (PouchdbUser'())
decodePouchdbUser' = read

encodePouchdbUser' :: PouchdbUser'() -> Foreign
encodePouchdbUser' = write
-----------------------------------------------------------
-- MONADPOUCHDB
-----------------------------------------------------------
type MonadPouchdb f = ReaderT (AVar (PouchdbState f)) Aff

-----------------------------------------------------------
-- RUNMONADPOUCHDB
-----------------------------------------------------------
-- | Run an action in MonadCouchdb, given a username and password etc.
-- | Its primary use is in addAttachment_ (to add an attachment using the default "admin" account).
runMonadPouchdb :: forall a. UserName -> Password -> SystemIdentifier -> Maybe Url -> MonadPouchdb () a
  -> Aff a
runMonadPouchdb userName password systemId couchdbUrl mp = do
  (rf :: AVar (PouchdbState ())) <- new $
    { userInfo:
      { systemIdentifier: systemId
      , password
      , couchdbUrl
      -- compat:
      , _rev: Nothing
      , userName: CDBstate.UserName userName
    }
    , databases: empty
    -- compat
    , couchdbPassword: password
    , couchdbHost: "https://127.0.0.1"
    , couchdbPort: 6984
  }
  runReaderT mp rf

-----------------------------------------------------------
-- POUCHERROR
-----------------------------------------------------------
type PouchError =
  { status :: Maybe Int
  , name :: String
  , message :: String
  , error :: Either Boolean String
  -- , reason :: String -- Skip; at most a duplicate of message.
  , docId :: Maybe String
}

readPouchError :: String -> Either MultipleErrors PouchError
readPouchError s = runExcept do
  inter <- readJSON' s
  error <- Left <$> readImpl inter.error <|> Right <$> readImpl inter.error
  pure inter {error = error}

-----------------------------------------------------------
-- STATE UTILITIES
-----------------------------------------------------------
getSystemIdentifier :: forall f. MonadPouchdb f String
getSystemIdentifier = gets $ _.userInfo >>> _.systemIdentifier

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall f. MonadPouchdb f (Maybe Url)
getCouchdbBaseURL = gets $ _.userInfo >>> _.couchdbUrl

-----------------------------------------------------------
-- DOCUMENTWITHREVISION
-----------------------------------------------------------
type DocumentWithRevision = {_rev :: Maybe String}

-----------------------------------------------------------
-- CREATE DATABASE
-----------------------------------------------------------
-- Database names must comply to rules given in https://docs.couchdb.org/en/stable/api/database/common.html#db
createDatabase_old :: forall f. DatabaseName -> MonadPouchdb f Unit
createDatabase_old = CDB.createDatabase

foreign import createDatabaseImpl :: EffectFn1
  String
  PouchdbDatabase

foreign import createRemoteDatabaseImpl :: EffectFn2
  DatabaseName
  CouchdbUrl
  PouchdbDatabase

-- | Creates a new Pouchdb db object and adds it to PouchdbState with the given name as key.
-- | If PouchdbState contains a couchdbUrl, creates a database on that endpoint.
-- | If dbname is a full url, creates a database on that endpoint.
-- | Ensures authentication.
createDatabase :: forall f. DatabaseName -> MonadPouchdb f Unit
createDatabase dbname = if startsWithDatabaseEndpoint dbname
  then do
    pdb <- CDB.ensureAuthentication (liftEffect $ runEffectFn1 createDatabaseImpl dbname)
    modify \(s@{databases}) -> s {databases = insert dbname pdb databases}
  else do
    mprefix <- gets (_.userInfo >>> _.couchdbUrl)
    case mprefix of
      Nothing -> do
        pdb <- CDB.ensureAuthentication (liftEffect $ runEffectFn1 createDatabaseImpl dbname)
        modify \(s@{databases}) -> s {databases = insert dbname pdb databases}
      Just prefix -> do
        pdb <- CDB.ensureAuthentication (liftEffect $ runEffectFn2 createRemoteDatabaseImpl dbname prefix)
        modify \(s@{databases}) -> s {databases = insert dbname pdb databases}
  where
    endpointRegex :: Regex
    endpointRegex = unsafeRegex "^https?" noFlags

    startsWithDatabaseEndpoint :: DatabaseName -> Boolean
    startsWithDatabaseEndpoint = test endpointRegex

-----------------------------------------------------------
-- ENSUREDATABASE
-----------------------------------------------------------
-- | Creates the database if it does not exist.
-- | Authentication ensured.
ensureDatabase :: forall f. DatabaseName -> MonadPouchdb f PouchdbDatabase
ensureDatabase dbName = do
  mdb <- gets \{databases} -> lookup dbName databases
  case mdb of
    Nothing -> do
      createDatabase dbName
      gets \{databases} -> unsafePartial $ fromJust $ lookup dbName databases
    Just db -> pure db

-----------------------------------------------------------
-- WITHDATABASE
-----------------------------------------------------------
-- | Provides the PouchdbDatabase to the function in MonadPouchdb.
-- | Ensures authentication.
withDatabase :: forall f a.
  DatabaseName ->
  (PouchdbDatabase -> MonadPouchdb f a) ->
  MonadPouchdb f a
withDatabase dbName fun = CDB.ensureAuthentication $ do
  (db :: PouchdbDatabase) <- ensureDatabase dbName
  fun db

-----------------------------------------------------------
-- HANDLEPOUCHERROR
-----------------------------------------------------------
-- | Handle Htpp status codes in case of low level errors in interaction with Couchdb by Pouchdb.
-- | Guarantees to give the same messages as perspectives-couchdb.
handlePouchError :: forall m a. MonadError Error m => String -> DocumentName -> Error -> m a
handlePouchError funcName docName e = case readPouchError (message e) of
  -- Generate messages as we did before, using handleError.
  Right ({status, message} :: PouchError) -> case status of
    Nothing -> throwError e
    Just s -> handleError s mempty (funcName <> " for " <> docName <> " (" <> message <> ")")
  Left parseErrors -> throwError $ error (funcName <> ": cannot parse error thrown by Pouchdb.\n" <> "The PouchError is:\n" <> (message e) <> "\nAnd these are the parse errors:\n" <> (show parseErrors))

-----------------------------------------------------------
-- ADD DOCUMENT
-----------------------------------------------------------
-- OBSOLETE. CDB.addDocument is not used anywhere in the core anymore.
-- | Add a document with a GenericEncode instance to a database.
addDocument_old :: forall d f. Encode d => DatabaseName -> d -> DocumentName -> MonadPouchdb f Unit
addDocument_old = CDB.addDocument

foreign import addDocumentImpl :: EffectFn3
  PouchdbDatabase
  Foreign
  DocumentName
  (Promise Foreign)

-- | Creates the database if it does not exist.
-- | Creates the document in the database.
-- | Returns the new revision.
-- | Authentication ensured.
-- TODO. Zolang we de generic encode gebruiken die een tagged versie oplevert zonder _id en _rev members,
-- hebben we docName als parameter nodig en moeten we de hack addNameAndVersion toepassen.
addDocument :: forall d f. Encode d => Revision d => DatabaseName -> d -> DocumentName -> MonadPouchdb f Revision_
addDocument dbName doc docName = withDatabase dbName
  \db -> do
    -- TODO. Omdat encoding een json doc oplevert zonder _id en _rev, moeten we die hier toevoegen.
    doc' <- liftEffect (addNameAndVersion (encode doc) docName (maybe "" identity (rev doc)))
    prom <- liftEffect $ runEffectFn3 addDocumentImpl db doc' docName
    catchError
      do
        f <- (lift $ toAff prom)
        -- NOTE we do not use the Decode class instance based on genericDecode here anymore.
        case PutCouchdbDocument <$> (read f) of
          Left e -> throwError $ error ("addDocument: error in decoding result: " <> show e)
          Right (PutCouchdbDocument {rev}) -> pure rev
      -- Promise rejected. Convert the incoming message to a PouchError type.
      (handlePouchError "addDocment" docName)

foreign import addNameAndVersionHack :: EffectFn3 Foreign DocumentName String Foreign

-- TODO. Zodra we een encoding toepassen waarbij _rev en _id bewaard blijven, is deze functie overbodig.
addNameAndVersion :: Foreign -> DocumentName -> String -> Effect Foreign
addNameAndVersion = runEffectFn3 addNameAndVersionHack

-----------------------------------------------------------
-- DELETE DOCUMENT
-----------------------------------------------------------
foreign import deleteDocumentImpl :: EffectFn3
  PouchdbDatabase
  DocumentName
  String
  (Promise Foreign)

-- | Deletes the document.
-- | Returns true if no revision is supplied, or if the document is deleted correctly.
-- | Note: if the database did not exist, it will exist after calling this function!
-- | Authentication ensured.
deleteDocument :: forall f. DatabaseName -> DocumentName -> Revision_ -> MonadPouchdb f Boolean
deleteDocument dbName docName mrev = case mrev of
  Just rev -> withDatabase dbName
    \db -> do
      prom <- liftEffect $ runEffectFn3 deleteDocumentImpl db docName rev
      catchError
        do
          f <- lift $ toAff prom
          case DeleteCouchdbDocument <$> (read f) of
            Left e -> pure false
            Right (DeleteCouchdbDocument {ok}) -> case ok of
              Just b -> pure b
              Nothing -> pure false
        -- Promise rejected. Convert the incoming message to a PouchError type.
        (handlePouchError "deleteDocument" docName)
  Nothing -> retrieveDocumentVersion dbName docName >>= deleteDocument dbName docName

-----------------------------------------------------------
-- GET DOCUMENT
-----------------------------------------------------------
-- | Get the document.
-- | Authentication ensured.
-- TODO. Zolang we de generic encode gebruiken die een tagged versie oplevert zonder _id en _rev members,
-- moeten we de _rev van de entiteit overschrijven met die van de envelope.
-- Zodra dat niet meer nodig is, kunnen we getDocument vervangen door getDocument_
getDocument :: forall d f. Revision d => Decode d => DatabaseName -> DocumentName -> MonadPouchdb f d
getDocument dbName docName = withDatabase dbName
  \db -> do
    prom <- liftEffect $ runEffectFn2 getDocumentImpl db docName
    catchError
      do
        f <- lift $ toAff prom
        case (runExcept do
          -- Get the _rev from the envelope.
          rev <- getRev f
          a <- decode f
          -- Set the obtained rev in the inner value, the entity.
          pure ((changeRevision rev) a)) of
          Left e -> throwError $ error ("getDocument : error in decoding result: " <> show e)
          Right result -> pure result
      (handlePouchError "getDocument" docName)

-- | Similar to getDocument_ but without the requirement that the resulting document is an instance of Revision.
getDocument_ :: forall d f. Decode d => DatabaseName -> DocumentName -> MonadPouchdb f d
getDocument_ dbName docName = withDatabase dbName
  \db -> do
    prom <- liftEffect $ runEffectFn2 getDocumentImpl db docName
    catchError
      do
        f <- lift $ toAff prom
        case runExcept $ decode f of
          Left e -> throwError $ error ("getDocument : error in decoding result: " <> show e)
          Right result -> pure result
      (handlePouchError "getDocument_" docName)

foreign import getDocumentImpl :: EffectFn2
  PouchdbDatabase
  DocumentName
  (Promise Foreign)

-----------------------------------------------------------
-- DOCUMENT VERSION
-----------------------------------------------------------
retrieveDocumentVersion :: forall f. DatabaseName -> DocumentName -> MonadPouchdb f (Maybe String)
retrieveDocumentVersion dbName docName = withDatabase dbName
  \db -> do
    prom <- liftEffect $ runEffectFn2 getDocumentImpl db docName
    catchError
      do
        f <- lift $ toAff prom
        case read f of
          Left e -> throwError $ error ("retrieveDocumentVersion : error in decoding result: " <> show e)
          Right (a :: DocumentWithRevision) -> pure a._rev
      (handlePouchError "retrieveDocumentVersion" docName)

-----------------------------------------------------------
-- ADDATTACHMENT
-----------------------------------------------------------
-- | Just handles attachments whose representation is a String.
-- | Requires a revision if the document exists prior to adding the attachment.
-- | Notice that the revision of the document changes if an attachment is added succesfully!
addAttachment :: forall f. DatabaseName -> DocumentName -> Revision_ -> AttachmentName -> String -> MediaType -> MonadPouchdb f DeleteCouchdbDocument
addAttachment dbName docName rev attachmentName attachment mimetype = withDatabase dbName
  \db -> do
    prom <- liftEffect $ runEffectFn6 addAttachmentImpl db docName attachmentName (toNullable rev) attachment mimetype
    catchError
      do
        f <- lift $ toAff prom
        case runExcept $ decode f of
          Left e -> throwError $ error ("addAttachment : error in decoding result: " <> show e)
          Right d -> pure d
      (handlePouchError "addAttachment" docName)

foreign import addAttachmentImpl :: EffectFn6
  PouchdbDatabase
  DocumentName
  AttachmentName
  (Nullable String)
  String
  MediaType
  (Promise Foreign)
