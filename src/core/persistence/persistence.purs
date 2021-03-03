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


module Perspectives.Persistence.API where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Except (class MonadError, runExcept)
import Control.Monad.Reader (ReaderT, lift, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.MediaType (MediaType)
import Data.Nullable (Nullable, toNullable)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, Error, catchError, error, message, throwError)
import Effect.Aff.AVar (new)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn6, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign, MultipleErrors, unsafeFromForeign, F)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Object (Object, delete, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Couchdb (DatabaseName, DeleteCouchdbDocument(..), PutCouchdbDocument(..), SecurityDocument(..), ViewResult(..), ViewResultRow(..), handleError)
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
type ViewName = String

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
-- RUNEFFECTFNAFF2
-----------------------------------------------------------
-- | With `fromEffectFnAff`, we apply Effect functions of a single argument to Aff.
-- | Such functions must return a `EffectFnAff`. As an example:
-- |  `foreign import deleteDatabaseImpl :: PouchdbDatabase -> EffectFnAff Foreign`
-- | And then:
-- |  `fromEffectFnAff $ deleteDatabaseImpl db`
-- | However, in this module we have various foreign functions that have more than one
-- | parameter. `addDocumentImpl` is an example:
-- |  `foreign import addDocumentImpl :: EffectFn2 PouchdbDatabase Foreign Foreign`
-- | We cannot apply `fromEffectFnAff` directly to functions of this type.
-- | `runEffectFnAff2` comes to the rescue, allowing you to write:
-- |  addDocumentImpl' :: PouchdbDatabase -> Foreign -> EffectFnAff Foreign
-- |  addDocumentImpl' = `runEffectFnAff2 addDocumentImpl`
-- | And then:
-- |  `fromEffectFnAff $ addDocumentImpl' db doc`
foreign import runEffectFnAff2 :: forall a b r.
  EffectFn2 a b r -> a -> b -> EffectFnAff r

-----------------------------------------------------------
-- RUNEFFECTFNAFF3
-----------------------------------------------------------
foreign import runEffectFnAff3 :: forall a b c r.
  EffectFn3 a b c r -> a -> b -> c -> EffectFnAff r

-----------------------------------------------------------
-- RUNEFFECTFNAFF6
-----------------------------------------------------------
foreign import runEffectFnAff6 :: forall a b c d e f r.
  EffectFn6 a b c d e f r -> a -> b -> c -> d -> e -> f -> EffectFnAff r

-----------------------------------------------------------
-- HANDLEPOUCHERROR
-----------------------------------------------------------
-- | Handle Htpp status codes in case of low level errors in interaction with Couchdb by Pouchdb.
-- | Guarantees to give the same messages as perspectives-couchdb.
handlePouchError :: forall m a. MonadError Error m => String -> DocumentName -> Error -> m a
handlePouchError funcName docName e = parsePouchError funcName docName e >>=
  \({status, message} :: PouchError) -> case status of
    Nothing -> throwError e
    Just s -> handleError s mempty (funcName <> " for " <> docName <> " (" <> message <> ")")

handleNotFound :: forall m a. MonadError Error m => String -> DocumentName -> Error -> m (Maybe a)
handleNotFound funcName docName e = parsePouchError funcName docName e >>=
  \err -> case err.status of
    Just 404 -> pure Nothing
    Just s -> handleError s mempty ("getAttachment for " <> docName <> " (" <> err.message <> ")")
    Nothing -> throwError e

parsePouchError :: forall m. MonadError Error m => String -> DocumentName -> Error -> m PouchError
parsePouchError funcName docName e = case readPouchError (message e) of
  -- Generate messages as we did before, using handleError.
  Right err -> pure err
  Left parseErrors -> throwError $ error (funcName <> ": cannot parse error thrown by Pouchdb.\n" <> "The PouchError is:\n" <> (show e) <> "\nAnd these are the parse errors:\n" <> (show parseErrors))

-----------------------------------------------------------
-- CREATE DATABASE
-----------------------------------------------------------
-- | Creates a new Pouchdb db object and adds it to PouchdbState with the given name as key.
-- | If PouchdbState contains a couchdbUrl, creates a database on that endpoint.
-- | If dbname is a full url, creates a database on that endpoint.
-- | Ensures authentication.
-- | Database names must comply to rules given in https://docs.couchdb.org/en/stable/api/database/common.html#db
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
-- DATABASEINFO
-----------------------------------------------------------
type DatabaseInfo =
  { db_name :: DatabaseName
  , doc_count :: Int
  , update_seq :: String
  }

databaseInfo :: forall f. DatabaseName -> MonadPouchdb f DatabaseInfo
databaseInfo dbName = withDatabase dbName
  \db -> do
    catchError
      do
        f <- lift $ fromEffectFnAff $ databaseInfoImpl db
        case (read f) of
          Left e -> throwError $ error ("databaseInfo: error in decoding result: " <> show e)
          Right info -> pure info
      -- Convert the incoming message to a PouchError type.
      (handlePouchError "deleteDatabase" dbName)

foreign import databaseInfoImpl :: PouchdbDatabase -> EffectFnAff Foreign

-----------------------------------------------------------
-- DOCUMENTSINDATABASE
-----------------------------------------------------------
documentsInDatabase :: forall f. DatabaseName -> MonadPouchdb f PouchdbAllDocs
documentsInDatabase dbName = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ documentsInDatabaseImpl db
      case (read f) of
        Left e -> throwError $ error ("documentsInDatabase: error in decoding result: " <> show e)
        Right r -> pure r
    -- Convert the incoming message to a PouchError type.
    (handlePouchError "documentsInDatabase" dbName)

foreign import documentsInDatabaseImpl :: PouchdbDatabase -> EffectFnAff Foreign

type Rev = String
type PouchdbAllDocs =
  { offset :: Int
  , rows :: Array { id :: String, value :: Rev}
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
-- TODO. Zolang we de generic encode gebruiken die een tagged versie oplevert zonder _id en _rev members,
-- hebben we docName als parameter nodig en moeten we de hack addNameAndVersion toepassen.
addDocument :: forall d f. Encode d => Revision d => DatabaseName -> d -> DocumentName -> MonadPouchdb f Revision_
addDocument dbName doc docName = withDatabase dbName
  \db -> do
    -- TODO. Omdat encoding een json doc oplevert zonder _id en _rev, moeten we die hier toevoegen.
    doc' <- liftEffect (addNameAndVersion (encode doc) docName (maybe "" identity (rev doc)))
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
    (handlePouchError "addDocument" docName)

foreign import addDocumentImpl :: EffectFn2 PouchdbDatabase Foreign Foreign

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
  Foreign

-- | Deletes the document.
-- | Returns true if no revision is supplied, or if the document is deleted correctly.
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
    f <- catchError
      (lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName)
      (handlePouchError "getDocument" docName)
    case (runExcept do
      -- Get the _rev from the envelope.
      rev <- getRev f
      a <- decode f
      -- Set the obtained rev in the inner value, the entity.
      pure ((changeRevision rev) a)) of
      Left e -> throwError $ error ("getDocument : error in decoding result: " <> show e)
      Right result -> pure result

tryGetDocument :: forall d f. Revision d => Decode d => DatabaseName -> DocumentName -> MonadPouchdb f (Maybe d)
tryGetDocument dbName docName = withDatabase dbName
  \db -> do
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff2 getDocumentImpl db docName
        case (runExcept do
          -- Get the _rev from the envelope.
          rev <- getRev f
          (a :: d) <- decode f
          -- Set the obtained rev in the inner value, the entity.
          pure ((changeRevision rev) a)) of
          Left e -> throwError $ error ("tryGetDocument : error in decoding result: " <> show e)
          Right result -> pure $ Just result
      (handleNotFound "tryGetDocument" docName)

-- | Similar to getDocument but without the requirement that the resulting document is an instance of Revision.
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

-- | Similar to tryGetDocument but without the requirement that the resulting document is an instance of Revision.
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
    catchError
      do
        f <- lift $ fromEffectFnAff $ runEffectFnAff6 addAttachmentImpl db docName attachmentName (toNullable rev) attachment mimetype
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
  Foreign

-----------------------------------------------------------
-- GETATTACHMENT
-----------------------------------------------------------
-- | Just handles attachments whose representation is a String.
getAttachment :: forall f. DatabaseName -> DocumentName -> AttachmentName -> MonadPouchdb f (Maybe String)
getAttachment dbName docName attachmentName = withDatabase dbName
  \db -> catchError
    do
      f <- lift $ fromEffectFnAff $ runEffectFnAff3 getAttachmentImpl db docName attachmentName
      case runExcept $ decode f of
        Left e -> throwError $ error ("addAttachment : error in decoding result: " <> show e)
        Right d -> pure $ Just d
    (handleNotFound "getAttachment" docName)

foreign import getAttachmentImpl :: EffectFn3
  PouchdbDatabase
  DocumentName
  AttachmentName
  Foreign

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

newtype DesignDocument = DesignDocument
  { _id :: String
  , _rev :: Maybe String
  , views :: Object View
}

instance decodeDesignDocument :: Decode DesignDocument where
  decode = (map DesignDocument) <<< readImpl <<< unsafeFromForeign
instance encodeDesignDocument :: Encode DesignDocument where
  encode (DesignDocument d) = write d

defaultDesignDocumentWithViewsSection :: String -> DesignDocument
defaultDesignDocumentWithViewsSection n = DesignDocument
  { _id: "_design/" <> n
  , _rev: Nothing
  , views: empty
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
