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


module Perspectives.Persistence.Types where

import Prelude
 
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (new)
import Foreign (Foreign, MultipleErrors)
import Foreign.Object (Object, empty, singleton)
import Perspectives.Couchdb.Revision (Revision_)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser)
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
type DatabaseName = String

-----------------------------------------------------------
-- POUCHDBDOCUMENTFIELDS
-----------------------------------------------------------
type PouchbdDocumentFields f = 
  { _id :: String
  , _rev :: Revision_
  | f
  }
-----------------------------------------------------------
-- POUCHDBSTATE
-----------------------------------------------------------
type PouchdbState f =
  { databases :: Object PouchdbDatabase
  -- Notice that this is the schemaless identifier,
  , systemIdentifier :: String
  -- while this is the schemed identifier cast as PerspectivesUser.
  , perspectivesUser :: PerspectivesUser
  , couchdbUrl :: Maybe String
  -- The keys are the URLs of CouchdbServers
  -- The values are passwords.
  -- Each account is in the name of the systemIdentifier.
  -- TODO: de values moeten een combinatie van username en paswoord worden!
  , couchdbCredentials :: Object Credential
  | f}

data Credential = Credential UserName Password

-- | We need a Semigroup instance, but there is no real application for appending.
instance Semigroup Credential where
  append c1 c2 = c1

type PouchdbUser =
  { systemIdentifier :: String        -- the schemaless string
  , perspectivesUser :: String        -- the schemaless string
  , userName :: Maybe String          -- this MAY be equal to perspectivesUser but it is not required.
  , password :: Maybe String
  , couchdbUrl :: Maybe String
  }

type PouchdbExtraState f =
  ( databases :: Object PouchdbDatabase
  | f)

type CouchdbUrl = String

decodePouchdbUser' :: Foreign -> E PouchdbUser
decodePouchdbUser' = read

encodePouchdbUser' :: PouchdbUser -> Foreign
encodePouchdbUser' = write
-----------------------------------------------------------
-- MONADPOUCHDB
-----------------------------------------------------------
type MonadPouchdb f = ReaderT (AVar (PouchdbState f)) Aff

-----------------------------------------------------------
-- RUNMONADPOUCHDB
-----------------------------------------------------------
-- | Run an action in MonadPouchdb, given a username and password etc.
-- | Its primary use is in addAttachment_ (to add an attachment using the default "admin" account).
runMonadPouchdb :: forall a. UserName -> Password -> PerspectivesUser -> SystemIdentifier -> Maybe Url -> MonadPouchdb () a
  -> Aff a
runMonadPouchdb userName password perspectivesUser systemId couchdbUrl mp = do
  (rf :: AVar (PouchdbState ())) <- new $
    { systemIdentifier: systemId
    , perspectivesUser
    , couchdbUrl
    , couchdbCredentials: maybe empty ((flip singleton) (Credential userName password)) couchdbUrl 
    , databases: empty
    -- compat
    -- , couchdbPassword: password
    -- , couchdbHost: "https://127.0.0.1"
    -- , couchdbPort: 6984
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
-- DOCUMENTWITHREVISION
-----------------------------------------------------------
type DocumentWithRevision = {_rev :: Maybe String}
