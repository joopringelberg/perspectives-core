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
import Data.Maybe (Maybe)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (new)
import Foreign (Foreign, MultipleErrors)
import Foreign.Object (Object, empty)
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
-- POUCHDBSTATE
-----------------------------------------------------------
type PouchdbState f =
  { userInfo :: PouchdbUser
  , databases :: Object PouchdbDatabase

	-- TODO. De volgende drie kunnen weg zodra Perspectives.Persistence.API alles heeft overgenomen:
  -- , couchdbPassword :: String
  -- , couchdbHost :: String
  -- , couchdbPort :: Int
  | f}


-- TODO. Te verwijderen zodra Perspectives.Persistence.API alles heeft overgenomen.
-- We do not need the UserName value in the core, as long as we have the systemIdentifier.
-- type PouchdbUser = PouchdbUser' (userName :: CDBstate.UserName)
type PouchdbUser =
  { systemIdentifier :: String
  , password :: String -- Maybe String om met IndexedDB rekening te houden?
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
      -- , _rev: Nothing
      -- , userName: CDBstate.UserName userName
    }
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
