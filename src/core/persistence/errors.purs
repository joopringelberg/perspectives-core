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


module Perspectives.Persistence.Errors

where

import Prelude

import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Effect.Aff (Error, error, message, throwError)
import Perspectives.Couchdb (handleError)
import Perspectives.Persistence.Types (DocumentName, PouchError, readPouchError)

-----------------------------------------------------------
-- HANDLEPOUCHERROR
-----------------------------------------------------------
-- | Handle Htpp status codes in case of low level errors in interaction with Couchdb by Pouchdb.
-- | Guarantees to give the same messages as perspectives-couchdb.
-- | Always re-throws.
handlePouchError :: forall m a. MonadError Error m => String -> DocumentName -> Error -> m a
handlePouchError funcName docName e = parsePouchError funcName docName e >>=
  \({status, message} :: PouchError) -> case status of
    Nothing -> throwError e
    Just s -> handleError s empty (funcName <> " for " <> docName <> " (" <> message <> ")")

-- | Returns Nothing if the status code is 404; throws an error instead.
handleNotFound :: forall m a. MonadError Error m => String -> DocumentName -> Error -> m (Maybe a)
handleNotFound funcName docName e = parsePouchError funcName docName e >>=
  \err -> case err.status of
    Just 404 -> pure Nothing
    Just s -> handleError s empty (funcName <> " for " <> docName <> " (" <> err.message <> ")")
    Nothing -> throwError e

parsePouchError :: forall m. MonadError Error m => String -> DocumentName -> Error -> m PouchError
parsePouchError funcName docName e = do
  case readPouchError (message e) of
    -- Generate messages as we did before, using handleError.
    Right err -> pure err
    Left parseErrors -> throwError $ error (funcName <> ": cannot parse error thrown by Pouchdb.\n" <> "The PouchError is:\n" <> (show e) <> "\nAnd these are the parse errors:\n" <> (show parseErrors))
