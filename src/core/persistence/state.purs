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


module Perspectives.Persistence.State

where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object, lookup)
import Perspectives.Persistence.Types (Credential, MonadPouchdb, Url)

-----------------------------------------------------------
-- STATE UTILITIES
-----------------------------------------------------------
-- | The value of field systemIdentifier of the PouchdbUser object that is passed in to runPDR.
-- | Currently, this is the raw username that is used to log into MyContexts.
-- | So if that raw username is "dev1", that is the value returned by getSystemIdentifier.
-- | The result of getMySystem would be def:#dev1.
-- | The result of getUserIdentifier would be def:#dev1$User.
getSystemIdentifier :: forall f. MonadPouchdb f String
getSystemIdentifier = gets _.systemIdentifier

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall f. MonadPouchdb f (Maybe Url)
getCouchdbBaseURL = gets _.couchdbUrl

getCouchdbCredentials :: forall f. MonadPouchdb f (Maybe Credential)
getCouchdbCredentials = do 
  mcbUrl <- gets _.couchdbUrl
  case mcbUrl of 
    Just cbUrl -> do
      credentials :: Object Credential <- gets _.couchdbCredentials
      pure $ lookup cbUrl credentials
    _ -> pure Nothing

-----------------------------------------------------------
-- WITHCOUCHDBURL
-----------------------------------------------------------
withCouchdbUrl :: forall f a. (Url -> MonadPouchdb f a) -> MonadPouchdb f (Maybe a)
withCouchdbUrl f = do
  mUrl <- getCouchdbBaseURL
  case mUrl of
    Just url -> Just <$> f url
    _ -> pure Nothing
