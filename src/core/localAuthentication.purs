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

module Perspectives.LocalAuthentication where

import Control.Monad.Error.Class (catchJust)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, message)
import Perspectives.Couchdb.Databases (getDocument)
import Perspectives.CouchdbState (CouchdbUser, runMonadCouchdb)
import Prelude (pure, bind, ($), (==), class Eq, class Show, (<$>))

data AuthenticationResult = UnknownUser | WrongCredentials | OK CouchdbUser

derive instance genericRepAuthenticationResult :: Generic AuthenticationResult _

instance showAuthenticationResult :: Show AuthenticationResult where
  show = genericShow

instance eqAuthenticationResult :: Eq AuthenticationResult where
  eq = genericEq

-- | We authenticate by retrieving the Perspectives CouchdbUser document with the given user name.
authenticate :: String -> String -> String -> Int -> Aff AuthenticationResult
authenticate usr pwd host port = do
  result <- catchJust
    (\e -> if message e ==  "UNAUTHORIZED" then Just "UNAUTHORIZED" else Nothing)
    (Right <$> runMonadCouchdb usr pwd "" host port (getDocument "localusers" usr))
    (\_ -> pure $ Left WrongCredentials)
  case result of
    Left _ -> pure WrongCredentials
    Right muser -> case muser of
      Nothing -> pure UnknownUser
      Just cdbu -> pure $ OK cdbu
