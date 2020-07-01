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

module Perspectives.LocalAuthentication where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Perspectives.CouchdbState (CouchdbUser(..), UserName(..))
import Perspectives.Persistent (tryGetPerspectEntiteit)
import Perspectives.RunPerspectives (runPerspectives)
import Prelude (pure, bind, ($), (==), class Eq, class Show)

data AuthenticationResult = UnknownUser | WrongPassword | OK CouchdbUser

derive instance genericRepAuthenticationResult :: Generic AuthenticationResult _

instance showAuthenticationResult :: Show AuthenticationResult where
  show = genericShow

instance eqAuthenticationResult :: Eq AuthenticationResult where
  eq = genericEq

-- | We authenticate by retrieving the Perspectives CouchdbUser document with the given user name.
-- | Because we do so using the Persistent class, we need a Perspectives user to retrieve the documents!
-- | We solve this by using a special account `authenticator` who just has access to the `localusers` database.
-- TODO: verander dit naar een query die in het document kijkt naar het veld userName. Anders kan de user
-- zijn username niet meer veranderen.
authenticate :: String -> String -> String -> Int -> Aff AuthenticationResult
authenticate usr pwd host port = do
  muser <- runPerspectives "authenticator" "secret" "authenticator" host port (tryGetPerspectEntiteit $ UserName usr)
  case muser of
    Nothing -> pure UnknownUser
    Just cdbu@(CouchdbUser{couchdbPassword}) -> if pwd == couchdbPassword
      then pure $ OK cdbu
      else pure WrongPassword
