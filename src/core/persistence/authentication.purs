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


module Perspectives.Persistence.Authentication

where

import Prelude

import Affjax as AJ
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Except (catchJust)
import Data.Argonaut (fromObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Error, error, throwError)
import Effect.Aff.Class (liftAff)
import Foreign.Object (fromFoldable, insert, lookup)
import Perspectives.Couchdb (onAccepted_)
import Perspectives.ErrorLogging (logError)
import Perspectives.Identifiers (url2Authority)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.ResourceIdentifiers (databaseLocation)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------

foreign import isUnauthorized :: Error -> Boolean

-- | In Resource s, s is a string with a Resource Identifiying Scheme as defined in [Perspectives.ResourceIdentifiers](Perspectives.ResourceIdentifiers.html).
data AuthoritySource = Resource String | Authority String | Url String

instance Show AuthoritySource where 
  show (Resource s) = "Resource " <> s
  show (Authority s) = "Authority " <> s
  show (Url s) = "Url " <> s

-- | A string of the form https://authority
type Authority = String

ensureAuthentication :: forall f a. AuthoritySource -> (Unit -> MonadPouchdb f a) -> MonadPouchdb f a
ensureAuthentication authSource a = catchJust
  (\e -> if isUnauthorized e then Just true else Nothing)
  (a unit)
  (\_ -> (requestAuthentication authSource) *> (a unit))

requestAuthentication :: forall f. AuthoritySource -> MonadPouchdb f Unit
requestAuthentication authSource = do
  mauthority <- authSource2Authority authSource
  usr <- getSystemIdentifier
  case mauthority of 
    Just authority -> do
      mpwd <- getCredentials authority
      case mpwd of 
        Just pwd -> do
          (rq :: (AJ.Request String)) <- defaultPerspectRequest
          res <- liftAff $ AJ.request $ rq {method = Left POST, url = (authority <> "_session"), content = Just $ RequestBody.json (fromObject (fromFoldable [Tuple "name" (unsafeCoerce usr), Tuple "password" (unsafeCoerce pwd)]))}
          onAccepted_
            (\response _ -> throwError (error $ "Failure in requestAuthentication. " <> "HTTP statuscode " <> show response.status))
            res
            [StatusCode 200, StatusCode 203]
            "requestAuthentication"
            \_ -> pure unit
        -- we do not throw an error, because authentication may have been requested in a situation where it was not necessary.
        Nothing -> logError (error $ "No password found for " <> authority)
    Nothing -> throwError (error $ "Impossible case in requestAuthentication for " <> show authSource)

  where 

  authSource2Authority :: AuthoritySource -> MonadPouchdb f (Maybe Authority)
  authSource2Authority (Resource s) = Just <$> databaseLocation s
  authSource2Authority (Authority s) = pure $ Just s
  authSource2Authority (Url s) = pure $ url2Authority s

defaultPerspectRequest :: forall f. MonadPouchdb f (AJ.Request String)
defaultPerspectRequest = pure
  { method: Left GET
  , url: "http://localhost:5984/"
  , headers: []
  , content: Nothing
  -- TODO. Zonder de credentials weer mee te sturen, ben je niet geauthenticeerd.
  , username: Nothing
  , password: Nothing
  , withCredentials: true
  , responseFormat: ResponseFormat.string
  , timeout: Nothing
}

-- | Looks up the credentials for a given Authority.
getCredentials :: forall f. Authority -> MonadPouchdb f (Maybe String)
getCredentials authority = do 
  credentials <- gets _.couchdbCredentials
  pure $ lookup authority credentials

type Url = String
type Password = String
addCredentials :: forall f. Url -> Password -> MonadPouchdb f Unit
addCredentials url password = modify \s@{couchdbCredentials} -> s { couchdbCredentials = insert url password couchdbCredentials }
