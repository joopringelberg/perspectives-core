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

import Perspectives.Persistence.State
import Perspectives.Persistence.Types
import Prelude

import Affjax as AJ
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (catchJust)
import Data.Argonaut (fromObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Error, error, throwError)
import Effect.Aff.Class (liftAff)
import Foreign.Object (fromFoldable)
import Perspectives.Couchdb (onAccepted_)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------

foreign import isUnauthorized :: Error -> Boolean

ensureAuthentication :: forall f a. MonadPouchdb f a -> MonadPouchdb f a
ensureAuthentication a = catchJust
  (\e -> if isUnauthorized e then Just true else Nothing)
  a
  \_ -> requestAuthentication *> a

requestAuthentication :: forall f. MonadPouchdb f Unit
requestAuthentication = do
  usr <- getSystemIdentifier
  pwd <- getCouchdbPassword
  mbase <- getCouchdbBaseURL
  case mbase of
    Nothing -> pure unit
    Just base -> do
      (rq :: (AJ.Request String)) <- defaultPerspectRequest
      res <- liftAff $ AJ.request $ rq {method = Left POST, url = (base <> "_session"), content = Just $ RequestBody.json (fromObject (fromFoldable [Tuple "name" (unsafeCoerce usr), Tuple "password" (unsafeCoerce pwd)]))}
      onAccepted_
        (\response _ -> throwError (error $ "Failure in requestAuthentication. " <> "HTTP statuscode " <> show response.status))
        res
        [StatusCode 200, StatusCode 203]
        "requestAuthentication"
        \_ -> pure unit

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
