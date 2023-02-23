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

-- | This module defines External Core functions for model:RabbitMQ.

module Perspectives.AMQP.RabbitMQManagement where

import Prelude

import Affjax (Request, Response, printError, request)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, evalStateT, gets)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Simple.JSON (readJSON)

type VirtualHost = String
type BrokerServiceUrl = String
type NodeName = String
type AdminUserName = String
type AdminPassword = String

nodesEndpoint :: String
nodesEndpoint = "api/nodes/"

virtualHost :: String
virtualHost = "inplace"

type RabbitState = 
  { virtualHost :: VirtualHost
  , brokerServiceUrl :: BrokerServiceUrl
  , nodeName :: NodeName
  , adminUserName :: AdminUserName
  , adminPassword :: AdminPassword
  }

type WithRabbitState = StateT RabbitState MonadPerspectives

runRabbitState :: forall x. VirtualHost -> BrokerServiceUrl -> NodeName -> AdminUserName -> AdminPassword -> WithRabbitState x -> MonadPerspectives x
runRabbitState vhost brokerServiceUrl nodeName adminUserName adminPassword computation = evalStateT computation
  { virtualHost: vhost, brokerServiceUrl, nodeName, adminUserName, adminPassword } 

getAuthenticationHeader :: AdminUserName -> AdminPassword -> RequestHeader
getAuthenticationHeader uname pwd = RequestHeader uname pwd

defaultRabbitRequest :: BrokerServiceUrl -> AdminUserName -> AdminPassword -> WithRabbitState (Request String)
defaultRabbitRequest url username password = pure
  { method: Left GET
  , url
  , headers: []
  , content: Nothing
  -- TODO. Zonder de credentials weer mee te sturen, ben je niet geauthenticeerd.
  , username: Just username
  , password: Just password
  , withCredentials: true
  , responseFormat: ResponseFormat.string
  , timeout: Nothing
}

getNodes :: WithRabbitState (Array String)
getNodes = do
  url <- gets _.brokerServiceUrl
  username <- gets _.adminUserName
  password <- gets _.adminPassword
  (rq :: (Request String)) <- defaultRabbitRequest (url <> nodesEndpoint) username password
  res <- liftAff $ request rq 
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.getNodes: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 200
      then case (readJSON response.body) of
        Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.getNodes: error in decoding result: " <> show e)
        Right nodeNames -> pure nodeNames
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.getNodes: unexpected statuscode " <> show response.status)

