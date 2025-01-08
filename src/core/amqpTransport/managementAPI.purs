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

import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Request, Response, printError, request)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, evalStateT, gets)
import Data.Argonaut (Json)
import Data.Array (head)
import Data.Either (Either(..), fromRight)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String.Base64 (btoa)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign (MultipleErrors)
import Foreign.Object (Object, empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (Password, onAccepted_, toJson)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.Types (UserName)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Simple.JSON (readJSON, writeJSON)

type VirtualHost = String
type BrokerServiceUrl = String
type NodeName = String
type AdminUserName = String
type AdminPassword = String
type QueueName = String


nodesEndpoint :: String
nodesEndpoint = "api/nodes/"

userEndpoint :: String
userEndpoint = "api/users/"

permissionsEndpoint :: String
permissionsEndpoint = "api/permissions/"

virtualHost :: String
virtualHost = "mycontexts"

type RabbitState = 
  { virtualHost :: VirtualHost
  , brokerServiceUrl :: BrokerServiceUrl
  , adminUserName :: AdminUserName
  , adminPassword :: AdminPassword
  }

type WithRabbitState = StateT RabbitState MonadPerspectives

runRabbitState :: forall x. VirtualHost -> BrokerServiceUrl -> AdminUserName -> AdminPassword -> WithRabbitState x -> MonadPerspectives x
runRabbitState vhost brokerServiceUrl adminUserName adminPassword computation = evalStateT computation
  { virtualHost: vhost, brokerServiceUrl, adminUserName, adminPassword } 

runRabbitState' :: forall x. RabbitState -> WithRabbitState x -> MonadPerspectives x
runRabbitState' state computation = evalStateT computation state

getAuthenticationHeader :: Partial => AdminUserName -> AdminPassword -> RequestHeader
getAuthenticationHeader uname pwd = RequestHeader "Authorization" ("Basic " <> (fromRight "" (btoa (uname <> ":" <> pwd))))

-- | A request with the credentials and url from RabbitState
defaultRabbitRequest :: WithRabbitState (Request String)
defaultRabbitRequest = do 
  url <- gets _.brokerServiceUrl
  username <- gets _.adminUserName
  password <- gets _.adminPassword
  pure
    { method: Left GET
    , url
    , headers: [unsafePartial getAuthenticationHeader username password] 
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: true
    , responseFormat: ResponseFormat.string
    , timeout: Nothing
  }

getNodes :: WithRabbitState (Array String)
getNodes = do
  (rq :: (Request String)) <- defaultRabbitRequest
  res <- liftAff $ request rq { url = rq.url <> nodesEndpoint}
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.getNodes: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 200
      then case (readJSON response.body) of
        Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.getNodes: error in decoding result: " <> show e)
        Right (nodeNames :: Array {name :: String}) -> pure $ _.name <$> nodeNames
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.getNodes: unexpected statuscode " <> show response.status)

createUser :: {password :: String, tags :: String } ->  UserName -> WithRabbitState Unit
createUser user userName = do
  (rq :: (Request String)) <- defaultRabbitRequest
  res <- liftAff $ request (rq 
    { url = rq.url <> userEndpoint <> userName
    , method = Left PUT
    , content = Just $ RequestBody.string $ writeJSON user
    })
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.createUser: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 201
      then pure unit
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.createUser: unexpected statuscode " <> show response.status)

type RabbitUser = 
  { name :: String
  , password_hash :: String
  , hashing_algorithm :: String
  , tags :: Array String
  -- , limits:{}
  }
getUser :: UserName -> WithRabbitState (Maybe RabbitUser)
getUser userName = do
  (rq :: (Request String)) <- defaultRabbitRequest 
  res <- liftAff $ request rq { url = rq.url <> userEndpoint <> userName }
  onAccepted_
    (\response _ -> logPerspectivesError 
      (Custom $ "getUser received the following statuscode when trying to retrieve " <> userName <> ": " <> show response.status) *> pure Nothing)
    res
    [StatusCode 200]
    "getUser"
    \response -> do 
      (x :: Either MultipleErrors RabbitUser) <- pure $ readJSON response.body
      case x of
        (Left e) -> do
          throwError $ error ("getUser: error in decoding result: " <> show e)
        (Right user) -> pure $ Just user

deleteUser :: UserName -> WithRabbitState Unit
deleteUser userName = do
  (rq :: (Request String)) <- defaultRabbitRequest
  res <- liftAff $ request (rq 
    { url = rq.url <> userEndpoint <> userName
    , method = Left DELETE
    })
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.deleteUser: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 204
      then pure unit
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.deleteUser: unexpected statuscode " <> show response.status)

createQueue :: QueueName -> WithRabbitState Unit
createQueue queueName = do 
  vhost <- gets _.virtualHost
  nodes <- getNodes
  case head nodes of
    Nothing -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.createQueue: no nodes found in AMQP server.")
    Just node -> do
      queueEndpoint <- pure $ "api/queues/" <> vhost <> "/"
      (rq :: (Request String)) <- defaultRabbitRequest
      res <- liftAff $ request (rq 
        { url = rq.url <> queueEndpoint <> queueName
        , method = Left PUT
        , content = Just $ RequestBody.string $ writeJSON ({ auto_delete: false, durable: true, arguments: empty, node: node } :: Queue)
        })
      case res of 
        Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.createQueue: error in call: " <> printError e)
        Right (response :: Response String) -> if response.status == StatusCode 201 || response.status == StatusCode 204
          then pure unit
          else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.createQueue: unexpected statuscode " <> show response.status <> ", status text is: " <> response.statusText)
  
type Queue = { auto_delete :: Boolean, durable :: Boolean, arguments :: Object String, node :: String}

deleteQueue :: QueueName -> WithRabbitState Unit
deleteQueue queueName = do 
  vhost <- gets _.virtualHost
  queueEndpoint <- pure $ "api/queues/" <> vhost <> "/"
  (rq :: (Request String)) <- defaultRabbitRequest
  res <- liftAff $ request (rq 
    { url = rq.url <> queueEndpoint <> queueName
    , method = Left DELETE
    })
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.deleteQueue: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 204
      then pure unit
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.deleteQueue: unexpected statuscode " <> show response.status <> ", status text is: " <> response.statusText)
  
              -- { "configure": "^$"                         // No configuring rights for an ordinary Perspectives user.
              -- , "write": "amq.topic"                      // Just write to the default exchange.
              -- , "read": component.state.newAccountName    // Just read from the accounts own queue.
              -- } )

type AMQPUserPermissions = { configure :: String, write :: String, read :: String}
setPermissions :: UserName -> AMQPUserPermissions -> WithRabbitState Unit
setPermissions userName permissions = do
  vhost <- gets _.virtualHost
  rq <- defaultRabbitRequest
  res <- liftAff $ request (rq
    { url = rq.url <> permissionsEndpoint <> vhost <> "/" <> userName
    , method = Left PUT
    , content = Just $ RequestBody.string $ writeJSON permissions
    })
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.setPermissions: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 201
      then pure unit
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.setPermissions: unexpected statuscode " <> show response.status <> ", status text is: " <> response.statusText)

createBinding :: RoleInstance -> QueueName -> WithRabbitState Unit
createBinding (RoleInstance rid) queueName = do
  vhost <- gets _.virtualHost
  bindingsEndpoint <- pure $ "api/bindings/" <> vhost <> "/e/amq.topic/q/" <> queueName
  rq <- defaultRabbitRequest
  res <- liftAff $ request (rq
    { url = rq.url <> bindingsEndpoint
    , method = Left POST
    , content = Just $ RequestBody.string $ writeJSON ({routing_key: takeGuid rid } :: { routing_key :: String})
    })
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.setBindings: error in call: " <> printError e)
    Right (response :: Response String) -> if response.status == StatusCode 201
      then pure unit
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.setBindings: unexpected statuscode " <> show response.status <> ", status text is: " <> response.statusText)
  
type SelfRegisterInformation = { userName :: String, password :: String, queueName :: String }

selfRegisterWithRabbitMQ_ :: BrokerServiceUrl -> UserName -> Password -> QueueName -> MonadPerspectives Unit
selfRegisterWithRabbitMQ_ brokerServiceUrl userName password queueName = do
  res <- liftAff $ request 
    { method: Left POST
    , url: brokerServiceUrl
    , headers: []
    , content: Just $ RequestBody.json $ toJson ({ userName, password, queueName } :: SelfRegisterInformation)
    , username: Nothing
    , password: Nothing
    -- See: https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials
    -- As this request is within the same domain, the setting has no effect.
    , withCredentials: false
    , responseFormat: ResponseFormat.json
    , timeout: Nothing
    }
  case res of 
    Left e -> throwError $ error ("Perspectives.AMQP.RabbitMQManagement.selfRegisterWithRabbitMQ: error in call: " <> printError e)
    Right (response :: Response Json) -> if response.status == StatusCode 200 || response.status == StatusCode 201
      then pure unit
      else throwError $ error ("Perspectives.AMQP.RabbitMQManagement.selfRegisterWithRabbitMQ: unexpected statuscode " <> show response.status <> ", status text is: " <> response.statusText)
