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

module Perspectives.Extern.RabbitMQ where

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Perspectives.AMQP.IncomingPost (retrieveBrokerService)
import Perspectives.AMQP.RabbitMQManagement (AdminPassword, AdminUserName, BrokerServiceUrl, QueueName, createBinding, createUser, deleteQueue, deleteUser, runRabbitState', selfRegisterWithRabbitMQ_, setPermissions, virtualHost)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectivesQuery)
import Perspectives.Error.Boundaries (handleExternalFunctionError, handleExternalStatementError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

type AccountName = String
type RoutingKey = String
type AccountPassword = String

-- | Create a user account. Provide a CUID as username and a CUID as password.
-- | Create user permissions.
-- | Create a queue. Provide a CUID for the queue's name.
-- | Requires RABBITMQ ADMINISTRATOR permissions.
prepareAMQPaccount :: 
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array AccountName -> 
  Array AccountPassword ->
  Array QueueName -> 
  RoleInstance ->  
  MonadPerspectivesTransaction Unit
prepareAMQPaccount url_ adminUserName_ adminPassword_ accountName_ accountPassword_ queueName_ _ = try
  (case 
      head url_, 
      head adminUserName_, 
      head adminPassword_, 
      head accountName_, 
      head accountPassword_,
      head queueName_ of
    Just brokerServiceUrl, Just adminUserName, Just adminPassword, Just userName, Just password, Just queueName -> do
      -- userName <- lift createCuid
      -- queueName <- lift createCuid
      lift $ runRabbitState' {virtualHost, brokerServiceUrl, adminUserName, adminPassword } do 
        r <- try do
          createUser {password, tags: ""} userName
          setPermissions userName {configure: queueName, write: queueName <> "|amq\\.topic", read: queueName <> "|amq\\.topic"}
        case r of 
          Left e -> logPerspectivesError $ Custom $ show e
          Right _ -> pure unit
    _, _, _, _, _, _ -> throwError $ error "Missing some arguments in prepareAMQPaccount.")
  >>= handleExternalStatementError "model://perspectives.domains#RabbitMQ$PrepareAMQPAccount"

-- | This function calls a service on the url that registers a user with RabbitMQ, using 
-- | Admin credentials fixed in the service's code.
-- | The url is based on another management endpoint than that which is used in prepareAMQPaccount.
-- | The actual service lives at 'rbsr'.
-- | For mycontexts this is "https://mycontexts.com/rbsr/". Apache proxies this to the RabbitMQ management endpoint, 
-- | with server admin credentials provided on starting the service on the server.
selfRegisterWithRabbitMQ :: 
  Array BrokerServiceUrl -> 
  Array AccountName -> 
  Array AccountPassword ->
  Array QueueName -> 
  RoleInstance ->
  MonadPerspectivesQuery Value
selfRegisterWithRabbitMQ url_ accountName_ accountPassword_ queueName_ _ = try
  (case 
      head url_, 
      head accountName_, 
      head accountPassword_, 
      head queueName_ of
    Just brokerServiceUrl, Just userName, Just password, Just queueName -> do
      r <- try $ lift $ lift $ selfRegisterWithRabbitMQ_ brokerServiceUrl userName password queueName
      case r of 
        Left e -> do 
          (logPerspectivesError $ Custom $ show e)
          pure $ Value "false"
        Right _ -> pure $ Value "true"
    _, _, _, _ -> throwError $ error "Missing some arguments in selfRegisterWithRabbitMQ.")
  >>= handleExternalFunctionError "model://perspectives.domains#RabbitMQ$SelfRegisterWithRabbitMQ"


-- | Creates the binding between a user identifier and a queue, so peers can push transactions
-- | to the users queue.
-- | Requires RABBITMQ ADMINISTRATOR permissions.
setBindingKey ::
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array QueueName -> 
  Array RoutingKey ->
  RoleInstance ->
  MonadPerspectivesTransaction Unit
setBindingKey url_ userName_ password_ queueName_ routingKey_ _ = try
  (case 
      head url_, 
      head userName_, 
      head password_, 
      head queueName_,
      head routingKey_ of
    Just brokerServiceUrl, Just userName, Just password, Just queueName, Just routingKey -> do
        r <- try $ lift $ runRabbitState' {virtualHost, brokerServiceUrl, adminUserName: userName, adminPassword: password }
          -- TODO: misschien kunnen we het resource argument gebruiken?
          (createBinding (RoleInstance routingKey) queueName)
        case r of 
          Left e -> logPerspectivesError $ Custom $ show e
          Right _ -> pure unit  
    _, _, _, _, _ -> throwError $ error "Missing some arguments in setBindingKey.")
  >>= handleExternalStatementError "model://perspectives.domains#RabbitMQ$SetBindingKey"

setPassword :: 
  BrokerServiceUrl -> 
  AdminUserName -> 
  AdminPassword -> 
  AccountName -> 
  AccountPassword -> 
  Array RoleInstance ->   -- NOTE: this may have to be a ContextInstance.
  MonadPerspectivesTransaction Unit
setPassword url_ adminUserName_ adminPassword_ accountName_ accountPassword_ _ = pure unit

-- | Deletes the users queue and his account at the BrokerService
deleteAMQPaccount :: 
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array AccountName -> 
  RoleInstance ->
  MonadPerspectivesTransaction Unit
deleteAMQPaccount url_ adminUserName_ adminPassword_ accountName_ _ = try 
  (case 
      head url_, 
      head adminUserName_, 
      head adminPassword_, 
      head accountName_ of
    Just brokerServiceUrl, Just adminUserName, Just adminPassword, Just accountName -> do
        r <- try $ lift $ runRabbitState' {virtualHost, brokerServiceUrl, adminUserName, adminPassword } do
          deleteUser accountName
        case r of 
          Left e -> logPerspectivesError $ Custom $ show e
          Right _ -> pure unit
    _, _, _, _ -> throwError $ error "Missing some arguments in deleteAMQPaccount.")
  >>= handleExternalStatementError "model://perspectives.domains#RabbitMQ$DeleteAMQPaccount"

deleteQueue_ :: 
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array QueueName ->
  RoleInstance ->
  MonadPerspectivesTransaction Unit
deleteQueue_ url_ adminUserName_ adminPassword_ queueName_ _ = try 
  (case 
      head url_, 
      head adminUserName_, 
      head adminPassword_, 
      head queueName_ of
    Just brokerServiceUrl, Just adminUserName, Just adminPassword, Just queueName -> do
        r <- try $ lift $ runRabbitState' {virtualHost, brokerServiceUrl, adminUserName, adminPassword } do
          deleteQueue queueName
        case r of 
          Left e -> logPerspectivesError $ Custom $ show e
          Right _ -> pure unit
    _, _, _, _ -> throwError $ error "Missing some arguments in deleteQueue.")
  >>= handleExternalStatementError "model://perspectives.domains#RabbitMQ$DeleteQueue"

setPermissionsForAMQPaccount ::
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array AccountName -> 
  Array QueueName ->
  RoleInstance ->
  MonadPerspectivesTransaction Unit
setPermissionsForAMQPaccount = setPermissionsForAMQPaccount_ \queueName -> {configure: queueName, write: queueName <> "|amq\\.topic", read: queueName <> "|amq\\.topic"}

setPermissionsForAMQPaccount_ ::
  (String -> {configure :: String, write :: String, read :: String}) ->
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array AccountName -> 
  Array QueueName ->
  RoleInstance ->
  MonadPerspectivesTransaction Unit
setPermissionsForAMQPaccount_ permissionsf url_ adminUserName_ adminPassword_ accountName_ queueName_ _ = try
  (case 
      head url_, 
      head adminUserName_, 
      head adminPassword_, 
      head accountName_,
      head queueName_ of
    Just brokerServiceUrl, Just adminUserName, Just adminPassword, Just accountName, Just queueName -> do
        r <- try $ lift $ runRabbitState' {virtualHost, brokerServiceUrl, adminUserName, adminPassword } do
          setPermissions accountName (permissionsf queueName)
        case r of 
          Left e -> logPerspectivesError $ Custom $ show e
          Right _ -> pure unit
    _, _, _, _, _ -> throwError $ error "Missing some arguments in SetPermissionsForAMQPaccount.")
  >>= handleExternalStatementError "model://perspectives.domains#RabbitMQ$SetPermissionsForAMQPaccount"

-- | Sets the read permissions for the Account's queue to '^$', effectively stops him from reading it.
preventAMQPaccountFromReading ::
  Array BrokerServiceUrl -> 
  Array AdminUserName -> 
  Array AdminPassword -> 
  Array AccountName -> 
  Array QueueName ->
  RoleInstance ->
  MonadPerspectivesTransaction Unit
preventAMQPaccountFromReading = setPermissionsForAMQPaccount_ \queueName -> {configure: queueName, write: queueName <> "|amq\\.topic", read: "^$"}

startListening :: RoleInstance -> MonadPerspectivesTransaction Unit
startListening _ = try (lift retrieveBrokerService)
  >>= handleExternalStatementError "model://perspectives.domains#RabbitMQ$StartListening"

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#RabbitMQ$PrepareAMQPaccount" {func: unsafeCoerce prepareAMQPaccount, nArgs: 6, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$SetBindingKey" {func: unsafeCoerce setBindingKey, nArgs: 5, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$SetPassword" {func: unsafeCoerce setPassword, nArgs: 5, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$DeleteAMQPaccount" {func: unsafeCoerce deleteAMQPaccount, nArgs: 4, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$DeleteQueue" {func: unsafeCoerce deleteQueue_, nArgs: 4, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$SetPermissionsForAMQPaccount" {func: unsafeCoerce setPermissionsForAMQPaccount, nArgs: 5, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$StartListening" {func: unsafeCoerce startListening, nArgs: 0, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#RabbitMQ$SelfRegisterWithRabbitMQ" {func: unsafeCoerce selfRegisterWithRabbitMQ, nArgs: 4, isFunctional: True, isEffect: false}
  ]
