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

import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectivesTransaction)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Prelude (Unit, pure, unit)
import Unsafe.Coerce (unsafeCoerce)

type BrokerServiceUrl = String
type NodeName = String
type AdminUserName = String
type AdminPassword = String
type AccountName = String
type RoutingKey = String
type AccountPassword = String
type QueueName = String

createAMQPaccount :: 
  BrokerServiceUrl -> 
  NodeName -> 
  AdminUserName -> 
  AdminPassword -> 
  AccountName -> 
  RoutingKey -> 
  RoleInstance ->   -- NOTE: this may have to be a ContextInstance.
  MonadPerspectivesQuery AccountPassword
createAMQPaccount url_ nodeName_ adminUserName_ adminPassword_ accountName_ routingKey_ _ = pure "some password"

prepareAMQPaccount :: 
  BrokerServiceUrl -> 
  NodeName -> 
  AdminUserName -> 
  AdminPassword -> 
  AccountName -> 
  QueueName -> 
  RoleInstance ->   -- NOTE: this may have to be a ContextInstance.
  MonadPerspectivesQuery AccountPassword
prepareAMQPaccount url_ nodeName_ adminUserName_ adminPassword_ accountName_ queueName_ _ = pure "some password"

setBindingKey ::
  BrokerServiceUrl -> 
  AdminUserName -> 
  AdminPassword -> 
  QueueName -> 
  RoutingKey ->
  Array RoleInstance ->   -- NOTE: this may have to be a ContextInstance.
  MonadPerspectivesTransaction Unit
setBindingKey url_ adminUserName_ adminPassword_ queueName_ routingKey_ _ = pure unit

setPassword :: 
  BrokerServiceUrl -> 
  NodeName -> 
  AdminUserName -> 
  AdminPassword -> 
  AccountName -> 
  AccountPassword -> 
  Array RoleInstance ->   -- NOTE: this may have to be a ContextInstance.
  MonadPerspectivesTransaction Unit
setPassword url_ nodeName_ adminUserName_ adminPassword_ accountName_ accountPassword_ _ = pure unit

deleteAMQPaccount :: 
  BrokerServiceUrl -> 
  NodeName -> 
  AdminUserName -> 
  AdminPassword -> 
  AccountName -> 
  Array RoleInstance ->   -- NOTE: this may have to be a ContextInstance.
  MonadPerspectivesTransaction Unit
deleteAMQPaccount url_ nodeName_ adminUserName_ adminPassword_ accountName_ _ = pure unit

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:RabbitMQ$CreateAMQPaccount" {func: unsafeCoerce createAMQPaccount, nArgs: 6}
  , Tuple "model:RabbitMQ$PrepareAMQPaccount" {func: unsafeCoerce createAMQPaccount, nArgs: 6}
  , Tuple "model:RabbitMQ$SetBindingKey" {func: unsafeCoerce setPassword, nArgs: 5}
  , Tuple "model:RabbitMQ$SetPassword" {func: unsafeCoerce setPassword, nArgs: 6}
  , Tuple "model:RabbitMQ$DeleteAMQPaccount" {func: unsafeCoerce deleteAMQPaccount, nArgs: 5}
  ]
