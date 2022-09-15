module Perspectives.ModelDependencies  where

-- | This module contains all Arc identifiers that are used in the PDR source code.
-- | None of these identifiers can be changed in their models without breaking the PDR.
-- | Relevant models are:
-- |    * model:System
-- |    * model:BrokerServices

------------------------------------------------------------------------------------
-- SYSTEM
------------------------------------------------------------------------------------

sysMe :: String
sysMe = "model:System$Me"

mySystem :: String
mySystem = "model:System$MySystem"

sysUser :: String
sysUser = "model:System$PerspectivesSystem$User"

modelsInUse :: String
modelsInUse = "model:System$PerspectivesSystem$ModelsInUse"

connectedToAMQPBroker :: String
connectedToAMQPBroker = "model:System$PerspectivesSystem$External$ConnectedToAMQPBroker"

userChannel :: String
userChannel = "model:System$PerspectivesSystem$User$Channel"

roleWithId :: String
roleWithId = "model:System$RoleWithId$Id"

------------------------------------------------------------------------------------
-- ROOTCONTEXT
------------------------------------------------------------------------------------
rootContext :: String
rootContext = "model:System$RootContext$External"

rootUser :: String
rootUser = "model:System$RootContext$RootUser"

------------------------------------------------------------------------------------
-- NOTIFICATION
------------------------------------------------------------------------------------
contextWithNotification :: String
contextWithNotification = "model:System$ContextWithNotification"

notifications :: String
notifications = "model:System$ContextWithNotification$Notifications"

notificationMessage :: String
notificationMessage = "model:System$ContextWithNotification$Notifications$Message"


------------------------------------------------------------------------------------
-- MODEL
------------------------------------------------------------------------------------
indexedRole :: String
indexedRole = "model:System$Model$IndexedRole"

indexedRoleName :: String
indexedRoleName = "model:System$Model$IndexedRole$Name"

indexedContext :: String
indexedContext = "model:System$Model$IndexedContext"

indexedContextName :: String
indexedContextName = "model:System$Model$IndexedContext$Name"

modelDescription :: String
modelDescription = "model:System$Model$External$Description"

modelManagementDescription :: String
modelManagementDescription = "model:ModelManagement$ManagedModel$ModelDescription"

modelUrl :: String
modelUrl = "model:System$Model$External$Url"

modelExternal :: String
modelExternal = "model:System$Model$External"

modelExternalModelIdentification :: String
modelExternalModelIdentification = "model:System$Model$External$ModelIdentification"

------------------------------------------------------------------------------------
-- INVITATION
------------------------------------------------------------------------------------
privateChannel :: String
privateChannel = "model:System$Invitation$PrivateChannel"

------------------------------------------------------------------------------------
-- CHANNEL
------------------------------------------------------------------------------------
channelInitiator :: String
channelInitiator = "model:System$Channel$Initiator"

channelDatabase :: String
channelDatabase= "model:System$Channel$External$ChannelDatabaseName"

channelPartner :: String
channelPartner = "model:System$Channel$ConnectedPartner"

------------------------------------------------------------------------------------
-- PHYSICALCONTEXT
------------------------------------------------------------------------------------
addressHost :: String
addressHost = "model:System$PhysicalContext$UserWithAddress$Host"

addressPort :: String
addressPort = "model:System$PhysicalContext$UserWithAddress$Port"

addressRelayHost :: String
addressRelayHost = "model:System$PhysicalContext$UserWithAddress$RelayHost"

addressRelayPort :: String
addressRelayPort = "model:System$PhysicalContext$UserWithAddress$RelayPort"

------------------------------------------------------------------------------------
-- BROKERSERVICES
------------------------------------------------------------------------------------
brokerService :: String
brokerService = "model:BrokerServices$BrokerService"

brokerServiceAccounts :: String
brokerServiceAccounts = "model:BrokerServices$BrokerService$Accounts"

brokerServiceUrl :: String
brokerServiceUrl = "model:BrokerServices$BrokerService$External$Url"

brokerServiceExchange :: String
brokerServiceExchange = "model:BrokerServices$BrokerService$External$Exchange"

brokerContract :: String
brokerContract = "model:BrokerServices$BrokerContract"

accountHolder :: String
accountHolder = "model:BrokerServices$BrokerContract$AccountHolder"

accountHolderName :: String
accountHolderName = "model:BrokerServices$BrokerContract$AccountHolder$AccountName"

accountHolderPassword :: String
accountHolderPassword = "model:BrokerServices$BrokerContract$AccountHolder$AccountPassword"

accountHolderQueueName :: String
accountHolderQueueName = "model:BrokerServices$BrokerContract$AccountHolder$QueueName"

-- "model:Couchdb$ContextInstances"

------------------------------------------------------------------------------------
-- AUTHENTICATION
------------------------------------------------------------------------------------
userWithCredentials :: String
userWithCredentials = "model:BodiesWithAccounts$WithCredentials"

userWithCredentialsPassword :: String
userWithCredentialsPassword = "model:BodiesWithAccounts$WithCredentials$Password"

userWithCredentialsAuthorizedDomain :: String
userWithCredentialsAuthorizedDomain = "model:BodiesWithAccounts$WithCredentials$AuthorizedDomain"