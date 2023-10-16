module Perspectives.ModelDependencies  where

-- | This module contains all Arc identifiers that are used in the PDR source code.
-- | None of these identifiers can be changed in their models without breaking the PDR.
-- | Relevant models are:
-- |    * model://perspectives.domains#System
-- |    * model://perspectives.domains#CouchdbManagement
-- |    * model://perspectives.domains#BrokerServices

------------------------------------------------------------------------------------
-- SYSTEM
------------------------------------------------------------------------------------

systemModelName :: String
systemModelName = "model://perspectives.domains#System"

sysMe :: String
sysMe = "model://perspectives.domains#System$Me"

mySystem :: String
mySystem = "model://perspectives.domains#System$MySystem"

theSystem :: String
theSystem = "model://perspectives.domains#System$PerspectivesSystem"

sysUser :: String
sysUser = "model://perspectives.domains#System$PerspectivesSystem$User"

idProperty :: String
idProperty = "model://perspectives.domains#System$PerspectivesSystem$User$Id"

modelsInUse :: String
modelsInUse = "model://perspectives.domains#System$PerspectivesSystem$ModelsInUse"

modelToRemove :: String
modelToRemove = "model://perspectives.domains#System$PerspectivesSystem$ModelsInUse$ModelToRemove"

connectedToAMQPBroker :: String
connectedToAMQPBroker = "model://perspectives.domains#System$PerspectivesSystem$External$ConnectedToAMQPBroker"

userChannel :: String
userChannel = "model://perspectives.domains#System$PerspectivesSystem$User$Channel"

roleWithId :: String
roleWithId = "model://perspectives.domains#System$RoleWithId$Id"

installer :: String
installer = "model://perspectives.domains#System$PerspectivesSystem$Installer"

------------------------------------------------------------------------------------
-- ROOTCONTEXT
------------------------------------------------------------------------------------
rootContext :: String
rootContext = "model://perspectives.domains#System$RootContext$External"

rootUser :: String
rootUser = "model://perspectives.domains#System$RootContext$RootUser"

------------------------------------------------------------------------------------
-- NOTIFICATION
------------------------------------------------------------------------------------
contextWithNotification :: String
contextWithNotification = "model://perspectives.domains#System$ContextWithNotification"

notifications :: String
notifications = "model://perspectives.domains#System$ContextWithNotification$Notifications"

notificationMessage :: String
notificationMessage = "model://perspectives.domains#System$ContextWithNotification$Notifications$Message"


------------------------------------------------------------------------------------
-- MODEL
------------------------------------------------------------------------------------
indexedRole :: String
indexedRole = "model://perspectives.domains#System$PerspectivesSystem$IndexedRoles"

indexedRoleName :: String
indexedRoleName = "model://perspectives.domains#System$PerspectivesSystem$IndexedRoles$Name"

indexedContext :: String
indexedContext = "model://perspectives.domains#System$PerspectivesSystem$IndexedContexts"

indexedContextName :: String
indexedContextName = "model://perspectives.domains#System$PerspectivesSystem$IndexedContexts$Name"

modelDescription :: String
modelDescription = "model://perspectives.domains#System$Model$External$Description"

modelManagementDescription :: String
modelManagementDescription = "model://perspectives.domains#ModelManagement$ManagedModel$ModelDescription"

modelUrl :: String
modelUrl = "model://perspectives.domains#System$Model$External$Url"

modelExternal :: String
modelExternal = "model://perspectives.domains#System$Model$External"

modelManifest :: String
modelManifest = "model://perspectives.domains#System$VersionedModelManifest$External"

domeinFileName :: String
domeinFileName = "model://perspectives.domains#System$ModelManifest$External$DomeinFileName"

versionedDomeinFileName :: String
versionedDomeinFileName = "model://perspectives.domains#System$VersionedModelManifest$External$DomeinFileName"

patch :: String
patch = "model://perspectives.domains#System$VersionedModelManifest$External$Patch"

build :: String
build = "model://perspectives.domains#System$VersionedModelManifest$External$Build"

installedPatch :: String
installedPatch =  "model://perspectives.domains#System$PerspectivesSystem$ModelsInUse$InstalledPatch"

installedBuild :: String
installedBuild =  "model://perspectives.domains#System$PerspectivesSystem$ModelsInUse$InstalledBuild"

------------------------------------------------------------------------------------
-- COUCHDBMANAGEMENT
------------------------------------------------------------------------------------
versionToInstall :: String
versionToInstall = "model://perspectives.domains#CouchdbManagement$ModelManifest$External$VersionToInstall"

------------------------------------------------------------------------------------
-- INVITATION
------------------------------------------------------------------------------------
privateChannel :: String
privateChannel = "model://perspectives.domains#System$Invitation$PrivateChannel"

------------------------------------------------------------------------------------
-- CHANNEL
------------------------------------------------------------------------------------
channelInitiator :: String
channelInitiator = "model://perspectives.domains#System$Channel$Initiator"

channel :: String
channel = "model://perspectives.domains#System$Channel"

channelDatabase :: String
channelDatabase= "model://perspectives.domains#System$Channel$External$ChannelDatabaseName"

channelPartner :: String
channelPartner = "model://perspectives.domains#System$Channel$ConnectedPartner"

------------------------------------------------------------------------------------
-- PHYSICALCONTEXT
------------------------------------------------------------------------------------
addressHost :: String
addressHost = "model://perspectives.domains#System$PhysicalContext$UserWithAddress$Host"

addressPort :: String
addressPort = "model://perspectives.domains#System$PhysicalContext$UserWithAddress$Port"

addressRelayHost :: String
addressRelayHost = "model://perspectives.domains#System$PhysicalContext$UserWithAddress$RelayHost"

addressRelayPort :: String
addressRelayPort = "model://perspectives.domains#System$PhysicalContext$UserWithAddress$RelayPort"

------------------------------------------------------------------------------------
-- BROKERSERVICES
------------------------------------------------------------------------------------
brokerService :: String
brokerService = "model://perspectives.domains#BrokerServices$BrokerService"

brokerServiceAccounts :: String
brokerServiceAccounts = "model://perspectives.domains#BrokerServices$BrokerService$Accounts"

brokerEndpoint :: String
brokerEndpoint = "model://perspectives.domains#BrokerServices$BrokerService$External$Url"

brokerServiceExchange :: String
brokerServiceExchange = "model://perspectives.domains#BrokerServices$BrokerService$External$Exchange"

brokerContract :: String
brokerContract = "model://perspectives.domains#BrokerServices$BrokerContract"

accountHolder :: String
accountHolder = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder"

accountHolderName :: String
accountHolderName = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder$AccountName"

accountHolderPassword :: String
accountHolderPassword = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder$AccountPassword"

accountHolderQueueName :: String
accountHolderQueueName = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder$QueueName"

-- "model:perspectives.domains#Couchdb$ContextInstances"

------------------------------------------------------------------------------------
-- AUTHENTICATION
------------------------------------------------------------------------------------
userWithCredentials :: String
userWithCredentials = "model://perspectives.domains#System$WithCredentials"

userWithCredentialsUsername :: String
userWithCredentialsUsername = "model://perspectives.domains#System$WithCredentials$UserName"

userWithCredentialsPassword :: String
userWithCredentialsPassword = "model://perspectives.domains#System$WithCredentials$Password"

userWithCredentialsAuthorizedDomain :: String
userWithCredentialsAuthorizedDomain = "model://perspectives.domains#System$WithCredentials$AuthorizedDomain"