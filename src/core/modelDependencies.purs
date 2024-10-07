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

perspectivesUsers :: String
perspectivesUsers = "model://perspectives.domains#System$TheWorld$PerspectivesUsers" 

identifiableLastName :: String
identifiableLastName = "model://perspectives.domains#System$Identifiable$LastName" 

identifiableFirstName :: String
identifiableFirstName = "model://perspectives.domains#System$Identifiable$FirstName" 

theWorld :: String
theWorld = "model://perspectives.domains#System$TheWorld" 

theWorldInitializer :: String
theWorldInitializer = "model://perspectives.domains#System$TheWorld$Initializer" 

perspectivesUsersCancelled :: String
perspectivesUsersCancelled = "model://perspectives.domains#System$Identifiable$Cancelled" 

perspectivesUsersPublicKey :: String
perspectivesUsersPublicKey = "model://perspectives.domains#System$Identifiable$PublicKey" 

socialEnvironment :: String
socialEnvironment = "model://perspectives.domains#System$SocialEnvironment" 

mySocialEnvironment :: String
mySocialEnvironment = "model://perspectives.domains#System$MySocialEnvironment"

socialEnvironmentPersons :: String
socialEnvironmentPersons = "model://perspectives.domains#System$SocialEnvironment$Persons" 

socialEnvironmentMe :: String
socialEnvironmentMe = "model://perspectives.domains#System$SocialEnvironment$Me" 

indexedSocialMe :: String
indexedSocialMe = "model://perspectives.domains#System$SocialMe"

systemIdentityValues :: String
systemIdentityValues = "model://perspectives.domains#System$TheWorld$SystemIdentities" 

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

currentSystemHour :: String
currentSystemHour = "model://perspectives.domains#System$PerspectivesSystem$External$CurrentHour"

currentSystemDate :: String
currentSystemDate = "model://perspectives.domains#System$PerspectivesSystem$External$CurrentDate"

cardClipBoard :: String
cardClipBoard = "model://perspectives.domains#System$PerspectivesSystem$External$CardClipBoard"

userChannel :: String
userChannel = "model://perspectives.domains#System$PerspectivesSystem$User$Channel"

roleWithId :: String
roleWithId = "model://perspectives.domains#System$RoleWithId$Id"

installer :: String
installer = "model://perspectives.domains#System$PerspectivesSystem$Installer"

baseRepository :: String
baseRepository = "model://perspectives.domains#System$PerspectivesSystem$BaseRepository"

------------------------------------------------------------------------------------
-- ROOTCONTEXT
------------------------------------------------------------------------------------
rootContext :: String
rootContext = "model://perspectives.domains#System$RootContext$External"

rootUser :: String
rootUser = "model://perspectives.domains#System$RootContext$RootUser"

------------------------------------------------------------------------------------
-- INVITATION
------------------------------------------------------------------------------------
invitation :: String
invitation = "model://perspectives.domains#System$Invitation$External"

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

indexedContextFuzzies :: String
indexedContextFuzzies = "model://perspectives.domains#System$PerspectivesSystem$IndexedContextsFuzzies"

indexedContextName :: String
indexedContextName = "model://perspectives.domains#System$PerspectivesSystem$IndexedContexts$Name"

modelManifest :: String
modelManifest = "model://perspectives.domains#System$ModelManifest$External"

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
couchdbManagementModelName :: String
couchdbManagementModelName = "model://perspectives.domains#CouchdbManagement"

versionToInstall :: String
versionToInstall = "model://perspectives.domains#CouchdbManagement$ModelManifest$External$VersionToInstall"

modelURI :: String
modelURI = "model://perspectives.domains#CouchdbManagement$VersionedModelManifest$External$ModelURI"

------------------------------------------------------------------------------------
-- INVITATION
------------------------------------------------------------------------------------

-- TODO, IMPORTANT! this type is no longer implemented in model:System, but it is used in code.
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

myBrokers :: String
myBrokers = "model://perspectives.domains#BrokerServices$MyBrokers"

brokerServiceAccounts :: String
brokerServiceAccounts = "model://perspectives.domains#BrokerServices$BrokerService$Accounts"

brokerServiceContractInUse :: String
brokerServiceContractInUse = "model://perspectives.domains#BrokerServices$BrokerServices$ContractInUse"

brokerEndpoint :: String
brokerEndpoint = "model://perspectives.domains#BrokerServices$BrokerContract$External$Url"

brokerServiceExchange :: String
brokerServiceExchange = "model://perspectives.domains#BrokerServices$BrokerContract$External$Exchange"

brokerContract :: String
brokerContract = "model://perspectives.domains#BrokerServices$BrokerContract"

accountHolder :: String
accountHolder = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder"

accountHolderName :: String
accountHolderName = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder$AccountName"

accountHolderPassword :: String
accountHolderPassword = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder$AccountPassword"

accountHolderQueueName :: String
accountHolderQueueName = "model://perspectives.domains#BrokerServices$BrokerContract$External$CurrentQueueName"

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

------------------------------------------------------------------------------------
-- BODIESWITHACCOUNTS
------------------------------------------------------------------------------------
bodiesWithAccountsModelName :: String
bodiesWithAccountsModelName = "model://perspectives.domains#BodiesWithAccounts"

------------------------------------------------------------------------------------
-- SHAREDFILESERVICES
------------------------------------------------------------------------------------
sharedFileServices :: String
sharedFileServices = "model://perspectives.domains#SharedFileServices"

mySharedFileServices :: String
mySharedFileServices = "model://perspectives.domains#SharedFileServices$MySharedFileServices"

actualSharedFileServer :: String
actualSharedFileServer = "model://perspectives.domains#SharedFileServices$SharedFileServices$ActualSharedFileServer"

fileShareCredentials :: String
fileShareCredentials = "model://perspectives.domains#SharedFileServices$SharedFileService$FileShareCredentials"