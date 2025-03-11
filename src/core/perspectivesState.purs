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

module Perspectives.PerspectivesState where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.String (Pattern(..), stripSuffix)
import Effect.Aff.AVar (AVar, put, read)
import Effect.Class (liftEffect)
import Foreign.Object (empty, singleton)
import Foreign.Object (lookup, insert, delete) as OBJ
import LRUCache (Cache, clear, defaultCreateOptions, defaultGetOptions, delete, get, newCache, set)
import Perspectives.AMQP.Stomp (StompClient)
import Perspectives.CoreTypes (AssumptionRegister, BrokerService, ContextInstances, DomeinCache, IndexedResource, IntegrityFix, JustInTimeModelLoad, PerspectivesState, QueryInstances, RepeatingTransaction, RolInstances, RuntimeOptions, TranslationTable, MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Instances.Environment (Environment, _pushFrame, addVariable, empty, lookup) as ENV
import Perspectives.Persistence.API (PouchdbUser)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (Credential(..))
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), RoleInstance)
import Perspectives.ResourceIdentifiers (createDefaultIdentifier)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (+), (<<<), (>>=), (<>))

newPerspectivesState :: 
  PouchdbUser -> 
  AVar Boolean -> 
  AVar RepeatingTransaction -> 
  AVar JustInTimeModelLoad -> 
  RuntimeOptions -> 
  AVar BrokerService -> 
  AVar IndexedResource -> 
  AVar IntegrityFix -> 
  String -> PerspectivesState
newPerspectivesState uinfo transFlag transactionWithTiming modelToLoad runtimeOptions brokerService indexedResourceToCreate missingResource currentLanguage =
  { rolInstances: newCache defaultCreateOptions
  , contextInstances: newCache defaultCreateOptions
  , domeinCache: newCache defaultCreateOptions
  , queryCache: newCache defaultCreateOptions
  , queryAssumptionRegister: empty
  , variableBindings: ENV.empty
  , systemIdentifier: uinfo.systemIdentifier
  , perspectivesUser: PerspectivesUser $ createDefaultIdentifier uinfo.perspectivesUser
  , couchdbUrl: uinfo.couchdbUrl 
  , couchdbCredentials: case uinfo.couchdbUrl, uinfo.password, uinfo.userName of
      Just url, Just password, Just userName -> singleton url (Credential userName password)
      _, _, _ -> empty
  , indexedRoles: empty
  , indexedContexts: empty
  , post: Nothing
  , developmentRepository: "http://127.0.0.1:5984/repository/"
  , transactionNumber: 0
  , transactionLevel: ""
  , brokerService
  , stompClient: Nothing
  , databases: empty
  , warnings: []
  , transactionFlag: transFlag 
  , transactionWithTiming
  , modelToLoad
  , transactionFibers: Map.empty
  , typeToStorage: Map.empty
  , publicRolesJustLoaded: []
  , runtimeOptions
  , entitiesToBeStored: []
  , indexedResourceToCreate
  , missingResource
  , currentLanguage
  , translations: empty
  }

defaultRuntimeOptions :: RuntimeOptions
defaultRuntimeOptions = 
  { isFirstInstallation: true
  , useSystemVersion: null
  , privateKey: Nothing
  , publicKey: Nothing
  , myContextsVersion: "1.0.0"
  }

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
domeinCache :: MonadPerspectives DomeinCache
domeinCache = gets _.domeinCache

contextCache :: MonadPerspectives ContextInstances
contextCache = gets _.contextInstances

roleCache :: MonadPerspectives RolInstances
roleCache = gets _.rolInstances

queryCache :: MonadPerspectives QueryInstances
queryCache = gets _.queryCache

clearQueryCache :: MonadPerspectives Unit
clearQueryCache = queryCache >>= liftEffect <<< clear

domeinCacheLookup :: String -> MonadPerspectives (Maybe (AVar DomeinFile))
domeinCacheLookup = lookup domeinCache

domeinCacheInsert :: String -> AVar DomeinFile -> MonadPerspectives (AVar DomeinFile)
domeinCacheInsert = insert domeinCache

domeinCacheRemove :: String -> MonadPerspectives (Maybe (AVar DomeinFile))
domeinCacheRemove = remove domeinCache

queryAssumptionRegister :: MonadPerspectives AssumptionRegister
queryAssumptionRegister = gets _.queryAssumptionRegister

queryAssumptionRegisterModify :: (AssumptionRegister -> AssumptionRegister) -> MonadPerspectives Unit
queryAssumptionRegisterModify f = modify \(s@{queryAssumptionRegister: q}) -> s {queryAssumptionRegister = f q}

developmentRepository :: MonadPerspectives String
developmentRepository = gets _.developmentRepository

transactionNumber :: MonadPerspectives Int
transactionNumber = gets _.transactionNumber

transactionFlag :: MonadPerspectives (AVar Boolean)
transactionFlag = gets _.transactionFlag

nextTransactionNumber :: MonadPerspectives Int
nextTransactionNumber = do
  n <- transactionNumber
  void $ modify \(s@{transactionNumber:cn}) -> s {transactionNumber = cn + 1}
  pure n

-- | Increases with two spaces every time we run an embedded transaction.
transactionLevel :: MonadPerspectives String
transactionLevel = gets _.transactionLevel

-- | Increases the transaction level with two spaces.
increaseTransactionLevel :: MonadPerspectives Unit
increaseTransactionLevel = modify \s -> s {transactionLevel = s.transactionLevel <> "  "}

-- | Decreases the transaction level with two spaces.
decreaseTransactionLevel :: MonadPerspectives Unit
decreaseTransactionLevel = modify \s -> s {transactionLevel = case stripSuffix (Pattern "  ") s.transactionLevel of
  Just s' -> s'
  Nothing -> "" }

getBrokerService :: MonadPerspectives BrokerService
getBrokerService = gets _.brokerService >>= lift <<< read

setBrokerService :: Maybe BrokerService -> MonadPerspectives Unit
setBrokerService bs = case bs of
  Just bs' -> gets _.brokerService >>= lift <<< put bs'
  Nothing -> pure unit

stompClient :: MonadPerspectives (Maybe StompClient)
stompClient = gets _.stompClient

setStompClient :: StompClient -> MonadPerspectives Unit
setStompClient bs = modify \s -> s {stompClient = Just bs}

getWarnings :: MonadPerspectives (Array String)
getWarnings = gets _.warnings

getModelToLoad :: MonadPerspectives (AVar JustInTimeModelLoad)
getModelToLoad = gets _.modelToLoad

getPublicRolesJustLoaded :: MonadPerspectives (Array RoleInstance)
getPublicRolesJustLoaded = gets _.publicRolesJustLoaded

clearPublicRolesJustLoaded :: MonadPerspectives Unit
clearPublicRolesJustLoaded = modify \s -> s {publicRolesJustLoaded = []}

getIndexedResourceToCreate :: MonadPerspectives (AVar IndexedResource)
getIndexedResourceToCreate = gets _.indexedResourceToCreate

getMissingResource :: MonadPerspectives (AVar IntegrityFix)
getMissingResource = gets _.missingResource

-- | The domain argument is the string version of the domain identifier.
getTranslationTable :: String -> MonadPerspectives (Maybe TranslationTable)
getTranslationTable domain = gets _.translations >>= pure <<< OBJ.lookup domain

setTranslationTable :: String -> TranslationTable -> MonadPerspectives Unit
setTranslationTable domain tt = modify \s -> s {translations = OBJ.insert domain tt s.translations}

removeTranslationTable :: String -> MonadPerspectives Unit
removeTranslationTable domain = modify \s -> s {translations = OBJ.delete domain s.translations}

getCurrentLanguage :: MonadPerspectives String
getCurrentLanguage = gets _.currentLanguage

setCurrentLanguage :: String -> MonadPerspectives Unit
setCurrentLanguage lang = modify \s -> s {currentLanguage = lang}

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_models")

-----------------------------------------------------------
-- PERSPECTIVESUSER
-----------------------------------------------------------
-- | Returns the role in TheWorld that fills SocialEnvironment$Me (and this instance in turn fills PerspectivesSystem$User).
-- | This is dependent on model://perspectives.domains#System.
getPerspectivesUser :: MonadPerspectives PerspectivesUser
getPerspectivesUser = gets _.perspectivesUser

-----------------------------------------------------------
-- RESETTING CACHES
-----------------------------------------------------------
resetDomeinCache :: MonadPerspectives Unit
resetDomeinCache = gets _.domeinCache >>= liftEffect <<< clear

resetRoleInstances :: MonadPerspectives Unit
resetRoleInstances = gets _.rolInstances >>= liftEffect <<< clear

resetContextInstances :: MonadPerspectives Unit
-- resetContextInstances = modify \s -> s {contextInstances = new unit}
resetContextInstances = gets _.contextInstances >>= liftEffect <<< clear

resetCaches :: MonadPerspectives Unit
resetCaches = do
  resetDomeinCache
  resetRoleInstances
  resetContextInstances

resetWarnings :: MonadPerspectives Unit
resetWarnings = modify \s -> s { warnings = []}
-----------------------------------------------------------
-- FUNCTIONS TO HANDLE VARIABLE BINDINGS
-----------------------------------------------------------
getVariableBindings :: MonadPerspectives (ENV.Environment (Array String))
getVariableBindings = gets _.variableBindings

addBinding :: String -> Array String -> MonadPerspectives Unit
addBinding varName qfd = void $ modify \s@{variableBindings} -> s {variableBindings = ENV.addVariable varName qfd variableBindings}

lookupVariableBinding :: String -> MonadPerspectives (Maybe (Array String))
lookupVariableBinding varName = getVariableBindings >>= pure <<< (ENV.lookup varName)

withFrame :: forall a. MonadPerspectives a -> MonadPerspectives a
withFrame computation = do
  old <- getVariableBindings
  void $ modify \s -> s {variableBindings = (ENV._pushFrame old)}
  r <- computation
  void $ modify \s -> s {variableBindings = old}
  pure r

pushFrame :: MonadPerspectives (ENV.Environment (Array String))
pushFrame = do
  old <- getVariableBindings
  void $ modify \s -> s {variableBindings = (ENV._pushFrame old)}
  pure old

restoreFrame :: ENV.Environment (Array String) -> MonadPerspectives Unit
restoreFrame frame = void $ modify \s -> s {variableBindings = frame}

-----------------------------------------------------------
-- FUNCTIONS TO MODIFY LRUCACHES IN PERSPECTIVESSTATE
-----------------------------------------------------------
insert :: forall a.
  MonadPerspectives (Cache a) ->
  String ->
  a ->
  MonadPerspectives a
insert g ns av = do
  (dc :: (Cache a)) <- g
  _ <- liftEffect $ set ns av Nothing dc
  -- _ <- pure $ (poke dc ns av)
  pure av

lookup :: forall a.
  MonadPerspectives (Cache a) ->
  String ->
  MonadPerspectives (Maybe a)
lookup g k = do
  dc <- g
  -- pure $ peek dc k
  liftEffect $ get k defaultGetOptions dc

remove :: forall a.
  MonadPerspectives (Cache a) ->
  String ->
  MonadPerspectives (Maybe a)
remove g k = do
  (dc :: (Cache a)) <- g
  -- ma <- pure $ peek dc k
  ma <- liftEffect $ get k defaultGetOptions dc
  -- _ <- pure $ (delete dc k)
  _ <- liftEffect $ delete k dc
  pure ma
