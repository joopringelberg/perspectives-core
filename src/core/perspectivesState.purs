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
import Data.Maybe (Maybe(..))
import Data.Map (empty) as Map
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)
import Foreign.Object (empty)
import LRUCache (Cache, clear, defaultCreateOptions, defaultGetOptions, get, newCache, set, delete)
import Perspectives.AMQP.Stomp (StompClient)
import Perspectives.CoreTypes (AssumptionRegister, BrokerService, DomeinCache, MonadPerspectives, PerspectivesState, RepeatingTransaction)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Instances.Environment (Environment, empty, lookup, addVariable, _pushFrame) as ENV
import Perspectives.Persistence.API (PouchdbUser, Url)
import Prelude (Unit, bind, discard, pure, void, ($), (+), (<<<), (>>=))

newPerspectivesState :: PouchdbUser -> Url -> AVar Int -> AVar RepeatingTransaction -> PerspectivesState
newPerspectivesState uinfo publicRepo transFlag transactionWithTiming =
  { rolInstances: newCache defaultCreateOptions
  , contextInstances: newCache defaultCreateOptions
  , domeinCache: newCache defaultCreateOptions
  , queryAssumptionRegister: empty
  , variableBindings: ENV.empty
  , publicRepository: publicRepo
  , userInfo: uinfo
  , indexedRoles: empty
  , indexedContexts: empty
  , post: Nothing
  , developmentRepository: "http://127.0.0.1:5984/repository/"
  , transactionNumber: 0
  , brokerService: Nothing
  , stompClient: Nothing
  , databases: empty
  -- , couchdbUrl: Nothing -- For using Couchdb as backend for Pouchdb.
  , warnings: []
  , transactionFlag: transFlag
  , transactionWithTiming
  , transactionFibers: Map.empty
  }

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
domeinCache :: MonadPerspectives DomeinCache
domeinCache = gets _.domeinCache

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

publicRepository :: MonadPerspectives String
publicRepository = gets _.publicRepository

developmentRepository :: MonadPerspectives String
developmentRepository = gets _.developmentRepository

transactionNumber :: MonadPerspectives Int
transactionNumber = gets _.transactionNumber

transactionFlag :: MonadPerspectives (AVar Int)
transactionFlag = gets _.transactionFlag

nextTransactionNumber :: MonadPerspectives Int
nextTransactionNumber = do
  n <- transactionNumber
  void $ modify \(s@{transactionNumber:cn}) -> s {transactionNumber = cn + 1}
  pure n

brokerService :: MonadPerspectives (Maybe BrokerService)
brokerService = gets _.brokerService

setBrokerService :: Maybe BrokerService -> MonadPerspectives Unit
setBrokerService bs = modify \s -> s {brokerService = bs}

stompClient :: MonadPerspectives (Maybe StompClient)
stompClient = gets _.stompClient

setStompClient :: StompClient -> MonadPerspectives Unit
setStompClient bs = modify \s -> s {stompClient = Just bs}

getWarnings :: MonadPerspectives (Array String)
getWarnings = gets _.warnings

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
