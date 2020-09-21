-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.PerspectivesState where

import Control.Monad.AvarMonadAsk (gets, modify)
import Data.Maybe (Maybe(..))
import Effect.Aff.AVar (AVar)
import Foreign.Object (empty)
import Perspectives.CoreTypes (AssumptionRegister, DomeinCache, MonadPerspectives, PerspectivesState)
import Perspectives.CouchdbState (CouchdbUser)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, delete, new, peek, poke)
import Perspectives.Instances.Environment (Environment, empty, lookup, addVariable, _pushFrame) as ENV
import Prelude (Unit, bind, pure, unit, ($), (<<<), (>>=), discard, void, (+))

newPerspectivesState :: CouchdbUser -> String -> Int -> String -> AVar String -> PerspectivesState
newPerspectivesState uinfo host port password av =
  { rolInstances: new unit
  , contextInstances: new unit
  , domeinCache: new unit
  , queryAssumptionRegister: empty
  , variableBindings: ENV.empty
  -- , publicRepository: "http://www.joopringelberg.nl/cdb/repository/"
  , publicRepository: "http://127.0.0.1:5984/repository/"
  -- CouchdbState
  , userInfo: uinfo
  , couchdbHost: host
  , couchdbPort: port
  , couchdbPassword: password
  , couchdbSessionStarted: false
  , indexedRoles: empty
  , indexedContexts: empty
  , post: Nothing
  , developmentRepository: "http://127.0.0.1:5984/repository/"
  , transactionNumber: 0
  }

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: MonadPerspectives Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: Boolean -> MonadPerspectives Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

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

nextTransactionNumber :: MonadPerspectives Int
nextTransactionNumber = do
  n <- transactionNumber
  void $ modify \(s@{transactionNumber:cn}) -> s {transactionNumber = cn + 1}
  pure n

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
  void $ modify \s@{variableBindings} -> s {variableBindings = (ENV._pushFrame old)}
  r <- computation
  void $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure r

-----------------------------------------------------------
-- FUNCTIONS TO MODIFY GLOBAL UNSAFE STRMAPS IN PERSPECTIVESSTATE
-----------------------------------------------------------
insert :: forall a.
  MonadPerspectives (GLStrMap a) ->
  String ->
  a ->
  MonadPerspectives a
insert g ns av = do
  (dc :: (GLStrMap a)) <- g
  _ <- pure $ (poke dc ns av)
  pure av

lookup :: forall a.
  MonadPerspectives (GLStrMap a) ->
  String ->
  MonadPerspectives (Maybe a)
lookup g k = do
  dc <- g
  pure $ peek dc k

remove :: forall a.
  MonadPerspectives (GLStrMap a) ->
  String ->
  MonadPerspectives (Maybe a)
remove g k = do
  (dc :: (GLStrMap a)) <- g
  ma <- pure $ peek dc k
  _ <- pure $ (delete dc k)
  pure ma
