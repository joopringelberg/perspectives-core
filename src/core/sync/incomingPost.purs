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

module Perspectives.Sync.IncomingPost where

import Control.Coroutine (Consumer, await, runProcess, ($$))
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (index, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign (MultipleErrors)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesExtraState)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistence.API (MonadPouchdb, deleteDocument)
import Perspectives.Persistence.State (getCouchdbCredentials)
import Perspectives.Persistence.Types (Credential(..))
import Perspectives.Persistent.ChangesFeed (DocProducer, EventSource, createEventSource, docProducer)
import Perspectives.PerspectivesState (transactionLevel)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.Sync.Channel (postDbName)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>), (<))

type URL = String

incomingPost :: URL -> MonadPerspectives Unit
incomingPost url = do
  -- get host and port
  base <- getCouchdbBaseURLWithCredentials url
  -- get the post database
  post <- postDbName
  -- Create an EventSource
  (es :: EventSource) <- liftEffect $ createEventSource (base <> post) Nothing true
  -- Save in state, so we can close it.
  void $ modify \s -> s {post = Just es}
  -- Produce new Transaction documents
  (transactionProducer :: DocProducer (PerspectivesExtraState) TransactionForPeer) <- pure $ docProducer es
  -- Handle them.
  void $ runProcess $ transactionProducer $$ transactionConsumer post
  where
    transactionConsumer :: String -> Consumer (Either MultipleErrors (Tuple String (Maybe TransactionForPeer))) MonadPerspectives Unit
    transactionConsumer database = forever do
      change <- await
      case change of
        -- TODO: iets met deze errors doen? Loggen, naar support sturen...
        Left me -> pure unit
        Right (Tuple _ Nothing) -> pure unit
        Right (Tuple id (Just t)) -> do
          -- Delete the document
          _ <- lift $ deleteDocument database id Nothing
          padding <- lift transactionLevel
          log $ padding <> "Executing incoming post transaction"
          lift (runMonadPerspectivesTransaction'
            false
            (ENR $ EnumeratedRoleType sysUser)
            (executeTransaction t))

-----------------------------------------------------------
-- GETCOUCHDBBASEURLWITHCREDENTIALS
-----------------------------------------------------------
-- | Returns a Url in the format http://user:password@{domain}:{port}/
getCouchdbBaseURLWithCredentials :: forall f . String -> MonadPouchdb f String
getCouchdbBaseURLWithCredentials url = do
  mcredential :: Maybe Credential <- getCouchdbCredentials
  case mcredential of 
    Just (Credential username password) -> do 
      case match domainRegex url of
        Nothing -> throwError (error $ "getCouchdbBaseURLWithCredentials: couchdbHost not well-formed: " <> url)
        Just matches | length matches < 3 -> throwError (error $ "getCouchdbBaseURLWithCredentials: couchdbHost not well-formed: " <> url)
        Just matches -> case (unsafePartial (fromJust (index matches 1))), (unsafePartial (fromJust (index matches 2))) of
          Just scheme, Just domain -> pure $ scheme <> username <> ":" <> password <> "@" <> url <> "/"
          _, _ -> throwError (error $ "getCouchdbBaseURLWithCredentials: couchdbHost not well-formed: " <> url)
    Nothing -> throwError (error $ "getCouchdbBaseURLWithCredentials: no password for " <> url)
  where
    domainRegex :: Regex
    domainRegex = unsafeRegex "^(https?\\:\\/\\/)(.*)$" noFlags
