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

module Perspectives.Sync.IncomingPost where

import Control.Coroutine (Consumer, await, runProcess, ($$))
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesExtraState)
import Perspectives.Couchdb.ChangesFeed (DocProducer, EventSource, createEventSource, docProducer)
import Perspectives.Couchdb.Databases (deleteDocument_)
import Perspectives.Persistence.API (PouchdbExtraState)
import Perspectives.Sync.Channel (postDbName)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Perspectives.User (getCouchdbBaseURLWithCredentials)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>))

incomingPost :: MonadPerspectives Unit
incomingPost = do
  -- get host and port
  base <- getCouchdbBaseURLWithCredentials
  -- get the post database
  post <- postDbName
  -- Create an EventSource
  (es :: EventSource) <- liftEffect $ createEventSource (base <> post) Nothing true
  -- Save in state, so we can close it.
  void $ modify \s -> s {post = Just es}
  -- Produce new Transaction documents
  (transactionProducer :: DocProducer (PouchdbExtraState PerspectivesExtraState) TransactionForPeer) <- pure $ docProducer es
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
          _ <- lift $ deleteDocument_ database id
          lift $ executeTransaction t
