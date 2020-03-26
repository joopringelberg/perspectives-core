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

module Main where
import Control.Coroutine (Consumer, await, runProcess, ($$))
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Error, forkAff, runAff)
import Effect.Aff.AVar (AVar, new)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Perspectives.Api (setupApi, setupTcpApi)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesExtraState)
import Perspectives.Couchdb.ChangesFeed (DocProducer, createEventSource, docProducer)
import Perspectives.CouchdbState (CouchdbUser(..), UserName(..))
import Perspectives.Extern.Couchdb (addExternalFunctions) as ExternalCouchdb
import Perspectives.LocalAuthentication (AuthenticationResult(..))
import Perspectives.LocalAuthentication (authenticate) as LA
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupCouchdb (partyMode, setupCouchdbForFirstUser)
import Perspectives.SetupUser (setupUser)
import Perspectives.Sync.Channel (postDbName)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.Transaction (Transaction)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<>))

-- | Runs the PDR with default credentials. Used for testing clients without authentication.
main :: Effect Unit
main = runPDR "cor" "geheim" "cor"
-- main = parseFromCommandLine

-- NOTE: For release v0.1.0, I've commented out the original main function because it requires the perspectivesproxy
-- While this is available, it is webpacked for usage in the browser and will not run from the command line
-- (The PDR is not meant for command line usage).

-- | Execute the Perspectives Distributed Runtime by creating a listener to the internal channel.
  -- TODO. Bewaar de fiber in state en kill om uit te loggen?
runPDR :: String -> String -> String -> Effect Unit
runPDR usr pwd ident = void $ runAff handleError do
  url <- pure "http://127.0.0.1:5984/"
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  state <- new $ newPerspectivesState (CouchdbUser
    { userName: UserName usr
    , couchdbPassword: pwd
    , couchdbHost: "http://127.0.0.1"
    , couchdbPort: 5984
    , systemIdentifier: ident
    , _rev: Nothing}) av
  void $ forkAff $ runPerspectivesWithState api state
  void $ forkAff $ runPerspectivesWithState incomingPost state
  -- void $ forkAff $ runPerspectivesWithState setupTcpApi state
  where
    api :: MonadPerspectives Unit
    api = do
      void $ setupUser
      ExternalCouchdb.addExternalFunctions
      setupApi

    incomingPost :: MonadPerspectives Unit
    incomingPost = do
      -- get host and port
      base <- getCouchdbBaseURL
      -- get the post database
      post <- postDbName
      -- Create an EventSource
      es <- liftEffect $ createEventSource (base <> post) Nothing true
      -- Produce new Transaction documents
      (transactionProducer :: DocProducer PerspectivesExtraState Transaction) <- pure $ docProducer es
      -- Handle them.
      void $ runProcess $ transactionProducer $$ transactionConsumer

    transactionConsumer :: Consumer (Either MultipleErrors (Maybe Transaction)) MonadPerspectives Unit
    transactionConsumer = forever do
      change <- await
      case change of
        Left me -> pure unit
        Right Nothing -> pure unit
        Right (Just t) -> lift $ executeTransaction t

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"

-- | The main entrance to the PDR for client programs. Runs the PDR on succesfull login.
-- | When Couchdb is in Party Mode, initialises the first admin.
authenticate :: String -> String -> (Int -> Effect Unit) -> Effect Unit
authenticate usr pwd callback = void $ runAff handler do
  pm <- partyMode
  if pm
    then setupCouchdbForFirstUser usr pwd
    else pure unit
  (LA.authenticate usr pwd)
  where
    handler :: Either Error AuthenticationResult -> Effect Unit
    handler (Left e) = log $ "An error condition: " <> (show e)
    handler (Right r) = case r of
      UnknownUser -> callback 0
      WrongPassword -> callback 1
      OK (CouchdbUser{systemIdentifier})-> do
        runPDR usr pwd systemIdentifier
        callback 2
