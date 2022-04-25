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

module Perspectives.SetupTcpChannel where

import Control.Aff.Sockets (ConnectionProcess, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Process, runProcess, transform, ($$), ($~))
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Foreign (Foreign, MultipleErrors, unsafeToForeign)
import Perspectives.ApiTypes (RequestType(..)) as Api
import Perspectives.ApiTypes (Request(..), Response)
import Perspectives.CoreTypes (MonadPerspectives)
import Prelude (Unit, negate, show, (<<<), (<>))

-- | Create a process that consumes requests from a producer that connects to a source over TCP.
setupTcpApi :: (Consumer Request MonadPerspectives Unit) -> MonadPerspectives Unit
setupTcpApi consumeRequest = runProcess server
  where

    server :: Process MonadPerspectives Unit
    server = (connectionProducer defaultTCPOptions) $$ (connectionConsumer connectionHandler)

    connectionHandler :: ConnectionProcess MonadPerspectives
    connectionHandler connection =
      (dataProducer connection $~ (forever (transform addCallback))) $$ consumeRequest
      where
        -- Here we add a callback to a Request (we don't receive a callback over TCP!).
        addCallback :: (Either MultipleErrors Request) -> Request
        addCallback (Right (Request r@{request, subject, object, predicate, corrId, contextDescription, rolDescription, authoringRole})) =
          Request
            { request: request
            , subject
            , predicate
            , object
            , corrId
            , reactStateSetter: Just setter
            , contextDescription
            , rolDescription
            , authoringRole }
        addCallback (Left e) =
          Request
            { request: Api.WrongRequest
            , subject: ("Perspectives could not decode this request: '" <> show e <> "'")
            , predicate: ""
            , object: ""
            , corrId: -1
            , reactStateSetter: Just setter
            , contextDescription: unsafeToForeign ""
            , rolDescription: Nothing
            , authoringRole: Nothing}

        setter :: Foreign
        setter = unsafeToForeign (launchAff_ <<< (writeData connection :: Response -> Aff Boolean))

        -- marshallRequest (Left e) = WrongRequest ("Perspectives does not recognise this request: '" <> show e <> "'") (launchAff_ <<< writeData connection) "universalErrorHandler"
