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
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Error, forkAff, runAff)
import Effect.Aff.AVar (AVar, new)
import Effect.Console (log)
-- import Perspectives.Api (setupApi, setupTcpApi)
import Perspectives.ParseFromCommandLine (parseFromCommandLine)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupUser (setupUser)
import Prelude (Unit, bind, pure, ($), (<>), show, void, discard)

main :: Effect Unit
-- main = runPDR
main = parseFromCommandLine

-- NOTE: For release v0.1.0, I've commented out the original main function because it requires the perspectivesproxy
-- While this is available, it is webpacked for usage in the browser and will not run from the command line
-- (The PDR is not meant for command line usage).

-- runPDR :: Effect Unit
-- runPDR = void $ runAff handleError do
--   -- TODO: retrieve the couchdb credentials from the trusted cluster or through the user interface.
--   usr <- pure "cor"
--   pwd <- pure "geheim"
--   url <- pure "http://127.0.0.1:5984/"
--   (av :: AVar String) <- new "This value will be removed on first authentication!"
--   state <- new $ newPerspectivesState {userName: usr, couchdbPassword: pwd, couchdbBaseURL: url} av
--   void $ forkAff $ runPerspectivesWithState f state
--   void $ forkAff $ runPerspectivesWithState setupTcpApi state
--   where
--     f = do
--       void $ setupUser
--       -- addComputedTripleGetters
--       setupApi

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"
