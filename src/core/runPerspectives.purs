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

module Perspectives.RunPerspectives where

import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, new)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.CouchdbState (CouchdbUser(..), UserName(..))
import Perspectives.PerspectivesState (newPerspectivesState)
import Prelude (bind, ($))

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a. String -> String -> MonadPerspectives a
  -> Aff a
runPerspectives userName password mp = do
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  (rf :: AVar PerspectivesState) <- new $
    newPerspectivesState (CouchdbUser
      { userName: UserName userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"
      , userIdentifier: userName
      , _rev: Nothing})
      av
  runReaderT mp rf

runPerspectivesWithState :: forall a. MonadPerspectives a -> (AVar PerspectivesState) -> Aff a
runPerspectivesWithState = runReaderT
