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

module Perspectives.RunPerspectives where

import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, new)
import Foreign.Object (singleton)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.CouchdbState (UserName(..))
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Prelude (bind, show, ($), (<>))

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a. String -> String -> String -> String -> Int -> String -> MonadPerspectives a
  -> Aff a
runPerspectives userName password systemId host port publicRepo mp = do
  (rf :: AVar PerspectivesState) <- new $
    ((newPerspectivesState
        { _rev: Nothing
        , systemIdentifier: systemId
        , password
        , couchdbUrl: Just (host <> ":" <> show port <> "/")
        , userName: UserName userName
        }
      host
      port
      password
      publicRepo) { indexedRoles = singleton "model:System$Me" (RoleInstance $ "model:System$" <> userName) })
  runReaderT mp rf

runPerspectivesWithState :: forall a. MonadPerspectives a -> (AVar PerspectivesState) -> Aff a
runPerspectivesWithState = runReaderT
