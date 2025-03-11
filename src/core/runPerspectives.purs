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
import Effect.Aff.AVar (AVar, empty, new)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.ModelTranslation (getCurrentLanguageFromIDB)
import Perspectives.PerspectivesState (defaultRuntimeOptions, newPerspectivesState)
import Prelude (bind, show, (<<<), (<>), (>>=))

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a. String -> String -> String -> String -> String -> Int -> MonadPerspectives a
  -> Aff a
runPerspectives userName password perspectivesUser systemId host port mp = do
  transactionFlag <- new true
  brokerService <- empty
  transactionWithTiming <- empty
  modelToLoad <- empty
  indexedResourceToCreate <- empty
  missingResource <- empty
  (rf :: AVar PerspectivesState) <- getCurrentLanguageFromIDB >>= new <<< 
    ((newPerspectivesState
        { systemIdentifier: systemId
        , perspectivesUser: perspectivesUser
        , userName: Just userName
        , password: Just password
        , couchdbUrl: Just (host <> ":" <> show port <> "/")
        }
      transactionFlag
      transactionWithTiming
      modelToLoad
      defaultRuntimeOptions
      brokerService
      indexedResourceToCreate
      missingResource))
  runReaderT mp rf

runPerspectivesWithState :: forall a. MonadPerspectives a -> (AVar PerspectivesState) -> Aff a
runPerspectivesWithState = runReaderT
