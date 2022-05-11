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
module Perspectives.CompileTimeFacets where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff (Aff, delay, forkAff)
import Perspectives.CoreTypes (MP, MonadPerspectivesTransaction, Updater, PerspectivesState)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Repetition (Duration, Repeater(..), fromDuration)
import Perspectives.Representation.Action (TimeFacets)
-- import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.RunPerspectives (runPerspectivesWithState)

addTimeFacets :: forall a f. Partial => Updater a -> TimeFacets f -> MP (Updater a)
addTimeFacets updater {startMoment, endMoment, repeats} = do
--   pure $ after $ repeat repeats updater
  pure $ after startMoment updater
  where
    after :: Maybe Duration -> Updater a -> Updater a
    after Nothing u = u
    after (Just d) u = \a -> do
      lift3 $ delay (fromDuration d)
      u a

    -- repeat :: Repeater -> Updater a -> Updater a
    -- repeat Never u = u
    -- repeat (Forever duration) u = ArrayT <<< \a -> do
    --     (state :: AVar PerspectivesState) <- ask
    --     registerRepeater 
    --         forkAff 
    --             (runPerspectivesWithState 
    --                 (runMonadPerspectivesTransaction -- Dit introduceert een cycle in de module dependencies.
    --                     -- authoringrole
    --                     (do
    --                         lift3 $ delay (fromDuration duration)
    --                         u a)
    --                         )
    --                 state)

registerRepeater :: forall a. a -> a
registerRepeater u = u

lift3 :: forall a. Aff a -> MonadPerspectivesTransaction a
lift3 = lift <<< lift