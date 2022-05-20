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

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Trans.Class (lift)
import Effect.AVar (AVar)
import Effect.Aff (Fiber)
import Effect.Aff.AVar (put)
import Perspectives.CoreTypes (MP, RepeatingTransaction(..), Updater)
import Perspectives.Repetition (Repeater(..))
import Perspectives.Representation.Action (TimeFacets)
import Perspectives.Representation.TypeIdentifiers (RoleType, StateIdentifier)
import Unsafe.Coerce (unsafeCoerce)

addTimeFacets :: forall a f. Partial => Updater a -> TimeFacets f -> RoleType -> StateIdentifier -> MP (Updater a)
addTimeFacets updater {startMoment, endMoment, repeats} authoringRole stateId = do
  pure $ repeat repeats updater
  where
    repeat :: Repeater -> Updater a -> Updater a
    repeat Never u = u
    repeat (Forever duration) u = \a -> do
      (av :: AVar RepeatingTransaction) <- lift $ gets _.transactionWithTiming
      lift $ lift $ put (TransactionWithTiming
        { transaction: u a
        , interval: duration
        , instanceId: unsafeUnwrapResource a
        , stateId
        , authoringRole
        , startMoment
        , endMoment}) 
        av
    repeat (RepeatFor nrOfTimes duration) u = \a -> do
      (av :: AVar RepeatingTransaction) <- lift $ gets _.transactionWithTiming
      lift $ lift $ put (RepeatNtimes
        { transaction: u a
        , interval: duration
        , nrOfTimes
        , instanceId: unsafeUnwrapResource a
        , stateId
        , authoringRole
        , startMoment
        , endMoment}) 
        av

    returnFiber :: Fiber Unit -> Unit
    returnFiber f = unit

    unsafeUnwrapResource :: a -> String
    unsafeUnwrapResource = unsafeCoerce
