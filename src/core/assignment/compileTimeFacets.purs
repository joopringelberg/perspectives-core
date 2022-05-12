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
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff (Fiber, delay)
import Effect.Aff.AVar (put)
import Perspectives.CoreTypes (MP, MonadPerspectivesTransaction, TransactionWithTiming(..), Updater)
import Perspectives.Repetition (Duration, Repeater(..), fromDuration)
import Perspectives.Representation.Action (TimeFacets)
import Perspectives.Representation.TypeIdentifiers (RoleType, StateIdentifier)
import Unsafe.Coerce (unsafeCoerce)

addTimeFacets :: forall a f. Partial => Updater a -> TimeFacets f -> RoleType -> StateIdentifier -> MP (Updater a)
addTimeFacets updater {startMoment, endMoment, repeats} authoringRole stateId = do
  pure $ after startMoment $ repeat repeats updater
  where
    after :: Maybe Duration -> Updater a -> Updater a
    after Nothing u = u
    after (Just d) u = \a -> do
      lift $ lift $ delay (fromDuration d)
      u a

    repeat :: Repeater -> Updater a -> Updater a
    repeat Never u = u
    repeat (Forever duration) u = \a -> do
      (av :: AVar TransactionWithTiming) <- lift $ gets _.transactionWithTiming
      lift $ lift $ put (TransactionWithTiming 
        { transaction: transaction a
        , instanceId: unsafeUnwrapResource a
        , stateId
        , authoringRole}) 
        av
      where 
        transaction :: a -> MonadPerspectivesTransaction Unit
        transaction a = forever do
          lift $ lift $ delay (fromDuration duration)
          u a

    returnFiber :: Fiber Unit -> Unit
    returnFiber f = unit

    unsafeUnwrapResource :: a -> String
    unsafeUnwrapResource = unsafeCoerce

registerRepeater :: forall a. a -> a
registerRepeater u = u
