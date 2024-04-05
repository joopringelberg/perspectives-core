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

-- | This module gives a tiny wrapper around idb-keyval (https://www.npmjs.com/package/idb-keyval).

module IDBKeyVal  

(idbGet, idbSet, clear)

where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)

foreign import getValueByKeyImpl :: EffectFn1 String (Promise (Nullable Foreign))

idbGet :: String -> Aff (Maybe Foreign)
idbGet = getValueByKeyImpl_ >>> toAffE >>> map toMaybe

getValueByKeyImpl_ :: String -> Effect (Promise (Nullable Foreign))
getValueByKeyImpl_ = runEffectFn1 getValueByKeyImpl

foreign import setKeyValueImpl :: EffectFn2 String Foreign Unit

setKeyValueImpl_ :: String -> Foreign -> Effect Unit
setKeyValueImpl_ = runEffectFn2 setKeyValueImpl

idbSet :: String -> Foreign -> Aff Unit
idbSet key value = liftEffect (setKeyValueImpl_ key value)

foreign import clear :: Effect Unit