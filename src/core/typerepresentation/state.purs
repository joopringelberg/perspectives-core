-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2021 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Representation.State where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign (unsafeFromForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap (EncodableMap(..), empty)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.SideEffect (SideEffect)
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleType, StateIdentifier)

newtype State = State StateRecord

type StateRecord =
	{ id :: StateIdentifier
	, context :: ContextType
	, query :: Calculation
	, object :: Maybe Calculation
	-- the key in these maps is the subject the effect or notification is for.
	, notifyOnEntry :: EncodableMap RoleType NotificationLevel
	, notifyOnExit :: EncodableMap RoleType NotificationLevel
	, automaticOnEntry :: EncodableMap RoleType SideEffect
	, automaticOnExit :: EncodableMap RoleType SideEffect
	, subStates :: Array StateIdentifier
	}

constructState :: StateIdentifier -> Step -> ContextType -> List State -> State
constructState id condition context subStates = State
	{id: id
	, context
	, query: S condition
	, object: Nothing
	, notifyOnEntry: EncodableMap empty
	, notifyOnExit: EncodableMap empty
	, automaticOnEntry: EncodableMap empty
	, automaticOnExit: EncodableMap empty
	, subStates: []
	}

derive instance genericState :: Generic State _
instance showState :: Show State where show = genericShow
instance eqState :: Eq State where eq = genericEq
instance encodeState :: Encode State where encode = genericEncode defaultOptions
instance decodeState :: Decode State where decode = unsafeFromForeign

data NotificationLevel = Alert
derive instance genericNotificationLevel :: Generic NotificationLevel _
instance showNotificationLevel :: Show NotificationLevel where show = genericShow
instance eqNotificationLevel :: Eq NotificationLevel where eq = genericEq
instance encodeNotificationLevel :: Encode NotificationLevel where encode = genericEncode defaultOptions
instance decodeNotificationLevel :: Decode NotificationLevel where decode = genericDecode defaultOptions

instance identifiableState :: Identifiable State StateIdentifier where
  identifier (State{id}) = id
  displayName (State{id}) = (unwrap id)

instance revisionState :: Revision State where
  rev _ = Nothing
  changeRevision _ s = s
