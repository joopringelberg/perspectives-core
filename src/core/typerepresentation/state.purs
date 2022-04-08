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
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap (EncodableMap, empty)
import Perspectives.Query.QueryTypes (Calculation, QueryFunctionDescription)
import Perspectives.Representation.Action (AutomaticAction)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Sentence (Sentence)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, PropertyType, RoleType, StateIdentifier)

newtype State = State StateRecord

type StateRecord =
  { id :: StateIdentifier
  , stateFulObject :: StateFulObject
  , query :: Calculation
  , object :: Maybe QueryFunctionDescription
  -- the key in these maps is the subject the effect or notification or perspective is for.
  , notifyOnEntry :: EncodableMap RoleType Notification
  , notifyOnExit :: EncodableMap RoleType Notification
  , automaticOnEntry :: EncodableMap RoleType AutomaticAction
  , automaticOnExit :: EncodableMap RoleType AutomaticAction
  , perspectivesOnEntry :: EncodableMap RoleType StateDependentPerspective
  , subStates :: Array StateIdentifier
  }

data StateDependentPerspective =
  ContextPerspective
    { properties :: Array PropertyType
    , selfOnly :: Boolean
    , isSelfPerspective :: Boolean
  } |
  RolePerspective
    { currentContextCalculation :: QueryFunctionDescription
    , properties :: Array PropertyType
    , selfOnly :: Boolean
    , isSelfPerspective :: Boolean
    }

derive instance genericStateDependentPerspective :: Generic StateDependentPerspective _
instance showStateDependentPerspective :: Show StateDependentPerspective where show = genericShow
instance eqStateDependentPerspective :: Eq StateDependentPerspective where eq = genericEq
instance encodeStateDependentPerspective :: Encode StateDependentPerspective where encode = genericEncode defaultOptions
instance decodeStateDependentPerspective :: Decode StateDependentPerspective where decode = genericDecode defaultOptions

constructState :: StateIdentifier -> Calculation -> StateFulObject -> Array StateIdentifier -> State
constructState id condition stateFulObject subStates = State
  {id: id
  , stateFulObject
  , query: condition
  , object: Nothing -- used to compute the objects in enteringState, to bind to "currentobject".
  , notifyOnEntry: empty
  , notifyOnExit: empty
  , automaticOnEntry: empty
  , automaticOnExit: empty
  , perspectivesOnEntry: empty
  , subStates
  }
derive instance newtypeState :: Newtype State _

derive instance genericState :: Generic State _
instance showState :: Show State where show = genericShow
instance eqState :: Eq State where eq = genericEq
instance encodeState :: Encode State where encode = genericEncode defaultOptions
instance decodeState :: Decode State where decode = genericDecode defaultOptions

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


data StateFulObject = Cnt ContextType | Orole EnumeratedRoleType | Srole EnumeratedRoleType
derive instance genericStateFulObject :: Generic StateFulObject _
instance showStateFulObject :: Show StateFulObject where show = genericShow
instance eqStateFulObject :: Eq StateFulObject where eq = genericEq
instance encodeStateFulObject :: Encode StateFulObject where encode = genericEncode defaultOptions
instance decodeStateFulObject :: Decode StateFulObject where decode = genericDecode defaultOptions

data Notification = ContextNotification Sentence |
  RoleNotification
    { currentContextCalculation :: QueryFunctionDescription
    , sentence :: Sentence
    }

derive instance genericNotification :: Generic Notification _
instance showNotification :: Show Notification where show = genericShow
instance eqNotification :: Eq Notification where eq = genericEq
instance encodeNotification :: Encode Notification where encode = genericEncode defaultOptions
instance decodeNotification :: Decode Notification where decode = genericDecode defaultOptions
