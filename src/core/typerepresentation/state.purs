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
import Data.Maybe (Maybe(..))
import Foreign (unsafeFromForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Data.EncodableMap (EncodableMap(..), empty)
import Perspectives.Identifiers (isContainingNamespace)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.SideEffect (SideEffect)
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleType)

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
	}

constructState :: StateIdentifier -> Step -> ContextType -> State
constructState id condition context = State
	{id: id
	, context
	, query: S condition
	, object: Nothing
	, notifyOnEntry: EncodableMap empty
	, notifyOnExit: EncodableMap empty
	, automaticOnEntry: EncodableMap empty
	, automaticOnExit: EncodableMap empty
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

-- | To identify all states of, say sys:PerspectivesSystem, use AllStates model:Perspectives$PerspectivesSystem.
-- | This distinguishes those states from those of, for example, model:SimpleChat$Chat.
-- | AllStates is only ever meant as: all states of a context. In theory we could use a Role identifier; however,
-- | we have no use for that.
-- | All other state definitions have names scoped to their lexically enclosing context or role.
type ArcIdentifier = String

data StateIdentifier = AllStates ArcIdentifier | State_ ArcIdentifier

unwrapStateIdentifier :: StateIdentifier -> String
unwrapStateIdentifier (AllStates s) = s
unwrapStateIdentifier (State_ s) = s

instance semigroupStateIdentifier :: Semigroup StateIdentifier where
	append (AllStates ctxt1) (AllStates ctxt2) = AllStates (ctxt1 <> ctxt2)
	append (AllStates ctxt1) (State_ s) = State_ (ctxt1 <> s)
	append (State_ s) (AllStates ctxt2) = AllStates (s <> ctxt2)
	append (State_ s1) (State_ s2) = State_ (s1 <> s2)

derive instance genericStateIdentifier :: Generic StateIdentifier _
instance showStateIdentifier :: Show StateIdentifier where show = genericShow
instance eqStateIdentifier :: Eq StateIdentifier where eq = genericEq
instance encodeStateIdentifier :: Encode StateIdentifier where encode = genericEncode defaultOptions
instance decodeStateIdentifier :: Decode StateIdentifier where decode = genericDecode defaultOptions
instance ordStateIdentifier :: Ord StateIdentifier where
  compare (AllStates s1) (State_ s2) = if s1 `isContainingNamespace` s2 then LT else EQ -- If s1 is a prefix of s2, (AllStates s1) is LE and otherwise EQ
  compare (State_ x) (State_ y) = compare x y
  compare (State_ s2) (AllStates s1) = if s1 `isContainingNamespace` s2 then GT else EQ
  compare (AllStates s1) (AllStates s2) = if s1 `isContainingNamespace` s2 then LT
    else if s2 `isContainingNamespace` s1 then GT else EQ
