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

import Control.Alt ((<|>))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap (EncodableMap, empty)
import Perspectives.Query.QueryTypes (Calculation, QueryFunctionDescription)
import Perspectives.Representation.Action (AutomaticAction, TimeFacets)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Sentence (Sentence)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), PropertyType, RoleType, StateIdentifier)
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

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
    , authorOnly :: Boolean
    , isSelfPerspective :: Boolean
  } |
  RolePerspective
    { currentContextCalculation :: QueryFunctionDescription
    , properties :: Array PropertyType
    , selfOnly :: Boolean
    , authorOnly :: Boolean
    , isSelfPerspective :: Boolean
    }

derive instance genericStateDependentPerspective :: Generic StateDependentPerspective _
instance showStateDependentPerspective :: Show StateDependentPerspective where show = genericShow
instance eqStateDependentPerspective :: Eq StateDependentPerspective where eq = genericEq



type StateDependentPerspective_ = 
  { constructor :: String
  , currentContextCalculation :: Maybe QueryFunctionDescription
  , properties :: Array PropertyType
  , selfOnly :: Boolean
  , authorOnly :: Boolean
  , isSelfPerspective :: Boolean
  }
instance WriteForeign StateDependentPerspective where
  writeImpl (ContextPerspective {properties, selfOnly, authorOnly, isSelfPerspective}) = writeImpl 
    ({ constructor: "ContextPerspective"
    , currentContextCalculation: Nothing
    , properties
    , selfOnly
    , authorOnly
    , isSelfPerspective
    } :: StateDependentPerspective_)
  writeImpl (RolePerspective {properties, selfOnly, authorOnly, isSelfPerspective, currentContextCalculation}) = writeImpl 
    { constructor: "RolePerspective"
    , currentContextCalculation: Just currentContextCalculation
    , properties
    , selfOnly
    , authorOnly
    , isSelfPerspective
    }

instance ReadForeign StateDependentPerspective where
  readImpl f = do
    {constructor, properties, currentContextCalculation, selfOnly, authorOnly, isSelfPerspective} :: StateDependentPerspective_ <- read' f
    unsafePartial case constructor of
      "ContextPerspective" -> pure $ ContextPerspective {properties, selfOnly, authorOnly, isSelfPerspective}
      "RolePerspective" -> 
        pure $ RolePerspective {properties, selfOnly, authorOnly, isSelfPerspective, currentContextCalculation: unsafePartial fromJust currentContextCalculation}

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


derive newtype instance WriteForeign State
derive newtype instance ReadForeign State

data NotificationLevel = Alert
derive instance genericNotificationLevel :: Generic NotificationLevel _
instance showNotificationLevel :: Show NotificationLevel where show = genericShow
instance eqNotificationLevel :: Eq NotificationLevel where eq = genericEq



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



instance WriteForeign StateFulObject where
  writeImpl (Cnt typ) = writeImpl { constructor: "Cnt", typ}
  writeImpl (Orole typ) = writeImpl { constructor: "Orole", typ}
  writeImpl (Srole typ) = writeImpl { constructor: "Srole", typ}

instance ReadForeign StateFulObject where
  readImpl f = do 
    {constructor, typ} :: {constructor :: String, typ :: String}<- read' f
    unsafePartial case constructor of 
      "Cnt" -> pure $ Cnt $ ContextType typ
      "Orole" -> pure $ Orole $ EnumeratedRoleType typ
      "Srole" -> pure $ Srole $ EnumeratedRoleType typ

data Notification = 
  ContextNotification (TimeFacets (sentence :: Sentence, domain :: String) )
  |
  RoleNotification (TimeFacets (currentContextCalculation :: QueryFunctionDescription, sentence :: Sentence, domain :: String))

derive instance genericNotification :: Generic Notification _
instance showNotification :: Show Notification where show = genericShow
instance eqNotification :: Eq Notification where eq = genericEq

instance WriteForeign Notification where
  writeImpl (ContextNotification r) = writeImpl { constructor: "ContextNotification", r}
  writeImpl (RoleNotification r) = writeImpl { constructor: "RoleNotification", r}

instance ReadForeign Notification where  
  readImpl f = 
    -- Order matters here!
    do 
      {r} :: {r :: TimeFacets ( sentence :: Sentence, currentContextCalculation :: QueryFunctionDescription, domain :: String )} <- read' f
      pure $ RoleNotification r
    <|>
    do 
      {r} :: {r :: TimeFacets ( sentence :: Sentence, domain :: String )} <- read' f
      pure $ ContextNotification r
