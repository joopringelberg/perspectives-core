-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Representation.Action where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Query.QueryTypes (Calculation)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.SideEffect (SideEffect)
import Perspectives.Representation.TypeIdentifiers (ActionType, RoleType, ViewType)
import Prelude (class Eq, class Show, (<<<), (==))

-----------------------------------------------------------
-- ACTION
-----------------------------------------------------------
newtype Action = Action ActionRecord

type ActionRecord =
  { _id :: ActionType
  , _rev :: Revision_
  , displayName :: String

  , subject :: RoleType
  , verb :: Verb
  , object :: Calculation
  , requiredObjectProperties :: Maybe ViewType
  , requiredSubjectProperties :: Maybe ViewType
  , requiredIndirectObjectProperties :: Maybe ViewType
  , indirectObject :: Maybe RoleType
  , condition :: Calculation
  , effect :: Maybe SideEffect
  , executedByBot :: Boolean
  , pos :: ArcPosition
  }

derive instance genericRepAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

instance eqAction :: Eq Action where
  eq (Action {_id : id1}) (Action {_id : id2}) = id1 == id2

derive instance newtypeAction :: Newtype Action _

instance encodeAction :: Encode Action where
  encode = genericEncode defaultOptions

instance decodeAction :: Decode Action where
  decode = genericDecode defaultOptions

instance revisionAction :: Revision Action where
  rev = _._rev <<< unwrap
  changeRevision s = over Action (\vr -> vr {_rev = s})

instance identifiableAction :: Identifiable Action ActionType where
  identifier (Action{_id}) = _id

-----------------------------------------------------------
-- VERB
-----------------------------------------------------------

data Verb = Create | Consult | Change | Delete | CreateAndBindContext | Become | Custom String

derive instance genericRepVerb :: Generic Verb _
instance encodeVerb :: Encode Verb where
  encode = genericEncode defaultOptions
instance decodeVerb :: Decode Verb where
  decode = genericDecode defaultOptions
instance showVerb :: Show Verb where
  show = genericShow
instance eqVerb :: Eq Verb where
  eq = genericEq
