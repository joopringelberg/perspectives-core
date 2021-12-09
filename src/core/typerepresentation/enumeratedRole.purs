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

module Perspectives.Representation.EnumeratedRole where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object, empty) as OBJ
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Data.EncodableMap (EncodableMap(..), empty)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.Perspective (Perspective, StateSpec)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), PropertyType, RoleKind, ViewType)
import Prelude (class Eq, class Show, (<<<), (==))

-----------------------------------------------------------
-- ENUMERATEDROLE
-----------------------------------------------------------
newtype EnumeratedRole = EnumeratedRole EnumeratedRoleRecord

type EnumeratedRoleRecord =
  { _id :: EnumeratedRoleType
  , _rev :: Revision_
  , displayName :: String
  , kindOfRole :: RoleKind

  , roleAspects :: Array EnumeratedRoleType
  , properties :: Array PropertyType

  , context :: ContextType
  , binding :: ADT EnumeratedRoleType

  , views :: Array ViewType

  , perspectives :: Array Perspective

  , actions :: EncodableMap StateSpec (OBJ.Object Action)

  , functional :: Boolean
  , mandatory :: Boolean

  , pos :: ArcPosition

  -- The keys in these objects are the String representations of context types.
  , onRoleDelta_binding :: OBJ.Object (Array InvertedQuery)
  , onRoleDelta_binder :: OBJ.Object (Array InvertedQuery)

  , onContextDelta_context :: OBJ.Object (Array InvertedQuery)
  , onContextDelta_role :: OBJ.Object (Array InvertedQuery)

  , indexedRole :: Maybe RoleInstance

  , unlinked :: Boolean

  }

defaultEnumeratedRole :: String -> String -> RoleKind -> String -> ArcPosition -> EnumeratedRole
defaultEnumeratedRole qname dname kindOfRole context pos = EnumeratedRole
  { _id: EnumeratedRoleType qname
  , _rev: Nothing
  , displayName: dname
  , kindOfRole: kindOfRole

  , roleAspects: []
  , properties: []

  , context: ContextType context
  , binding: EMPTY

  , views: []

  , perspectives: []

  , actions: EncodableMap empty

  , functional: true
  , mandatory: false

  , pos: pos

  , onRoleDelta_binding: OBJ.empty
  , onRoleDelta_binder: OBJ.empty
  , onContextDelta_context: OBJ.empty
  , onContextDelta_role: OBJ.empty

  , indexedRole: Nothing
  , unlinked: false
  }

derive instance genericRepEnumeratedRole :: Generic EnumeratedRole _

instance showEnumeratedRole :: Show EnumeratedRole where
  show = genericShow

instance eqEnumeratedRole :: Eq EnumeratedRole where
  eq (EnumeratedRole {_id : id1}) (EnumeratedRole {_id : id2}) = id1 == id2

derive instance newtypeEnumeratedRole :: Newtype EnumeratedRole _

instance encodeEnumeratedRole :: Encode EnumeratedRole where
  encode = genericEncode defaultOptions

instance decodeEnumeratedRole :: Decode EnumeratedRole where
  decode = genericDecode defaultOptions

instance revisionEnumeratedRole :: Revision EnumeratedRole where
  rev = _._rev <<< unwrap
  changeRevision s = over EnumeratedRole (\vr -> vr {_rev = s})

instance identifiableEnumeratedRole :: Identifiable EnumeratedRole EnumeratedRoleType where
  identifier (EnumeratedRole{_id}) = _id
  displayName (EnumeratedRole{displayName:d}) = d
