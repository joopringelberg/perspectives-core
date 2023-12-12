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

module Perspectives.Representation.CalculatedRole where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Data.EncodableMap (EncodableMap, empty)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Perspective (Perspective, StateSpec)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), RoleKind, ViewType)
import Prelude (class Eq, class Show, (<<<), (==), ($))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CALCULATEDROLE
-----------------------------------------------------------
newtype CalculatedRole = CalculatedRole CalculatedRoleRecord

type CalculatedRoleRecord =
  { id :: CalculatedRoleType
  , _rev :: Revision_
  , displayName :: String
  , kindOfRole :: RoleKind
  , context :: ContextType
  , views :: Array ViewType
  , pos :: ArcPosition

  , perspectives :: Array Perspective
  , actions :: EncodableMap StateSpec (Object Action)

  , calculation :: Calculation
  }

defaultCalculatedRole :: String -> String -> RoleKind -> String -> ArcPosition -> CalculatedRole
defaultCalculatedRole qname dname kindOfRole context pos = CalculatedRole
  { id: CalculatedRoleType qname
  , _rev: Nothing
  , displayName: dname
  , kindOfRole: kindOfRole

  , calculation: S (Simple $ Identity $ ArcPosition{column: 0, line: 0}) false
  , context: ContextType context

  , views: []

  , perspectives: []

  , actions: empty

  , pos: pos
  }

derive instance genericRepCalculatedRole :: Generic CalculatedRole _

instance showCalculatedRole :: Show CalculatedRole where
  show = genericShow

instance eqCalculatedRole :: Eq CalculatedRole where
  eq (CalculatedRole {id : id1}) (CalculatedRole {id : id2}) = id1 == id2

derive instance newtypeCalculatedRole :: Newtype CalculatedRole _

derive newtype instance ReadForeign CalculatedRole
derive newtype instance WriteForeign CalculatedRole

instance revisionCalculatedRole :: Revision CalculatedRole where
  rev = _._rev <<< unwrap
  changeRevision s = over CalculatedRole (\vr -> vr {_rev = s})

instance identifiableCalculatedRole :: Identifiable CalculatedRole CalculatedRoleType where
  identifier (CalculatedRole{id}) = id
  displayName (CalculatedRole{displayName:d}) = d
