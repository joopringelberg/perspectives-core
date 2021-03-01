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

module Perspectives.Representation.CalculatedProperty where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..))
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedRoleType(..))
import Prelude (class Eq, class Show, (<<<), (==), ($))

-----------------------------------------------------------
-- CALCULATEDPROPERTY
-----------------------------------------------------------
newtype CalculatedProperty = CalculatedProperty CalculatedPropertyRecord

type CalculatedPropertyRecord =
  { _id :: CalculatedPropertyType
  , _rev :: Revision_
  , displayName :: String

  , calculation :: Calculation
  -- , computation :: Maybe (RoleInContext ~~> Value)
  , role :: EnumeratedRoleType

  , pos :: ArcPosition
  }

defaultCalculatedProperty :: String -> String -> String -> ArcPosition -> CalculatedProperty
defaultCalculatedProperty id dn role pos = CalculatedProperty
  { _id: CalculatedPropertyType id
  , _rev: Nothing
  , displayName: dn
  , calculation: S $ Simple $ Identity $ ArcPosition{column: 0, line: 0}
  , role: EnumeratedRoleType role
  , pos: pos}

derive instance genericRepCalculatedProperty :: Generic CalculatedProperty _

instance showCalculatedProperty :: Show CalculatedProperty where
  show = genericShow

instance eqCalculatedProperty :: Eq CalculatedProperty where
  eq (CalculatedProperty {_id : id1}) (CalculatedProperty {_id : id2}) = id1 == id2

derive instance newtypeCalculatedProperty :: Newtype CalculatedProperty _

instance encodeCalculatedProperty :: Encode CalculatedProperty where
  encode = genericEncode defaultOptions

instance decodeCalculatedProperty :: Decode CalculatedProperty where
  decode = genericDecode defaultOptions

instance revisionCalculatedProperty :: Revision CalculatedProperty where
  rev = _._rev <<< unwrap
  changeRevision s = over CalculatedProperty (\vr -> vr {_rev = s})

instance identifiableCalculatedProperty :: Identifiable CalculatedProperty CalculatedPropertyType where
  identifier (CalculatedProperty{_id}) = _id
