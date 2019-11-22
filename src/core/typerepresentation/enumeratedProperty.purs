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

module Perspectives.Representation.EnumeratedProperty where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Kishimen (genericSumToVariant)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Prelude (class Eq, class Show, (<<<), (==))

-----------------------------------------------------------
-- ENUMERATEDPROPERTY
-----------------------------------------------------------
newtype EnumeratedProperty = EnumeratedProperty EnumeratedPropertyRecord

type EnumeratedPropertyRecord =
  { _id :: EnumeratedPropertyType
  , _rev :: Revision_
  , displayName :: String

  , role :: EnumeratedRoleType
  , range :: Range
  , functional :: Boolean
  , mandatory :: Boolean

  , pos :: ArcPosition
  }

defaultEnumeratedProperty :: String -> String -> String -> Range -> ArcPosition -> EnumeratedProperty
defaultEnumeratedProperty id dn role range pos = EnumeratedProperty
  { _id: EnumeratedPropertyType id
  , _rev: Nothing
  , displayName: dn
  , role: EnumeratedRoleType role
  , range: range
  , functional: true
  , mandatory: false
  , pos: pos}

derive instance genericRepEnumeratedProperty :: Generic EnumeratedProperty _

instance showEnumeratedProperty :: Show EnumeratedProperty where
  show = genericShow

instance eqEnumeratedProperty :: Eq EnumeratedProperty where
  eq (EnumeratedProperty {_id : id1}) (EnumeratedProperty {_id : id2}) = id1 == id2

derive instance newtypeEnumeratedProperty :: Newtype EnumeratedProperty _

instance decodeEnumeratedProperty :: Decode EnumeratedProperty where
  decode = genericDecode defaultOptions

instance encodeEnumeratedProperty :: Encode EnumeratedProperty where
  encode = genericEncode defaultOptions

instance revisionEnumeratedProperty :: Revision EnumeratedProperty where
  rev = _._rev <<< unwrap
  changeRevision s = over EnumeratedProperty (\vr -> vr {_rev = s})

instance identifiableEnumeratedProperty :: Identifiable EnumeratedProperty EnumeratedPropertyType where
  identifier (EnumeratedProperty{_id}) = _id

-----------------------------------------------------------
-- RANGE
-----------------------------------------------------------
data Range = PString | PBool | PNumber | PDate

derive instance genericRange :: Generic Range _

instance eqRange :: Eq Range where eq = genericEq

instance encodeRange :: Encode Range where
  encode = genericEncode defaultOptions

instance decodeRange :: Decode Range where
  decode = genericDecode defaultOptions

instance rangeShow :: Show Range where
  show = genericShow
