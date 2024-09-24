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

module Perspectives.Representation.EnumeratedProperty where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object, empty)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.Parsing.Arc.AST (PropertyFacet)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- ENUMERATEDPROPERTY
-----------------------------------------------------------
newtype EnumeratedProperty = EnumeratedProperty EnumeratedPropertyRecord

type EnumeratedPropertyRecord =
  { id :: EnumeratedPropertyType
  , _rev :: Revision_
  , displayName :: String

  , role :: EnumeratedRoleType
  , range :: Range
  , functional :: Boolean
  , mandatory :: Boolean
  , selfonly :: Boolean
  , authoronly :: Boolean

  , pos :: ArcPosition

  -- The keys are the string versions of EnumeratedRoleTypes.
  , onPropertyDelta :: Object (Array InvertedQuery)

  , constrainingFacets :: Array PropertyFacet
  }

defaultEnumeratedProperty :: String -> String -> String -> Range -> ArcPosition -> EnumeratedProperty
defaultEnumeratedProperty id dn role range pos = EnumeratedProperty
  { id: EnumeratedPropertyType id
  , _rev: Nothing
  , displayName: dn
  , role: EnumeratedRoleType role
  , range: range
  , functional: true
  , mandatory: false
  , selfonly: false
  , authoronly: false
  , pos: pos
  , onPropertyDelta: empty
  , constrainingFacets: []
}

derive instance genericRepEnumeratedProperty :: Generic EnumeratedProperty _

instance showEnumeratedProperty :: Show EnumeratedProperty where
  show = genericShow

instance eqEnumeratedProperty :: Eq EnumeratedProperty where
  eq (EnumeratedProperty {id : id1}) (EnumeratedProperty {id : id2}) = id1 == id2

derive instance newtypeEnumeratedProperty :: Newtype EnumeratedProperty _

derive newtype instance ReadForeign EnumeratedProperty

derive newtype instance WriteForeign EnumeratedProperty

instance revisionEnumeratedProperty :: Revision EnumeratedProperty where
  rev = _._rev <<< unwrap
  changeRevision s = over EnumeratedProperty (\vr -> vr {_rev = s})

instance identifiableEnumeratedProperty :: Identifiable EnumeratedProperty EnumeratedPropertyType where
  identifier (EnumeratedProperty{id}) = id
  displayName (EnumeratedProperty{displayName:d}) = d
