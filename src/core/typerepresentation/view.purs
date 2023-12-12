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

module Perspectives.Representation.View where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType, ViewType)
import Prelude (class Eq, class Ord, class Show, compare, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- VIEW TYPE CLASS
-----------------------------------------------------------
class ViewClass r where
  role :: r -> RoleType
  propertyReferences :: r -> Array PropertyType

instance calculatedPropertyViewClass :: ViewClass View where
  role r = (unwrap r).role
  propertyReferences r = (unwrap r).propertyReferences

-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------
newtype View = View ViewRecord

type ViewRecord =
  { id :: ViewType
  , _rev :: Revision_
  , displayName :: String

  -- TODO: maak er een NonEmpty Array van.
  , propertyReferences :: Array PropertyType
  , role :: RoleType

  , pos :: ArcPosition
  }

derive instance genericRepView :: Generic View _

derive newtype instance ReadForeign View
derive newtype instance WriteForeign View

instance showView :: Show View where
  show = genericShow

instance eqView :: Eq View where
  eq (View {id : id1}) (View {id : id2}) = id1 == id2

derive instance newtypeView :: Newtype View _

instance revisionView :: Revision View where
  rev = _._rev <<< unwrap
  changeRevision s = over View (\vr -> vr {_rev = s})

instance identifiableView :: Identifiable View ViewType where
  identifier (View{id}) = id
  displayName (View{displayName:d}) = d

instance ordView :: Ord View where
  compare (View{id:id1}) (View{id:id2}) = compare id1 id2
