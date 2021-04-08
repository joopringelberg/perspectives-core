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

module Perspectives.Representation.Perspective where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Query.QueryTypes (Calculation)
import Perspectives.Representation.ExplicitSet (ExplicitSet)
import Perspectives.Representation.SideEffect (SideEffect)
import Perspectives.Representation.State (StateIdentifier)
import Perspectives.Representation.TypeIdentifiers (PropertyType)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerbList)
import Prelude (class Eq, class Show)

-----------------------------------------------------------
-- PERSPECTIVE
-----------------------------------------------------------
newtype Perspective = Perspective PerspectiveRecord

type PerspectiveRecord =
  { object :: Calculation
  , roleVerbs :: EncodableMap StateIdentifier RoleVerbList
	, propertyVerbs :: EncodableMap StateIdentifier (Array PropertyVerbs)
	, actions :: EncodableMap StateIdentifier (Object SideEffect)
  }


derive instance genericRepPerspective :: Generic Perspective _

instance showPerspective :: Show Perspective where
  show = genericShow

derive instance eqPerspective :: Eq Perspective

derive instance newtypePerspective :: Newtype Perspective _

instance encodePerspective :: Encode Perspective where
  encode = genericEncode defaultOptions

instance decodePerspective :: Decode Perspective where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- PROPERTYVERBS
-----------------------------------------------------------
data PropertyVerbs = PropertyVerbs (ExplicitSet PropertyType) (Array PropertyVerb)
derive instance genericPropertyVerbs :: Generic PropertyVerbs _
instance showPropertyVerbs :: Show PropertyVerbs where show = genericShow
derive instance eqPropertyVerbs :: Eq PropertyVerbs
instance encodePropertyVerbs :: Encode PropertyVerbs where encode = genericEncode defaultOptions
instance decodePropertyVerbs :: Decode PropertyVerbs where decode = genericDecode defaultOptions
