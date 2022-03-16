-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2022 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Representation.ScreenDefinition where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.Representation.Perspective (PropertyVerbs, PerspectiveId)
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleType)
import Perspectives.Representation.Verbs (RoleVerb)

-- | These types are part of a DomeinFile.
-- | Runtime, we generate a simpler structure from them, that has an automatic WriteForeign instance.
-- | A screen is not modelled to be state dependent. We handle state on serialising a screen by
-- |    - excluding properties;
-- |    - excluding verbs on properties;
-- |    - excluding verbs on role instances;
-- |    - excluding actions;
-- |    - excluding an entire Widget if the role type underlying it is unavailable in the given states.
-- | The PropertyVerbs and RoleVerb Array are used to filter the full perspective the user has on a the role.

newtype ScreenDefinition = ScreenDefinition
  { title :: String
  , rows :: Maybe (Array ScreenElementDef)
  , columns :: Maybe (Array ScreenElementDef)
  -- `subject` and `context` need not to be part of the ScreenDefinition itself.
  -- These fields are necessary to find a screen for a particular user and context,
  -- so instead we use them as a compound key to store the ScreenDefinition with in
  -- the DomeinFile.
  }

data ScreenElementDef =
  RowElementD RowDef
  | ColumnElementD ColumnDef
  | TableElementD TableDef
  | FormElementD FormDef

newtype RowDef = RowDef (Array ScreenElementDef)
newtype ColumnDef = ColumnDef (Array ScreenElementDef)

-----------------------------------------------------------
-- WIDGETS
-----------------------------------------------------------
-- | All Widgets share these fields.
type WidgetCommonFieldsDef =
  { title :: Maybe String
  -- `perspectiveId` replaces the RoleIdentification from the WidgetCommonFields.
  -- By construction, a screen can only be specified for Perspectives that have a Just value for RoleType.
  , perspectiveId :: PerspectiveId
  -- The runtime  has a perspective serialisation.
  -- These two fields are not serialised runtime; they are used to
  -- restrict the serialised perspective.
  , propertyVerbs :: Maybe PropertyVerbs
  , roleVerbs :: Array RoleVerb
  }

newtype TableDef = TableDef WidgetCommonFieldsDef
newtype FormDef = FormDef WidgetCommonFieldsDef

-----------------------------------------------------------
-- GENERIC INSTANCES
-----------------------------------------------------------
derive instance genericScreenDefinition :: Generic ScreenDefinition _
derive instance genericScreenElementDef :: Generic ScreenElementDef _
derive instance genericRowDef :: Generic RowDef _
derive instance genericColumnDef :: Generic ColumnDef _
derive instance genericTableDef :: Generic TableDef _
derive instance genericFormDef :: Generic FormDef _

-----------------------------------------------------------
-- SHOW INSTANCES
-----------------------------------------------------------
instance showScreenDefinition :: Show ScreenDefinition where show = genericShow
instance showScreenElementDef :: Show ScreenElementDef where show = genericShow
instance showRowDef :: Show RowDef where show x = genericShow x
instance showColumnDef :: Show ColumnDef where show x = genericShow x
instance showTableDef :: Show TableDef where show = genericShow
instance showFormDef :: Show FormDef where show = genericShow

-----------------------------------------------------------
-- EQ INSTANCES
-----------------------------------------------------------
instance eqScreenDefinition :: Eq ScreenDefinition where eq = genericEq
instance eqScreenElementDef :: Eq ScreenElementDef where eq = genericEq
instance eqRowDef :: Eq RowDef where eq a b = genericEq a b
instance eqColumnDef :: Eq ColumnDef where eq a b = genericEq a b
instance eqTableDef :: Eq TableDef where eq = genericEq
instance eqFormDef :: Eq FormDef where eq = genericEq

-----------------------------------------------------------
-- ENCODE INSTANCES
-----------------------------------------------------------
instance encodeScreenDefinition :: Encode ScreenDefinition where encode = genericEncode $ defaultOptions
instance encodeScreenElementDef :: Encode ScreenElementDef where encode = genericEncode $ defaultOptions
instance encodeRowDef :: Encode RowDef where encode x = genericEncode defaultOptions x
instance encodeColumnDef :: Encode ColumnDef where encode x = genericEncode defaultOptions x
instance encodeTableDef :: Encode TableDef where encode = genericEncode $ defaultOptions
instance encodeFormDef :: Encode FormDef where encode = genericEncode $ defaultOptions

-----------------------------------------------------------
-- DECODE INSTANCES
-----------------------------------------------------------
instance decodeScreenDefinition :: Decode ScreenDefinition where decode = genericDecode $ defaultOptions
instance decodeScreenElementDef :: Decode ScreenElementDef where decode = genericDecode $ defaultOptions
instance decodeRowDef :: Decode RowDef where decode x = genericDecode defaultOptions x
instance decodeColumnDef :: Decode ColumnDef where decode x = genericDecode defaultOptions x
instance decodeTableDef :: Decode TableDef where decode = genericDecode $ defaultOptions
instance decodeFormDef :: Decode FormDef where decode = genericDecode $ defaultOptions

-------------------------------------------------------------------------------
---- SCREENKEY
-------------------------------------------------------------------------------
data ScreenKey = ScreenKey ContextType RoleType
derive instance genericScreenKey :: Generic ScreenKey _
instance showScreenKey :: Show ScreenKey where show = genericShow
instance eqScreenKey :: Eq ScreenKey where eq = genericEq
instance encodeScreenKey :: Encode ScreenKey where encode = genericEncode $ defaultOptions
instance decodeScreenKey :: Decode ScreenKey where decode = genericDecode $ defaultOptions
derive instance ordScreenKey :: Ord ScreenKey

type ScreenMap = EncodableMap ScreenKey ScreenDefinition
