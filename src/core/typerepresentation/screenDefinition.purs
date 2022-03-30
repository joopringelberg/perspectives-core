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
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, encode, decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Representation.Perspective (PropertyVerbs, PerspectiveId)
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleType)
import Perspectives.Representation.Verbs (RoleVerb)
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (SerialisedPerspective')
import Simple.JSON (class WriteForeign, write)

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
  , tabs :: Maybe (Array TabDef)
  -- Will be an array of ColumnElementD elements.
  , rows :: Maybe (Array ScreenElementDef)
  -- Will be an array of RowElementD elements.
  , columns :: Maybe (Array ScreenElementDef)
  }

newtype TabDef = TabDef {title :: String, elements :: (Array ScreenElementDef)}

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
type WidgetCommonFieldsDef = WidgetCommonFieldsDefWithoutPerspective (perspective :: Maybe SerialisedPerspective')

type WidgetCommonFieldsDefWithoutPerspective f =
  { title :: Maybe String
  -- `perspectiveId` replaces the RoleIdentification from the WidgetCommonFields.
  -- By construction, a screen can only be specified for Perspectives that have a Just value for RoleType.
  , perspectiveId :: PerspectiveId
  -- The runtime  has a perspective serialisation.
  -- These three fields are not serialised runtime; they are used to
  -- create the restricted serialised perspective.
  , propertyVerbs :: Maybe PropertyVerbs
  , roleVerbs :: Array RoleVerb
  , userRole :: RoleType
  | f
  }

newtype TableDef = TableDef WidgetCommonFieldsDef
newtype FormDef = FormDef WidgetCommonFieldsDef

-- For en- and decoding. This discharges us from implementing a lot of instances for
-- SerialisedPerspective'.
newtype TableDef' = TableDef' (WidgetCommonFieldsDefWithoutPerspective ())
newtype FormDef' = FormDef' (WidgetCommonFieldsDefWithoutPerspective ())


-----------------------------------------------------------
-- GENERIC INSTANCES
-----------------------------------------------------------
derive instance genericScreenDefinition :: Generic ScreenDefinition _
derive instance genericScreenElementDef :: Generic ScreenElementDef _
derive instance genericTabDef :: Generic TabDef _
derive instance genericRowDef :: Generic RowDef _
derive instance genericColumnDef :: Generic ColumnDef _
derive instance genericTableDef :: Generic TableDef _
derive instance genericTableDef' :: Generic TableDef' _
derive instance genericFormDef :: Generic FormDef _
derive instance genericFormDef' :: Generic FormDef' _

-----------------------------------------------------------
-- SHOW INSTANCES
-----------------------------------------------------------
instance showScreenDefinition :: Show ScreenDefinition where show = genericShow
instance showScreenElementDef :: Show ScreenElementDef where show = genericShow
instance showTabDef :: Show TabDef where show x = genericShow x
instance showRowDef :: Show RowDef where show x = genericShow x
instance showColumnDef :: Show ColumnDef where show x = genericShow x
instance showTableDef :: Show TableDef where show = genericShow
instance showFormDef :: Show FormDef where show = genericShow

-----------------------------------------------------------
-- EQ INSTANCES
-----------------------------------------------------------
instance eqScreenDefinition :: Eq ScreenDefinition where eq = genericEq
instance eqScreenElementDef :: Eq ScreenElementDef where eq = genericEq
instance eqTabDef :: Eq TabDef where eq a b = genericEq a b
instance eqRowDef :: Eq RowDef where eq a b = genericEq a b
instance eqColumnDef :: Eq ColumnDef where eq a b = genericEq a b
instance eqTableDef :: Eq TableDef where eq = genericEq
instance eqFormDef :: Eq FormDef where eq = genericEq

-----------------------------------------------------------
-- WRITEFOREIGN INSTANCES
-----------------------------------------------------------
-- These instances are used to serialise the screen for the client.
instance writeForeignScreenDefinition :: WriteForeign ScreenDefinition where
  writeImpl (ScreenDefinition r) = write r

instance writeForeignScreenElementDef :: WriteForeign ScreenElementDef where
  writeImpl (RowElementD r) = write { row: write r}
  writeImpl (ColumnElementD c) = write { column: write c}
  writeImpl (TableElementD t) = write { table: write t}
  writeImpl (FormElementD f) = write { form: write f}

instance writeForeignTabDef :: WriteForeign TabDef where
  writeImpl (TabDef widgetCommonFields) = write widgetCommonFields

instance writeForeignRowDef :: WriteForeign RowDef where
  writeImpl (RowDef screenElements) = write screenElements

instance writeForeignColumnDef :: WriteForeign ColumnDef where
  writeImpl (ColumnDef screenElements) = write screenElements

instance writeForeignTableDef :: WriteForeign TableDef where
  writeImpl (TableDef r) = writeWidgetCommonFields r

instance writeForeignFormDef :: WriteForeign FormDef where
  writeImpl (FormDef r) = writeWidgetCommonFields r

-- | Serialise just the title and perspective field, for the client side.
writeWidgetCommonFields :: WidgetCommonFieldsDef -> Foreign
writeWidgetCommonFields {title, perspective} = write
  { title: write title
  , perspective: write perspective}

-----------------------------------------------------------
-- ENCODE INSTANCES
-----------------------------------------------------------
instance encodeScreenDefinition :: Encode ScreenDefinition where encode = genericEncode $ defaultOptions
instance encodeScreenElementDef :: Encode ScreenElementDef where encode = genericEncode $ defaultOptions
instance encodeTabDef :: Encode TabDef where encode x = genericEncode defaultOptions x
instance encodeRowDef :: Encode RowDef where encode x = genericEncode defaultOptions x
instance encodeColumnDef :: Encode ColumnDef where encode x = genericEncode defaultOptions x
instance encodeTableDef' :: Encode TableDef' where encode = genericEncode $ defaultOptions
instance encodeTableDef :: Encode TableDef where
  encode (TableDef {title, perspectiveId, propertyVerbs, roleVerbs, userRole}) = encode
    (TableDef' {title, perspectiveId, propertyVerbs, roleVerbs, userRole})
instance encodeFormDef' :: Encode FormDef' where encode = genericEncode $ defaultOptions
instance encodeFormDef :: Encode FormDef where
  encode (FormDef {title, perspectiveId, propertyVerbs, roleVerbs, userRole}) = encode
    (FormDef' {title, perspectiveId, propertyVerbs, roleVerbs, userRole})

-----------------------------------------------------------
-- DECODE INSTANCES
-----------------------------------------------------------
instance decodeScreenDefinition :: Decode ScreenDefinition where decode = genericDecode $ defaultOptions
instance decodeScreenElementDef :: Decode ScreenElementDef where decode = genericDecode $ defaultOptions
instance decodeTabDef :: Decode TabDef where decode x = genericDecode defaultOptions x
instance decodeRowDef :: Decode RowDef where decode x = genericDecode defaultOptions x
instance decodeColumnDef :: Decode ColumnDef where decode x = genericDecode defaultOptions x
instance decodeTableDef' :: Decode TableDef' where decode = genericDecode $ defaultOptions
instance decodeTableDef :: Decode TableDef where
  decode f = do
    (TableDef' {title, perspectiveId, propertyVerbs, roleVerbs, userRole}) <- decode f
    pure $ TableDef {title, perspectiveId, propertyVerbs, roleVerbs, userRole, perspective: Nothing}
instance decodeFormDef' :: Decode FormDef' where decode = genericDecode $ defaultOptions
instance decodeFormDef :: Decode FormDef where
  decode f = do
    (FormDef' {title, perspectiveId, propertyVerbs, roleVerbs, userRole}) <- decode f
    pure $ FormDef {title, perspectiveId, propertyVerbs, roleVerbs, userRole, perspective: Nothing}

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