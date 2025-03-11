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

import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (F, Foreign, ForeignError(..), fail)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.Perspective (PropertyVerbs, PerspectiveId)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType, RoleType(..), roletype2string)
import Perspectives.Representation.Verbs (RoleVerb)
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (SerialisedPerspective')
import Simple.JSON (class ReadForeign, class WriteForeign, read, read', write, writeImpl)

-- | These types are part of a DomeinFile.
-- | Runtime, we generate a simpler structure from them, that has an automatic WriteForeign instance.
-- | A screen is not modelled to be state dependent. We handle state on serialising a screen by
-- |    - excluding properties;
-- |    - excluding verbs on properties;
-- |    - excluding verbs on role instances;
-- |    - excluding actions;
-- |    - excluding an entire Widget if the role type underlying it is unavailable in the given states.
-- | The PropertyVerbs and RoleVerb Array are used to filter the full perspective the user has on a the role.

type MainScreenElements = 
  ( tabs :: Maybe (Array TabDef)
  -- Will be an array of RowElementD elements.
  , rows :: Maybe (Array ScreenElementDef)
  -- Will be an array of ColumnElementD elements.
  , columns :: Maybe (Array ScreenElementDef)
  )

-- When a WhoWhatWhereScreen is defined, we will not have tabs, rows or columns beside that.
-- However, during transition to the new GUI, we will have to support both.
type ScreenDefinitionElements f = {title :: Maybe String, whoWhatWhereScreen :: Maybe WhoWhatWhereScreenDef | f}

newtype ScreenDefinition = ScreenDefinition (ScreenDefinitionElements MainScreenElements)

newtype TabDef = TabDef {title :: String, isDefault :: Boolean, elements :: (Array ScreenElementDef)}

data ScreenElementDef =
  RowElementD RowDef
  | ColumnElementD ColumnDef
  | TableElementD TableDef
  | FormElementD FormDef
  | MarkDownElementD MarkDownDef
  | ChatElementD  ChatDef

newtype RowDef = RowDef (Array ScreenElementDef)
newtype ColumnDef = ColumnDef (Array ScreenElementDef)
newtype TableDef = TableDef WidgetCommonFieldsDef
newtype FormDef = FormDef WidgetCommonFieldsDef
data MarkDownDef = 
  MarkDownConstantDef {text :: String, condition :: Maybe QueryFunctionDescription, domain :: String} |
  MarkDownPerspectiveDef { widgetFields :: WidgetCommonFieldsDef, conditionProperty :: Maybe PropertyType} |
  MarkDownExpressionDef {textQuery :: QueryFunctionDescription, condition :: Maybe QueryFunctionDescription,  text :: Maybe String}
newtype ChatDef = ChatDef (ChatDefFields (chatRole :: RoleType))
type ChatDefFields f = { chatInstance :: Maybe RoleInstance, messageProperty :: EnumeratedPropertyType, mediaProperty :: EnumeratedPropertyType | f}
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
  , roleVerbs :: Maybe (Array RoleVerb)
  , userRole :: RoleType
  | f
  }

-- For en- and decoding. This discharges us from implementing a lot of instances for
-- SerialisedPerspective'.
newtype TableDef' = TableDef' (WidgetCommonFieldsDefWithoutPerspective ())
newtype FormDef' = FormDef' (WidgetCommonFieldsDefWithoutPerspective ())

-----------------------------------------------------------
-- WHOWHATWHERESCREEN
-----------------------------------------------------------
newtype WhoWhatWhereScreenDef = WhoWhatWhereScreenDef {who :: Array TableFormDef, what :: What, whereto :: Array TableFormDef}

-----------------------------------------------------------
-- TABLEFORM
-----------------------------------------------------------
newtype TableFormDef = TableFormDef {table :: TableDef, form :: FormDef}

-----------------------------------------------------------
-- WHAT
-----------------------------------------------------------
data What = TableForms (Array TableFormDef) | FreeFormScreen { | MainScreenElements}

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
derive instance Generic MarkDownDef _
derive instance Generic ChatDef _
derive instance Generic WhoWhatWhereScreenDef _
derive instance Generic TableFormDef _
derive instance Generic What _

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
instance Show MarkDownDef where show = genericShow
instance Show ChatDef where show = genericShow
instance Show WhoWhatWhereScreenDef where show = genericShow
instance Show TableFormDef where show = genericShow
instance Show What where show = genericShow

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
instance Eq MarkDownDef where eq = genericEq
instance Eq ChatDef where eq = genericEq
instance Eq WhoWhatWhereScreenDef where eq = genericEq
instance Eq TableFormDef where eq = genericEq
instance Eq What where eq = genericEq

-----------------------------------------------------------
-- WRITEFOREIGN INSTANCES
-----------------------------------------------------------
-- These instances are used to serialise the screen for the client.
instance writeForeignScreenDefinition :: WriteForeign ScreenDefinition where
  writeImpl (ScreenDefinition r) = write r

instance writeForeignScreenElementDef :: WriteForeign ScreenElementDef where
  writeImpl (RowElementD r) = write { elementType: "RowElementD", element: r}
  writeImpl (ColumnElementD c) = write { elementType: "ColumnElementD", element: c}
  writeImpl (TableElementD t) = write { elementType: "TableElementD", element: t}
  writeImpl (FormElementD f) = write { elementType: "FormElementD", element: f}
  writeImpl (MarkDownElementD f) = write { elementType: "MarkDownElementD", element: f}
  writeImpl (ChatElementD c) = write { elementType: "ChatElementD", element: c}

instance writeForeignTabDef :: WriteForeign TabDef where
  writeImpl (TabDef widgetCommonFields) = write widgetCommonFields

instance WriteForeign RowDef where
  writeImpl (RowDef elements) = write { tag: "RowDef", elements}
instance WriteForeign ColumnDef where
  writeImpl (ColumnDef elements) = write { tag: "ColumnDef", elements}
instance WriteForeign TableDef where
  writeImpl (TableDef fields) = write { tag: "TableDef", fields}
instance WriteForeign FormDef where
  writeImpl (FormDef fields) = write { tag: "FormDef", fields}
instance WriteForeign MarkDownDef where
  writeImpl (MarkDownConstantDef f) = write { tag: "MarkDownConstantDef", element: f}
  writeImpl (MarkDownPerspectiveDef f) = write { tag: "MarkDownPerspectiveDef", element: f}
  writeImpl (MarkDownExpressionDef f) = write { tag: "MarkDownExpressionDef", element: f}
instance WriteForeign ChatDef where
  writeImpl (ChatDef fields) = write {tag: "ChatDef", fields: fields {chatRole = roletype2string fields.chatRole}}

instance WriteForeign ScreenKey where
  writeImpl (ScreenKey ct rt) = writeImpl {ct, rt}

instance WriteForeign TableFormDef where
  writeImpl (TableFormDef {table, form}) = write {tag: "TableFormDef", table, form}

instance WriteForeign WhoWhatWhereScreenDef where
  writeImpl (WhoWhatWhereScreenDef {who, what, whereto}) = write {tag: "WhoWhatWhereScreenDef", who, what, whereto}

instance WriteForeign What where
  writeImpl (TableForms t) = write {tag: "TableForms", elements: t}
  writeImpl (FreeFormScreen f) = write {tag: "FreeFormScreen", elements: f}
-----------------------------------------------------------
-- READFOREIGN INSTANCES
-----------------------------------------------------------
derive newtype instance ReadForeign ScreenDefinition 

instance ReadForeign ScreenElementDef where
  readImpl f = do 
    {elementType, element} :: {elementType :: String, element :: Foreign} <- read' f
    unsafePartial $ case elementType of 
      "RowElementD" -> RowElementD <$> ((read' element) :: F RowDef)
      "ColumnElementD" -> ColumnElementD <$> ((read' element) :: F ColumnDef)
      "TableElementD" -> TableElementD <$> ((read' element) :: F TableDef)
      "FormElementD" -> FormElementD <$> ((read' element) :: F FormDef)
      "MarkDownElementD" -> do 
        ({tag, element:subElement} :: {tag :: String, element :: Foreign}) <- read' element
        unsafePartial $ case tag of
          "MarkDownConstantDef" -> MarkDownElementD <<< MarkDownConstantDef <$> ((read' subElement) :: F {text :: String, condition :: Maybe QueryFunctionDescription, domain :: String})
          "MarkDownPerspectiveDef" -> MarkDownElementD <<< MarkDownPerspectiveDef <$> ((read' subElement) :: F { widgetFields :: WidgetCommonFieldsDef, conditionProperty :: Maybe PropertyType})
          "MarkDownExpressionDef" -> MarkDownElementD <<< MarkDownExpressionDef <$> ((read' subElement) :: F {textQuery :: QueryFunctionDescription, condition :: Maybe QueryFunctionDescription,  text :: Maybe String})
      "ChatElementD" -> ChatElementD <$> ((read' element) :: F ChatDef)

instance ReadForeign ScreenKey where
  readImpl f = do 
    {ct, rt} :: {ct :: ContextType, rt :: RoleType} <- read' f
    pure $ ScreenKey ct rt

instance ReadForeign RowDef where
  readImpl f = do 
    ({tag, elements} :: { tag :: String, elements :: Array ScreenElementDef}) <- read' f
    case tag of 
      "RowDef" -> pure $ RowDef elements
      _ -> fail (TypeMismatch "RowDef" tag)

instance ReadForeign ColumnDef where
  readImpl f = do 
    ({tag, elements} :: { tag :: String, elements :: Array ScreenElementDef}) <- read' f
    case tag of 
      "ColumnDef" -> pure $ ColumnDef elements
      _ -> fail (TypeMismatch "ColumnDef" tag)

instance ReadForeign TableDef where 
  readImpl f = do 
    ({tag, fields} :: { tag :: String, fields :: WidgetCommonFieldsDef}) <- read' f
    case tag of 
      "TableDef" -> pure $ TableDef fields
      _ -> fail (TypeMismatch "TableDef" tag)

instance ReadForeign FormDef where
  readImpl f = do 
    ({tag, fields} :: { tag :: String, fields :: WidgetCommonFieldsDef}) <- read' f
    case tag of 
      "FormDef" -> pure $ FormDef fields
      _ -> fail (TypeMismatch "FormDef" tag)

instance ReadForeign ChatDef where
  readImpl f = do
    ({tag, fields} :: { tag :: String, fields :: ChatDefFields (chatRole :: String)}) <- read' f
    case tag of 
      "ChatDef" -> pure $ ChatDef fields {chatRole = ENR $ EnumeratedRoleType fields.chatRole} -- TODO: we weten niet of het een enumerated role is!
      _ -> fail (TypeMismatch "ChatDef" tag)
  
derive newtype instance ReadForeign TabDef

instance ReadForeign TableFormDef where
  readImpl f = do 
    ({tag, table, form} :: {tag :: String, table :: TableDef, form :: FormDef}) <- read' f
    case tag of 
      "TableFormDef" -> pure $ TableFormDef {table, form}
      _ -> fail (TypeMismatch "TableFormDef" tag)

instance ReadForeign WhoWhatWhereScreenDef where
  readImpl f = do 
    ({tag, who, what, whereto} :: {tag :: String, who :: Array TableFormDef, what :: What, whereto :: Array TableFormDef}) <- read' f
    case tag of 
      "WhoWhatWhereScreenDef" -> pure $ WhoWhatWhereScreenDef {who, what, whereto}
      _ -> fail (TypeMismatch "WhoWhatWhereScreenDef" tag)

instance ReadForeign What where
  readImpl f = do 
    ({tag, elements} :: {tag :: String, elements :: Foreign}) <- read' f
    case tag of 
      "TableForms" -> case read elements of 
        Right tableForms -> pure $ TableForms tableForms
        Left err -> fail (ForeignError (show err))
      "FreeFormScreen" -> case read elements of 
        Right screenElements -> pure $ FreeFormScreen screenElements
        Left err -> fail (ForeignError (show err))
      _ -> fail (TypeMismatch "What" tag)
-------------------------------------------------------------------------------
---- SCREENKEY
---- A ScreenKey is constructed in PhaseThree based on the lexical context and subject.
-------------------------------------------------------------------------------
data ScreenKey = ScreenKey ContextType RoleType
derive instance genericScreenKey :: Generic ScreenKey _
instance showScreenKey :: Show ScreenKey where show = genericShow
instance eqScreenKey :: Eq ScreenKey where eq = genericEq

derive instance ordScreenKey :: Ord ScreenKey

type ScreenMap = EncodableMap ScreenKey ScreenDefinition

-- | Serialise just the title and perspective field, for the client side.
writeWidgetCommonFields :: WidgetCommonFieldsDef -> Foreign
writeWidgetCommonFields {title, perspective} = write
  { title: write title
  , perspective: write perspective}
