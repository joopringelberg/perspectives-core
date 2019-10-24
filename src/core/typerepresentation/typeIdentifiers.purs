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

-- | All types are identified with a `TypeIdentifier`. Invariably, a TypeIdentifier is a Purescript newtype (there is no representation of newtypes in the compiled code: they are just used by the compiler).
-- | Examples are `ContextType`, `EnumeratedRoleType`, `ViewType`, etc.
-- |
-- | Each newtype is an instance of several Type Classes:
-- |  * Generic
-- |  * Newtype
-- |  * WriteForeign
-- |  * ReadForeign
-- |  * Show
-- |  * Eq.
-- |
-- | Of interest are furthermore [RoleType](#t:RoleType) and [PropertyType](#t:PropertyType): abstractions over enumerated and calculated types.
-- |
-- | [RoleKind](#t:RoleKind) distinghuishes various EnumeratedRoleTypes: does the role represent a user, a bot, a context and so on.


module Perspectives.Representation.TypeIdentifiers where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Kishimen (genericSumToVariant)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype ContextType = ContextType String
derive instance newtypeContextType :: Newtype ContextType _
derive instance genericRepContextType :: Generic ContextType _
derive newtype instance writeForeignContextType :: WriteForeign ContextType
derive newtype instance readForeignContextType :: ReadForeign ContextType
instance showContextType :: Show ContextType where
  show i = "ContextType " <> (unwrap i)
instance eqContextType :: Eq ContextType where
  eq (ContextType id1) (ContextType id2) = id1 == id2

newtype EnumeratedRoleType = EnumeratedRoleType String
derive instance newtypeEnumeratedRolType :: Newtype EnumeratedRoleType _
derive instance genericRepEnumeratedRolType :: Generic EnumeratedRoleType _
derive newtype instance writeForeignEnumeratedRolType :: WriteForeign EnumeratedRoleType
derive newtype instance readForeignEnumeratedRolType :: ReadForeign EnumeratedRoleType
instance showEnumeratedRolType :: Show EnumeratedRoleType where
  show i = "EnumeratedRoleType " <> (unwrap i)
instance eqEnumeratedRolType :: Eq EnumeratedRoleType where
  eq (EnumeratedRoleType id1) (EnumeratedRoleType id2) = id1 == id2

newtype CalculatedRoleType = CalculatedRoleType String
derive instance newtypeComputedRolType :: Newtype CalculatedRoleType _
derive instance genericRepComputedRolType :: Generic CalculatedRoleType _
derive newtype instance writeForeignComputedRolType :: WriteForeign CalculatedRoleType
derive newtype instance readForeignComputedRolType :: ReadForeign CalculatedRoleType
instance showComputedRolType :: Show CalculatedRoleType where
  show i = "CalculatedRoleType " <> (unwrap i)
instance eqComputedRolType :: Eq CalculatedRoleType where
  eq (CalculatedRoleType id1) (CalculatedRoleType id2) = id1 == id2

data RoleType = ENR EnumeratedRoleType | CR CalculatedRoleType
derive instance genericRepRoleType :: Generic RoleType _
instance writeForeignRoleType :: WriteForeign RoleType where
  writeImpl (ENR r) = writeImpl r
  writeImpl (CR r) = writeImpl r
instance readForeignRoleType :: ReadForeign RoleType where
  readImpl r = readImpl r
instance showRoleType :: Show RoleType where
  show (ENR r) = "ENR " <>  show r
  show (CR r) = "CR " <> show r
instance eqRoleType :: Eq RoleType where
  eq (ENR _) (CR _) = false
  eq (CR _) (ENR _) = false
  eq (CR r1) (CR r2) = r1 == r2
  eq (ENR r1) (ENR r2) = r1 == r2

-- | Get the string representation of a RoleType.
roletype2string :: RoleType -> String
roletype2string (ENR s) = unwrap s
roletype2string (CR s) = unwrap s

-- | RoleKind codes the 'role' of the role in the context. Is it an external rol, a bot role, etc.
data RoleKind = RoleInContext | ContextRole | ExternalRole | UserRole | BotRole
derive instance genericRepRoleKind :: Generic RoleKind _
instance writeForeignRoleKind :: WriteForeign RoleKind where
  writeImpl = writeImpl <<< genericSumToVariant
instance readForeignRoleKind :: ReadForeign RoleKind where
  readImpl f = readImpl f
instance showRoleKind :: Show RoleKind where
  show = genericShow
instance eqRoleKind :: Eq RoleKind where
  eq = genericEq

newtype EnumeratedPropertyType = EnumeratedPropertyType String
derive instance newtypeEnumeratedPropertyType :: Newtype EnumeratedPropertyType _
derive instance genericRepEnumeratedPropertyType :: Generic EnumeratedPropertyType _
derive newtype instance writeForeignEnumeratedPropertyType :: WriteForeign EnumeratedPropertyType
derive newtype instance readForeignEnumeratedPropertyType :: ReadForeign EnumeratedPropertyType
instance showEnumeratedPropertyType :: Show EnumeratedPropertyType where
  show i = "EnumeratedPropertyType " <> (unwrap i)
instance eqEnumeratedPropertyType :: Eq EnumeratedPropertyType where
  eq (EnumeratedPropertyType id1) (EnumeratedPropertyType id2) = id1 == id2

newtype CalculatedPropertyType = CalculatedPropertyType String
derive instance newtypeCalculatedPropertyType :: Newtype CalculatedPropertyType _
derive instance genericRepCalculatedPropertyType :: Generic CalculatedPropertyType _
derive newtype instance writeForeignCalculatedPropertyType :: WriteForeign CalculatedPropertyType
derive newtype instance readForeignCalculatedPropertyType :: ReadForeign CalculatedPropertyType
instance showCalculatedPropertyType :: Show CalculatedPropertyType where
  show i = "CalculatedPropertyType " <> (unwrap i)
instance eqCalculatedPropertyType :: Eq CalculatedPropertyType where
  eq (CalculatedPropertyType id1) (CalculatedPropertyType id2) = id1 == id2

data PropertyType = ENP EnumeratedPropertyType | CP CalculatedPropertyType

propertytype2string :: PropertyType -> String
propertytype2string (ENP s) = unwrap s
propertytype2string (CP s) = unwrap s

derive instance genericRepPropertyType :: Generic PropertyType _
instance writeForeignPropertyType :: WriteForeign PropertyType where
  writeImpl (ENP r) = writeImpl r
  writeImpl (CP r) = writeImpl r
instance readForeignPropertyType :: ReadForeign PropertyType where
  readImpl r = readImpl r
instance showPropertyType :: Show PropertyType where
  show (ENP r) = show r
  show (CP r) = show r
instance eqPropertyType :: Eq PropertyType where
  eq (ENP _) (CP _) = false
  eq (CP _) (ENP _) = false
  eq (CP r1) (CP r2) = r1 == r2
  eq (ENP r1) (ENP r2) = r1 == r2

newtype ViewType = ViewType String
derive instance newtypeViewType :: Newtype ViewType _
derive instance genericRepViewType :: Generic ViewType _
derive newtype instance writeForeignViewType :: WriteForeign ViewType
derive newtype instance readForeignViewType :: ReadForeign ViewType
instance showViewType :: Show ViewType where
  show i = "ViewType " <> (unwrap i)
instance eqViewType :: Eq ViewType where
  eq (ViewType id1) (ViewType id2) = id1 == id2

newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _
derive instance genericRepActionType :: Generic ActionType _
derive newtype instance writeForeignActionType :: WriteForeign ActionType
derive newtype instance readForeignActionType :: ReadForeign ActionType
instance showActionType :: Show ActionType where
  show i = "ActionType " <> (unwrap i)
instance eqActionType :: Eq ActionType where
  eq (ActionType id1) (ActionType id2) = id1 == id2
