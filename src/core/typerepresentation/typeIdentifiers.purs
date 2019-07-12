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
  show = show <<< unwrap
instance eqContextType :: Eq ContextType where
  eq (ContextType id1) (ContextType id2) = id1 == id2

newtype EnumeratedRoleType = EnumeratedRoleType String
derive instance newtypeEnumeratedRolType :: Newtype EnumeratedRoleType _
derive instance genericRepEnumeratedRolType :: Generic EnumeratedRoleType _
derive newtype instance writeForeignEnumeratedRolType :: WriteForeign EnumeratedRoleType
derive newtype instance readForeignEnumeratedRolType :: ReadForeign EnumeratedRoleType
instance showEnumeratedRolType :: Show EnumeratedRoleType where
  show = show <<< unwrap
instance eqEnumeratedRolType :: Eq EnumeratedRoleType where
  eq (EnumeratedRoleType id1) (EnumeratedRoleType id2) = id1 == id2

newtype CalculatedRoleType = CalculatedRoleType String
derive instance newtypeComputedRolType :: Newtype CalculatedRoleType _
derive instance genericRepComputedRolType :: Generic CalculatedRoleType _
derive newtype instance writeForeignComputedRolType :: WriteForeign CalculatedRoleType
derive newtype instance readForeignComputedRolType :: ReadForeign CalculatedRoleType
instance showComputedRolType :: Show CalculatedRoleType where
  show = show <<< unwrap
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
  show (ENR r) = show r
  show (CR r) = show r
instance eqRoleType :: Eq RoleType where
  eq (ENR _) (CR _) = false
  eq (CR _) (ENR _) = false
  eq (CR r1) (CR r2) = r1 == r2
  eq (ENR r1) (ENR r2) = r1 == r2

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
  show = show <<< unwrap
instance eqEnumeratedPropertyType :: Eq EnumeratedPropertyType where
  eq (EnumeratedPropertyType id1) (EnumeratedPropertyType id2) = id1 == id2

newtype CalculatedPropertyType = CalculatedPropertyType String
derive instance newtypeCalculatedPropertyType :: Newtype CalculatedPropertyType _
derive instance genericRepCalculatedPropertyType :: Generic CalculatedPropertyType _
derive newtype instance writeForeignCalculatedPropertyType :: WriteForeign CalculatedPropertyType
derive newtype instance readForeignCalculatedPropertyType :: ReadForeign CalculatedPropertyType
instance showCalculatedPropertyType :: Show CalculatedPropertyType where
  show = show <<< unwrap
instance eqCalculatedPropertyType :: Eq CalculatedPropertyType where
  eq (CalculatedPropertyType id1) (CalculatedPropertyType id2) = id1 == id2

data PropertyType = ENP EnumeratedPropertyType | CP CalculatedPropertyType

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
  show = show <<< unwrap
instance eqViewType :: Eq ViewType where
  eq (ViewType id1) (ViewType id2) = id1 == id2

newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _
derive instance genericRepActionType :: Generic ActionType _
derive newtype instance writeForeignActionType :: WriteForeign ActionType
derive newtype instance readForeignActionType :: ReadForeign ActionType
instance showActionType :: Show ActionType where
  show = show <<< unwrap
instance eqActionType :: Eq ActionType where
  eq (ActionType id1) (ActionType id2) = id1 == id2
