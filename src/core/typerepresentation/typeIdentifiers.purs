module Perspectives.Representation.TypeIdentifiers where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Foreign (unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype VertexType = VertexType String
derive instance newtypeVertexType :: Newtype VertexType _
derive instance genericRepVertexType :: Generic VertexType _
derive newtype instance writeForeignVertexType :: WriteForeign VertexType
instance showVertexType :: Show VertexType where
  show = show <<< unwrap
instance encodeVertexType :: Encode VertexType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeVertexType :: Decode VertexType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqVertexType :: Eq VertexType where
  eq (VertexType id1) (VertexType id2) = id1 == id2


newtype EdgeType = EdgeType String
derive instance newtypeEdgeType :: Newtype EdgeType _
derive instance genericRepEdgeType :: Generic EdgeType _
derive newtype instance writeForeignEdgeType :: WriteForeign EdgeType
instance showEdgeType :: Show EdgeType where
  show = show <<< unwrap
instance encodeEdgeType :: Encode EdgeType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeEdgeType :: Decode EdgeType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqEdgeType :: Eq EdgeType where
  eq (EdgeType id1) (EdgeType id2) = id1 == id2

newtype PropertyType = PropertyType String
derive instance newtypePropertyType :: Newtype PropertyType _
derive instance genericRepPropertyType :: Generic PropertyType _
derive newtype instance writeForeignPropertyType :: WriteForeign PropertyType
instance showPropertyType :: Show PropertyType where
  show = show <<< unwrap
instance encodePropertyType :: Encode PropertyType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodePropertyType :: Decode PropertyType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqPropertyType :: Eq PropertyType where
  eq (PropertyType id1) (PropertyType id2) = id1 == id2

-- A type that matches every other type.
newtype EveryType = EveryType String
derive instance newtypeEveryType :: Newtype EveryType _
derive instance genericRepEveryType :: Generic EveryType _
derive newtype instance writeForeignEveryType :: WriteForeign EveryType
instance showEveryType :: Show EveryType where
  show = show <<< unwrap
instance encodeEveryType :: Encode EveryType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeEveryType :: Decode EveryType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqEveryType :: Eq EveryType where
  eq (EveryType id1) (EveryType id2) = id1 == id2

-----------------------------------------------------------
-- PERSPECTIVES TYPES
-----------------------------------------------------------
newtype ContextType = ContextType String
derive instance newtypeContextType :: Newtype ContextType _
derive instance genericRepContextType :: Generic ContextType _
derive newtype instance writeForeignContextType :: WriteForeign ContextType
derive newtype instance readForeignContextType :: ReadForeign ContextType
instance showContextType :: Show ContextType where
  show = show <<< unwrap
instance eqContextType :: Eq ContextType where
  eq (ContextType id1) (ContextType id2) = id1 == id2

newtype EnumeratedRolType = EnumeratedRolType String
derive instance newtypeEnumeratedRolType :: Newtype EnumeratedRolType _
derive instance genericRepEnumeratedRolType :: Generic EnumeratedRolType _
derive newtype instance writeForeignEnumeratedRolType :: WriteForeign EnumeratedRolType
derive newtype instance readForeignEnumeratedRolType :: ReadForeign EnumeratedRolType
instance showEnumeratedRolType :: Show EnumeratedRolType where
  show = show <<< unwrap
instance encodeEnumeratedRolType :: Encode EnumeratedRolType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeEnumeratedRolType :: Decode EnumeratedRolType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqEnumeratedRolType :: Eq EnumeratedRolType where
  eq (EnumeratedRolType id1) (EnumeratedRolType id2) = id1 == id2

newtype ComputedRolType = ComputedRolType String
derive instance newtypeComputedRolType :: Newtype ComputedRolType _
derive instance genericRepComputedRolType :: Generic ComputedRolType _
derive newtype instance writeForeignComputedRolType :: WriteForeign ComputedRolType
derive newtype instance readForeignComputedRolType :: ReadForeign ComputedRolType
instance showComputedRolType :: Show ComputedRolType where
  show = show <<< unwrap
instance encodeComputedRolType :: Encode ComputedRolType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeComputedRolType :: Decode ComputedRolType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqComputedRolType :: Eq ComputedRolType where
  eq (ComputedRolType id1) (ComputedRolType id2) = id1 == id2

data RoleType = ENR EnumeratedRolType | CR ComputedRolType
derive instance genericRepRoleType :: Generic RoleType _
instance writeForeignRoleType :: WriteForeign RoleType where
  writeImpl (ENR r) = writeImpl r
  writeImpl (CR r) = writeImpl r
instance readForeignRoleType :: ReadForeign RoleType where
  readImpl r = readImpl r
instance showRoleType :: Show RoleType where
  show (ENR r) = show r
  show (CR r) = show r
instance encodeRoleType :: Encode RoleType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeRoleType :: Decode RoleType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance eqRoleType :: Eq RoleType where
  eq (ENR _) (CR _) = false
  eq (CR _) (ENR _) = false
  eq (CR r1) (CR r2) = r1 == r2
  eq (ENR r1) (ENR r2) = r1 == r2

data RoleKind = RoleInContext | ContextRole | ExternalRole | UserRole | BotRole
derive instance genericRepRoleKind :: Generic RoleKind _
instance writeForeignRoleKind :: WriteForeign RoleKind where
  writeImpl RoleInContext = unsafeToForeign "RoleInContext"
  writeImpl ContextRole = unsafeToForeign "ContextRole"
  writeImpl ExternalRole = unsafeToForeign "ExternalRole"
  writeImpl UserRole = unsafeToForeign "UserRole"
  writeImpl BotRole = unsafeToForeign "BotRole"
instance readForeignRoleKind :: ReadForeign RoleKind where
  readImpl f = readImpl f
instance showRoleKind :: Show RoleKind where
  show = genericShow
instance eqRoleKind :: Eq RoleKind where
  eq = genericEq
instance encodeRoleKind :: Encode RoleKind where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance decodeRoleKind :: Decode RoleKind where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
