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

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow) 
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Utilities (class PrettyPrint)
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

newtype ContextType = ContextType String
derive instance newtypeContextType :: Newtype ContextType _
derive instance genericRepContextType :: Generic ContextType _

instance showContextType :: Show ContextType where
  show i = "ContextType \"" <> (unwrap i) <> "\""
instance eqContextType :: Eq ContextType where
  eq (ContextType id1) (ContextType id2) = id1 == id2
instance prettyPrintContextType :: PrettyPrint ContextType where
  prettyPrint' t = show
derive newtype instance writeForeignContextType :: WriteForeign ContextType
derive newtype instance readForeignContextType :: ReadForeign ContextType
derive instance ordContextType :: Ord ContextType

newtype EnumeratedRoleType = EnumeratedRoleType String
derive instance newtypeEnumeratedRolType :: Newtype EnumeratedRoleType _
derive instance genericRepEnumeratedRolType :: Generic EnumeratedRoleType _

instance showEnumeratedRolType :: Show EnumeratedRoleType where
  show i = "EnumeratedRoleType \"" <> (unwrap i) <> "\""
instance eqEnumeratedRolType :: Eq EnumeratedRoleType where
  eq (EnumeratedRoleType id1) (EnumeratedRoleType id2) = id1 == id2
instance ordEnumeratedRoleType :: Ord EnumeratedRoleType where
  compare (EnumeratedRoleType p1) (EnumeratedRoleType p2) = compare p1 p2
instance prettyPrintEnumeratedRoleType :: PrettyPrint EnumeratedRoleType where
  prettyPrint' t = show
derive newtype instance writeForeignEnumeratedRoleType :: WriteForeign EnumeratedRoleType
derive newtype instance readForeignEnumeratedRoleType :: ReadForeign EnumeratedRoleType

newtype CalculatedRoleType = CalculatedRoleType String
derive instance newtypeComputedRolType :: Newtype CalculatedRoleType _
derive instance genericRepComputedRolType :: Generic CalculatedRoleType _

instance showComputedRolType :: Show CalculatedRoleType where
  show i = "CalculatedRoleType " <> (unwrap i)
instance eqComputedRolType :: Eq CalculatedRoleType where
  eq (CalculatedRoleType id1) (CalculatedRoleType id2) = id1 == id2
instance ordCalculatedRoleType :: Ord CalculatedRoleType where
  compare (CalculatedRoleType p1) (CalculatedRoleType p2) = compare p1 p2
instance prettyPrintCalculatedRoleType :: PrettyPrint CalculatedRoleType where
  prettyPrint' t = show
derive newtype instance writeForeignCalculatedRoleType :: WriteForeign CalculatedRoleType
derive newtype instance readForeignCalculatedRoleType :: ReadForeign CalculatedRoleType

data RoleType = ENR EnumeratedRoleType | CR CalculatedRoleType
derive instance genericRepRoleType :: Generic RoleType _
instance showRoleType :: Show RoleType where
  show (ENR r) = "ENR " <>  show r
  show (CR r) = "CR " <> show r
instance eqRoleType :: Eq RoleType where
  eq (ENR _) (CR _) = false
  eq (CR _) (ENR _) = false
  eq (CR r1) (CR r2) = r1 == r2
  eq (ENR r1) (ENR r2) = r1 == r2
instance ordRoleType :: Ord RoleType where
  compare = genericCompare
instance prettyPrintRoleType :: PrettyPrint RoleType where
  prettyPrint' t = show
instance writeForeignRoleType :: WriteForeign RoleType where
  writeImpl (ENR ert) = writeImpl {type: "ENR", value: unwrap ert} 
  writeImpl (CR crt) = writeImpl {type: "CR", value: unwrap crt} 
instance readForeightRoleType :: ReadForeign RoleType where
  readImpl f = do 
    x :: {type :: String, value :: String} <- read' f
    unsafePartial case x.type, x.value of 
      "ENR", ert -> pure $ ENR $ EnumeratedRoleType ert
      "CR", crt -> pure $ CR $ CalculatedRoleType crt

-- | We have rare occasions where we want to lose the difference between
-- | CalculatedRoletype and EnumeratedRoleType.
newtype RoleType_ = RoleType_ String
derive instance newtypeRoleType_ :: Newtype RoleType_ _
derive instance genericRepRoleType_ :: Generic RoleType_ _

instance showRoleType_ :: Show RoleType_ where
  show i = "RoleType_ " <> (unwrap i)
instance eqRoleType_ :: Eq RoleType_ where
  eq (RoleType_ id1) (RoleType_ id2) = id1 == id2
instance ordRoleType_ :: Ord RoleType_ where
  compare (RoleType_ p1) (RoleType_ p2) = compare p1 p2

toRoleType_ :: RoleType -> RoleType_
toRoleType_ (ENR (EnumeratedRoleType r)) = RoleType_ r
toRoleType_ (CR (CalculatedRoleType r)) = RoleType_ r

-- | Get the string representation of a RoleType.
roletype2string :: RoleType -> String
roletype2string (ENR s) = unwrap s
roletype2string (CR s) = unwrap s

-- | RoleKind codes the 'role' of the role in the context. Is it an external rol, a bot role, etc.
data RoleKind = RoleInContext | ContextRole | ExternalRole | UserRole | Public | PublicProxy
derive instance genericRepRoleKind :: Generic RoleKind _
instance showRoleKind :: Show RoleKind where
  show = genericShow
instance eqRoleKind :: Eq RoleKind where
  eq = genericEq
instance writeForeignRoleKind :: WriteForeign RoleKind where
  writeImpl = unsafeToForeign <<< show
instance readForeignRoleKind :: ReadForeign RoleKind where
  readImpl = enumReadForeign

newtype EnumeratedPropertyType = EnumeratedPropertyType String
derive instance newtypeEnumeratedPropertyType :: Newtype EnumeratedPropertyType _
derive instance genericRepEnumeratedPropertyType :: Generic EnumeratedPropertyType _
instance showEnumeratedPropertyType :: Show EnumeratedPropertyType where
  show i = "EnumeratedPropertyType \"" <> (unwrap i) <> "\""
instance eqEnumeratedPropertyType :: Eq EnumeratedPropertyType where
  eq (EnumeratedPropertyType id1) (EnumeratedPropertyType id2) = id1 == id2
instance ordEnumeratedPropertyType :: Ord EnumeratedPropertyType where
  compare (EnumeratedPropertyType p1) (EnumeratedPropertyType p2) = compare p1 p2
instance prettyPrintEnumeratedPropertyType :: PrettyPrint EnumeratedPropertyType where
  prettyPrint' t = show
derive newtype instance writeForeignEnumeratedPropertyType :: WriteForeign EnumeratedPropertyType
derive newtype instance readForeignEnumeratedPropertyType :: ReadForeign EnumeratedPropertyType

newtype CalculatedPropertyType = CalculatedPropertyType String
derive instance newtypeCalculatedPropertyType :: Newtype CalculatedPropertyType _
derive instance genericRepCalculatedPropertyType :: Generic CalculatedPropertyType _
instance showCalculatedPropertyType :: Show CalculatedPropertyType where
  show i = "CalculatedPropertyType \"" <> (unwrap i) <> "\""
instance eqCalculatedPropertyType :: Eq CalculatedPropertyType where
  eq (CalculatedPropertyType id1) (CalculatedPropertyType id2) = id1 == id2
instance ordCalculatedPropertyType :: Ord CalculatedPropertyType where
  compare (CalculatedPropertyType p1) (CalculatedPropertyType p2) = compare p1 p2
instance prettyPrintCalculatedPropertyType :: PrettyPrint CalculatedPropertyType where
  prettyPrint' t = show
derive newtype instance writeForeignCalculatedPropertyType :: WriteForeign CalculatedPropertyType
derive newtype instance readForeignCalculatedPropertyType :: ReadForeign CalculatedPropertyType

data PropertyType = ENP EnumeratedPropertyType | CP CalculatedPropertyType

instance ordPropertyType :: Ord PropertyType where
  compare = genericCompare

propertytype2string :: PropertyType -> String
propertytype2string (ENP s) = unwrap s
propertytype2string (CP s) = unwrap s

derive instance genericRepPropertyType :: Generic PropertyType _
instance showPropertyType :: Show PropertyType where
  show (ENP r) = "ENP (" <> show r <> ")"
  show (CP r) = "CP (" <> show r <> ")"
instance eqPropertyType :: Eq PropertyType where
  eq (ENP _) (CP _) = false
  eq (CP _) (ENP _) = false
  eq (CP r1) (CP r2) = r1 == r2
  eq (ENP r1) (ENP r2) = r1 == r2
instance prettyPrintPropertyType :: PrettyPrint PropertyType where
  prettyPrint' t = show
instance writeForeignPropertyType :: WriteForeign PropertyType where
  writeImpl (ENP ept) = writeImpl {type: "ENP", value: unwrap ept} 
  writeImpl (CP cpt) = writeImpl {type: "CP", value: unwrap cpt} 

instance readForeightPropertyType :: ReadForeign PropertyType where
  readImpl f = do 
    x :: {type :: String, value :: String} <- read' f
    unsafePartial case x.type, x.value of 
      "ENP", ept -> pure $ ENP $ EnumeratedPropertyType ept
      "CP", cpt -> pure $ CP $ CalculatedPropertyType cpt

newtype ViewType = ViewType String
derive instance newtypeViewType :: Newtype ViewType _
derive instance genericRepViewType :: Generic ViewType _
instance showViewType :: Show ViewType where
  show i = "ViewType " <> (unwrap i)
instance eqViewType :: Eq ViewType where
  eq (ViewType id1) (ViewType id2) = id1 == id2
instance ordViewType :: Ord ViewType where
  compare (ViewType p1) (ViewType p2) = compare p1 p2
instance prettyPrintViewType :: PrettyPrint ViewType where
  prettyPrint' t = show
derive newtype instance writeForeignViewType :: WriteForeign ViewType
derive newtype instance readForeignViewType :: ReadForeign ViewType

newtype PerspectiveType = PerspectiveType String
derive instance newtypePerspectiveType :: Newtype PerspectiveType _
derive instance genericRepPerspectiveType :: Generic PerspectiveType _
instance showPerspectiveType :: Show PerspectiveType where
  show i = "PerspectiveType " <> (unwrap i)
instance eqPerspectiveType :: Eq PerspectiveType where
  eq (PerspectiveType id1) (PerspectiveType id2) = id1 == id2
instance ordPerspectiveType :: Ord PerspectiveType where
  compare (PerspectiveType a1) (PerspectiveType a2) = compare a1 a2
instance prettyPrintPerspectiveType :: PrettyPrint PerspectiveType where
  prettyPrint' t = show
derive newtype instance writeForeignPerspectiveType :: WriteForeign PerspectiveType
derive newtype instance readForeignPerspectiveType :: ReadForeign PerspectiveType

newtype ActionIdentifier = ActionIdentifier String
derive instance newtypeActionIdentifier :: Newtype ActionIdentifier _
derive instance genericRepActionIdentifier :: Generic ActionIdentifier _
instance showActionIdentifier :: Show ActionIdentifier where
  show i = "ActionIdentifier " <> (unwrap i)
instance eqActionIdentifier :: Eq ActionIdentifier where
  eq (ActionIdentifier id1) (ActionIdentifier id2) = id1 == id2
instance ordActionIdentifier :: Ord ActionIdentifier where
  compare (ActionIdentifier a1) (ActionIdentifier a2) = compare a1 a2
instance prettyPrintActionIdentifier :: PrettyPrint ActionIdentifier where
  prettyPrint' t = show
derive newtype instance writeForeignActionIdentifier :: WriteForeign ActionIdentifier
derive newtype instance readForeignActionIdentifier :: ReadForeign ActionIdentifier

newtype StateIdentifier = StateIdentifier String
derive instance newtypeStateIdentifier :: Newtype StateIdentifier _
derive instance genericRepStateIdentifier :: Generic StateIdentifier _
instance showStateIdentifier :: Show StateIdentifier where
  show i = "StateIdentifier " <> (unwrap i)
instance eqStateIdentifier :: Eq StateIdentifier where
  eq (StateIdentifier id1) (StateIdentifier id2) = id1 == id2
instance ordStateIdentifier :: Ord StateIdentifier where
  compare (StateIdentifier s1) (StateIdentifier s2) = compare s1 s2
instance prettyPrintStateIdentifier :: PrettyPrint StateIdentifier where
  prettyPrint' t = show
derive newtype instance writeForeignStateIdentifier :: WriteForeign StateIdentifier
derive newtype instance readForeignStateIdentifier :: ReadForeign StateIdentifier
instance semiGroupStateIdentifier :: Semigroup StateIdentifier where
  append (StateIdentifier s1) (StateIdentifier s2) = if s1 == "" then StateIdentifier s2 else StateIdentifier (s1 <> "$" <> s2)

externalRoleType :: ContextType -> EnumeratedRoleType
externalRoleType (ContextType ct) = EnumeratedRoleType (ct <> "$External")

externalRoleType_ :: String -> String
externalRoleType_ ct = ct <> "$External"

newtype DomeinFileId = DomeinFileId String
derive instance newtypeDomeinFileId :: Newtype DomeinFileId _
derive instance genericRepDomeinFileId :: Generic DomeinFileId _

instance showDomeinFileId :: Show DomeinFileId where
  show = unwrap
instance eqDomeinFileId :: Eq DomeinFileId where
  eq (DomeinFileId id1) (DomeinFileId id2) = id1 == id2
instance ordDomeinFileId :: Ord DomeinFileId where
  compare (DomeinFileId a) (DomeinFileId b) = compare a b
instance prettyPrintDomeinFileId :: PrettyPrint DomeinFileId where
  prettyPrint' t (DomeinFileId n) = n
derive newtype instance ReadForeign DomeinFileId
derive newtype instance WriteForeign DomeinFileId

data ResourceType = CType ContextType | RType EnumeratedRoleType
derive instance Generic ResourceType _
instance Show ResourceType where show = genericShow
instance Eq ResourceType where eq = genericEq
instance Ord ResourceType where
  compare (CType ct1)(CType ct2) = compare ct1 ct2
  compare (RType ct1)(RType ct2) = compare ct1 ct2
  compare (CType ct1)(RType ct2) = GT
  compare (RType ct1)(CType ct2) = LT