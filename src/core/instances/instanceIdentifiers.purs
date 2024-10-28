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

module Perspectives.Representation.InstanceIdentifiers where

import Data.Generic.Rep (class Generic) 
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Perspectives.Utilities (class PrettyPrint)
import Prelude (class Eq, class Ord, class Show, compare, show, (<>), (==))
import Safe.Coerce (coerce)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype ContextInstance = ContextInstance String
derive instance newtypeContextInstance :: Newtype ContextInstance _
derive instance genericRepContextInstance :: Generic ContextInstance _

derive newtype instance writeForeignContextInstance :: WriteForeign ContextInstance
derive newtype instance readForeignContextInstance :: ReadForeign ContextInstance
instance showContextInstance :: Show ContextInstance where
  show x = show (unwrap x)
instance eqContextInstance :: Eq ContextInstance where
  eq (ContextInstance id1) (ContextInstance id2) = id1 == id2
instance ordContextInstance :: Ord ContextInstance where
  compare (ContextInstance a) (ContextInstance b) = compare a b
instance prettyPrintContextInstance :: PrettyPrint ContextInstance where
  prettyPrint' t = show


newtype RoleInstance = RoleInstance String
derive instance newtypeRoleInstance :: Newtype RoleInstance _
derive instance genericRepRoleInstance :: Generic RoleInstance _

derive newtype instance writeForeignRoleInstance :: WriteForeign RoleInstance
derive newtype instance readForeignRoleInstance :: ReadForeign RoleInstance
instance showRoleInstance :: Show RoleInstance where
  show x = show (unwrap x)
instance eqRoleInstance :: Eq RoleInstance where
  eq (RoleInstance id1) (RoleInstance id2) = id1 == id2
instance ordRoleInstance :: Ord RoleInstance where
  compare (RoleInstance a) (RoleInstance b) = compare a b
instance prettyPrintRoleInstance :: PrettyPrint RoleInstance where
  prettyPrint' t = show

newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance genericRepValue :: Generic Value _

derive newtype instance WriteForeign Value
derive newtype instance ReadForeign Value
instance showValue :: Show Value where
  show x = show (unwrap x)
instance eqValue :: Eq Value where
  eq (Value id1) (Value id2) = id1 == id2
instance ordValue :: Ord Value where
  compare (Value v1) (Value v2) = compare v1 v2

instance prettyPrintValue :: PrettyPrint Value where
  prettyPrint' t = show

externalRole :: ContextInstance -> RoleInstance
externalRole ct = RoleInstance (unwrap ct <> "$External")

newtype PerspectivesUser = PerspectivesUser String
derive instance Newtype PerspectivesUser _
derive instance Generic PerspectivesUser _
derive newtype instance WriteForeign PerspectivesUser
derive newtype instance ReadForeign PerspectivesUser
instance Eq PerspectivesUser where 
  eq (PerspectivesUser p1) (PerspectivesUser p2) = p1 == p2
instance Show PerspectivesUser where show = genericShow
instance PrettyPrint PerspectivesUser where
  prettyPrint' t = show
instance Ord PerspectivesUser where
  compare (PerspectivesUser v1) (PerspectivesUser v2) = compare v1 v2

perspectivesUser2RoleInstance :: PerspectivesUser -> RoleInstance
perspectivesUser2RoleInstance = coerce

roleInstance2PerspectivesUser :: RoleInstance -> PerspectivesUser
roleInstance2PerspectivesUser = coerce

newtype PerspectivesSystemUser = PerspectivesSystemUser String
derive instance Newtype PerspectivesSystemUser _

perspectivesSystemUser2RoleInstance :: PerspectivesSystemUser -> RoleInstance
perspectivesSystemUser2RoleInstance = coerce

roleInstance2PerspectivesSystemUser :: RoleInstance -> PerspectivesSystemUser
roleInstance2PerspectivesSystemUser = coerce
