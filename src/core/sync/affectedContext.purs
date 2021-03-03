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

module Perspectives.Sync.AffectedContext where

import Prelude

import Data.Array (null, difference) as ARR
import Data.Array.NonEmpty (NonEmptyArray, toArray, difference)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Encode, encode)
import Foreign.Generic (defaultOptions, genericEncode)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (RoleType)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

-- | By construction the contextInstances will be of the same type and the userTypes are roles in that type of Context.
newtype AffectedContext = AffectedContext {contextInstances :: NonEmptyArray ContextInstance, userTypes :: Array RoleType}

derive instance genericAffectedContext :: Generic AffectedContext _

instance showAffectedContext :: Show AffectedContext where
  show = genericShow

newtype AffectedContext' = AffectedContext' {contextInstances :: Array ContextInstance, userTypes :: Array RoleType}

derive instance genericAffectedContext' :: Generic AffectedContext' _

instance encodeAffectedContext' :: Encode AffectedContext' where
  encode = genericEncode defaultOptions

instance encodeAffectedContext :: Encode AffectedContext where
  encode (AffectedContext {contextInstances, userTypes}) = encode (AffectedContext' {contextInstances: toArray contextInstances, userTypes})

instance eqAffectedContext :: Eq AffectedContext where
  eq (AffectedContext {contextInstances: c1, userTypes: u1}) (AffectedContext {contextInstances: c2, userTypes: u2}) =
    ARR.null (difference c1 c2) && ARR.null (ARR.difference u1 u2)

instance prettyPrintAffectedContext :: PrettyPrint AffectedContext where
  prettyPrint' t (AffectedContext r@{contextInstances}) = "AffectedContext " <> prettyPrint' (t <> "  ") (r {contextInstances = toArray contextInstances})
