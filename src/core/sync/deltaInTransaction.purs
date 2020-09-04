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

module Perspectives.Sync.DeltaInTransaction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

newtype DeltaInTransaction = DeltaInTransaction {users :: Array RoleInstance, delta :: SignedDelta}

derive instance genericRepDeltaInTransaction :: Generic DeltaInTransaction _

instance showDeltaInTransaction :: Show DeltaInTransaction where
  show = genericShow

instance encodeDeltaInTransaction :: Encode DeltaInTransaction where
  encode = genericEncode defaultOptions

instance decodeDeltaInTransaction :: Decode DeltaInTransaction where
  decode = genericDecode defaultOptions

instance eqDeltaInTransaction :: Eq DeltaInTransaction where
  eq = genericEq

derive instance newtypeDeltaInTransaction :: Newtype DeltaInTransaction _

instance prettyPrintDeltaInTransaction :: PrettyPrint DeltaInTransaction where
  prettyPrint' t (DeltaInTransaction r) = "DeltaInTransaction " <> prettyPrint' (t <> "  ") r