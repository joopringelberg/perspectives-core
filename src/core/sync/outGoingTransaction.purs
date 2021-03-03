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

module Perspectives.Sync.OutgoingTransaction where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Prelude (class Show)

-----------------------------------------------------------
-- OUTGOINGTRANSACTION
-----------------------------------------------------------
newtype OutgoingTransaction = OutgoingTransaction
  { receiver :: String
  , transaction :: TransactionForPeer
  }

derive instance genericRepOutgoingTransaction :: Generic OutgoingTransaction _

derive instance newtypeOutgoingTransaction :: Newtype OutgoingTransaction _

instance showOutgoingTransaction :: Show OutgoingTransaction where
  show = genericShow

instance encodeOutgoingTransaction :: Encode OutgoingTransaction where
  encode = genericEncode defaultOptions

instance decodeTransactie :: Decode OutgoingTransaction where
  decode = genericDecode defaultOptions

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionOutgoingTransaction :: Revision OutgoingTransaction where
  rev t = Nothing
  changeRevision _ t = t
