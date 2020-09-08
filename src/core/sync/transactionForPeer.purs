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

module Perspectives.Sync.TransactionForPeer where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
import Data.Array (snoc)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Sync.DateTime (SerializableDateTime)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Prelude (class Show, (<>), show)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype TransactionForPeer = TransactionForPeer
  { author :: String
  , timeStamp :: SerializableDateTime
  , deltas :: Array SignedDelta
  }

derive instance genericRepTransactionForPeer :: Generic TransactionForPeer _

derive instance newtypeTransactionForPeer :: Newtype TransactionForPeer _

instance showTransactie :: Show TransactionForPeer where
  show = genericShow

instance encodeTransactionForPeer :: Encode TransactionForPeer where
  encode = genericEncode defaultOptions

instance decodeTransactie :: Decode TransactionForPeer where
  decode = genericDecode defaultOptions

addToTransactionForPeer :: SignedDelta -> TransactionForPeer -> TransactionForPeer
addToTransactionForPeer d (TransactionForPeer r@{deltas}) = TransactionForPeer r {deltas = snoc deltas d}

transactieID :: TransactionForPeer -> String
transactieID (TransactionForPeer{author, timeStamp}) = author <> "_" <> show timeStamp

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransactionForPeer :: Revision TransactionForPeer where
  rev t = Nothing
  changeRevision _ t = t
