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

module Perspectives.Sync.TransactionForPeer where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------

import Data.Array (elemIndex, snoc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Persistence.Attachment (class Attachment)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser)
import Perspectives.Sync.DateTime (SerializableDateTime)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Sync.Transaction (PublicKeyInfo)
import Prelude (class Ord, class Show, class Eq, compare, show, ($), (<>), (&&), eq)
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype TransactionForPeer = TransactionForPeer
  { author :: PerspectivesUser
  , perspectivesSystem :: ContextInstance
  , timeStamp :: SerializableDateTime
  , deltas :: Array SignedDelta
  , publicKeys :: ENCMAP.EncodableMap PerspectivesUser PublicKeyInfo
  }

derive instance genericRepTransactionForPeer :: Generic TransactionForPeer _

derive instance newtypeTransactionForPeer :: Newtype TransactionForPeer _

instance showTransactionForPeer :: Show TransactionForPeer where
  show = genericShow

derive newtype instance ReadForeign TransactionForPeer
derive newtype instance WriteForeign TransactionForPeer

instance Attachment TransactionForPeer where 
  getAttachments _ = Nothing
  setAttachment t _ = t

-- | Add the new delta to the end of the array, in the Transaction.
addToTransactionForPeer :: SignedDelta -> TransactionForPeer -> TransactionForPeer
addToTransactionForPeer d (TransactionForPeer r@{deltas}) = TransactionForPeer r {deltas = if isJust $ elemIndex d deltas
    then deltas
    else snoc deltas d}

transactieID :: TransactionForPeer -> String
transactieID (TransactionForPeer{author, timeStamp}) = show author <> "_" <> show timeStamp

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransactionForPeer :: Revision TransactionForPeer where
  rev t = Nothing
  changeRevision _ t = t

instance eqTransactionForPeer :: Eq TransactionForPeer where
  eq (TransactionForPeer{timeStamp: t1, author: a1}) (TransactionForPeer{timeStamp: t2, author: a2}) = eq t1 t2 && eq a1 a2

instance ordTransactionForPeer :: Ord TransactionForPeer where
  compare (TransactionForPeer{timeStamp: t1}) (TransactionForPeer{timeStamp: t2}) = compare t1 t2
