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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Persistence.Attachment (class Attachment)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier)
import Prelude (class Eq, class Ord, class Show, compare, eq)
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- OUTGOINGTRANSACTION
-----------------------------------------------------------
newtype OutgoingTransaction = OutgoingTransaction
  { _id :: String
  , receiver :: UnschemedResourceIdentifier
  , transaction :: TransactionForPeer
  }

derive instance genericRepOutgoingTransaction :: Generic OutgoingTransaction _

derive instance newtypeOutgoingTransaction :: Newtype OutgoingTransaction _

instance showOutgoingTransaction :: Show OutgoingTransaction where
  show = genericShow

derive newtype instance WriteForeign OutgoingTransaction
derive newtype instance ReadForeign OutgoingTransaction

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionOutgoingTransaction :: Revision OutgoingTransaction where
  rev t = Nothing
  changeRevision _ t = t

instance eqOutgoingTransaction :: Eq OutgoingTransaction where
  eq (OutgoingTransaction t1) (OutgoingTransaction t2) = eq t1 t2

instance ordOutgoingTransaction :: Ord OutgoingTransaction where
  compare (OutgoingTransaction {transaction: t1}) (OutgoingTransaction {transaction: t2}) = compare t1 t2

instance Attachment OutgoingTransaction where
  getAttachments o = Nothing
  setAttachment o _ = o