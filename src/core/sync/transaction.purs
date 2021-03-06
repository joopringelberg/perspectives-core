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

module Perspectives.Sync.Transaction where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
import Data.Array (null, union)
import Data.DateTime.Instant (toDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.DomeinFile (DomeinFileId)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Semigroup, class Show, bind, ($), (<>), pure)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transaction = Transaction (TransactionRecord
  ( invertedQueryResults :: Array InvertedQueryResult
  , correlationIdentifiers :: Array CorrelationIdentifier
  , authoringRole :: RoleType
  , rolesToBeRemoved :: Array RoleInstance
  , contextsToBeRemoved :: Array ContextInstance
  , modelsToBeRemoved :: Array DomeinFileId
  , createdContexts :: Array ContextInstance
  , createdRoles :: Array RoleInstance
  ))

type TransactionRecord f =
  { author :: String
  , timeStamp :: SerializableDateTime
  , deltas :: Array DeltaInTransaction
  , changedDomeinFiles :: Array String
  | f
  }

newtype Transaction' = Transaction' (TransactionRecord())

derive instance genericRepTransactie :: Generic Transaction _
derive instance genericRepTransaction' :: Generic Transaction' _

derive instance newtypeTransactie :: Newtype Transaction _

instance showTransactie :: Show Transaction where
  show = genericShow

instance encodeTransactie :: Encode Transaction where
  encode (Transaction{author, timeStamp, deltas, changedDomeinFiles}) = encode (Transaction'{author, timeStamp, deltas, changedDomeinFiles})

instance encodeTransactie' :: Encode Transaction' where
  encode = genericEncode defaultOptions

instance decodeTransactie :: Decode Transaction where
  decode f = do
    ((Transaction' {author, timeStamp, deltas, changedDomeinFiles}) :: Transaction') <- decode f
    pure $ Transaction{author, timeStamp, deltas, changedDomeinFiles, invertedQueryResults: [], correlationIdentifiers: [], authoringRole: ENR $ EnumeratedRoleType "model:System$PerspectivesContext$User", rolesToBeRemoved: [], contextsToBeRemoved: [], modelsToBeRemoved: [], createdContexts: [], createdRoles: []}

instance decodeTransactie' :: Decode Transaction' where
  decode = genericDecode defaultOptions

instance semiGroupTransactie :: Semigroup Transaction where
  append t1@(Transaction {author, timeStamp, deltas, correlationIdentifiers, changedDomeinFiles, authoringRole, rolesToBeRemoved, contextsToBeRemoved, modelsToBeRemoved, createdContexts, createdRoles})
    t2@(Transaction {author: a, timeStamp: t, deltas: ds, changedDomeinFiles: cd, correlationIdentifiers: ci, rolesToBeRemoved: rtbr, contextsToBeRemoved: ctbr, modelsToBeRemoved: mtbr, createdContexts: cc, createdRoles: cr}) = Transaction
      { author: author
      , timeStamp: timeStamp
      , deltas: deltas `union` ds
      , changedDomeinFiles: union changedDomeinFiles cd
      , invertedQueryResults: []
      , correlationIdentifiers: union correlationIdentifiers ci
      , authoringRole
      , rolesToBeRemoved: rolesToBeRemoved <> rtbr
      , contextsToBeRemoved: contextsToBeRemoved <> ctbr
      , modelsToBeRemoved: modelsToBeRemoved <> mtbr
      , createdContexts: createdContexts <> cc
      , createdRoles: createdRoles <> cr
    }

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransaction :: Revision Transaction where
  rev t = Nothing
  changeRevision _ t = t

instance prettyPrintTransaction :: PrettyPrint Transaction where
  prettyPrint' t (Transaction r) = "Transaction " <> prettyPrint' (t <> "  ") r

createTransaction :: RoleType -> String -> Aff Transaction
createTransaction authoringRole author =
  do
    n <- liftEffect $ now
    pure $ Transaction
      { author: author
      , timeStamp: SerializableDateTime (toDateTime n)
      , deltas: []
      , changedDomeinFiles: []
      , invertedQueryResults: []
      , correlationIdentifiers: []
      , authoringRole
      , rolesToBeRemoved: []
      , contextsToBeRemoved: []
      , modelsToBeRemoved: []
      , createdContexts: []
      , createdRoles: []
    }

cloneEmptyTransaction :: Transaction -> Transaction
cloneEmptyTransaction (Transaction{ author, timeStamp, authoringRole}) = Transaction
  { author: author
  , timeStamp: timeStamp
  , deltas: []
  , changedDomeinFiles: []
  , invertedQueryResults: []
  , correlationIdentifiers: []
  , authoringRole
  , rolesToBeRemoved: []
  , contextsToBeRemoved: []
  , modelsToBeRemoved: []
  , createdContexts: []
  , createdRoles: []
}

isEmptyTransaction :: Transaction -> Boolean
isEmptyTransaction (Transaction {deltas}) = null deltas
