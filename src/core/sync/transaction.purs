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
import Data.Array (length, null, union)
import Data.DateTime.Instant (toDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
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
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Semigroup, class Show, bind, ($), (<>), pure, (>))

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transaction = Transaction (TransactionRecord
  ( invertedQueryResults :: Array InvertedQueryResult
  , correlationIdentifiers :: Array CorrelationIdentifier
  , authoringRole :: RoleType
  -- rolesToExit is the only complete collection of role instances that will be removed
  -- (including role instances that are removed with their context).
  , rolesToExit :: Array RoleInstance
  -- rolesToBeRemoved just holds role instances that are individually removed.
  , rolesToBeRemoved :: Array RoleInstance
  , contextsToBeRemoved :: Array (Tuple ContextInstance (Maybe RoleType))
  , modelsToBeRemoved :: Array DomeinFileId
  , createdContexts :: Array ContextInstance
  , createdRoles :: Array RoleInstance
  -- The first RoleInstance has its binding modified; the second RoleInstance, if present, is the new binding.
  , rolesToUnbind :: Array (Tuple RoleInstance (Maybe RoleInstance))
  -- No Delta and no InvertedQueryResult (leading to state evaluation) should be constructed using one of the
  -- resources in the next two members.
  , untouchableContexts :: Array ContextInstance
  , untouchableRoles :: Array RoleInstance
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
    pure $ Transaction{author, timeStamp, deltas, changedDomeinFiles, invertedQueryResults: [], correlationIdentifiers: [], authoringRole: ENR $ EnumeratedRoleType "model:System$PerspectivesContext$User", rolesToBeRemoved: [], rolesToExit: [], contextsToBeRemoved: [], modelsToBeRemoved: [], createdContexts: [], createdRoles: [], untouchableRoles: [], untouchableContexts: [], rolesToUnbind: []}

instance decodeTransactie' :: Decode Transaction' where
  decode = genericDecode defaultOptions

instance semiGroupTransactie :: Semigroup Transaction where
  append t1@(Transaction {author, timeStamp, deltas, correlationIdentifiers, changedDomeinFiles, invertedQueryResults, authoringRole, rolesToBeRemoved, rolesToExit, contextsToBeRemoved, modelsToBeRemoved, createdContexts, createdRoles, rolesToUnbind, untouchableRoles, untouchableContexts})
    t2@(Transaction {author: a, timeStamp: t, deltas: ds, changedDomeinFiles: cd, invertedQueryResults: iqr, correlationIdentifiers: ci, rolesToBeRemoved: rtbr, rolesToExit: rte, contextsToBeRemoved: ctbr, modelsToBeRemoved: mtbr, createdContexts: cc, rolesToUnbind: rtu, createdRoles: cr, untouchableRoles: ur, untouchableContexts: uc}) = Transaction
      { author: author
      , timeStamp: timeStamp
      , deltas: deltas `union` ds
      , changedDomeinFiles: union changedDomeinFiles cd
      , invertedQueryResults: invertedQueryResults `union` iqr
      , correlationIdentifiers: union correlationIdentifiers ci
      , authoringRole
      , rolesToBeRemoved: rolesToBeRemoved <> rtbr
      , rolesToExit: rolesToExit <> rte
      , contextsToBeRemoved: contextsToBeRemoved <> ctbr
      , modelsToBeRemoved: modelsToBeRemoved <> mtbr
      , createdContexts: createdContexts <> cc
      , createdRoles: createdRoles <> cr
      , untouchableRoles: if length untouchableRoles > length ur then untouchableRoles else ur
      , untouchableContexts: if length untouchableContexts > length uc then untouchableContexts else uc
      , rolesToUnbind: rolesToUnbind <> rtu
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
      , rolesToExit: []
      , contextsToBeRemoved: []
      , modelsToBeRemoved: []
      , createdContexts: []
      , createdRoles: []
      , untouchableContexts: []
      , untouchableRoles: []
      , rolesToUnbind: []
    }

cloneEmptyTransaction :: Transaction -> Transaction
cloneEmptyTransaction (Transaction{ author, timeStamp, authoringRole, untouchableRoles, untouchableContexts}) = Transaction
  { author: author
  , timeStamp: timeStamp
  , deltas: []
  , changedDomeinFiles: []
  , invertedQueryResults: []
  , correlationIdentifiers: []
  , authoringRole
  , rolesToBeRemoved: []
  , rolesToExit: []
  , contextsToBeRemoved: []
  , modelsToBeRemoved: []
  , createdContexts: []
  , createdRoles: []
  , untouchableRoles
  , untouchableContexts
  , rolesToUnbind: []
}

isEmptyTransaction :: Transaction -> Boolean
isEmptyTransaction (Transaction {deltas}) = null deltas
