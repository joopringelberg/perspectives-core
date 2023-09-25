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
import Data.Map (Map, empty, union) as MAP
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign.Class (class Decode, class Encode, encode, decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Persistence.Attachment (class Attachment)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId, EnumeratedRoleType(..), RoleType(..))
import Perspectives.ScheduledAssignment (ScheduledAssignment)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Semigroup, class Show, bind, pure, ($), (&&), (<>), (>))

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transaction = Transaction (TransactionRecord
  ( invertedQueryResults :: Array InvertedQueryResult
  , correlationIdentifiers :: Array CorrelationIdentifier
  -- The role type that must have a sufficient perspective for the deltas in the transaction.
  -- Notice that it may be changed during the transaction, so not all delta's need have the same authoringRole.
  , authoringRole :: RoleType
  -- rolesToExit is the only complete collection of role instances that will be removed
  -- (including role instances that are removed with their context).
  , rolesToExit :: Array RoleInstance
  , scheduledAssignments :: Array ScheduledAssignment
  , modelsToBeRemoved :: Array DomeinFileId
  , createdContexts :: Array ContextInstance
  , createdRoles :: Array RoleInstance
  -- No Delta and no InvertedQueryResult (leading to state evaluation) should be constructed using one of the
  -- resources in the next two members.
  , untouchableContexts :: Array ContextInstance
  , untouchableRoles :: Array RoleInstance
  -- A Map from any RoleInstance to its most deeply nested filler.
  , userRoleBottoms :: MAP.Map RoleInstance RoleInstance
  ))

type TransactionRecord f =
  -- The author is an instance of sys:PerspectivesSystem$User who signs deltas.
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

-- Only used in Tests
instance encodeTransactie :: Encode Transaction where
  encode (Transaction{author, timeStamp, deltas, changedDomeinFiles}) = encode (Transaction'{author, timeStamp, deltas, changedDomeinFiles})

instance encodeTransactie' :: Encode Transaction' where
  encode = genericEncode defaultOptions

-- Only used in Tests
instance decodeTransactie :: Decode Transaction where
  decode f = do
    ((Transaction' {author, timeStamp, deltas, changedDomeinFiles}) :: Transaction') <- decode f
    pure $ Transaction
      { author
      , timeStamp
      , deltas
      , changedDomeinFiles
      , scheduledAssignments: []
      , invertedQueryResults: []
      , correlationIdentifiers: []
      , authoringRole: ENR $ EnumeratedRoleType sysUser
      , rolesToExit: []
      , modelsToBeRemoved: []
      , createdContexts: []
      , createdRoles: []
      , untouchableRoles: []
      , untouchableContexts: []
      , userRoleBottoms: empty
      }

instance decodeTransactie' :: Decode Transaction' where
  decode = genericDecode defaultOptions

instance semiGroupTransactie :: Semigroup Transaction where
  append t1@(Transaction {author, timeStamp, deltas, correlationIdentifiers, changedDomeinFiles, scheduledAssignments, invertedQueryResults, authoringRole, rolesToExit, modelsToBeRemoved, createdContexts, createdRoles, untouchableRoles, untouchableContexts, userRoleBottoms})
    t2@(Transaction {author: a, timeStamp: t, deltas: ds, changedDomeinFiles: cd, scheduledAssignments: sa, invertedQueryResults: iqr, correlationIdentifiers: ci, rolesToExit: rte, modelsToBeRemoved: mtbr, createdContexts: cc, createdRoles: cr, untouchableRoles: ur, untouchableContexts: uc, userRoleBottoms: urb}) = Transaction
      { author: author
      , timeStamp: timeStamp
      , deltas: deltas `union` ds
      , changedDomeinFiles: union changedDomeinFiles cd
      , scheduledAssignments: scheduledAssignments <> sa
      , invertedQueryResults: invertedQueryResults `union` iqr
      , correlationIdentifiers: union correlationIdentifiers ci
      , authoringRole
      , rolesToExit: rolesToExit <> rte
      , modelsToBeRemoved: modelsToBeRemoved <> mtbr
      , createdContexts: createdContexts <> cc
      , createdRoles: createdRoles <> cr
      , untouchableRoles: if length untouchableRoles > length ur then untouchableRoles else ur
      , untouchableContexts: if length untouchableContexts > length uc then untouchableContexts else uc
      , userRoleBottoms: userRoleBottoms `MAP.union` urb
    }

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransaction :: Revision Transaction where
  rev t = Nothing
  changeRevision _ t = t

instance prettyPrintTransaction :: PrettyPrint Transaction where
  prettyPrint' t (Transaction r) = "Transaction " <> prettyPrint' (t <> "  ") r

instance Attachment Transaction where
  setAttachment t _ = t
  getAttachments t = Nothing

createTransaction :: RoleType -> String -> Aff Transaction
createTransaction authoringRole author =
  do
    n <- liftEffect $ now
    pure $ Transaction
      { author: author
      , timeStamp: SerializableDateTime (toDateTime n)
      , deltas: []
      , changedDomeinFiles: []
      , scheduledAssignments: []
      , invertedQueryResults: []
      , correlationIdentifiers: []
      , authoringRole
      , rolesToExit: []
      , modelsToBeRemoved: []
      , createdContexts: []
      , createdRoles: []
      , untouchableContexts: []
      , untouchableRoles: []
      , userRoleBottoms: MAP.empty
    }

cloneEmptyTransaction :: Transaction -> Transaction
cloneEmptyTransaction (Transaction{ author, timeStamp, authoringRole, untouchableRoles, untouchableContexts, userRoleBottoms}) = Transaction
  { author
  , timeStamp
  , authoringRole
  , deltas: []
  , changedDomeinFiles: []
  , scheduledAssignments: []
  , invertedQueryResults: []
  , correlationIdentifiers: []
  , rolesToExit: []
  , modelsToBeRemoved: []
  , createdContexts: []
  , createdRoles: []
  , untouchableRoles
  , untouchableContexts
  , userRoleBottoms
}

-- | We consider a Transaction to be 'empty' when it shows no difference to the clone of the original.
-- | This means it is considered to be not empty when one of the members that is wiped on cloning, has content.
isEmptyTransaction :: Transaction -> Boolean
isEmptyTransaction (Transaction tr) =
  null tr.deltas
  && null tr.changedDomeinFiles
  && null tr.scheduledAssignments
  && null tr.invertedQueryResults
  && null tr.correlationIdentifiers
  && null tr.rolesToExit
  && null tr.modelsToBeRemoved
  && null tr.createdContexts
  && null tr.createdRoles

-----------------------------------------------------------
-- STORAGE SCHEME
-----------------------------------------------------------
-- | Resources (Context- or role instances) are stored under one of several 'schemes'.
-- | All storage options should be understood in terms of Pouchdb databases.
-- | A resource identified by the Default scheme is stored locally, in a database whose 
-- | identifier derives from the identifier sys:Me.
-- | A resource identified by the Local scheme is stored in another private, local database.
-- | Finally, a resource identified by the Remote scheme is stored in a database 
-- | through a REST interface. Because Pouchdb doesn't support the notion of a read-only 
-- | database, we separate a writing endpoint from a reading endpoint.
data StorageScheme = Default DbName | Local DbName | Remote Url

type DbName = String
type Url = String

derive instance Generic StorageScheme _
instance Show StorageScheme where show = genericShow