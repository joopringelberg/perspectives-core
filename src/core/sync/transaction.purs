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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Persistence.Attachment (class Attachment)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser, RoleInstance, perspectivesUser2RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId, EnumeratedRoleType(..), RoleType(..))
import Perspectives.ScheduledAssignment (ScheduledAssignment, StateEvaluation)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Eq, class Ord, class Semigroup, class Show, bind, compare, eq, pure, show, ($), (&&), (<>), (>))
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transaction = Transaction (TransactionRecord
  ( -- used in runAllAutomaticActions to compute the states that need evaluation.
    invertedQueryResults :: Array InvertedQueryResult
    -- used in runMonadPerspectivesTransaction'.run to recompute query results.
  , correlationIdentifiers :: Array CorrelationIdentifier
  -- The role type that must have a sufficient perspective for the deltas in the transaction.
  -- Notice that it may be changed during the transaction, so not all delta's need have the same authoringRole.
  , authoringRole :: RoleType
  -- rolesToExit is the only complete collection of role instances that will be removed
  -- (including role instances that are removed with their context).
  -- Used in runEntryAndExitActions to exit role states, for synchronization and for query updates.
  , rolesToExit :: Array RoleInstance
  -- used in runMonadPerspectivesTransaction'.run to execute ContextRemoval and RoleRemoval.
  -- used in runAllAutomaticActions to execute RoleUnbinding and ExecuteDestructiveEffect.
  , scheduledAssignments :: Array ScheduledAssignment
  , modelsToBeRemoved :: Array DomeinFileId
  -- Used in runEntryAndExitActions to enter the root state of a new context.
  , createdContexts :: Array ContextInstance
  -- Used in runEntryAndExitActions to enter the root state of a new context.
  , createdRoles :: Array RoleInstance
  -- No Delta and no InvertedQueryResult (leading to state evaluation) should be constructed using one of the
  -- resources in the next two members.
  -- No state condition should depend on them either, but we do not have a mechanism to compute state conditions
  -- that reckons with them. Instead, we postpone state evaluations that depend on them (which we CAN establish) till
  -- after they have been actually removed.
  , untouchableContexts :: Array ContextInstance
  , untouchableRoles :: Array RoleInstance
  -- A Map from any RoleInstance to its most deeply nested filler.
  , userRoleBottoms :: MAP.Map RoleInstance TransactionDestination
  -- used in runMonadPerspectivesTransaction'.run to evaluate states again (and execute actions on entry and exit).
  , postponedStateEvaluations :: Array StateEvaluation
  ))

data TransactionDestination = PublicDestination RoleInstance | Peer PerspectivesUser
instance Show TransactionDestination where
  show (PublicDestination r) = "(PublicDestination " <> show r <> ")"
  show (Peer p) = "(Peer " <> show p <> ")"
instance Ord TransactionDestination where
  compare (Peer p1) (Peer p2) = compare p1 p2
  compare (PublicDestination p1) (PublicDestination p2) = compare p1 p2
  compare (Peer p1) (PublicDestination p2) = compare (perspectivesUser2RoleInstance p1) p2
  compare (PublicDestination p1) (Peer p2)  = compare p1 (perspectivesUser2RoleInstance p2)
instance Eq TransactionDestination where
  eq (Peer p1) (Peer p2) = eq p1 p2
  eq (PublicDestination p1) (PublicDestination p2) = eq p1 p2
  eq _ _ = false

type TransactionRecord f =
  { author :: PerspectivesUser
  , timeStamp :: SerializableDateTime
  , deltas :: Array DeltaInTransaction
  , changedDomeinFiles :: Array String
  , publicKeys :: ENCMAP.EncodableMap PerspectivesUser PublicKeyInfo
  | f
  }

type PublicKeyInfo = 
  { key :: String                   -- the cryptographic (public) key, serialised as JWK.
  , deltas :: Array SignedDelta
  }

newtype Transaction' = Transaction' (TransactionRecord())

derive instance genericRepTransactie :: Generic Transaction _
derive instance genericRepTransaction' :: Generic Transaction' _

derive instance newtypeTransactie :: Newtype Transaction _

instance showTransactie :: Show Transaction where
  show = genericShow

-- Only used in Tests
instance WriteForeign Transaction where
  writeImpl (Transaction{author, timeStamp, deltas, changedDomeinFiles}) = writeImpl {author, timeStamp, deltas, changedDomeinFiles}

instance ReadForeign Transaction where
  readImpl f = do
    ((Transaction' {author, timeStamp, deltas, changedDomeinFiles}) :: Transaction') <- read' f
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
      , userRoleBottoms: MAP.empty
      , publicKeys: ENCMAP.empty
      , postponedStateEvaluations: []
      }

derive newtype instance ReadForeign Transaction'

instance semiGroupTransactie :: Semigroup Transaction where
  append t1@(Transaction {author, timeStamp, deltas, correlationIdentifiers, changedDomeinFiles, scheduledAssignments, invertedQueryResults, authoringRole, rolesToExit, modelsToBeRemoved, createdContexts, createdRoles, untouchableRoles, untouchableContexts, userRoleBottoms, publicKeys, postponedStateEvaluations})
    t2@(Transaction {author: a, timeStamp: t, deltas: ds, changedDomeinFiles: cd, scheduledAssignments: sa, invertedQueryResults: iqr, correlationIdentifiers: ci, rolesToExit: rte, modelsToBeRemoved: mtbr, createdContexts: cc, createdRoles: cr, untouchableRoles: ur, untouchableContexts: uc, userRoleBottoms: urb, publicKeys: pk, postponedStateEvaluations: pse}) = Transaction
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
      , publicKeys: publicKeys `ENCMAP.union` pk
      , postponedStateEvaluations: postponedStateEvaluations <> pse
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

createTransaction :: RoleType -> PerspectivesUser -> Aff Transaction
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
      , publicKeys: ENCMAP.empty
      , postponedStateEvaluations: []
    }

cloneEmptyTransaction :: Transaction -> Transaction
cloneEmptyTransaction (Transaction{ author, timeStamp, authoringRole, untouchableRoles, untouchableContexts, userRoleBottoms, publicKeys, postponedStateEvaluations}) = Transaction
  { author
  , timeStamp
  , authoringRole

  , deltas: []
  , changedDomeinFiles: []
  , scheduledAssignments: []      --
  , invertedQueryResults: []
  , correlationIdentifiers: []
  , rolesToExit: []               --
  , modelsToBeRemoved: []
  , createdContexts: []           --
  , createdRoles: []              --

  , untouchableRoles
  , untouchableContexts
  , userRoleBottoms
  , publicKeys
  , postponedStateEvaluations
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