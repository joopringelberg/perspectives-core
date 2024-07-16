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

import Data.Array (null)
import Data.DateTime.Instant (toDateTime)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, empty) as MAP
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign (Foreign)
import Persistence.Attachment (class Attachment)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId, EnumeratedRoleType(..), RoleType(..))
import Perspectives.ScheduledAssignment (ScheduledAssignment, StateEvaluation)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier, unschemeRoleInstance)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Eq, class Ord, class Show, bind, compare, eq, pure, show, ($), (&&), (<>))
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
  -- A Transaction for the users TheWorld and SocialEnvironment. Included as uninterpreted data to avoid
  -- module cycles. Use read_ to transform to Maybe TransactionForPeer.
  , identityDocument :: Maybe UninterpretedTransactionForPeer
  -- Maybe an index where to insert deltas.
  , insertionPoint :: Maybe Int
  ))

data TransactionDestination = PublicDestination RoleInstance | Peer UnschemedResourceIdentifier
instance Show TransactionDestination where
  show (PublicDestination r) = "(PublicDestination " <> show r <> ")"
  show (Peer p) = "(Peer " <> show p <> ")"
instance Ord TransactionDestination where
  compare (Peer p1) (Peer p2) = compare p1 p2
  compare (PublicDestination p1) (PublicDestination p2) = compare p1 p2
  compare (Peer p1) (PublicDestination p2) = compare p1 (unschemeRoleInstance p2)
  compare (PublicDestination p1) (Peer p2)  = compare (unschemeRoleInstance p1) p2
instance Eq TransactionDestination where
  eq (Peer p1) (Peer p2) = eq p1 p2
  eq (PublicDestination p1) (PublicDestination p2) = eq p1 p2
  eq _ _ = false

type TransactionRecord f =
  { timeStamp :: SerializableDateTime
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
  writeImpl (Transaction{timeStamp, deltas, changedDomeinFiles}) = writeImpl {timeStamp, deltas, changedDomeinFiles}

instance ReadForeign Transaction where
  readImpl f = do
    ((Transaction' {timeStamp, deltas, changedDomeinFiles}) :: Transaction') <- read' f
    pure $ Transaction
      { timeStamp
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
      , identityDocument: Nothing
      , insertionPoint: Nothing
      }

derive newtype instance ReadForeign Transaction'

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransaction :: Revision Transaction where
  rev t = Nothing
  changeRevision _ t = t

instance prettyPrintTransaction :: PrettyPrint Transaction where
  prettyPrint' t (Transaction r) = "Transaction " <> prettyPrint' (t <> "  ") r

instance Attachment Transaction where
  setAttachment t _ = t
  getAttachments t = Nothing

createTransaction :: RoleType -> Aff Transaction
createTransaction authoringRole =
  do
    n <- liftEffect $ now
    pure $ Transaction
      { timeStamp: SerializableDateTime (toDateTime n)
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
      , identityDocument: Nothing
      , insertionPoint: Nothing
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


newtype UninterpretedTransactionForPeer = UninterpretedTransactionForPeer Foreign
derive instance Newtype UninterpretedTransactionForPeer _
instance Show UninterpretedTransactionForPeer where show _ = "UninterpretedTransactionForPeer"
instance PrettyPrint UninterpretedTransactionForPeer where prettyPrint' t _ = t <> "UninterpretedTransactionForPeer"
