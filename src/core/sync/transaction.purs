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

module Perspectives.Sync.Transaction where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
import Data.Array (union)
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
import Perspectives.Sync.AffectedContext (AffectedContext)
import Perspectives.Sync.Class.DeltaClass (addBase)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.TypesForDeltas (ContextDelta, RoleBindingDelta, RolePropertyDelta, UniverseContextDelta, UniverseRoleDelta)
import Prelude (class Semigroup, class Show, bind, ($), (<>), show, pure, (<$>), (==), (+), (-))

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transaction = Transaction (TransactionRecord( affectedContexts :: Array AffectedContext, correlationIdentifiers :: Array CorrelationIdentifier, nextDeltaIndex :: Int ))

type TransactionRecord f =
  { author :: String
  , timeStamp :: SerializableDateTime
  , contextDeltas :: Array ContextDelta
  , roleDeltas :: Array RoleBindingDelta
  , propertyDeltas :: Array RolePropertyDelta
  , universeContextDeltas :: Array UniverseContextDelta
  , universeRoleDeltas :: Array UniverseRoleDelta
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
  encode (Transaction{author, timeStamp, contextDeltas, roleDeltas, propertyDeltas, universeContextDeltas, universeRoleDeltas, changedDomeinFiles}) = encode (Transaction'{author, timeStamp, contextDeltas, roleDeltas, propertyDeltas, universeContextDeltas, universeRoleDeltas, changedDomeinFiles})

instance encodeTransactie' :: Encode Transaction' where
  encode = genericEncode defaultOptions

instance decodeTransactie :: Decode Transaction where
  decode f = do
    ((Transaction' {author, timeStamp, contextDeltas, roleDeltas, propertyDeltas, universeContextDeltas, universeRoleDeltas, changedDomeinFiles}) :: Transaction') <- decode f
    pure $ Transaction{author, timeStamp, contextDeltas, roleDeltas, propertyDeltas, universeContextDeltas, universeRoleDeltas, changedDomeinFiles, affectedContexts: [], correlationIdentifiers: [], nextDeltaIndex: 0}

instance decodeTransactie' :: Decode Transaction' where
  decode = genericDecode defaultOptions

instance semiGroupTransactie :: Semigroup Transaction where
  append t1@(Transaction {author, timeStamp, contextDeltas, roleDeltas, propertyDeltas, universeContextDeltas, universeRoleDeltas, changedDomeinFiles, nextDeltaIndex: base})
    t2@(Transaction {author: a, timeStamp: t, contextDeltas: r, roleDeltas: b, propertyDeltas: p, universeContextDeltas: uc, universeRoleDeltas: ur, changedDomeinFiles: cd, nextDeltaIndex: extra}) = Transaction
      { author: author
      , timeStamp: timeStamp
      , contextDeltas: union contextDeltas (addBase base <$> r)
      , roleDeltas: union roleDeltas (addBase base <$> b)
      , propertyDeltas: union propertyDeltas (addBase base <$> p)
      , universeContextDeltas: union universeContextDeltas (addBase base <$> uc)
      , universeRoleDeltas: union universeRoleDeltas (addBase base <$> ur)
      , changedDomeinFiles: union changedDomeinFiles cd
      , affectedContexts: []
      , correlationIdentifiers: []
      , nextDeltaIndex: base + extra - 1
    }

-- | The Revision instance is a stub; we don't really need it (except in tests).
instance revisionTransaction :: Revision Transaction where
  rev t = Nothing
  changeRevision _ t = t

createTransactie :: String -> Aff Transaction
createTransactie author =
  do
    n <- liftEffect $ now
    pure $ Transaction
      { author: author
      , timeStamp: SerializableDateTime (toDateTime n)
      , contextDeltas: []
      , roleDeltas: []
      , propertyDeltas: []
      , universeContextDeltas: []
      , universeRoleDeltas: []
      , changedDomeinFiles: []
      , affectedContexts: []
      , correlationIdentifiers: []
      , nextDeltaIndex: 0}

cloneEmptyTransaction :: Transaction -> Transaction
cloneEmptyTransaction (Transaction{ author, timeStamp}) = Transaction
  { author: author
  , timeStamp: timeStamp
  , contextDeltas: []
  , roleDeltas: []
  , propertyDeltas: []
  , universeContextDeltas: []
  , universeRoleDeltas: []
  , changedDomeinFiles: []
  , affectedContexts: []
  , correlationIdentifiers: []
  , nextDeltaIndex: 0}

isEmptyTransaction :: Transaction -> Boolean
isEmptyTransaction (Transaction {nextDeltaIndex}) = nextDeltaIndex == 0

transactieID :: Transaction -> String
transactieID (Transaction{author, timeStamp}) = author <> "_" <> show timeStamp
