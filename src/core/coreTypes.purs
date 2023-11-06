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


module Perspectives.CoreTypes

  where

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (foldMap, foldl, foldr, head, union)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, throwError)
import Effect.Aff.AVar (AVar)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as F
import LRUCache (Cache)
import Perspectives.AMQP.Stomp (ConnectAndSubscriptionParameters, StompClient)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.Environment (Environment)
import Perspectives.Persistence.Types (PouchdbState)
import Perspectives.Persistent.ChangesFeed (EventSource)
import Perspectives.Repetition (Duration)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, DomeinFileId, EnumeratedPropertyType, EnumeratedRoleType, ResourceType, RoleType, StateIdentifier)
import Perspectives.Sync.Transaction (Transaction, StorageScheme)
import Prelude (class Eq, class Monoid, class Ord, class Semigroup, class Show, Unit, bind, compare, eq, pure, show, ($), (<<<), (<>), (>>=))

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextInstances = Cache (AVar PerspectContext)
type RolInstances = Cache (AVar PerspectRol)
type DomeinCache = Cache (AVar DomeinFile)

type BrokerService = ConnectAndSubscriptionParameters ( url :: String )

type PerspectivesState = PouchdbState PerspectivesExtraState

type PerspectivesExtraState =
  -- Caching instances
  ( rolInstances :: RolInstances

  , contextInstances :: ContextInstances

  -- Caching Domein files
  , domeinCache :: DomeinCache

  , queryAssumptionRegister :: AssumptionRegister

  , variableBindings :: Environment (Array String)

  , indexedRoles :: Object RoleInstance

  , indexedContexts :: Object ContextInstance

  , post :: Maybe EventSource

  , developmentRepository :: String

  -- Do not confuse with transactionFlag! This member is used for numbering transactions that are sent to peers.
  , transactionNumber :: Int

  , brokerService :: Maybe BrokerService

  , stompClient :: Maybe StompClient

  , warnings :: Array String

  -- Do not confuse with transactionNumber! This member is used in runMonadPerspectivesTransaction.
  , transactionFlag :: AVar Int

  , transactionWithTiming :: AVar RepeatingTransaction

  , modelToLoad :: AVar JustInTimeModelLoad

  , transactionFibers :: Map (Tuple String StateIdentifier) (Fiber Unit)

  , typeToStorage :: Map ResourceType StorageScheme

  -- We want to check on locally stored roles filled with these roles.
  , publicRolesJustLoaded :: Array RoleInstance

  , runtimeOptions :: RuntimeOptions

  )

-- | These are options that can be provided to the PDR at startup.
type RuntimeOptions = 
  -- Default: true. Should be false when someone installs MyContexts on a second device.
  { isFirstInstallation :: Boolean
  -- Default: null. Provide a value to test setup of an experimental new System version.
  , useSystemVersion :: Nullable String
  }


data RepeatingTransaction = TransactionWithTiming
  { transaction :: MonadPerspectivesTransaction Unit
  , interval :: Duration
  , instanceId :: String
  , stateId :: StateIdentifier
  , authoringRole :: RoleType
  , startMoment :: Maybe Duration
  , endMoment :: Maybe Duration
  }
  |
  RepeatNtimes 
  { transaction :: MonadPerspectivesTransaction Unit
  , interval :: Duration
  , nrOfTimes :: Int
  , instanceId :: String
  , stateId :: StateIdentifier
  , authoringRole :: RoleType
  , startMoment :: Maybe Duration
  , endMoment :: Maybe Duration
  }

data JustInTimeModelLoad = LoadModel DomeinFileId | ModelLoaded | LoadingFailed String | Stop | HotLine (AVar JustInTimeModelLoad)

-----------------------------------------------------------
-- ASSUMPTIONS
-----------------------------------------------------------
-- | An Assumption is a combination of a resource (ContextInstance or RoleInstance) and a type
-- | (EnumeratedRoleType, CalculatedRoleType, EnumeratedPropertyType or CalculatedPropertyType).
-- | However, in the assumption administration we omit these newtypes.
type Assumption = Tuple String String

assumption :: String -> String -> Assumption
assumption = Tuple

-- | The AssumptionRegister is indexed by the two elements of an Assumption, in order.
-- | An instance of the AssumptionRegister is stored in PerspectivesState, so all functions operating on it
-- | must be run in MonadPerspectives.
-- | The CorrelationIdentifiers identify an effect and a query that depends (partly) on the assumption that it is
-- | registered with.
type AssumptionRegister = F.Object (F.Object (Array CorrelationIdentifier))

-----------------------------------------------------------
-- INFORMED ASSUMPTIONS
-----------------------------------------------------------
data InformedAssumption =
  RoleAssumption ContextInstance EnumeratedRoleType
  | Me ContextInstance
  -- Filler = filler role
  | Filler RoleInstance                               -- RoleInstance is the role with a binding
  -- FilledRolesAssumption fillerId filledContextType filledType
  | FilledRolesAssumption RoleInstance ContextType EnumeratedRoleType
  | Property RoleInstance EnumeratedPropertyType
  | Context RoleInstance
  | External ContextInstance
  | State ContextInstance
  | RoleState RoleInstance

derive instance genericInformedAssumption :: Generic InformedAssumption _

instance eqInformedAssumption :: Eq InformedAssumption where
  eq = genericEq

instance showInformedAssumption :: Show InformedAssumption where
  show = genericShow

-----------------------------------------------------------
-- STATE EXECUTION
-----------------------------------------------------------
-- | A StateEvaluation is a combination of an instance of a Context, the user role type
-- | played in it by the current (own) user and a State type (its root state).
-- TODO. We don't use the user role type anymore.
data StateEvaluation =
  ContextStateEvaluation StateIdentifier ContextInstance RoleType |
  RoleStateEvaluation StateIdentifier RoleInstance RoleType

derive instance genericStateEvaluation :: Generic StateEvaluation _

instance eqStateEvaluation :: Eq StateEvaluation where
  eq = genericEq

instance showStateEvaluation :: Show StateEvaluation where
  show = genericShow

-----------------------------------------------------------
-- MONADPERSPECTIVES
-----------------------------------------------------------
-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives = ReaderT (AVar PerspectivesState) Aff

type MP = MonadPerspectives

-----------------------------------------------------------
-- OBJECTSGETTER
-----------------------------------------------------------
type ObjectsGetter s o = s -> MP (Array o)

infixl 0 type ObjectsGetter as ##>

-----------------------------------------------------------
-- TYPELEVELGETTER
-----------------------------------------------------------
type TypeLevelResults o = ArrayT MP o

type TypeLevelGetter s o = s -> TypeLevelResults o

infixl 0 type TypeLevelGetter as ~~~>

-- | Use to make a TypeLevelGetter accept an array of values:
-- | forceArray >=> <some TypeLevelGetter> :: Array s ~~~> o
forceTypeArray :: forall a. Array a ~~~> a
forceTypeArray = ArrayT <<< pure

-----------------------------------------------------------
-- RUN TYPELEVELGETTER TO GET AN ARRAY OF RESULTS
-----------------------------------------------------------
-- | Run a TypeLevelGetter on its argument to obtain the Array of results in MonadPerspectives.
runTypeLevelToArray :: forall s o.
  s
  -> (s ~~~> o)
  -> MonadPerspectives (Array o)
runTypeLevelToArray a f = runArrayT (f a)

infixl 1 runTypeLevelToArray as ###=

-----------------------------------------------------------
-- OBTAIN JUST A SINGLE RESULT OR NOTHING (###>) FROM A TYPELEVELGETTER
-----------------------------------------------------------
runTypeLevelToMaybeObject :: forall s o. s -> (s ~~~> o) -> (MonadPerspectives) (Maybe o)
runTypeLevelToMaybeObject id tog = runTypeLevelToArray id tog >>= pure <<< head

infix 1 runTypeLevelToMaybeObject as ###>

-----------------------------------------------------------
-- OBTAIN A SINGLE RESULT OR AN ERROR (###>>) FROM A TYPELEVELGETTER
-----------------------------------------------------------
runTypeLevelToObject :: forall s o. Show s => s -> (s ~~~> o) -> (MonadPerspectives) o
runTypeLevelToObject id tog = runTypeLevelToArray id tog >>= \objects ->
  case head objects of
  Nothing -> throwError $ error $ "TypeLevelGetter returns no values for '" <> (show id) <> "'."
  (Just obj) -> pure obj

infix 1 runTypeLevelToObject as ###>>

-----------------------------------------------------------
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment accumulates Assumptions.

type AssumptionTracking = (WriterT (ArrayWithoutDoubles InformedAssumption) MonadPerspectives)

type MonadPerspectivesQuery =  ArrayT AssumptionTracking

type MPQ = MonadPerspectivesQuery

newtype ArrayWithoutDoubles a = ArrayWithoutDoubles (Array a)

derive instance newtypeArrayWithoutDoubles :: Newtype (ArrayWithoutDoubles a) _

instance semigroupArrayWithoutDoubles :: Eq a => Semigroup (ArrayWithoutDoubles a) where
  append (ArrayWithoutDoubles a1) (ArrayWithoutDoubles a2) = ArrayWithoutDoubles (a1 `union` a2)

instance monoidArrayWithoutDoubles :: Eq a => Monoid (ArrayWithoutDoubles a) where
  mempty = ArrayWithoutDoubles []

instance foldableArrayWithoutDoubles :: Eq a => Foldable ArrayWithoutDoubles where
  foldl f b (ArrayWithoutDoubles fa) = foldl f b fa
  foldr f b (ArrayWithoutDoubles fa) = foldr f b fa
  foldMap f (ArrayWithoutDoubles fa) = foldMap f fa

-----------------------------------------------------------
-- TRACKINGOBJECTSGETTER
-----------------------------------------------------------
type TrackingObjectsGetter s o = s -> MPQ o

infixl 5 type TrackingObjectsGetter as ~~>

-- | Use to make a TypeLevelGetter accept an array of values:
-- | forceArray >=> <some TypeLevelGetter> :: Array s ~~~> o
forceArray :: forall a. Array a ~~> a
forceArray = ArrayT <<< pure

type RoleGetter = ContextInstance ~~> RoleInstance

type PropertyValueGetter = RoleInstance ~~> Value

type ContextPropertyValueGetter = ContextInstance ~~> Value
-----------------------------------------------------------
-- RUN TRACKINGOBJECTSGETTER TO GET ASSUMPTIONS AND AN ARRAY OF RESULTS
-----------------------------------------------------------
-- | Run a TrackingObjectsGetter on its argument to obtain both the underlying assumptions and the Array of results in MonadPerspectives.
runMonadPerspectivesQuery :: forall s o.
  s
  -> (s ~~> o)
  -> (MonadPerspectives (WithAssumptions o))
runMonadPerspectivesQuery a f = runWriterT (runArrayT (f a))

type WithAssumptions o = Tuple (Array o) (ArrayWithoutDoubles InformedAssumption)
-----------------------------------------------------------
-- EVAL TRACKINGOBJECTSGETTER TO GET AN ARRAY OF RESULTS
-----------------------------------------------------------
-- | Apply a TrackingObjectsGetter to an argument to obtain an Array of results in MonadPerspectives.
evalMonadPerspectivesQuery :: forall s o.
  s
  -> (s ~~> o)
  -> (MonadPerspectives (Array o))
evalMonadPerspectivesQuery a f = do
    (Tuple result _) <- runMonadPerspectivesQuery a f
    pure result

infix 0 evalMonadPerspectivesQuery as ##=

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN JUST A SINGLE RESULT OR NOTHING (##>) FROM TRACKINGOBJECTSGETTER
------------------------------------------------------------------------------------------------------------------------
evalMonadPerspectivesQueryToMaybeObject :: forall s o. s -> (s ~~> o) -> (MonadPerspectives) (Maybe o)
evalMonadPerspectivesQueryToMaybeObject id tog = evalMonadPerspectivesQuery id tog >>= pure <<< head

infix 0 evalMonadPerspectivesQueryToMaybeObject as ##>

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A SINGLE RESULT OR AN ERROR (##>>) FROM TRACKINGOBJECTSGETTER
------------------------------------------------------------------------------------------------------------------------
runMonadPerspectivesQueryToObject :: forall s o. Newtype s String => s -> (s ~~> o) -> (MonadPerspectives) o
runMonadPerspectivesQueryToObject id tog = evalMonadPerspectivesQuery id tog >>= \objects ->
  case head objects of
  Nothing -> throwError $ error $ "ObjectsGetter returns no values for '" <> (unwrap id) <> "' (look for `##>>`)."
  (Just obj) -> pure obj

infix 0 runMonadPerspectivesQueryToObject as ##>>

-----------------------------------------------------------
-- EXEC TRACKINGOBJECTSGETTER TO GET AN ARRAY INFORMEDASSUMPTIONS
-----------------------------------------------------------
-- | Apply a TrackingObjectsGetter to an argument to obtain the informedAssumptions that underly it.
execMonadPerspectivesQuery :: forall s o.
  s
  -> (s ~~> o)
  -> (MonadPerspectives (Array InformedAssumption))
execMonadPerspectivesQuery a f = do
    (Tuple _ assumptions) <- runMonadPerspectivesQuery a f
    pure (unwrap assumptions)

-----------------------------------------------------------
-- COMPOSE INSTANCE LEVEL AND TYPE LEVEL GETTERS
-----------------------------------------------------------
liftToInstanceLevel :: forall s o. (s ~~~> o) -> (s ~~> o)
liftToInstanceLevel f = ArrayT <<< lift <<< runArrayT <<< f

-----------------------------------------------------------
-- MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | The Transaction accumulates Deltas.

type MonadPerspectivesTransaction =  ReaderT (AVar Transaction) MonadPerspectives

type MPT = MonadPerspectivesTransaction

-----------------------------------------------------------
-- UPDATER
-----------------------------------------------------------
type Updater s = s -> MonadPerspectivesTransaction Unit

-----------------------------------------------------------
-- ORDEREDDELTA
-----------------------------------------------------------
data OrderedDelta = OrderedDelta (Unit -> MonadPerspectivesTransaction Unit) Int

instance eqOrderedDelta :: Eq OrderedDelta where
  eq (OrderedDelta _ i) (OrderedDelta _ j) = eq i j

instance ordOrderedDelta :: Ord OrderedDelta where
  compare (OrderedDelta _ i) (OrderedDelta _ j) = compare i j
