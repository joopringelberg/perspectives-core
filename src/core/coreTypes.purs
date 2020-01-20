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


module Perspectives.CoreTypes where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, throwError)
import Effect.Aff.AVar (AVar)
import Effect.Exception (error)
import Foreign.Object as F
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CouchdbState (CouchdbState)
import Perspectives.DependencyTracking.Array.Trans (ArrayT, runArrayT)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.Environment (Environment)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ActionType)
import Perspectives.Sync.Transaction (Transaction)
import Prelude (class Eq, class Show, Unit, bind, eq, pure, show, ($), (&&), (<<<), (<>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextInstances = GLStrMap (AVar PerspectContext)
type RolInstances = GLStrMap (AVar PerspectRol)
type DomeinCache = GLStrMap (AVar DomeinFile)

type PerspectivesState = CouchdbState
  -- Caching instances
  ( rolInstances :: RolInstances
  , contextInstances :: ContextInstances

  -- Caching Domein files
  , domeinCache :: DomeinCache

  , queryAssumptionRegister :: AssumptionRegister

  , variableBindings :: Environment (Array String)
  )

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
-- ASSIGNMENT (RULE) DEPENDENCYTRACKING
-----------------------------------------------------------
-- | An ActionInstance represents the applicability of an Action to the instance of a Context.
data ActionInstance = ActionInstance ContextInstance ActionType

instance eqActionInstance :: Eq ActionInstance where
  eq (ActionInstance c1 a1) (ActionInstance c2 a2) = eq c1 c2 && eq a1 a2

instance showActionInstance :: Show ActionInstance where
  show (ActionInstance c1 a1) = "ActionInstance( " <> show c1 <> ", " <> show a1 <> " )"

-- | Actions should be re-run as the Assumptions underlying their computation change.
-- | We register the dependency of Actions for ContextInstances with a double registration that allows us to
-- | travel from Assumptions to ActionInstances and vice versa by simple lookup.

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
runTypeLevelToObject :: forall s o. s -> (s ~~~> o) -> (MonadPerspectives) o
runTypeLevelToObject id tog = runTypeLevelToArray id tog >>= \objects ->
  case head objects of
  Nothing -> throwError $ error $ "TypeLevelGetter returns no values for '" <> (unsafeCoerce id) <> "'."
  (Just obj) -> pure obj

infix 1 runTypeLevelToObject as ###>>

-----------------------------------------------------------
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment accumulates Assumptions.

type MonadPerspectivesQuery =  ArrayT (WriterT (Array Assumption) MonadPerspectives)

type MPQ = MonadPerspectivesQuery

-----------------------------------------------------------
-- TRACKINGOBJECTSGETTER
-----------------------------------------------------------
type TrackingObjectsGetter s o = s -> MPQ o

infixl 5 type TrackingObjectsGetter as ~~>

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

type WithAssumptions o = Tuple (Array o) (Array Assumption)
-----------------------------------------------------------
-- EVAL TRACKINGOBJECTSGETTER TO GET AN ARRAY OF RESULTS
-----------------------------------------------------------
-- | Apply a TrackingObjectsGetter to an argument to obtain an Array of results in MonadPerspectives.
evalMonadPerspectivesQuery :: forall s o.
  s
  -> (s ~~> o)
  -> (MonadPerspectives (Array o))
evalMonadPerspectivesQuery a f = do
    (Tuple result assumptions) <- runMonadPerspectivesQuery a f
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
runMonadPerspectivesQueryToObject :: forall s o. s -> (s ~~> o) -> (MonadPerspectives) o
runMonadPerspectivesQueryToObject id tog = evalMonadPerspectivesQuery id tog >>= \objects ->
  case head objects of
  Nothing -> throwError $ error $ "ObjectsGetter returns no values for '" <> (unsafeCoerce id) <> "'."
  (Just obj) -> pure obj

infix 0 runMonadPerspectivesQueryToObject as ##>>

-----------------------------------------------------------
-- MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | The Transaction accumulates Deltas.

type MonadPerspectivesTransaction =  ArrayT (ReaderT (AVar Transaction) MonadPerspectives)

type MPT = MonadPerspectivesTransaction

-----------------------------------------------------------
-- UPDATER
-----------------------------------------------------------
type Updater s = s -> MonadPerspectivesTransaction Unit
