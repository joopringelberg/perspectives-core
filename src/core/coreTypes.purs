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
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Sync.Transaction (Transaction)
import Prelude (Unit, bind, pure, ($), (<<<), (<>), (>>=))
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
