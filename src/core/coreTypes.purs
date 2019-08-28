module Perspectives.CoreTypes where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Foreign.Object as F
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CouchdbState (CouchdbState)
import Perspectives.DependencyTracking.Array.Trans (ArrayT)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.View (View)
import Perspectives.Sync.Transactie (Transactie)

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type Contexts = GLStrMap (AVar Context)
type EnumeratedRoles = GLStrMap (AVar EnumeratedRole)
type CalculatedRoles = GLStrMap (AVar CalculatedRole)
type EnumeratedProperties = GLStrMap (AVar EnumeratedProperty)
type CalculatedProperties = GLStrMap (AVar CalculatedProperty)
type Views = GLStrMap (AVar View)
type Actions = GLStrMap (AVar Action)
type DomeinCache = GLStrMap (AVar DomeinFile)
-- type QueryCache = GLStrMap (TypedTripleGetter String String)

type PerspectivesState = CouchdbState
  -- Caching instances
  ( rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions

  -- Caching type definitions
  , contexts :: Contexts
  , enumeratedRoles :: EnumeratedRoles
  , calculatedRoles :: CalculatedRoles
  , enumeratedProperties :: EnumeratedProperties
  , calculatedProperties :: CalculatedProperties
  , views :: Views
  , actions :: Actions

  -- Caching Domein files
  , domeinCache :: DomeinCache

  , memorizeQueryResults :: Boolean -- obsolete
  , transactie :: Transactie
  , assumptionRegister :: AssumptionRegister
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
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment accumulates Assumptions.

type MonadPerspectivesQuery =  WriterT (Array Assumption) (ArrayT MonadPerspectives)

type MPQ = MonadPerspectivesQuery

-----------------------------------------------------------
-- TRACKINGOBJECTSGETTER
-----------------------------------------------------------
type TrackingObjectsGetter s o = s -> MonadPerspectivesQuery (Array o)

infixl 5 type TrackingObjectsGetter as ~~>
