module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Domain(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.Persistent (CalculatedRoleType, ContextType, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, RoleKind, RoleType(..))
import Prelude (pure, (<>), show, (>=>))

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class RoleClass r where
  kindOfRole :: r -> RoleKind
  roleAspects :: r -> Array EnumeratedRoleType
  context :: r -> ContextType
  binding :: r -> MonadPerspectives RoleType
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> QueryFunctionDescription

instance calculatedRoleRoleClass :: RoleClass CalculatedRole where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = []
  context r = (unwrap r).context
  binding = rangeOfCalculation >=> binding
  functional = rangeOfCalculation >=> functional
  mandatory = rangeOfCalculation >=> mandatory
  calculation r = (unwrap r).calculation

rangeOfCalculation :: CalculatedRole -> MonadPerspectives EnumeratedRole
rangeOfCalculation cp = case calculation cp of
  QD _ _ (RDOM p) -> getPerspectType p
  otherwise -> throwError (error ("range of calculation of " <> show (identifier cp :: CalculatedRoleType) <> " is not an enumerated role."))

instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = (unwrap r).roleAspects
  context r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = QD (CDOM (context r)) (RolGetter (ENR (identifier r))) (RDOM (identifier r))
