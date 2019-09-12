module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, (<|>))
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Domain(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..))
import Prelude (class Show, pure, show, (<<<), (<>), (>=>), (>>=), ($))

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class (Show r, Identifiable r i, PersistentType r i) <= RoleClass r i | r -> i, i -> r where
  kindOfRole :: r -> RoleKind
  roleAspects :: r -> MonadPerspectives (Array EnumeratedRoleType)
  context :: r -> ContextType
  binding :: r -> MonadPerspectives RoleType
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> QueryFunctionDescription
  properties :: r -> MonadPerspectives (Array PropertyType)

instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects = rangeOfCalculation >=> roleAspects
  context r = (unwrap r).context
  binding = rangeOfCalculation >=> binding
  functional = rangeOfCalculation >=> functional
  mandatory = rangeOfCalculation >=> mandatory
  calculation r = (unwrap r).calculation
  properties = rangeOfCalculation >=> properties

rangeOfCalculation :: CalculatedRole -> MonadPerspectives EnumeratedRole
rangeOfCalculation cp = case calculation cp of
  SQD _ _ (RDOM p) -> getPerspectType p
  UQD _ _ _ (RDOM p) -> getPerspectType p
  BQD _ _ _ _ (RDOM p) -> getPerspectType p
  otherwise -> throwError (error ("range of calculation of " <> show (identifier cp :: CalculatedRoleType) <> " is not an enumerated role."))

instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = pure (unwrap r).roleAspects
  context r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = SQD (CDOM (context r)) (RolGetter (ENR (identifier r))) (RDOM (identifier r))
  properties r = pure (unwrap r).properties

data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

getRole :: RoleType -> MonadPerspectives Role
getRole (ENR e) = getPerspectType e >>= pure <<< E
getRole (CR c) = getPerspectType c >>= pure <<< C

getCalculation :: Role -> QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

effectiveRoleType_ :: String -> MonadPerspectives EnumeratedRoleType
effectiveRoleType_ r = effectiveRoleType (ENR $ EnumeratedRoleType r) <|> effectiveRoleType (CR $ CalculatedRoleType r)

effectiveRoleType :: RoleType -> MonadPerspectives EnumeratedRoleType
effectiveRoleType = getRole >=> pure <<< getCalculation >=> case _ of
    SQD _ _ (RDOM p) -> pure p
    UQD _ _ _ (RDOM p) -> pure p
    BQD _ _ _ _ (RDOM p) -> pure p
    otherwise -> empty

-- Here are alternative functions, using functional dependencies in RoleClass,
-- omitting Role.
getRole' :: forall r i. RoleClass r i => i -> MonadPerspectives r
getRole' i = getPerspectType i

getCalculation' :: forall r i. RoleClass r i => r -> QueryFunctionDescription
getCalculation' r = calculation r

effectiveRoleType' :: String -> MonadPerspectives EnumeratedRoleType
effectiveRoleType' r = effectiveRoleType'_ (EnumeratedRoleType r) <|> effectiveRoleType'_ (CalculatedRoleType r)
  where
    effectiveRoleType'_ :: forall r i. RoleClass r i => i -> MonadPerspectives EnumeratedRoleType
    effectiveRoleType'_ i = getRole' i >>= pure <<< getCalculation' >>= case _ of
        SQD _ _ (RDOM p) -> pure p
        UQD _ _ _ (RDOM p) -> pure p
        BQD _ _ _ _ (RDOM p) -> pure p
        otherwise -> empty
