module Perspectives.Query.Compiler where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj, on)
import Effect.Exception (error)
import Perspectives.ContextAndRole (context_rolInContext)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MP)
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), range)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Unsafe.Coerce (unsafeCoerce)

context2role :: QueryFunctionDescription -> MonadPerspectives (ContextInstance ~~> RoleInstance)
context2role (SQD _ (RolGetter (ENR r)) _) = pure $ getRole r

context2role (SQD _ (RolGetter (CR cr)) _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  context2role (RC.calculation ct)

context2role (BQD _ (BinaryCombinator "compose") f1 f2 r) = do
  case range f1 of
    -- f1 :: ContextInstance ~~> RoleInstance
    (RDOM er) -> do
      f1' <- context2role f1
      f2' <- role2role f2
      pure (f1' >=> f2')
    -- f1 :: ContextInstance ~~> ContextInstance
    (CDOM ct) -> do
      f1' <- context2context f1
      f2' <- context2role f2
      pure (f1' >=> f2')
    (PDOM _) -> throwError (error "First function in compose cannot return property value")

-- The last case
context2role _ = throwError (error "Unknown QueryFunction expression")

role2role :: QueryFunctionDescription -> MonadPerspectives (RoleInstance ~~> RoleInstance)
-- The last case
role2role _ = throwError (error "Unknown QueryFunction expression")

context2context :: QueryFunctionDescription -> MonadPerspectives (ContextInstance ~~> ContextInstance)
-- The last case
context2context _ = throwError (error "Unknown QueryFunction expression")

-- From a string that maybe identifies a Property, construct a function to get that property from
-- a Role instance. Notice that this function may fail.
-- getPropertyFunction ::
--   String ->
--   MonadPerspectives StringTypedTripleGetter
-- getPropertyFunction id =
--   do
--     (p :: EnumeratedProperty) <- getPerspectType (EnumeratedPropertyType id)
--     compileQuery $ PC.calculation p
--   <|>
--   do
--     (p :: CalculatedProperty) <- getPerspectType (CalculatedPropertyType id)
--     compileQuery $ PC.calculation p

-- From a string that maybe identifies a Role, construct a function to get that role from
-- a Context instance. Notice that this function may fail.
getRoleFunction ::
  String -> MonadPerspectives (ContextInstance ~~> RoleInstance)
getRoleFunction id =
  do
    (p :: EnumeratedRole) <- getPerspectType (EnumeratedRoleType id)
    context2role $ RC.calculation p
  <|>
  do
    (p :: CalculatedRole) <- getPerspectType (CalculatedRoleType id)
    context2role $ RC.calculation p
