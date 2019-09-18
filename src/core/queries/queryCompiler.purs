module Perspectives.Query.Compiler where

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MP)
import Perspectives.Instances.Combinators (filter, disjunction, conjunction) as Combinators
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getProperty, getRole, makeBoolean)
import Perspectives.ObjectGetterLookup (lookupPropertyValueGetterByName, lookupRoleGetterByName)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), range)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Prelude (bind, ($), pure, (>=>), class Eq)

---------------------------------------------------------------------------------------------------
-- CONTEXT TO CONTEXT
---------------------------------------------------------------------------------------------------
context2context :: QueryFunctionDescription -> MonadPerspectives (ContextInstance ~~> ContextInstance)

context2context qd@(BQD _ (BinaryCombinator "compose") f1 f2 _) = case range f1 of
  (RDOM er) -> compose qd context2role role2context
  (CDOM ct) -> compose qd context2context context2context
  (PDOM _) -> throwError (error "First function in compose cannot return property value")

context2context (BQD _ (BinaryCombinator "filter") criterium source _) = do
  (criterium' :: ContextInstance ~~> Value) <- context2propertyValue criterium
  source' <- context2context source
  pure $ Combinators.filter source' (makeBoolean criterium')

context2context qd@(BQD _ (BinaryCombinator "disjunction") f1 f2 _) = disjunction qd context2context

-- The last case
context2context _ = throwError (error "Unknown QueryFunction expression")

---------------------------------------------------------------------------------------------------
-- CONTEXT TO ROLE
---------------------------------------------------------------------------------------------------
-- Handles Enumerated RoleTypes
context2role :: QueryFunctionDescription -> MonadPerspectives (ContextInstance ~~> RoleInstance)
context2role (SQD _ (RolGetter (ENR r)) _) = pure $ getRole r

-- Handles Calculated RoleTypes
context2role (SQD _ (RolGetter (CR cr)) _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  context2role (RC.calculation ct)

context2role (SQD _ (DataTypeGetter "externalRole") _) = pure externalRole

context2role qd@(BQD _ (BinaryCombinator "compose") f1 f2 _) = case range f1 of
    (RDOM _) -> compose qd context2role role2role
    (CDOM _) -> compose qd context2context context2role
    -- Note: this case will not happen as the DescriptionCompiler has checked on the types.
    -- However, Purescript requires we handle all Range cases.
    (PDOM _) -> throwError (error "First function in compose cannot return property value")

context2role (BQD _ (BinaryCombinator "filter") criterium source _) = do
  (criterium' :: RoleInstance ~~> Value) <- role2propertyValue criterium
  source' <- context2role source
  pure $ Combinators.filter source' (makeBoolean criterium')

context2role qd@(BQD _ (BinaryCombinator "disjunction") f1 f2 _) = disjunction qd context2role

-- The last case
context2role _ = throwError (error "Unknown QueryFunction expression")

---------------------------------------------------------------------------------------------------
-- CONTEXT TO PROPERTYVALUE
---------------------------------------------------------------------------------------------------
context2propertyValue :: QueryFunctionDescription -> MonadPerspectives (ContextInstance ~~> Value)
context2propertyValue qd@(BQD _ (BinaryCombinator "compose") f1 f2 _) = compose qd context2role role2propertyValue

context2propertyValue qd@(BQD _ (BinaryCombinator "disjunction") f1 f2 _) = disjunction qd context2propertyValue

-- The last case
context2propertyValue _ = throwError (error "Unknown QueryFunction expression")

---------------------------------------------------------------------------------------------------
-- ROLE TO CONTEXT
---------------------------------------------------------------------------------------------------
role2context :: QueryFunctionDescription -> MonadPerspectives (RoleInstance ~~> ContextInstance)
role2context (SQD _ (DataTypeGetter "context") _) = pure context

role2context qd@(BQD _ (BinaryCombinator "compose") f1 f2 _) = case range f1 of
  (CDOM _) -> compose qd role2context context2context
  (RDOM _) -> compose qd role2role role2context
  otherwise -> throwError (error "First function in compose cannot return property value")

role2context (BQD _ (BinaryCombinator "filter") criterium source _) = do
  (criterium' :: ContextInstance ~~> Value) <- context2propertyValue criterium
  source' <- role2context source
  pure $ Combinators.filter source' (makeBoolean criterium')

role2context qd@(BQD _ (BinaryCombinator "disjunction") f1 f2 _) = disjunction qd role2context

-- The last case
role2context _ = throwError (error "Unknown QueryFunction expression")

---------------------------------------------------------------------------------------------------
-- ROLE TO ROLE
---------------------------------------------------------------------------------------------------
role2role :: QueryFunctionDescription -> MonadPerspectives (RoleInstance ~~> RoleInstance)
role2role (SQD _ (DataTypeGetter "binding") _) = pure binding

role2role (BQD _ (BinaryCombinator "filter") criterium source _) = do
  (criterium' :: RoleInstance ~~> Value) <- role2propertyValue criterium
  source' <- role2role source
  pure $ Combinators.filter source' (makeBoolean criterium')

role2role qd@(BQD _ (BinaryCombinator "disjunction") f1 f2 _) = disjunction qd role2role

-- The last case
role2role _ = throwError (error "Unknown QueryFunction expression")

---------------------------------------------------------------------------------------------------
-- ROLE TO PROPERTYVALUE
---------------------------------------------------------------------------------------------------
role2propertyValue :: QueryFunctionDescription -> MonadPerspectives (RoleInstance ~~> Value)
role2propertyValue (SQD _ (PropertyGetter (ENP pt)) _) = pure $ getProperty pt

role2propertyValue (SQD _ (PropertyGetter (CP pt)) _) = do
  (cp :: CalculatedProperty) <- getPerspectType pt
  role2propertyValue (PC.calculation cp)

role2propertyValue qd@(BQD _ (BinaryCombinator "compose") f1 f2 _) = case range f1 of
  (CDOM _) -> compose qd role2context context2propertyValue
  (RDOM _) -> compose qd role2role role2propertyValue
  otherwise -> throwError (error "First function in compose cannot return property value")

role2propertyValue qd@(BQD _ (BinaryCombinator "disjunction") f1 f2 _) = disjunction qd role2propertyValue

-- The last case
role2propertyValue _ = throwError (error "Unknown QueryFunction expression")

---------------------------------------------------------------------------------------------------
-- THE COMPOSITION PATTERN
---------------------------------------------------------------------------------------------------
compose :: forall a b c.
  QueryFunctionDescription ->
  (QueryFunctionDescription -> MP (a ~~> b)) ->
  (QueryFunctionDescription -> MP (b ~~> c)) ->
  MP (a ~~> c)
compose (BQD _ (BinaryCombinator "compose") f1 f2 _) p1 p2 = do
  (f1' :: (a ~~> b)) <- p1 f1
  (f2' :: (b ~~> c)) <- p2 f2
  pure (f1' >=> f2')
compose _ _ _ = throwError (error "Perspectives.Query.Compiler.compose just handles BinaryCombinator 'compose'.")

---------------------------------------------------------------------------------------------------
-- THE DISJUNCTION and CONJUNCTION PATTERNS
---------------------------------------------------------------------------------------------------
disjunction :: forall a b.
  QueryFunctionDescription ->
  (QueryFunctionDescription -> MP (a ~~> b)) ->
  MP (a ~~> b)
disjunction (BQD _ (BinaryCombinator "disjunction") f1 f2 _) p = do
  f1' <- p f1
  f2' <- p f2
  pure (Combinators.disjunction f1' f2')
disjunction _ _ = throwError (error "Perspectives.Query.Compiler.disjunction just handles BinaryCombinator 'disjunction'.")

conjunction :: forall a b. Eq b =>
  QueryFunctionDescription ->
  (QueryFunctionDescription -> MP (a ~~> b)) ->
  MP (a ~~> b)
conjunction (BQD _ (BinaryCombinator "conjunction") f1 f2 _) p = do
  f1' <- p f1
  f2' <- p f2
  pure (Combinators.conjunction f1' f2')
conjunction _ _ = throwError (error "Perspectives.Query.Compiler.disjunction just handles BinaryCombinator 'disjunction'.")

---------------------------------------------------------------------------------------------------
-- CONSTRUCT ROLE- AND PROPERTYVALUE GETTERS
---------------------------------------------------------------------------------------------------
-- From a string that maybe identifies a Role, retrieve or construct a function to get that role from
-- a Context instance. Notice that this function may fail.
getRoleFunction ::
  String -> MonadPerspectives (ContextInstance ~~> RoleInstance)
getRoleFunction id =
  case lookupRoleGetterByName id of
    Nothing -> empty
    (Just g) -> pure g
  <|>
  do
    (p :: EnumeratedRole) <- getPerspectType (EnumeratedRoleType id)
    context2role $ RC.calculation p
  <|>
  do
    (p :: CalculatedRole) <- getPerspectType (CalculatedRoleType id)
    context2role $ RC.calculation p

-- From a string that maybe identifies a Role, retrieve or construct a function to get that role from
-- a Context instance. Notice that this function may fail.
getPropertyFunction ::
  String -> MonadPerspectives (RoleInstance ~~> Value)
getPropertyFunction id =
  case lookupPropertyValueGetterByName id of
    Nothing -> empty
    (Just g) -> pure g
  <|>
  do
    (p :: EnumeratedProperty) <- getPerspectType (EnumeratedPropertyType id)
    role2propertyValue $ PC.calculation p
  <|>
  do
    (p :: CalculatedProperty) <- getPerspectType (CalculatedPropertyType id)
    role2propertyValue $ PC.calculation p
