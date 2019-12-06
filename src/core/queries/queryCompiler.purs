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

-- | The QueryCompiler constructs functions to calculate an instance of a Role for a Context or a Value for a Property,
-- | from a `QueryFunctionDescription`. It operates on a Variant `CompiledFunction` that covers all combinations of
-- | Domain and Range that can be computed.
-- | Instances of `QueryFunctionDescription` are computed by the function [compileQueryStep](Perspectives.Query.DescriptionCompiler.html#t:compileQueryStep).

module Perspectives.Query.Compiler where

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array (elemIndex)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MP)
import Perspectives.Instances.Combinators (filter, disjunction, conjunction) as Combinators
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getProperty, getRole, makeBoolean)
import Perspectives.ObjectGetterLookup (lookupPropertyValueGetterByName, lookupRoleGetterByName)
import Perspectives.PerspectivesState (addBinding, lookupVariableBinding)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Prelude (class Eq, class Ord, bind, const, discard, eq, identity, notEq, pure, show, ($), (*>), (<$>), (<*>), (<<<), (<>), (>=>), (>>=), (<), (<=), (>), (>=))
import Unsafe.Coerce (unsafeCoerce)

-- | A Sum to hold the six types of functions that can be computed.
data CompiledFunction =
    C2C (ContextInstance ~~> ContextInstance)
  | C2R (ContextInstance ~~> RoleInstance)
  | C2V (ContextInstance ~~> Value)
  | R2C (RoleInstance ~~> ContextInstance)
  | R2R (RoleInstance ~~> RoleInstance)
  | R2V (RoleInstance ~~> Value)

-- | Construct a function wrapped in CompiledFunction that actually computes the described function.
-- | A note on kind of roles ([RoleKind](Perspectives.Representation.TypeIdentifiers.html#t:RoleKind)).
-- | In the type representation, we keep UserRoles, BotRoles, etc. in seperate members of Context.
-- | But in the instance representation, there is no need for that. All (Enumerated)
-- | roles have the same runtime representation and their names are unique.
compileFunction :: QueryFunctionDescription -> MP CompiledFunction
compileFunction (SQD _ (RolGetter (ENR r)) _ _ _) = pure $ C2R $ getRole r

compileFunction (SQD _ (RolGetter (CR cr)) _ _ _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  RC.calculation ct >>= compileFunction

compileFunction (SQD _ (PropertyGetter (ENP pt)) _ _ _) = pure $ R2V $ getProperty pt

compileFunction (SQD _ (PropertyGetter (CP pt)) _ _ _) = do
  (cp :: CalculatedProperty) <- getPerspectType pt
  PC.calculation cp >>= compileFunction

compileFunction (SQD _ (DataTypeGetter ExternalRoleF) _ _ _) = pure $ C2R externalRole

compileFunction (SQD _ (DataTypeGetter ContextF) _ _ _) = pure $ R2C context

compileFunction (SQD _ (DataTypeGetter BindingF) _ _ _) = pure $ R2R binding

compileFunction (SQD dom Identity _ _ _) = case dom of
  CDOM _ -> pure $ C2C (pure <<< identity)
  RDOM _ -> pure $ R2R (pure <<< identity)
  VDOM _ -> throwError (error "There is no identity function for value.")

compileFunction (SQD dom (Constant range value) _ _ _) = case dom of
  CDOM _ -> pure $ C2V (pure <<< const (Value value))
  RDOM _ -> pure $ R2V (pure <<< const (Value value))
  VDOM _ -> throwError (error "There is no constant function for value.")


compileFunction (SQD _ (ComputedRoleGetter functionName) _ _ _) = pure $ C2R $ unsafePartial $ fromJust $ lookupRoleGetterByName functionName

compileFunction (SQD dom (VariableLookup varName) range _ _) =
  case dom, range of
    (CDOM _), (CDOM _) -> pure $ C2C (unsafeCoerce (lookup varName) :: ContextInstance ~~> ContextInstance)
    (CDOM _), (RDOM _) -> pure $ C2R (unsafeCoerce (lookup varName) :: ContextInstance ~~> RoleInstance)
    (CDOM _), (VDOM _) -> pure $ C2V (unsafeCoerce (lookup varName) :: ContextInstance ~~> Value)
    (RDOM _), (RDOM _) -> pure $ R2R (unsafeCoerce (lookup varName) :: RoleInstance ~~> RoleInstance)
    (RDOM _), (CDOM _) -> pure $ R2C (unsafeCoerce (lookup varName) :: RoleInstance ~~> ContextInstance)
    (RDOM _), (VDOM _) -> pure $ R2V (unsafeCoerce (lookup varName) :: RoleInstance ~~> Value)
    _, _ -> throwError (error ("Impossible domain-range combination for looking up variable '" <> varName <> "': " <> show dom <> ", " <> show range))

compileFunction (BQD _ (BinaryCombinator ComposeF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2R a), (R2C b) -> pure $ C2C (a >=> b)
    (C2C a), (C2C b) -> pure $ C2C (a >=> b)
    (C2R a), (R2R b) -> pure $ C2R (a >=> b)
    (C2C a), (C2R b) -> pure $ C2R (a >=> b)
    (C2R a), (R2V b) -> pure $ C2V (a >=> b)
    (C2C a), (C2V b) -> pure $ C2V (a >=> b)
    (R2C a), (C2C b) -> pure $ R2C (a >=> b)
    (R2R a), (R2C b) -> pure $ R2C (a >=> b)
    (R2R a), (R2R b) -> pure $ R2R (a >=> b)
    (R2C a), (C2R b) -> pure $ R2R (a >=> b)
    (R2C a), (C2V b) -> pure $ R2V (a >=> b)
    (R2R a), (R2V b) -> pure $ R2V (a >=> b)
    _,  _ -> throwError (error $  "Cannot compose '" <> show f1 <> "' with '" <> show f2 <> "'.")

compileFunction (BQD _ (BinaryCombinator FilterF) criterium source _ _ _) = do
  criterium' <- compileFunction criterium
  source' <- compileFunction source
  case criterium', source' of
    (C2V a), (C2C b) -> pure $ C2C $ Combinators.filter b (makeBoolean a)
    (R2V a), (C2R b) -> pure $ C2R $ Combinators.filter b (makeBoolean a)
    (R2V a), (R2R b) -> pure $ R2R $ Combinators.filter b (makeBoolean a)
    (C2V a), (R2C b) -> pure $ R2C $ Combinators.filter b (makeBoolean a)
    -- TODO: filter Values.
    _,  _ -> throwError (error $  "Cannot filter '" <> show source <> "' with '" <> show criterium <> "'.")

compileFunction (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2C a), (C2C b) -> pure $ C2C (a *> b)
    (C2R a), (C2C b) -> pure $ C2C (a *> b)
    (C2V a), (C2C b) -> pure $ C2C (a *> b)
    (C2C a), (C2R b) -> pure $ C2R (a *> b)
    (C2R a), (C2R b) -> pure $ C2R (a *> b)
    (C2V a), (C2R b) -> pure $ C2R (a *> b)
    (C2C a), (C2V b) -> pure $ C2V (a *> b)
    (C2R a), (C2V b) -> pure $ C2V (a *> b)
    (C2V a), (C2V b) -> pure $ C2V (a *> b)
    (R2C a), (R2C b) -> pure $ R2C (a *> b)
    (R2R a), (R2C b) -> pure $ R2C (a *> b)
    (R2V a), (R2C b) -> pure $ R2C (a *> b)
    (R2C a), (R2R b) -> pure $ R2R (a *> b)
    (R2R a), (R2R b) -> pure $ R2R (a *> b)
    (R2V a), (R2R b) -> pure $ R2R (a *> b)
    (R2C a), (R2V b) -> pure $ R2V (a *> b)
    (R2R a), (R2V b) -> pure $ R2V (a *> b)
    (R2V a), (R2V b) -> pure $ R2V (a *> b)
    _, _ -> throwError (error $ "This is not a valid sequence: '" <> show f1 <> "', '" <> show f2 <> "'." )

compileFunction (BQD _ (BinaryCombinator DisjunctionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2C a), (C2C b) -> pure $ C2C $ Combinators.disjunction a b
    (C2R a), (C2R b) -> pure $ C2R $ Combinators.disjunction a b
    (C2V a), (C2V b) -> pure $ C2V $ Combinators.disjunction a b
    (R2C a), (R2C b) -> pure $ R2C $ Combinators.disjunction a b
    (R2R a), (R2R b) -> pure $ R2R $ Combinators.disjunction a b
    (R2V a), (R2V b) -> pure $ R2V $ Combinators.disjunction a b
    _,  _ -> throwError (error $ "Cannot create disjunction of '" <> show f1 <> "' and '" <> show f2 <> "'.")

compileFunction (BQD _ (BinaryCombinator ConjunctionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2C a), (C2C b) -> pure $ C2C $ Combinators.conjunction a b
    (C2R a), (C2R b) -> pure $ C2R $ Combinators.conjunction a b
    (C2V a), (C2V b) -> pure $ C2V $ Combinators.conjunction a b
    (R2C a), (R2C b) -> pure $ R2C $ Combinators.disjunction a b
    (R2R a), (R2R b) -> pure $ R2R $ Combinators.conjunction a b
    (R2V a), (R2V b) -> pure $ R2V $ Combinators.conjunction a b
    _,  _ -> throwError (error $ "Cannot create conjunction of '" <> show f1 <> "' and '" <> show f2 <> "'.")

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [EqualsF, NotEqualsF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2C a), (C2C b) -> pure $ C2V $ compareContexts a b (unsafePartial $ compareFunction g)
    (C2R a), (C2R b) -> pure $ C2V $ compareContexts a b (unsafePartial $ compareFunction g)
    (C2V a), (C2V b) -> pure $ C2V $ compareContexts a b (unsafePartial $ compareFunction g)
    (R2C a), (R2C b) -> pure $ R2V $ compareRoles a b (unsafePartial $ compareFunction g)
    (R2R a), (R2R b) -> pure $ R2V $ compareRoles a b (unsafePartial $ compareFunction g)
    (R2V a), (R2V b) -> pure $ R2V $ compareRoles a b (unsafePartial $ compareFunction g)
    _,  _ -> throwError (error $ "Cannot create comparison for == or \\= of '" <> show f1 <> "' and '" <> show f2 <> "'.")

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2C a), (C2C b) -> pure $ C2V $ orderContexts a b (unsafePartial $ orderFunction g)
    (C2R a), (C2R b) -> pure $ C2V $ orderContexts a b (unsafePartial $ orderFunction g)
    (C2V a), (C2V b) -> pure $ C2V $ orderContexts a b (unsafePartial $ orderFunction g)
    (R2C a), (R2C b) -> pure $ R2V $ orderRoles a b (unsafePartial $ orderFunction g)
    (R2R a), (R2R b) -> pure $ R2V $ orderRoles a b (unsafePartial $ orderFunction g)
    (R2V a), (R2V b) -> pure $ R2V $ orderRoles a b (unsafePartial $ orderFunction g)
    _,  _ -> throwError (error $ "Cannot create comparison for == or \\= of '" <> show f1 <> "' and '" <> show f2 <> "'.")

compileFunction (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- compileFunction f1
  case f1' of
    (C2C a) -> pure $ C2C (unsafeCoerce addBinding_ varName a)
    (C2R a) -> pure $ C2R (unsafeCoerce addBinding_ varName a)
    (C2V a) -> pure $ C2V (unsafeCoerce addBinding_ varName a)
    (R2C a) -> pure $ R2C (unsafeCoerce addBinding_ varName a)
    (R2R a) -> pure $ R2R (unsafeCoerce addBinding_ varName a)
    (R2V a) -> pure $ R2V (unsafeCoerce addBinding_ varName a)

-- Catch all
compileFunction qd = throwError (error $ "Cannot create a function out of '" <> show qd <> "'.")

compareContexts :: forall a. Eq a => (ContextInstance ~~> a) -> (ContextInstance ~~> a) -> (a -> a -> Boolean) -> ContextInstance ~~> Value
compareContexts a b f c = Value <$> (show <$> (f <$> a c <*> b c))

compareRoles :: forall a. Eq a => (RoleInstance ~~> a) -> (RoleInstance ~~> a) -> (a -> a -> Boolean) -> RoleInstance ~~> Value
compareRoles a b f c = Value <$> (show <$> (f <$> a c <*> b c))

compareFunction :: forall a. Eq a => Partial => FunctionName -> (a -> a -> Boolean)
compareFunction fname = case fname of
  EqualsF -> eq
  NotEqualsF -> notEq

orderContexts :: forall a. Ord a => (ContextInstance ~~> a) -> (ContextInstance ~~> a) -> (a -> a -> Boolean) -> ContextInstance ~~> Value
orderContexts a b f c = Value <$> (show <$> (f <$> a c <*> b c))

orderRoles :: forall a. Ord a => (RoleInstance ~~> a) -> (RoleInstance ~~> a) -> (a -> a -> Boolean) -> RoleInstance ~~> Value
orderRoles a b f c = Value <$> (show <$> (f <$> a c <*> b c))

orderFunction :: forall a. Ord a => Partial => FunctionName -> (a -> a -> Boolean)
orderFunction fname = case fname of
  LessThanF -> (<)
  LessThanEqualF -> (<=)
  GreaterThanF -> (>)
  GreaterThanEqualF -> (>=)

operators :: Array FunctionName
operators =
  [ DisjunctionF
  , ConjunctionF

  , EqualsF
  , NotEqualsF
  , LessThanF
  , LessThanEqualF
  , GreaterThanF
  , GreaterThanEqualF

  , AddF
  , SubtractF
  , DivideF
  , MultiplyF

  , AndF
  , OrF
  ]

addBinding_ :: String -> (String ~~> String) -> String ~~> String
addBinding_ varName computation ctxt  = do
  v <- computation ctxt
  lift $ lift $ addBinding varName v
  pure v

lookup :: String -> String ~~> String
lookup varName _ = do
    mv <- lift $ lift (lookupVariableBinding varName)
    pure $ (unsafePartial (fromJust mv))

---------------------------------------------------------------------------------------------------
-- CONSTRUCT ROLE- AND PROPERTYVALUE GETTERS
---------------------------------------------------------------------------------------------------
-- | From a string that maybe identifies a Role, retrieve or construct a function to get that role from
-- | a Context instance. Notice that this function may fail.
getRoleFunction ::
  String -> MonadPerspectives (ContextInstance ~~> RoleInstance)
getRoleFunction id = unsafePartial $
  case lookupRoleGetterByName id of
    Nothing -> empty
    (Just g) -> pure g
  <|>
  do
    (p :: EnumeratedRole) <- getPerspectType (EnumeratedRoleType id)
    (C2R f) <- RC.calculation p >>= compileFunction
    pure f
  <|>
  do
    (p :: CalculatedRole) <- getPerspectType (CalculatedRoleType id)
    (C2R f) <- RC.calculation p >>= compileFunction
    pure f

-- | Construct a function to compute instances of a ContextType from an instance of a Context.
context2context :: QueryFunctionDescription -> MP (ContextInstance ~~> ContextInstance)
context2context qd = unsafePartial $ do
    (C2C f) <- compileFunction qd
    pure f

-- | Construct a function to compute instances of a RoleType from an instance of a Context.
context2role :: QueryFunctionDescription -> MP (ContextInstance ~~> RoleInstance)
context2role qd = unsafePartial $ do
    (C2R f) <- compileFunction qd
    pure f

-- | Construct a function to compute values of a Property for some RoleType from an instance of a Context.
context2propertyValue :: QueryFunctionDescription -> MP (ContextInstance ~~> Value)
context2propertyValue qd = unsafePartial $ do
    (C2V f) <- compileFunction qd
    pure f

-- From a string that maybe identifies a Role, retrieve or construct a function to get that role from
-- a Context instance. Notice that this function may fail.
getPropertyFunction ::
  String -> MonadPerspectives (RoleInstance ~~> Value)
getPropertyFunction id = unsafePartial $
  case lookupPropertyValueGetterByName id of
    Nothing -> empty
    (Just g) -> pure g
  <|>
  do
    (p :: EnumeratedProperty) <- getPerspectType (EnumeratedPropertyType id)
    (R2V f) <- PC.calculation p >>= compileFunction
    pure f
  <|>
  do
    (p :: CalculatedProperty) <- getPerspectType (CalculatedPropertyType id)
    (R2V f) <- PC.calculation p >>= compileFunction
    pure f
