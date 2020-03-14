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

import Control.Alt (void, (<|>))
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array (elemIndex, null, unsafeIndex)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MP, MPQ, (##=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Instances.Combinators (available, exists, logicalOperation, not, wrapLogicalOperator)
import Perspectives.Instances.Combinators (filter, disjunction, conjunction) as Combinators
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getProperty, getRole, getRoleBinders, makeBoolean)
import Perspectives.Instances.Values (parseInt)
import Perspectives.ObjectGetterLookup (lookupPropertyValueGetterByName, lookupRoleGetterByName)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, lookupVariableBinding)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Utilities (prettyPrint)
import Prelude (class Eq, class Ord, bind, const, discard, eq, identity, notEq, pure, show, ($), (&&), (*), (*>), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (<>), (>), (>=), (>=>), (>>=), (||))
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
  VDOM _ _ -> throwError (error "There is no identity function for value.")

compileFunction (SQD dom (Constant range value) _ _ _) = case dom of
  CDOM _ -> pure $ C2V (pure <<< const (Value value))
  RDOM _ -> pure $ R2V (pure <<< const (Value value))
  VDOM _ _ -> throwError (error "There is no constant function for value.")

compileFunction (MQD dom (ExternalCoreRoleGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (ContextInstance ~~> String)) <- traverse (\calc -> case calc of
      Q descr -> context2string descr
      S s -> throwError (error $ "Argument to ExternalCoreFunction not compiled: " <> show s))
    args
  pure $ C2R (\c -> do
    (values :: Array (Array String)) <- lift $ lift $ traverse (\g -> c ##= g) argFunctions
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: MPQ RoleInstance)
      1 -> (unsafeCoerce f :: (Array String -> MPQ RoleInstance)) (unsafePartial (unsafeIndex values 0))
      2 -> (unsafeCoerce f :: (Array String -> Array String -> MPQ RoleInstance))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 0))
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> MPQ RoleInstance))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 0))
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> MPQ RoleInstance))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 0))
      _ -> throwError (error "Too many arguments for external core module: maximum is 4")
    )

compileFunction (SQD dom (VariableLookup varName) range _ _) =
  case dom, range of
    (CDOM _), (CDOM _) -> pure $ C2C (unsafeCoerce (lookup varName) :: ContextInstance ~~> ContextInstance)
    (CDOM _), (RDOM _) -> pure $ C2R (unsafeCoerce (lookup varName) :: ContextInstance ~~> RoleInstance)
    (CDOM _), (VDOM _ _) -> pure $ C2V (unsafeCoerce (lookup varName) :: ContextInstance ~~> Value)
    (RDOM _), (RDOM _) -> pure $ R2R (unsafeCoerce (lookup varName) :: RoleInstance ~~> RoleInstance)
    (RDOM _), (CDOM _) -> pure $ R2C (unsafeCoerce (lookup varName) :: RoleInstance ~~> ContextInstance)
    (RDOM _), (VDOM _ _) -> pure $ R2V (unsafeCoerce (lookup varName) :: RoleInstance ~~> Value)
    _, _ -> throwError (error ("Impossible domain-range combination for looking up variable '" <> varName <> "': " <> show dom <> ", " <> show range))

-- If the second term is a constant, we can ignore the left term. This is an optimalisation.
compileFunction (BQD _ (BinaryCombinator ComposeF) f1 f2@(SQD _ (Constant _ _) _ _ _) _ _ _) = compileFunction f2

-- If the domain of f1 is a Value, ignore f1 and just compile f2.
-- This is an edge case that arises when we invert queries that have a Value as range.
-- The inverted query has a Value as domain. We then completely ignore that first step.
compileFunction (BQD _ (BinaryCombinator ComposeF) f1 f2 _ _ _) = if isValueDomain $ domain f1
  then compileFunction f2
  else do
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

  where
    isValueDomain :: Domain -> Boolean
    isValueDomain (VDOM _ _) = true
    isValueDomain _ = false

compileFunction (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) = do
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
    (C2C a), (C2C b) -> pure $ C2C \c -> (a c *> b c)
    (C2R a), (C2C b) -> pure $ C2C \c -> (a c *> b c)
    (C2V a), (C2C b) -> pure $ C2C \c -> (a c *> b c)
    (C2C a), (C2R b) -> pure $ C2R \c -> (a c *> b c)
    (C2R a), (C2R b) -> pure $ C2R \c -> (a c *> b c)
    (C2V a), (C2R b) -> pure $ C2R \c -> (a c *> b c)
    (C2C a), (C2V b) -> pure $ C2V \c -> (a c *> b c)
    (C2R a), (C2V b) -> pure $ C2V \c -> (a c *> b c)
    (C2V a), (C2V b) -> pure $ C2V \c -> (a c *> b c)
    (R2C a), (R2C b) -> pure $ R2C \c -> (a c *> b c)
    (R2R a), (R2C b) -> pure $ R2C \c -> (a c *> b c)
    (R2V a), (R2C b) -> pure $ R2C \c -> (a c *> b c)
    (R2C a), (R2R b) -> pure $ R2R \c -> (a c *> b c)
    (R2R a), (R2R b) -> pure $ R2R \c -> (a c *> b c)
    (R2V a), (R2R b) -> pure $ R2R \c -> (a c *> b c)
    (R2C a), (R2V b) -> pure $ R2V \c -> (a c *> b c)
    (R2R a), (R2V b) -> pure $ R2V \c -> (a c *> b c)
    (R2V a), (R2V b) -> pure $ R2V \c -> (a c *> b c)
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

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [AndF, OrF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2V a), (C2V b) -> pure $ C2V $ \c -> logicalOperation (unsafePartial $ mapLogicalOperator g) a b c
    (R2V a), (R2V b) -> pure $ R2V $ \c -> logicalOperation (unsafePartial $ mapLogicalOperator g) a b c
    _,  _ -> throwError (error $ "Cannot create comparison for == or \\= of '" <> show f1 <> "' and '" <> show f2 <> "'.")

-- Add and subtract for numbers and strings. Divide and multiply just for numbers.
compileFunction (BQD _ (BinaryCombinator g) f1 f2 ran _ _) | isJust $ elemIndex g [AddF, SubtractF, DivideF, MultiplyF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  case f1', f2' of
    (C2V a), (C2V b) -> pure $ C2V $ \c -> do
      a' <- a c
      b' <- b c
      (unsafePartial $ (mapNumericOperator g ran)) a' b'
    (R2V a), (R2V b) -> pure $ R2V $ \c -> do
      a' <- a c
      b' <- b c
      (unsafePartial $ (mapNumericOperator g ran)) a' b'
    _,  _ -> throwError (error $ "Cannot create comparison for == or \\= of '" <> show f1 <> "' and '" <> show f2 <> "'.")

compileFunction (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- compileFunction f1
  case f1' of
    (C2C a) -> pure $ C2C (addBinding_ varName a)
    (C2R a) -> pure $ C2R (addBinding_ varName a)
    (C2V a) -> pure $ C2V (addBinding_ varName a)
    (R2C a) -> pure $ R2C (addBinding_ varName a)
    (R2R a) -> pure $ R2R (addBinding_ varName a)
    (R2V a) -> pure $ R2V (addBinding_ varName a)

compileFunction (UQD _ WithFrame f1 _ _ _) = do
  f1' <- compileFunction f1
  case f1' of
    (C2C a) -> pure $ C2C \c -> withFrame_ a c
    (C2R a) -> pure $ C2R \c -> withFrame_ a c
    (C2V a) -> pure $ C2V \c -> withFrame_ a c
    (R2C a) -> pure $ R2C \c -> withFrame_ a c
    (R2R a) -> pure $ R2R \c -> withFrame_ a c
    (R2V a) -> pure $ R2V \c -> withFrame_ a c

compileFunction (UQD _ (UnaryCombinator ExistsF) f1 _ _ _) = do
  f1' <- compileFunction f1
  case f1' of
    (C2C a) -> pure $ C2V (exists a)
    (C2R a) -> pure $ C2V (exists a)
    (C2V a) -> pure $ C2V (exists a)
    (R2C a) -> pure $ R2V (exists a)
    (R2R a) -> pure $ R2V (exists a)
    (R2V a) -> pure $ R2V (exists a)

compileFunction (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) = do
  f1' <- compileFunction f1
  case f1' of
    (C2C a) -> pure $ C2V (available a)
    (C2R a) -> pure $ C2V (available a)
    (C2V a) -> pure $ C2V (available a)
    (R2C a) -> pure $ R2V (available a)
    (R2R a) -> pure $ R2V (available a)
    (R2V a) -> pure $ R2V (available a)

compileFunction (UQD _ (UnaryCombinator NotF) f1 _ _ _) = do
  f1' <- compileFunction f1
  case f1' of
    (C2V a) -> pure $ C2V (not a)
    (R2V a) -> pure $ R2V (not a)
    _ -> throwError (error $ "Cannot negate a non-boolean value, on compiling " <> show f1)

compileFunction (SQD _ (DataTypeGetterWithParameter functionName parameter) _ _ _ ) = do
  case functionName of
    GetRoleBindersF -> pure $ R2R (getRoleBinders (EnumeratedRoleType parameter))

    -- CreateContextF ->
    -- CreateRoleF ->
    _ -> throwError (error $ "Unknown function for DataTypeGetterWithParameter: " <> show functionName)

-- Catch all
compileFunction qd = throwError (error $ "Cannot create a function out of '" <> prettyPrint qd <> "'.")

-- We handle no results for the two ObjectGetters as follows:
--  * if both are empty, the result is true
--  * if one of them is empty, the result is false.
compareContexts :: forall a. Eq a => (ContextInstance ~~> a) -> (ContextInstance ~~> a) -> (a -> a -> Boolean) -> ContextInstance ~~> Value
-- compareContexts a b f c = Value <$> (show <$> (f <$> a c <*> b c))
compareContexts a b f c = ArrayT do
  (as :: Array a) <- runArrayT (a c)
  (bs :: Array a) <- runArrayT (b c)
  (rs :: Array Boolean) <- pure $ f <$> as <*> bs
  if null rs
    then pure [Value "false"]
    else pure (Value <<< show <$> rs)

compareRoles :: forall a. Eq a => (RoleInstance ~~> a) -> (RoleInstance ~~> a) -> (a -> a -> Boolean) -> RoleInstance ~~> Value
-- compareRoles a b f c = Value <$> (show <$> (f <$> a c <*> b c))
compareRoles a b f c = ArrayT do
  (as :: Array a) <- runArrayT (a c)
  (bs :: Array a) <- runArrayT (b c)
  (rs :: Array Boolean) <- pure $ f <$> as <*> bs
  if null rs
    then pure [Value "false"]
    else pure (Value <<< show <$> rs)

compareFunction :: forall a. Eq a => Partial => FunctionName -> (a -> a -> Boolean)
compareFunction fname = case fname of
  EqualsF -> eq
  NotEqualsF -> notEq

mapLogicalOperator :: Partial => FunctionName -> (Value -> Value -> Value)
mapLogicalOperator AndF = wrapLogicalOperator (&&)
mapLogicalOperator OrF = wrapLogicalOperator (||)

mapNumericOperator :: Partial => FunctionName -> Domain -> (Value -> Value ~~> Value)
mapNumericOperator AddF (VDOM PNumber _) = wrapNumericOperator (+)
mapNumericOperator AddF (VDOM PString _) = \(Value s1) (Value s2) -> pure (Value $ s1 <> s2)
mapNumericOperator SubtractF (VDOM PNumber _) = wrapNumericOperator (-)
mapNumericOperator SubtractF (VDOM PString _) = \v1@(Value s1) (Value s2) -> case (Pattern s1) `stripSuffix` s2 of
  Nothing -> pure v1
  Just r -> pure $ Value r
mapNumericOperator DivideF (VDOM PNumber _) = wrapNumericOperator (/)
mapNumericOperator MultiplyF (VDOM PNumber _) = wrapNumericOperator (*)

wrapNumericOperator :: (Int -> Int -> Int) -> (Value -> Value ~~> Value)
wrapNumericOperator g (Value p) (Value q) = (Value <<< show) <$> (g <$> (lift $ parseInt p) <*> (lift $ parseInt q))

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

addBinding_ :: forall a b. String -> (a ~~> b) -> a ~~> b
addBinding_ varName computation ctxt  = do
  v <- computation ctxt
  lift $ lift $ addBinding varName (unsafeCoerce v)
  pure v

withFrame_ :: forall a b. (a ~~> b) -> a ~~> b
withFrame_ computation ctxt = do
  old <- lift $ lift $ getVariableBindings
  void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
  r <- computation ctxt
  void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure r

lookup :: String -> String ~~> String
lookup varName _ = ArrayT do
    mv <- lift (lookupVariableBinding varName)
    pure $ (unsafePartial (fromJust mv))

---------------------------------------------------------------------------------------------------
-- CONSTRUCT ROLE- AND PROPERTYVALUE GETTERS
---------------------------------------------------------------------------------------------------
-- | From a string that maybe identifies a Role, retrieve or construct a function to get that role from
-- | a Context instance. Notice that this function may fail.
getRoleFunction ::
  String -> MonadPerspectives (ContextInstance ~~> RoleInstance)
getRoleFunction id = do
  ident <- expandDefaultNamespaces id
  (unsafePartial $
    case lookupRoleGetterByName ident of
      Nothing -> empty
      (Just g) -> pure g
    <|>
    do
      (p :: EnumeratedRole) <- getPerspectType (EnumeratedRoleType ident)
      (C2R f) <- RC.calculation p >>= compileFunction
      pure f
    <|>
    do
      (p :: CalculatedRole) <- getPerspectType (CalculatedRoleType ident)
      (C2R f) <- RC.calculation p >>= compileFunction
      pure f)

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

-- | Construct a function to compute instances of a ContextType from an instance of a Context.
context2string :: QueryFunctionDescription -> MP (ContextInstance ~~> String)
context2string qd = unsafeCoerce $ unsafePartial $ do
  cf <- compileFunction qd
  case cf of
    (C2C f) -> pure $ unsafeCoerce f
    (C2R f) -> pure $ unsafeCoerce f
    (C2V f) -> pure $ unsafeCoerce f

-- | Construct a function to compute instances of a RoleType from an instance of a Context.
role2context :: QueryFunctionDescription -> MP (RoleInstance ~~> ContextInstance)
role2context qd = unsafePartial $ do
    (R2C f) <- compileFunction qd
    pure f

-- | Construct a function to compute values of a Property for some RoleType from an instance of a Context.
context2propertyValue :: QueryFunctionDescription -> MP (ContextInstance ~~> Value)
context2propertyValue qd = unsafePartial $ do
    (C2V f) <- compileFunction qd
    pure f

-- From a string that maybe identifies a Property(Enumerated or Calculated), retrieve or construct a function to
-- get values for that Property from a Role instance. Notice that this function may fail.
getPropertyFunction ::
  String -> MonadPerspectives (RoleInstance ~~> Value)
getPropertyFunction id = do
  ident <- expandDefaultNamespaces id
  (unsafePartial $
    case lookupPropertyValueGetterByName ident of
      Nothing -> empty
      (Just g) -> pure g
    <|>
    do
      (p :: EnumeratedProperty) <- getPerspectType (EnumeratedPropertyType ident)
      (R2V f) <- PC.calculation p >>= compileFunction
      pure f
    <|>
    do
      (p :: CalculatedProperty) <- getPerspectType (CalculatedPropertyType ident)
      (R2V f) <- PC.calculation p >>= compileFunction
      pure f)

getHiddenFunction :: QueryFunctionDescription -> MP HiddenFunction
getHiddenFunction qfd = do
  cfunction <- compileFunction qfd
  case cfunction of
    C2C f -> pure $ unsafeCoerce f
    C2R f -> pure $ unsafeCoerce f
    C2V f -> pure $ unsafeCoerce f
    R2C f -> pure $ unsafeCoerce f
    R2R f -> pure $ unsafeCoerce f
    R2V f -> pure $ unsafeCoerce f
