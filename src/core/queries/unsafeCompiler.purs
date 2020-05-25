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

module Perspectives.Query.UnsafeCompiler where

import Control.Alt (void, (<|>))
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Control.Plus (empty)
import Data.Array (elemIndex, null, unsafeIndex)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MP, MPQ, MonadPerspectives, Assumption, liftToInstanceLevel, (##=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.Instances.Combinators (available_, exists, logicalOperation, not, wrapLogicalOperator)
import Perspectives.Instances.Combinators (filter, disjunction, conjunction) as Combinators
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (binding, context, contextType, externalRole, getProperty, getEnumeratedRoleInstances, getRoleBinders, makeBoolean)
import Perspectives.Instances.Values (parseInt)
import Perspectives.Names (expandDefaultNamespaces, lookupIndexedRole)
import Perspectives.ObjectGetterLookup (lookupPropertyValueGetterByName, lookupRoleGetterByName, propertyGetterCacheInsert)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, lookupVariableBinding)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation, functional, mandatory) as PC
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Types.ObjectGetters (allRoleTypesInContext, specialisesRoleType)
import Perspectives.Utilities (prettyPrint)
import Prelude (class Eq, class Ord, bind, discard, eq, flip, identity, notEq, pure, show, ($), (&&), (*), (*>), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (<>), (>), (>=), (>=>), (>>=), (||))
import Unsafe.Coerce (unsafeCoerce)

-- TODO. String dekt de lading niet sinds we RoleTypes toelaten. Een variabele zou
-- beter zijn.
compileFunction :: QueryFunctionDescription -> MP (String ~~> String)

compileFunction (SQD _ (RolGetter (ENR (EnumeratedRoleType r))) _ _ _) = if isExternalRole r
  then pure $ unsafeCoerce $ externalRole
  else pure $ unsafeCoerce $ getEnumeratedRoleInstances (EnumeratedRoleType r)

compileFunction (SQD _ (RolGetter (CR cr)) _ _ _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  RC.calculation ct >>= compileFunction

compileFunction (SQD _ (PropertyGetter (ENP pt)) _ _ _) = pure $ unsafeCoerce $ getProperty pt

compileFunction (SQD _ (PropertyGetter (CP pt)) _ _ _) = do
  (cp :: CalculatedProperty) <- getPerspectType pt
  PC.calculation cp >>= compileFunction

compileFunction (SQD _ (DataTypeGetter ExternalRoleF) _ _ _) = pure $ unsafeCoerce externalRole

compileFunction (SQD _ (DataTypeGetter ContextF) _ _ _) = pure $ unsafeCoerce context

compileFunction (SQD _ (TypeGetter TypeOfContextF) _ _ _) = pure $ unsafeCoerce contextType

compileFunction (SQD _ (TypeGetter RoleTypesF) _ _ _) = pure $ unsafeCoerce (liftToInstanceLevel allRoleTypesInContext)

compileFunction (SQD _ (DataTypeGetter BindingF) _ _ _) = pure $ unsafeCoerce binding

compileFunction (SQD dom Identity _ _ _) = case dom of
  VDOM _ _ -> throwError (error "There is no identity function for value.")
  otherwise -> pure $ (pure <<< identity)

compileFunction (SQD dom (Constant range value) _ _ _) = case dom of
  VDOM _ _ -> throwError (error "There is no constant function for value.")
  otherwise -> pure $ unsafeCoerce \x -> (pure (Value value) :: MPQ Value)

-- compileFunction (SQD dom (RoleIndividual individual) _ _ _) = pure $ unsafeCoerce (\x -> lift $ lift $ maybe [] identity (lookupIndexedRole (unwrap individual)) :: MPQ RoleInstance)

compileFunction (SQD dom (RoleIndividual individual) _ _ _) = pure $ unsafeCoerce \x -> ArrayT do
  mi <- ((lift $ lookupIndexedRole (unwrap individual)) :: (WriterT (Array Assumption) MonadPerspectives) (Maybe RoleInstance))
  case mi of
    Nothing -> pure []
    Just i -> pure [unwrap i]

compileFunction (SQD dom (Value2Role _) _ _ _) = pure $ unsafeCoerce (\x -> pure x :: MPQ String)

compileFunction (MQD dom (ExternalCoreRoleGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafeCoerce $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions) <- traverse compileFunction args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ lift $ traverse (\g -> c ##= g) argFunctions
    -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: (String -> MPQ String)) c
      1 -> (unsafeCoerce f :: (Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        c
      2 -> (unsafeCoerce f :: (Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        c
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        c
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        c
      _ -> throwError (error "Too many arguments for external core module: maximum is 4")
    )

compileFunction (MQD dom (ExternalCorePropertyGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (String ~~> String)) <- traverse compileFunction args
  pure (\r -> do
    (values :: Array (Array String)) <- lift $ lift $ traverse (\g -> r ##= g) argFunctions
    -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: (String -> MPQ String)) r
      1 -> (unsafeCoerce f :: (Array String -> String -> MPQ String)) (unsafePartial (unsafeIndex values 0)) r
      2 -> (unsafeCoerce f :: (Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        r
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        r
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        r
      _ -> throwError (error "Too many arguments for external core module: maximum is 4")
    )

compileFunction (SQD dom (VariableLookup varName) range _ _) = pure $ unsafeCoerce (lookup varName)

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
    pure $ (f1' >=> f2')

  where
    isValueDomain :: Domain -> Boolean
    isValueDomain (VDOM _ _) = true
    isValueDomain _ = false

compileFunction (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) = do
  (criterium' :: String ~~> String) <- (compileFunction criterium)
  (source' :: String ~~> String) <- compileFunction source
  pure $ Combinators.filter source' (makeBoolean $ unsafeCoerce criterium')

compileFunction (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure \c -> (f1' c *> f2' c)

compileFunction (BQD _ (BinaryCombinator DisjunctionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ Combinators.disjunction f1' f2'

compileFunction (BQD _ (BinaryCombinator ConjunctionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ Combinators.conjunction f1' f2'

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [EqualsF, NotEqualsF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ unsafeCoerce $ compare f1' f2' (unsafePartial $ compareFunction g)

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ order f1' f2' (unsafePartial $ orderFunction g)

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [AndF, OrF] = do
  (f1' :: String ~~> Value) <- unsafeCoerce $ compileFunction f1
  (f2' :: String ~~> Value) <- unsafeCoerce $ compileFunction f2
  pure \c -> (unsafeCoerce (logicalOperation (unsafePartial $ mapLogicalOperator g) f1' f2') c)

-- Add and subtract for numbers and strings. Divide and multiply just for numbers.
compileFunction (BQD _ (BinaryCombinator g) f1 f2 ran _ _) | isJust $ elemIndex g [AddF, SubtractF, DivideF, MultiplyF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ \c -> do
    (a' :: Value) <- unsafeCoerce $ f1' c
    (b' :: Value) <- unsafeCoerce $ f2' c
    (unsafeCoerce $ unsafePartial $ mapNumericOperator g ran) a' b'

compileFunction (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (addBinding_ varName f1')

compileFunction (UQD _ WithFrame f1 _ _ _) = do
  f1' <- compileFunction f1
  pure \c -> withFrame_ f1' c

compileFunction (UQD _ (UnaryCombinator ExistsF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ exists f1')

compileFunction (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ available_ f1')

compileFunction (UQD _ (UnaryCombinator NotF) f1 _ _ _) = do
  (f1' :: String ~~> Value) <- unsafeCoerce (compileFunction f1)
  pure (unsafeCoerce $ not f1')

compileFunction (SQD _ (DataTypeGetterWithParameter functionName parameter) _ _ _ ) = do
  case functionName of
    GetRoleBindersF -> pure $ unsafeCoerce (getRoleBinders (EnumeratedRoleType parameter))
    SpecialisesRoleTypeF -> pure $ unsafeCoerce (liftToInstanceLevel ((flip specialisesRoleType) (ENR $ EnumeratedRoleType parameter)))

    -- CreateContextF ->
    -- CreateRoleF ->
    _ -> throwError (error $ "Unknown function for DataTypeGetterWithParameter: " <> show functionName)

-- Catch all
compileFunction qd = throwError (error $ "Cannot create a function out of '" <> prettyPrint qd <> "'.")

---------------------------------------------------------------------------------------------------
-- COMPARING
---------------------------------------------------------------------------------------------------
-- We handle no results for the two ObjectGetters as follows:
--  * if both are empty, the result is true
--  * if one of them is empty, the result is false.
compare :: forall a. Eq a =>
  (a ~~> a) ->
  (a ~~> a) ->
  (a -> a -> Boolean) ->
  a ~~> Value
compare a b f c = ArrayT do
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

---------------------------------------------------------------------------------------------------
-- BINDING AND FRAMES
---------------------------------------------------------------------------------------------------
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
-- ORDERING
---------------------------------------------------------------------------------------------------
order :: (String ~~> String) -> (String ~~> String) -> (String -> String -> Boolean) -> String ~~> String
order a b f c = (show <$> (f <$> a c <*> b c))

orderFunction :: forall a. Ord a => Partial => FunctionName -> (a -> a -> Boolean)
orderFunction fname = case fname of
  LessThanF -> (<)
  LessThanEqualF -> (<=)
  GreaterThanF -> (>)
  GreaterThanEqualF -> (>=)

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
      unsafeCoerce $ RC.calculation p >>= compileFunction
    <|>
    do
      (p :: CalculatedRole) <- getPerspectType (CalculatedRoleType ident)
      unsafeCoerce $ RC.calculation p >>= compileFunction)

getRoleInstances :: RoleType -> (ContextInstance ~~> RoleInstance)
getRoleInstances (ENR rt) c = do
  (p :: EnumeratedRole) <- lift $ lift $ getPerspectType rt
  f <- (lift $ lift $ RC.calculation p) >>= lift <<< lift <<< compileFunction
  (unsafeCoerce f) c
getRoleInstances (CR rt) c = getCalculatedRoleInstances rt c

getCalculatedRoleInstances :: CalculatedRoleType -> (ContextInstance ~~> RoleInstance)
getCalculatedRoleInstances rt@(CalculatedRoleType ident) c = case lookupRoleGetterByName ident of
  (Just g) -> g c
  Nothing -> do
    (p :: CalculatedRole) <- lift $ lift $ getPerspectType rt
    f <- (lift $ lift $ RC.calculation p) >>= lift <<< lift <<< compileFunction
    (unsafeCoerce f) c

-- | Construct a function to compute instances of a ContextType from an instance of a Context.
context2context :: QueryFunctionDescription -> MP (ContextInstance ~~> ContextInstance)
context2context qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute instances of a RoleType from an instance of a Context.
context2role :: QueryFunctionDescription -> MP (ContextInstance ~~> RoleInstance)
context2role qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute Strings (not further typed) from an instance of a Context.
context2string :: QueryFunctionDescription -> MP (ContextInstance ~~> String)
context2string qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute instances of a RoleType from an instance of a Context.
role2context :: QueryFunctionDescription -> MP (RoleInstance ~~> ContextInstance)
role2context qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute values of a Property for some RoleType from an instance of a Context.
context2propertyValue :: QueryFunctionDescription -> MP (ContextInstance ~~> Value)
context2propertyValue qd = unsafeCoerce $ compileFunction qd

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
      unsafeCoerce $ PC.calculation p >>= compileFunction
    <|>
    do
      (p :: CalculatedProperty) <- getPerspectType (CalculatedPropertyType ident)
      unsafeCoerce $ PC.calculation p >>= compileFunction
      )

getterFromPropertyType :: PropertyType -> MP (RoleInstance ~~> Value)
getterFromPropertyType (ENP ep@(EnumeratedPropertyType id)) = case lookupPropertyValueGetterByName id of
  Nothing -> do
    p@(EnumeratedProperty {functional, mandatory}) <- getPerspectType ep
    getter <- unsafeCoerce $ PC.calculation p >>= compileFunction
    void $ pure $ propertyGetterCacheInsert id getter functional mandatory
    pure getter
  Just g -> pure g
getterFromPropertyType (CP cp@(CalculatedPropertyType id)) = case lookupPropertyValueGetterByName id of
  Nothing -> do
    p <- getPerspectType cp
    functional <- PC.functional p
    mandatory <- PC.mandatory p
    getter <- unsafeCoerce $ PC.calculation p >>= compileFunction
    void $ pure $ propertyGetterCacheInsert id getter functional mandatory
    pure getter
  Just g -> pure g

getHiddenFunction :: QueryFunctionDescription -> MP HiddenFunction
getHiddenFunction = unsafeCoerce $ compileFunction
