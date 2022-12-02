-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | The QueryCompiler constructs functions to calculate an instance of a Role for a Context or a Value for a Property,
-- | from a `QueryFunctionDescription`. It operates on a Variant `CompiledFunction` that covers all combinations of
-- | Domain and Range that can be computed.
-- | Instances of `QueryFunctionDescription` are computed by the function [compileQueryStep](Perspectives.Query.ExpressionCompiler.html#t:compileQueryStep).

module Perspectives.Query.UnsafeCompiler where

import Control.Alt (map, void, (<|>))
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Control.Plus (empty)
import Data.Array (elemIndex, findIndex, foldl, head, index, length, null, singleton, unsafeIndex)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripSuffix)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (maximum, minimum, traverse)
import Effect.Exception (error)
import Foreign.Object (empty, lookup) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles, Assumption, InformedAssumption, MP, MPQ, MonadPerspectives, MonadPerspectivesQuery, liftToInstanceLevel)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.Instances.Combinators (available_, exists, filter, logicalAnd, logicalOr, not, some)
import Perspectives.Instances.Combinators (filter, disjunction, conjunction) as Combinators
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (binding, bindingInContext, binding_, context, contextModelName, contextType, externalRole, filledByCombinator, filledByOperator, fillsCombinator, getEnumeratedRoleInstances, getFilledRoles, getMe, getPreferredUserRoleType, getProperty, getUnlinkedRoleInstances, isMe, makeBoolean, roleModelName, roleType, roleType_)
import Perspectives.Instances.Values (parseBool, parseDate, parseInt)
import Perspectives.ModelDependencies (roleWithId)
import Perspectives.Names (expandDefaultNamespaces, lookupIndexedContext, lookupIndexedRole)
import Perspectives.ObjectGetterLookup (lookupPropertyValueGetterByName, lookupRoleGetterByName, propertyGetterCacheInsert)
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP(..))
import Perspectives.PerspectivesState (addBinding, getVariableBindings, lookupVariableBinding)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, domain, domain2contextType, roleInContext2Role)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation, functional, mandatory) as PC
import Perspectives.Representation.Class.Property (getProperType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties)
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..)) as RAN
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.Types.ObjectGetters (allRoleTypesInContext, calculatedUserRole, contextTypeModelName', enumeratedUserRole, isUnlinked_, propertyAliases, roleTypeModelName', specialisesRoleType, userRole)
import Perspectives.Utilities (prettyPrint)
import Prelude (class Eq, class Ord, bind, discard, eq, flip, identity, notEq, pure, show, ($), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (<>), (>), (>=), (>=>), (>>=), (>>>), (||), (==))
import Unsafe.Coerce (unsafeCoerce)

-- TODO. String dekt de lading niet sinds we RoleTypes toelaten. Een variabele zou
-- beter zijn.
compileFunction :: QueryFunctionDescription -> MP (String ~~> String)

compileFunction (SQD _ (RolGetter (ENR (EnumeratedRoleType r))) _ _ _) = if isExternalRole r
  then pure $ unsafeCoerce $ externalRole
  else pure $ unsafeCoerce $ getEnumeratedRoleInstances (EnumeratedRoleType r)

compileFunction (SQD _ (DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF roleTypeName) _ _ _) = pure $ unsafeCoerce $ getUnlinkedRoleInstances (EnumeratedRoleType roleTypeName)

compileFunction (SQD _ (RolGetter (CR cr)) _ _ _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  -- TODO moeten we hier de currentcontext pushen?
  RC.calculation ct >>= compileFunction

compileFunction (SQD (RDOM roleAdt) (PropertyGetter (ENP (EnumeratedPropertyType pt))) _ _ _) = do
  g <- getDynamicPropertyGetter pt (roleInContext2Role <$> roleAdt)
  pure $ unsafeCoerce g

compileFunction (SQD _ (PropertyGetter (CP pt)) _ _ _) = do
  (cp :: CalculatedProperty) <- getPerspectType pt
  -- TODO moeten we hier de currentobject pushen?
  PC.calculation cp >>= compileFunction

compileFunction (SQD _ (DataTypeGetter ExternalRoleF) _ _ _) = pure $ unsafeCoerce externalRole

compileFunction (SQD _ (DataTypeGetter ContextF) _ _ _) = pure $ unsafeCoerce context

compileFunction (SQD _ (ExternalCoreContextGetter functionName) ran _ _) = do
  (f :: HiddenFunction) <- pure $ unsafeCoerce $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  pure $ unsafeCoerce f [ctype ran]
  where
    ctype :: Domain -> String
    ctype d = unsafePartial $ case domain2contextType d of
      ST (ContextType ct) -> ct

compileFunction (SQD _ (DataTypeGetter IdentityF) _ _ _) = pure $ (pure <<< identity)

compileFunction (SQD dom (DataTypeGetter ModelNameF) _ _ _) = case dom of
  RDOM _ -> pure $ unsafeCoerce roleModelName
  CDOM _ -> pure $ unsafeCoerce contextModelName
  VDOM _ (Just pt) -> pure \_ -> pure $ propertytype2string pt
  ContextKind -> pure $ unsafeCoerce contextTypeModelName'
  RoleKind -> pure $ unsafeCoerce roleTypeModelName'
  _ -> throwError (error $ "UnsaveCompiler: cannot retrieve modelname from " <> show dom)

compileFunction (SQD _ (TypeGetter TypeOfContextF) _ _ _) = pure $ unsafeCoerce contextType

compileFunction (SQD _ (TypeGetter RoleTypesF) _ _ _) = pure $ unsafeCoerce (liftToInstanceLevel allRoleTypesInContext)

compileFunction (SQD _ (DataTypeGetter BindingF) _ _ _) = pure $ unsafeCoerce binding

compileFunction (SQD dom (Constant range value) _ _ _) = pure \_ -> pure value

-- compileFunction (SQD dom (RoleIndividual individual) _ _ _) = pure $ unsafeCoerce (\x -> lift $ lift $ maybe [] identity (lookupIndexedRole (unwrap individual)) :: MPQ RoleInstance)

compileFunction (SQD dom (RoleIndividual individual) _ _ _) = pure $ unsafeCoerce \x -> ArrayT do
  mi <- ((lift $ lookupIndexedRole (unwrap individual)) :: (WriterT (Array Assumption) MonadPerspectives) (Maybe RoleInstance))
  case mi of
    Nothing -> pure []
    Just i -> pure [unwrap i]

compileFunction (SQD dom (ContextIndividual (ContextInstance ident)) _ _ _) = pure $ unsafeCoerce \x -> ArrayT do
  mi <- ((lift $ lookupIndexedContext ident) :: (WriterT (Array Assumption) MonadPerspectives)(Maybe ContextInstance))
  case mi of
    Nothing -> pure []
    Just i -> pure [unwrap i]

compileFunction (SQD dom (Value2Role _) _ _ _) = pure $ unsafeCoerce (\x -> pure x :: MPQ String)

compileFunction (MQD dom (ExternalCoreRoleGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafeCoerce $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions) <- traverse compileFunction args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ traverse (\g -> runArrayT $ g c) argFunctions
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
      5 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        c
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        (unsafePartial (unsafeIndex values 5))
        c
      _ -> throwError (error "Too many arguments for external core module: maximum is 6")
    )

compileFunction (MQD dom (ExternalCorePropertyGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (String ~~> String)) <- traverse compileFunction args
  pure (\r -> do
    (values :: Array (Array String)) <- lift $ traverse (\g -> runArrayT $ g r) argFunctions
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
      5 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        r
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        (unsafePartial (unsafeIndex values 5))
        r
      _ -> throwError (error "Too many arguments for external core module: maximum is 6")
    )

compileFunction (SQD dom (VariableLookup varName) range _ _) = pure $ unsafeCoerce (lookup varName)

-- If the second term is a constant, we can ignore the left term. This is an optimalisation.
compileFunction (BQD _ (BinaryCombinator ComposeF) f1 f2@(SQD _ (Constant _ _) _ _ _) _ _ _) = compileFunction f2

compileFunction (BQD _ (BinaryCombinator ComposeSequenceF) f1 f2 _ _ _) = do
  (f1' :: String ~~> String) <- compileFunction f1
  (f2' :: Array String ~~> String) <- compileSequenceFunction f2
  pure \s -> ArrayT do
    results <- runArrayT $ f1' s
    runArrayT $ f2' results

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
  if (typeTimeOnly f1)
    -- Skip all VarBindings that were meant for the description compiler only.
    -- These will be bindings that are added by the core in the StateCompilers.
    then if (typeTimeOnly f2)
      then pure \c -> ArrayT $ pure []
      else compileFunction f2
    else do
      if (typeTimeOnly f2)
        then compileFunction f1
        else do
          f1' <- compileFunction f1
          f2' <- compileFunction f2
          -- While this seems an attractive implementation, it fails:
          --    pure \c -> (f1' c *> f2' c)
          -- This is because when the first computation doesn't give any value at all,
          -- the entire computation does not give values.
          -- The documentation of applySecond states:
          -- "Combine two effectful actions, keeping only the result of the second."
          -- However, that is misleading, to say the least.
          pure \c -> ArrayT do
            ignored <- runArrayT $ f1' c
            results <- runArrayT $ f2' c
            pure results

compileFunction (BQD _ (BinaryCombinator IntersectionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ Combinators.disjunction f1' f2'

compileFunction (BQD _ (BinaryCombinator FilledByF) sourceOfBindingRoles sourceOfBoundRoles _ _ _) = do
  sourceOfFilledRoles' <- compileFunction sourceOfBindingRoles
  sourceOfFillerRoles' <- compileFunction sourceOfBoundRoles
  pure $ (unsafeCoerce filledByOperator (unsafeCoerce sourceOfFilledRoles') (unsafeCoerce sourceOfFillerRoles'))

compileFunction (BQD _ (BinaryCombinator UnionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ Combinators.conjunction f1' f2'

-- The compiler only allows f1 and f2 if they're functional.
compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [EqualsF, NotEqualsF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ unsafeCoerce $ compare f1' f2' (unsafePartial $ compareFunction g)

compileFunction (BQD dom (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  -- NOTE. We now order the string representations of the values. This is OK
  -- for PString, PNumber, PDate and PBool.
  -- Check for each new type added to Range in Perspectives.Representation.Range.
  pure $ order dom f1' f2' g

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | g `eq` AndF = do
  (f1' :: String ~~> Value) <- unsafeCoerce $ compileFunction f1
  (f2' :: String ~~> Value) <- unsafeCoerce $ compileFunction f2
  pure (unsafeCoerce (logicalAnd f1' f2'))

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | g `eq` OrF = do
  (f1' :: String ~~> Value) <- unsafeCoerce $ compileFunction f1
  (f2' :: String ~~> Value) <- unsafeCoerce $ compileFunction f2
  pure (unsafeCoerce (logicalOr f1' f2'))

-- Add and subtract for numbers and strings. Divide and multiply just for numbers.
compileFunction (BQD _ (BinaryCombinator g) f1 f2 ran _ _) | isJust $ elemIndex g [AddF, SubtractF, DivideF, MultiplyF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ performNumericOperation g ran f1' f2' (unsafePartial $ mapNumericOperator g ran)

compileFunction (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (addBinding_ varName f1')

compileFunction (UQD _ WithFrame f1 _ _ _) = do
  f1' <- compileFunction f1
  pure \c -> withFrame_ f1' c

compileFunction (UQD _ (UnaryCombinator ExistsF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ exists f1')

compileFunction (UQD _ (UnaryCombinator FilledByF) sourceOfBoundRoles _ _ _) = do
  sourceOfBoundRoles' <- compileFunction sourceOfBoundRoles
  pure (unsafeCoerce $ filledByCombinator (unsafeCoerce sourceOfBoundRoles'))

compileFunction (UQD _ (UnaryCombinator FillsF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ fillsCombinator (unsafeCoerce f1'))

compileFunction (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ available_ f1')

compileFunction (UQD _ (UnaryCombinator NotF) f1 _ _ _) = do
  (f1' :: String ~~> Value) <- unsafeCoerce (compileFunction f1)
  pure (unsafeCoerce $ not f1')

compileFunction (SQD _ (GetRoleBindersF enumeratedRoleType contextType) _ _ _ ) = pure $ unsafeCoerce (getFilledRoles contextType enumeratedRoleType)

compileFunction (SQD _ (DataTypeGetterWithParameter functionName parameter) _ _ _ ) = do
  case functionName of
    BindingF -> pure $ unsafeCoerce bindingInContext (ContextType parameter)
    SpecialisesRoleTypeF -> pure $ unsafeCoerce (liftToInstanceLevel ((flip specialisesRoleType) (ENR $ EnumeratedRoleType parameter)))

    _ -> throwError (error $ "Unknown function for DataTypeGetterWithParameter: " <> show functionName)

compileFunction (SQD _ (RegExMatch (RegExP (reg :: Regex))) _ _ _) = pure \s -> do
  case match reg s of
    Nothing -> pure "false"
    otherwise -> pure "true"

-- Catch all
compileFunction qd = throwError (error $ "Cannot create a function out of '" <> prettyPrint qd <> "'.")

---------------------------------------------------------------------------------------------------
-- COMPILESEQUENCEFUNCTION
---------------------------------------------------------------------------------------------------
-- We have interpretations of AddF for numbers and strings only.
-- For MinimumF and MaximumF we have interpretations for numbers and strings and booleans and dates.
-- For AndF and OrF we have an interpretation for Booleans only.
-- We also require that the VDOM should have an EnumeratedPropertyType.
-- TODO. Het probleem is dat we (String ~~> String) moeten opleveren!
compileSequenceFunction :: QueryFunctionDescription -> MP (Array String -> MonadPerspectivesQuery String)
compileSequenceFunction (SQD dom (UnaryCombinator sequenceFunctionName) _ _ _) | isJust $ elemIndex sequenceFunctionName [CountF, MinimumF, MaximumF, AddF]  = case sequenceFunctionName of
  CountF -> pure \things -> pure (show $ length things)
  MinimumF -> case dom of
    (VDOM RAN.PNumber _) -> pure \numbers -> ArrayT do
      nrs <- traverse parseInt numbers
      pure $ maybe [] (singleton <<< show) (minimum nrs)
    (VDOM RAN.PString _) -> pure \strings -> ArrayT $ pure $ (maybe [] singleton) (minimum strings)
    (VDOM RAN.PBool _) -> pure \bools -> ArrayT do
      bls <- traverse parseBool bools
      pure $ maybe [] (singleton <<< show) (minimum bools)
    _ -> throwError (error $ "compileSequenceFunction cannot handle domain '" <> show dom <> "' for 'minimum'.")
  MaximumF -> case dom of
    (VDOM RAN.PNumber _) -> pure \numbers -> ArrayT do
      nrs <- traverse parseInt numbers
      pure $ maybe [] (singleton <<< show) (maximum nrs)
    (VDOM RAN.PString _) -> pure \strings -> ArrayT $ pure $ (maybe [] singleton) (maximum strings)
    _ -> throwError (error $ "compileSequenceFunction cannot handle domain '" <> show dom <> "' for 'maximum'.")
  AddF -> case dom of
    (VDOM RAN.PNumber _) -> pure \numbers -> ArrayT $ do
      (nrs :: Array Int) <- traverse parseInt numbers
      pure $ [show (foldl (\cumulator nr -> cumulator + nr) 0 nrs)]
    -- pure (\numbers -> pure $ foldl (\cumulator str -> cumulator + str) "" numbers)
    (VDOM RAN.PString _) -> pure \strings -> ArrayT $ pure [foldl (\cumulator str -> cumulator <> str) "" strings]
    _ -> throwError (error $ "compileSequenceFunction cannot handle domain '" <> show dom <> "' for 'add'.")
  op -> throwError (error $ "compileSequenceFunction cannot handle sequence function: '" <> show op <> "'.")
-- Catch all
compileSequenceFunction qd = throwError (error $ "compileSequenceFunction cannot create a function out of '" <> prettyPrint qd <> "'.")

---------------------------------------------------------------------------------------------------
-- TYPETIMEONLY
---------------------------------------------------------------------------------------------------
-- | Detects variable bindings with function names that indicate the binding was used in
-- | type time only. This is for standard variables that are added in the state compilers,
-- | for automatic actions, notifications and actions.
typeTimeOnly :: QueryFunctionDescription -> Boolean
typeTimeOnly (UQD _ (BindVariable _) f1 _ _ _) = case f1 of
  SQD _ (TypeTimeOnlyContextF _) _ _ _ -> true
  SQD _ (TypeTimeOnlyEnumeratedRoleF _) _ _ _ -> true
  SQD _ (TypeTimeOnlyCalculatedRoleF _) _ _ _ -> true
  _ -> false
typeTimeOnly _ = false
---------------------------------------------------------------------------------------------------
-- COMPARING
---------------------------------------------------------------------------------------------------
-- We handle no results for the two ObjectGetters as follows:
--  * if both are empty, the result is true
--  * if one of them is empty, the result is false.
--  * because we know both a and b are functional, we just compare the first elements.
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
addBinding_ varName computation ctxt  = ArrayT do
  (v :: Array b) <- runArrayT $ computation ctxt
  lift $ addBinding varName (unsafeCoerce v)
  pure v

withFrame_ :: forall a b. (a ~~> b) -> a ~~> b
withFrame_ computation ctxt = ArrayT do
  old <- lift $ getVariableBindings
  void $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
  r <- runArrayT $ computation ctxt
  void $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure r

lookup :: String -> String ~~> String
lookup varName _ = ArrayT do
    mv <- lift (lookupVariableBinding varName)
    pure $ (unsafePartial (fromJust mv))

---------------------------------------------------------------------------------------------------
-- ORDERING
---------------------------------------------------------------------------------------------------
-- | `order` follows the same pattern as `logicalOperation` and `compare`, but does
-- | not have to deal with missing arguments in the same way. These functions, dealing
-- | with Booleans, map a missing operator on a `false` result. That we cannot do for
-- | ordering. Hence we return an empty result if one or both arguments are missing.
order :: Domain -> (String ~~> String) -> (String ~~> String) -> FunctionName -> String ~~> String
order (VDOM ran _) a b f c = ArrayT do
  as <- runArrayT $ a c
  bs <- runArrayT $ b c
  case head as, head bs of
    Just a', Just b' -> case ran of
      RAN.PString -> pure [show $ fString a' b']
      RAN.PNumber -> singleton <<< show <$> (fInt <$> (parseInt a') <*> (parseInt b'))
      RAN.PBool ->  singleton <<< show <$> (fBool <$> parseBool a' <*> parseBool b')
      RAN.PDate -> singleton <<< show <$> (fDate <$> parseDate a' <*> parseDate b')
      -- Compare email addresses as strings.
      RAN.PEmail -> pure [show $ fString a' b']
    _, _ -> pure []
  where
    fString :: String -> String -> Boolean
    fString = unsafePartial case f of
      LessThanF -> (<)
      LessThanEqualF -> (<=)
      GreaterThanF -> (>)
      GreaterThanEqualF -> (>=)

    fInt :: Int -> Int -> Boolean
    fInt = unsafePartial case f of
      LessThanF -> (<)
      LessThanEqualF -> (<=)
      GreaterThanF -> (>)
      GreaterThanEqualF -> (>=)

    fBool :: Boolean -> Boolean -> Boolean
    fBool = unsafePartial case f of
      LessThanF -> (<)
      LessThanEqualF -> (<=)
      GreaterThanF -> (>)
      GreaterThanEqualF -> (>=)

    fDate :: DateTime -> DateTime -> Boolean
    fDate = unsafePartial case f of
      LessThanF -> (<)
      LessThanEqualF -> (<=)
      GreaterThanF -> (>)
      GreaterThanEqualF -> (>=)

order _ _ _ _ _ = throwError (error "Not a Range type. Cannot order values.")

orderFunction :: forall a. Ord a => Partial => FunctionName -> (a -> a -> Boolean)
orderFunction fname = case fname of
  LessThanF -> (<)
  LessThanEqualF -> (<=)
  GreaterThanF -> (>)
  GreaterThanEqualF -> (>=)

------------------------------------------------------------------------------------
-- NUMERIC OPERATIONS
------------------------------------------------------------------------------------
-- | Just for Addition, Subtraction (on Strings and Integers), Multiplication and
-- | Division (just on Integers).
performNumericOperation ::
  FunctionName ->
  Range ->
  (String ~~> String) ->
  (String ~~> String) ->
  (String -> String ~~> String) ->
  (String ~~> String)
performNumericOperation g ran a b f c = ArrayT do
  (as :: Array String) <- runArrayT (a c)
  (bs :: Array String) <- runArrayT (b c)
  (performNumericOperation' g ran f) as bs

performNumericOperation' ::
  FunctionName ->
  Range ->
  (String -> String ~~> String) ->
  Array String ->
  Array String -> (WriterT (ArrayWithoutDoubles InformedAssumption) MonadPerspectives) (Array String)
performNumericOperation' g ran f as bs = do
  case head as, head bs of
    Just a1, Just b1 -> do
      runArrayT $ f a1 b1
    Nothing, Just b1 -> case g of
      -- Just return fr2
      AddF -> pure [b1]
      -- Subtract the second value from the first means: negate it, when a number.
      SubtractF -> case ran of
        VDOM RAN.PNumber _ -> (\x -> [show (0 - x)]) <$> parseInt b1
        otherwise -> pure []
      otherwise -> pure []
    Just a1, Nothing -> case g of
      AddF -> pure [a1]
      SubtractF -> pure [a1]
      otherwise -> pure []
    _, _ -> pure []

mapNumericOperator :: Partial => FunctionName -> Domain -> (String -> String ~~> String)
mapNumericOperator AddF (VDOM RAN.PNumber _) = wrapNumericOperator (+)
mapNumericOperator AddF (VDOM RAN.PString _) = \s1 s2 -> pure (s1 <> s2)
mapNumericOperator SubtractF (VDOM RAN.PNumber _) = wrapNumericOperator (-)
mapNumericOperator SubtractF (VDOM RAN.PString _) = \s1 s2 -> case (Pattern s1) `stripSuffix` s2 of
  Nothing -> pure s1
  Just r -> pure r
mapNumericOperator DivideF (VDOM RAN.PNumber _) = wrapNumericOperator (/)
mapNumericOperator MultiplyF (VDOM RAN.PNumber _) = wrapNumericOperator (*)

wrapNumericOperator :: (Int -> Int -> Int) -> (String -> String ~~> String)
wrapNumericOperator g p q = show <$> (g <$> (parseInt p) <*> (parseInt q))

---------------------------------------------------------------------------------------------------
-- CONSTRUCT ROLE- AND PROPERTYVALUE GETTERS
---------------------------------------------------------------------------------------------------
-- | From a string that maybe identifies a Role, retrieve or construct a function to get that role from
-- | a Context instance. Notice that this function may fail.
getRoleFunction ::
  String -> MonadPerspectives (ContextInstance ~~> RoleInstance)
getRoleFunction id = do
  ident <- expandDefaultNamespaces id
  (
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
      unsafeCoerce $ RC.calculation p >>= compileFunction
  )

getRoleInstances :: RoleType -> (ContextInstance ~~> RoleInstance)
getRoleInstances (ENR rt) c = do
  (p :: EnumeratedRole) <- lift $ lift $ getPerspectType rt
  f <- (lift $ lift $ RC.calculation p) >>= lift <<< lift <<< compileFunction
  (unsafeCoerce f) c
getRoleInstances (CR rt) c = getCalculatedRoleInstances rt c

roleFunctionFromQfd :: QueryFunctionDescription -> MonadPerspectives (ContextInstance ~~> RoleInstance)
roleFunctionFromQfd qfd = do
  f <- compileFunction qfd
  pure (unsafeCoerce f)

getCalculatedRoleInstances :: CalculatedRoleType -> (ContextInstance ~~> RoleInstance)
getCalculatedRoleInstances rt@(CalculatedRoleType ident) c = case lookupRoleGetterByName ident of
  (Just g) -> g c
  Nothing -> do
    (p :: CalculatedRole) <- lift $ lift $ getPerspectType rt
    f <- (lift $ lift $ RC.calculation p) >>= lift <<< lift <<< compileFunction
    -- TODO. Cache de functie!
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

-- | Construct a function to compute Strings (not further typed) from an instance of a Role.
role2string :: QueryFunctionDescription -> MP (RoleInstance ~~> String)
role2string qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute instances of a RoleType from an instance of a Context.
role2role :: QueryFunctionDescription -> MP (RoleInstance ~~> RoleInstance)
role2role qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute values of a Property for some RoleType from an instance of a Context.
context2propertyValue :: QueryFunctionDescription -> MP (ContextInstance ~~> Value)
context2propertyValue qd = unsafeCoerce $ compileFunction qd

-- | Construct a function to compute values of a Property for some RoleType from an instance of a Context.
role2propertyValue :: QueryFunctionDescription -> MP (RoleInstance ~~> Value)
role2propertyValue qd = unsafeCoerce $ compileFunction qd

-- | From a string that maybe identifies a Property(Enumerated or Calculated), retrieve or construct a function to
-- | get values for that Property from a Role instance. Notice that this function may fail.
-- TODO: make this function cache.
getPropertyFunction :: String -> MonadPerspectives (RoleInstance ~~> Value)
getPropertyFunction p = do
  ident <- expandDefaultNamespaces p
  getterFromPropertyType (ENP (EnumeratedPropertyType ident)) <|> getterFromPropertyType (CP (CalculatedPropertyType ident))

-- getPropertyFunction id = do
--   ident <- expandDefaultNamespaces id
--   (unsafePartial $
--     case lookupPropertyValueGetterByName ident of
--       Nothing -> empty
--       (Just g) -> pure g
--     <|>
--     do
--       (p :: EnumeratedProperty) <- getPerspectType (EnumeratedPropertyType ident)
--       unsafeCoerce $ PC.calculation p >>= compileFunction
--     <|>
--     do
--       (p :: CalculatedProperty) <- getPerspectType (CalculatedPropertyType ident)
--       unsafeCoerce $ PC.calculation p >>= compileFunction >>= pure <<< (withFrame_ >>> pushCurrentObject)
--       )

-- TODO: opruimen!
pushCurrentObject :: (String ~~> String) -> (String ~~> String)
pushCurrentObject f roleId = do
  -- save contextId in it under the name currentobject
  lift $ lift (addBinding "currentobject" [roleId])
  -- apply f to roleId
  f roleId

-- | From a PropertyType, retrieve or construct a function to get values for that Property from a Role instance.
-- | Caches the result.
getterFromPropertyType :: PropertyType -> MP (RoleInstance ~~> Value)
getterFromPropertyType (ENP ep@(EnumeratedPropertyType id)) = case lookupPropertyValueGetterByName id of
  Nothing -> do
    pure $ getProperty ep
  Just g -> pure g
getterFromPropertyType (CP cp@(CalculatedPropertyType id)) = case lookupPropertyValueGetterByName id of
  Nothing -> do
    p <- getPerspectType cp
    functional <- PC.functional p
    mandatory <- PC.mandatory p
    getter <- unsafeCoerce $ PC.calculation p >>= compileFunction >>= pure <<< (withFrame_ >>> pushCurrentObject)
    void $ pure $ propertyGetterCacheInsert id getter functional mandatory
    pure getter
  Just g -> pure g

getHiddenFunction :: QueryFunctionDescription -> MP HiddenFunction
getHiddenFunction = unsafeCoerce $ compileFunction

-- | From a string that represents either a Calculated or an Enumerated property,
-- | for a given abstract datatype of roles, retrieve the values from a role instance.
getDynamicPropertyGetter :: String -> ADT EnumeratedRoleType -> MP (RoleInstance ~~> Value)
getDynamicPropertyGetter p adt = do
  (pt :: PropertyType) <- getProperType p
  case pt of 
    CP _ -> do 
      allProps <- allLocallyRepresentedProperties adt
      -- Special case for the 'property of last resort' that is inserted in serialised perspectives for roles without properties.
      if (isJust $ elemIndex pt allProps) || pt == (CP $ CalculatedPropertyType roleWithId)
        then getterFromPropertyType pt
        else pure (binding >=> f)
    ENP eprop -> pure $ g eprop
  where
    g :: EnumeratedPropertyType -> RoleInstance ~~> Value
    g eprop rid = do 
      -- We must take aliases of the actual role type into account.
      (roleType :: EnumeratedRoleType) <- lift $ lift $ roleType_ rid
      -- NOTE. UP TILL version v0.20.0 we have a very specific problem with the external roles of the specialisations
      -- of model:System$Model. These roles are fetched before the models themselves are fetched; indeed, we don't mean to 
      -- get the models until the end user explicitly asks for it.
      -- This situation will go away in the next version.
      aliases <- catchError (lift $ lift $ propertyAliases roleType)
        \e -> pure OBJ.empty
      case OBJ.lookup (unwrap eprop) aliases of
        Just destination -> do 
          getter <- lift $ lift $ getterFromPropertyType (ENP destination)
          getter rid
        Nothing -> do
          allProps <- lift $ lift $ allLocallyRepresentedProperties adt
          if isJust $ elemIndex (ENP eprop) allProps
            then do 
              getter <- lift $ lift $ getterFromPropertyType (ENP eprop)
              getter rid
            else (binding >=> f) rid

    f :: RoleInstance ~~> Value
    f bnd = do
      (bndType :: EnumeratedRoleType) <- lift $ lift $ roleType_ bnd
      getter <- lift $ lift $ getDynamicPropertyGetter p (ST bndType)
      getter bnd

-- | From a string that represents part of the name of either a Calculated or an Enumerated property,
-- | for a given abstract datatype of roles, retrieve the values from a role instance.
-- | When multiple matches could be made with properties in the role telescope,
-- | the one closest to the root is preferred.
getDynamicPropertyGetterFromLocalName :: String -> ADT EnumeratedRoleType -> MP (RoleInstance ~~> Value)
getDynamicPropertyGetterFromLocalName ln adt = do
  (allProps :: Array PropertyType) <- allLocallyRepresentedProperties adt
  case findIndex ((test (unsafeRegex (ln <> "$") noFlags)) <<< propertytype2string) allProps of
    Nothing -> pure f
    (Just i) -> case (unsafePartial $ fromJust $ index allProps i) of
      prop@(CP _) -> getterFromPropertyType prop
      ENP eprop -> pure $ g eprop

  where
    g :: EnumeratedPropertyType -> RoleInstance ~~> Value
    g eprop rid = do 
      -- We must take aliases of the actual role type into account.
      (roleType :: EnumeratedRoleType) <- lift $ lift $ roleType_ rid
      -- NOTE. UP TILL version v0.20.0 we have a very specific problem with the external roles of the specialisations
      -- of model:System$Model. These roles are fetched before the models themselves are fetched; indeed, we don't mean to 
      -- get the models until the end user explicitly asks for it.
      -- This situation will go away in the next version.
      aliases <- catchError (lift $ lift $ propertyAliases roleType)
        \e -> pure OBJ.empty
      case OBJ.lookup (unwrap eprop) aliases of
        Just destination -> do 
          getter <- lift $ lift $ getterFromPropertyType (ENP destination)
          getter rid
        Nothing -> do
          getter <- lift $ lift $ getterFromPropertyType (ENP eprop)
          getter rid

    f :: (RoleInstance ~~> Value)
    f roleInstance = do
      bnd <- lift $ lift $ binding_ roleInstance
      case bnd of
        Nothing -> empty
        Just bnd' -> do
          (bndType :: EnumeratedRoleType) <- lift $ lift $ roleType_ bnd'
          getter <- lift $ lift $ getDynamicPropertyGetterFromLocalName ln (ST bndType)
          getter bnd'

-- | Preferrably returns the value of the type of 'me' of the context instance:
-- | this will be the enumerated role instance that is filled (ultimately) with sys:Me.
-- | Otherwise returns all Calculated roles that are ultimately filled with sys:Me.
-- | The Guest and Visitor conventions tap in here.
getMyType :: ContextInstance ~~> RoleType
getMyType ctxt = getPreferredUserRoleType ctxt
  <|>
  (getMe >=> map ENR <<< roleType) ctxt
  <|>
  -- NOTE: this is a safety measure that catches cases where the 'me' administration has gone wrong.
  findMeInEnumeratedRoles ctxt
  <|>
  findMeInUnlinkedRoles ctxt
  <|>
  findMeInCalculatedRoles ctxt
  where
    findMeInEnumeratedRoles :: ContextInstance ~~> RoleType
    findMeInEnumeratedRoles = (contextType >=> Combinators.filter (liftToInstanceLevel enumeratedUserRole) (computesMe ctxt))

    findMeInCalculatedRoles :: ContextInstance ~~> RoleType
    findMeInCalculatedRoles = (contextType >=> Combinators.filter (liftToInstanceLevel calculatedUserRole) (computesMe ctxt))

    findMeInUnlinkedRoles ::  ContextInstance ~~> RoleType
    findMeInUnlinkedRoles = Combinators.filter (Combinators.filter (contextType >=> liftToInstanceLevel enumeratedUserRole) isUnlinked) (computesMe ctxt)
      where
        isUnlinked :: RoleType ~~> Boolean
        isUnlinked (ENR rt) = lift $ lift $ isUnlinked_ rt
        isUnlinked (CR _) = pure false

getAllMyRoleTypes :: ContextInstance ~~> RoleType
getAllMyRoleTypes ctxt = ((contextType >=> Combinators.filter (liftToInstanceLevel userRole) (computesMe ctxt)) ctxt)

computesMe :: ContextInstance -> RoleType ~~> Boolean
computesMe ctxt' rt = some (getRoleInstances rt >=> lift <<< lift <<< isMe) ctxt'

getMeInRoleAndContext :: RoleType -> ContextInstance ~~> RoleInstance
getMeInRoleAndContext rt = filter (getRoleInstances rt) (lift <<< lift <<< isMe)
