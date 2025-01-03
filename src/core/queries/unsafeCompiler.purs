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

import Control.Alt (void, (<|>))
import Control.Alternative (guard)
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, tell)
import Control.Plus (empty)
import Data.Array (catMaybes, elemIndex, filterA, findIndex, foldl, head, index, length, null, singleton, unsafeIndex)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), lastIndexOf, stripSuffix, length) as STRING
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, maximum, minimum, traverse)
import Effect.Exception (error)
import Foreign.Object (empty, lookup) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (rol_binding, rol_context, rol_id, rol_pspType)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), Assumption, AssumptionTracking, InformedAssumption(..), MP, MPQ, MonadPerspectives, MonadPerspectivesQuery, liftToInstanceLevel, (###=), (##>), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), firstOfSequence, runArrayT)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Combinators (available_, exists, logicalAnd, logicalOr, not)
import Perspectives.Instances.Combinators (conjunction, intersection, orElse) as Combinators
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (binding, binding_, context, contextModelName, contextType, contextType_, externalRole, filledByCombinator, filledByOperator, fillsCombinator, getActiveRoleStates_, getActiveStates_, getEnumeratedRoleInstances, getProperty, getRecursivelyFilledRoles', getUnlinkedRoleInstances, indexedContextName, indexedRoleName, roleModelName, roleType_)
import Perspectives.Instances.Values (parseBool, parseNumber)
import Perspectives.ModelDependencies (roleWithId)
import Perspectives.Names (expandDefaultNamespaces, lookupIndexedContext, lookupIndexedRole)
import Perspectives.ObjectGetterLookup (lookupPropertyValueGetterByName, lookupRoleGetterByName, propertyGetterCacheInsert)
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP(..))
import Perspectives.Persistent (getPerspectRol)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, lookupVariableBinding)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), Range, RoleInContext, domain, domain2PropertyRange, domain2contextType, domain2roleType, range, roleInContext2Role)
import Perspectives.Representation.ADT (ADT(..), equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getEnumeratedRole, getPerspectType, getState)
import Perspectives.Representation.Class.Property (calculation, functional, mandatory) as PC
import Perspectives.Representation.Class.Property (getPropertyType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, toConjunctiveNormalForm_)
import Perspectives.Representation.Class.Role (calculation) as RC
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Duration_(..), isDateOrTime, isPDuration)
import Perspectives.Representation.Range (Range(..)) as RAN
import Perspectives.Representation.State (State(..), StateFulObject(..)) as STATE
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.Time (string2Date, string2DateTime, string2Time)
import Perspectives.Types.ObjectGetters (allRoleTypesInContext, contextTypeModelName', generalisesRoleType, propertyAliases, publicUserRole, roleTypeModelName')
import Perspectives.Utilities (prettyPrint)
import Prelude (class Eq, class Ord, add, bind, const, discard, eq, flip, identity, mul, negate, notEq, pure, show, sub, ($), (&&), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>=), (>=>), (>>=), (||))
import Unsafe.Coerce (unsafeCoerce)

-- TODO. String dekt de lading niet sinds we RoleTypes toelaten. Een variabele zou
-- beter zijn.
compileFunction :: QueryFunctionDescription -> MP (String ~~> String)

compileFunction (SQD _ (RolGetter (ENR (EnumeratedRoleType r))) _ _ _) = if isExternalRole r
  then pure $ unsafeCoerce $ externalRole
  else pure $ unsafeCoerce $ getEnumeratedRoleInstances (EnumeratedRoleType r) 

compileFunction (SQD _ (RolGetter (CR cr)) _ _ _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  -- TODO moeten we hier de currentcontext pushen?
  RC.calculation ct >>= compileFunction

compileFunction (SQD (RDOM roleAdt) (PropertyGetter (ENP prop@(EnumeratedPropertyType pt))) _ _ _) = do
  -- g <- getDynamicPropertyGetter pt (roleInContext2Role <$> roleAdt)
  pure $ unsafeCoerce (getPropertyFromTelescope prop)

compileFunction (SQD (RDOM roleAdt) (PropertyGetter (CP (CalculatedPropertyType pt))) _ _ _) = do
  g <- getDynamicPropertyGetter pt (roleInContext2Role <$> roleAdt)
  pure $ unsafeCoerce g

-- compileFunction (SQD _ (PropertyGetter (CP pt@(CalculatedPropertyType id))) _ _ _) = do
--   case lookupPropertyValueGetterByName id of
--     Nothing -> do
--       (cp :: CalculatedProperty) <- getPerspectType pt
--       -- PC.calculation cp >>= compileFunction
--       functional <- PC.functional cp
--       mandatory <- PC.mandatory cp
--       getter <- PC.calculation cp >>= compileFunction >>= pure <<< (withFrame_ >>> pushCurrentObject)
--       void $ pure $ propertyGetterCacheInsert id (unsafeCoerce getter) functional mandatory
--       pure getter
--     Just getter -> pure (unsafeCoerce getter)


compileFunction (SQD _ (DataTypeGetter ExternalRoleF) _ _ _) = pure $ unsafeCoerce externalRole

compileFunction (SQD _ (DataTypeGetter IndexedContextName) _ _ _) = pure $ unsafeCoerce indexedContextName

compileFunction (SQD _ (DataTypeGetter IndexedRoleName) _ _ _) = pure $ unsafeCoerce (indexedRoleName)

compileFunction (SQD _ (DataTypeGetter ContextF) _ _ _) = pure $ unsafeCoerce context

compileFunction (SQD _ (DataTypeGetter IdentityF) _ _ _) = pure $ (pure <<< identity)

compileFunction (SQD dom (DataTypeGetter ModelNameF) _ _ _) = case dom of
  RDOM _ -> pure $ unsafeCoerce roleModelName
  CDOM _ -> pure $ unsafeCoerce contextModelName
  VDOM _ (Just pt) -> pure \_ -> pure $ propertytype2string pt
  ContextKind -> pure $ unsafeCoerce contextTypeModelName'
  RoleKind -> pure $ unsafeCoerce roleTypeModelName'
  _ -> throwError (error $ "UnsaveCompiler: cannot retrieve modelname from " <> show dom)

compileFunction (SQD _ (TypeGetter TypeOfContextF) _ _ _) = pure $ unsafeCoerce contextType

compileFunction (SQD _ (RoleTypeConstant qname) RoleKind _ _) = pure $ unsafeCoerce ((pure <<< const qname) :: String ~~> RoleType)

compileFunction (SQD _ (ContextTypeConstant qname) ContextKind _ _) = pure $ unsafeCoerce ((pure <<< const qname) :: String ~~> ContextType)

compileFunction (SQD _ (TypeGetter RoleTypesF) _ _ _) = pure $ unsafeCoerce (liftToInstanceLevel allRoleTypesInContext)

compileFunction (SQD _ (DataTypeGetter FillerF) ran _ _) = pure $ unsafeCoerce (getFillerTypeRecursively $ unsafePartial domain2roleType ran)

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

compileFunction (SQD dom (PublicRole individual) _ _ _) = pure $ unsafeCoerce (\x -> (pure $ unwrap individual :: MonadPerspectivesQuery String))

compileFunction (SQD dom (PublicContext individual) _ _ _) = pure $ unsafeCoerce (\x -> (pure $ unwrap individual :: MonadPerspectivesQuery String))

compileFunction (SQD dom (Value2Role _) _ _ _) = pure $ unsafeCoerce (\x -> pure x :: MPQ String)

compileFunction (MQD _ (ExternalCoreContextGetter functionName) args ran _ _) = do
  (f :: HiddenFunction) <- pure $ unsafeCoerce $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  pure $ unsafeCoerce f [ctype ran]
  where
    ctype :: Domain -> String
    ctype d = unsafePartial $ case domain2contextType d of
      ST (ContextType ct) -> ct
      UET (ContextType ct) -> ct

compileFunction (MQD dom (ExternalCoreRoleGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafeCoerce $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions) <- traverse compileFunction args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ traverse (\g -> runArrayT $ g c) argFunctions
    (nrOfParameters :: Int) <- pure $ unsafePartial (fromJust $ lookupHiddenFunctionNArgs functionName)
    -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
    -- If we do have an extra argument value, supply it as the last argument instead of r.
    (lastArgument :: String) <- case index values nrOfParameters of
      Nothing -> pure c
      Just v -> pure $ unsafePartial (unsafeIndex v 0)
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: (String -> MPQ String)) lastArgument
      1 -> (unsafeCoerce f :: (Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        lastArgument
      2 -> (unsafeCoerce f :: (Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        lastArgument
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        lastArgument
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        lastArgument
      5 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        lastArgument
      6 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        (unsafePartial (unsafeIndex values 5))
        lastArgument
      _ -> throwError (error "Too many arguments for external core module: maximum is 6")
    )

compileFunction (MQD dom (ExternalCorePropertyGetter functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (String ~~> String)) <- traverse compileFunction args
  pure (\r -> do
    (values :: Array (Array String)) <- lift $ traverse (\g -> runArrayT $ g r) argFunctions
    (nrOfParameters :: Int) <- pure $ unsafePartial (fromJust $ lookupHiddenFunctionNArgs functionName)
    -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
    -- If we do have an extra argument value, supply it as the last argument instead of r.
    (lastArgument :: String) <- case index values nrOfParameters of
      Nothing -> pure r
      Just v -> pure $ unsafePartial (unsafeIndex v 0)
    case nrOfParameters of
      0 -> (unsafeCoerce f :: (String -> MPQ String)) lastArgument
      1 -> (unsafeCoerce f :: (Array String -> String -> MPQ String)) (unsafePartial (unsafeIndex values 0)) lastArgument
      2 -> (unsafeCoerce f :: (Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        lastArgument
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        lastArgument
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        lastArgument
      5 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        lastArgument
      6 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPQ String))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        (unsafePartial (unsafeIndex values 5))
        lastArgument
      _ -> throwError (error "Too many arguments for external core module: maximum is 6")
    )

compileFunction (SQD dom (VariableLookup varName) range _ _) = pure $ lookup varName

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

compileFunction (UQD _ FilterF criterium _ _ _) = do 
  (criterium' :: String ~~> String) <- (compileFunction criterium)
  pure \r -> do
    passes <- criterium' r
    guard (passes == "true")
    pure r

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
  pure $ Combinators.intersection f1' f2'

compileFunction (BQD _ (BinaryCombinator FilledByF) sourceOfFilledRoles sourceOfFillerRoles _ _ _) = do
  sourceOfFilledRoles' <- compileFunction sourceOfFilledRoles
  sourceOfFillerRoles' <- compileFunction sourceOfFillerRoles
  pure $ (unsafeCoerce filledByOperator (unsafeCoerce sourceOfFilledRoles') (unsafeCoerce sourceOfFillerRoles'))

compileFunction (BQD _ (BinaryCombinator UnionF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ Combinators.conjunction f1' f2'

compileFunction (BQD _ (BinaryCombinator OrElseF) f1 f2 _ _ _) = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  pure $ Combinators.orElse f1' f2'

-- The compiler only allows f1 and f2 if they're functional.
compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [EqualsF, NotEqualsF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2

  pure $ unsafeCoerce $ compare f1' f2' (unsafePartial $ compareFunction g)

compileFunction (BQD _ (BinaryCombinator g) f1 f2 _ _ _) | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] = do
  f1' <- compileFunction f1
  f2' <- compileFunction f2
  -- NOTE. We transform the string representation of Value to types that can be compared according to their Range types.
  -- Check for each new type added to Range in Perspectives.Representation.Range.
  pure $ order (range f1) f1' f2' g

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
  -- If both ranges are equal:
  if range f1 `eq` range f2
    then pure $ performNumericOperation g ran f1' f2' (unsafePartial $ mapNumericOperator g ran)
    -- Otherwise we know one of the ranges is a PDate and the other is a PDuration and the function is AddF or SubtractF.
    else if isDateOrTime (unsafePartial domain2PropertyRange $ range f1) && isPDuration (unsafePartial domain2PropertyRange $ range f2)
      then pure $ performNumericOperation g ran f1' f2' (unsafePartial $ mapDurationOperator g (range f2))
      else pure $ performNumericOperation g ran f2' f1' (unsafePartial $ mapDurationOperator g (range f1))

compileFunction (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (addBinding_ varName f1')

compileFunction (UQD _ WithFrame f1 _ _ _) = do
  f1' <- compileFunction f1
  pure \c -> withFrame_ f1' c

compileFunction (UQD _ (UnaryCombinator ExistsF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ exists f1')

compileFunction (UQD _ (UnaryCombinator FilledByF) sourceOfFillerRoles _ _ _) = do
  sourceOfFillerRoles' <- compileFunction sourceOfFillerRoles
  pure (unsafeCoerce $ filledByCombinator (unsafeCoerce sourceOfFillerRoles'))

compileFunction (UQD _ (UnaryCombinator FillsF) sourceOfFilledRoles _ _ _) = do
  sourceOfFilledRoles' <- compileFunction sourceOfFilledRoles
  pure (unsafeCoerce $ fillsCombinator (unsafeCoerce sourceOfFilledRoles'))

compileFunction (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) = do
  f1' <- compileFunction f1
  pure (unsafeCoerce $ available_ f1')

compileFunction (UQD _ (UnaryCombinator ContextIndividualF) contextExpr _ _ _) = compileFunction contextExpr

compileFunction (UQD _ (UnaryCombinator RoleIndividualF) contextExpr _ _ _) = compileFunction contextExpr

compileFunction (UQD _ (UnaryCombinator NotF) f1 _ _ _) = do
  (f1' :: String ~~> Value) <- unsafeCoerce (compileFunction f1)
  pure (unsafeCoerce $ not f1')

compileFunction (SQD _ (FilledF enumeratedRoleType contextType) _ _ _ ) = pure $ unsafeCoerce (getRecursivelyFilledRoles' contextType enumeratedRoleType)

compileFunction (SQD _ (DataTypeGetterWithParameter functionName parameter) ran _ _ ) = do
  case functionName of
    FillerF -> if parameter == "direct"
      then pure $ unsafeCoerce binding
      else pure $ unsafeCoerce (bindingInContext (ContextType parameter) (unsafePartial domain2roleType ran))
    SpecialisesRoleTypeF -> pure $ unsafeCoerce (liftToInstanceLevel ((flip generalisesRoleType) (ENR $ EnumeratedRoleType parameter)))
    IsInStateF -> do 
      (STATE.State {stateFulObject}) <- getState (StateIdentifier parameter)
      case stateFulObject of 
        STATE.Cnt ct -> pure $ \contextId -> lift $ lift $ getActiveStates_ (ContextInstance contextId) >>= pure <<< show <<< isJust <<< elemIndex (StateIdentifier parameter)
        STATE.Orole rt -> pure $ \roleId -> lift $ lift $ getActiveRoleStates_ (RoleInstance roleId) >>= pure <<< show <<< isJust <<< elemIndex (StateIdentifier parameter)
        STATE.Srole rt -> pure $ \roleId -> lift $ lift $ getActiveRoleStates_ (RoleInstance roleId) >>= pure <<< show <<< isJust <<< elemIndex (StateIdentifier parameter)
    GetRoleInstancesForContextFromDatabaseF -> pure $ unsafeCoerce $ getUnlinkedRoleInstances (EnumeratedRoleType parameter)

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
compileSequenceFunction (SQD dom (UnaryCombinator sequenceFunctionName) _ _ _) | isJust $ elemIndex sequenceFunctionName [CountF, MinimumF, MaximumF, AddF, FirstF]  = case sequenceFunctionName of
  CountF -> pure \things -> pure (show $ length things)
  MinimumF -> case dom of
    (VDOM RAN.PNumber _) -> pure smallestNumber
    (VDOM (RAN.PDuration _) _) -> pure smallestNumber 
    (VDOM RAN.PString _) -> pure \strings -> ArrayT $ pure $ (maybe [] singleton) (minimum strings)
    (VDOM RAN.PBool _) -> pure \bools -> ArrayT do
      bls <- traverse parseBool bools
      pure $ maybe [] (singleton <<< show) (minimum bools)
    _ -> throwError (error $ "compileSequenceFunction cannot handle domain '" <> show dom <> "' for 'minimum'.")
  MaximumF -> case dom of
    (VDOM RAN.PNumber _) -> pure largestNumber
    (VDOM (RAN.PDuration _) _) -> pure largestNumber
    (VDOM RAN.PString _) -> pure \strings -> ArrayT $ pure $ (maybe [] singleton) (maximum strings)
    _ -> throwError (error $ "compileSequenceFunction cannot handle domain '" <> show dom <> "' for 'maximum'.")
  AddF -> case dom of
    (VDOM RAN.PNumber _) -> pure addNumbers
    (VDOM (RAN.PDuration _) _) -> pure addNumbers
    (VDOM RAN.PString _) -> pure \strings -> ArrayT $ pure [foldl (\cumulator str -> cumulator <> str) "" strings]
    _ -> throwError (error $ "compileSequenceFunction cannot handle domain '" <> show dom <> "' for 'add'.")
  FirstF -> pure firstOfSequence

  op -> throwError (error $ "compileSequenceFunction cannot handle sequence function: '" <> show op <> "'.")
  where
    addNumbers :: Array String -> MonadPerspectivesQuery String
    addNumbers numbers = ArrayT $ do
      (nrs :: Array Number) <- traverse parseNumber numbers
      pure $ [show (foldl (\(cumulator :: Number) (nr :: Number) -> cumulator + nr) 1.0 nrs)]
    smallestNumber :: Array String -> MonadPerspectivesQuery String
    smallestNumber numbers = ArrayT do
      nrs <- traverse parseNumber numbers
      pure $ maybe [] (singleton <<< show) (minimum nrs)
    largestNumber :: Array String -> MonadPerspectivesQuery String
    largestNumber numbers = ArrayT do
      nrs <- traverse parseNumber numbers
      pure $ maybe [] (singleton <<< show) (maximum nrs)
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
      RAN.PString -> pure [show $ compareFun a' b']      
      RAN.PNumber -> singleton <<< show <$> (compareFun <$> (parseNumber a') <*> (parseNumber b'))
      RAN.PBool ->  singleton <<< show <$> (compareFun <$> parseBool a' <*> parseBool b')
      RAN.PDate -> singleton <<< show <$> (compareFun <$> (string2Date a') <*> string2Date b')
      RAN.PTime ->  singleton <<< show <$> (compareFun <$> (string2Time a') <*> string2Time b')
      RAN.PDateTime ->  singleton <<< show <$> (compareFun <$> (string2DateTime a') <*> string2DateTime b')
      -- Compare email addresses as strings.
      RAN.PEmail -> pure [show $ compareFun a' b']
      RAN.PFile -> pure [show $ compareFun a' b']
      -- Compare durations as Numbers. NOTE that we can only compare exactly equal Duration_ s!
      -- E.g. Days with Days, but not Days with Minutes.
      RAN.PDuration _ -> singleton <<< show <$> (compareFun <$> (parseNumber a') <*> (parseNumber b'))
      RAN.PMarkDown -> pure [show $ compareFun a' b']
    _, _ -> pure []
  where
    compareFun :: forall a. Ord a => a -> a -> Boolean
    compareFun = unsafePartial case f of
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
  r <- performNumericOperation' g ran f as bs
  pure r

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
        VDOM RAN.PNumber _ -> (\(x :: Number) -> [show (- x)]) <$> parseNumber b1
        otherwise -> pure []
      otherwise -> pure []
    Just a1, Nothing -> case g of
      AddF -> pure [a1]
      SubtractF -> pure [a1]
      otherwise -> pure []
    _, _ -> pure []

-- Notice that for PDuration, both operands have the same duration type!
mapNumericOperator :: Partial => FunctionName -> Domain -> (String -> String ~~> String)
mapNumericOperator AddF (VDOM RAN.PNumber _) = wrapNumericOperator (+)
mapNumericOperator AddF (VDOM RAN.PString _) = \s1 s2 -> pure (s1 <> s2)
mapNumericOperator AddF (VDOM (RAN.PDuration duration) _) = wrapNumericOperator (+)

-- TODO: vervang wrapNumericOperator door iets dat werkt op de respectievelijke tijd representaties.
mapNumericOperator AddF (VDOM RAN.PDate _) = wrapNumericOperator (+)
mapNumericOperator AddF (VDOM RAN.PTime _) = wrapNumericOperator (+)
mapNumericOperator AddF (VDOM RAN.PDateTime _) = wrapNumericOperator (+)

mapNumericOperator SubtractF (VDOM RAN.PNumber _) = wrapNumericOperator (-)
mapNumericOperator SubtractF (VDOM RAN.PString _) = \s1 s2 -> case (STRING.Pattern s1) `STRING.stripSuffix` s2 of
  Nothing -> pure s1
  Just r -> pure r
mapNumericOperator SubtractF (VDOM (RAN.PDuration duration) _) = wrapNumericOperator (-)

-- TODO: vervang wrapNumericOperator door iets dat werkt op de respectievelijke tijd representaties.
mapNumericOperator SubtractF (VDOM RAN.PDate _) = wrapNumericOperator (-)
mapNumericOperator SubtractF (VDOM RAN.PTime _) = wrapNumericOperator (-)
mapNumericOperator SubtractF (VDOM RAN.PDateTime _) = wrapNumericOperator (-)

mapNumericOperator DivideF (VDOM RAN.PNumber _) = wrapNumericOperator (/)
mapNumericOperator DivideF (VDOM (RAN.PDuration duration) _) = wrapNumericOperator (/)
mapNumericOperator MultiplyF (VDOM RAN.PNumber _) = wrapNumericOperator (*)
mapNumericOperator MultiplyF (VDOM (RAN.PDuration duration) _) = wrapNumericOperator (/)

-- Notice that the the compiler has made sure that the first operand is a PDateTime, PDate or Ptime while the second is a PDuration.
-- Furthermore, we can only add or subtract durations.
-- PDate and PDateTime are represented as Instants.
-- PTime is represented as milliseconds.
-- In all cases, we can just add or subtract the duration representation from the time or date representation.
-- Note that we might convert to DateTime, Date and Time and then adjust with a Duration and finally convert back to string via instant and milliseconds.
-- But this implementation in terms of milliseconds is ok as well.
-- NOTICE that we do not allow a Month duration.
mapDurationOperator_ :: Partial => (Number -> Number -> Number) -> Domain -> (String -> String ~~> String)
mapDurationOperator_ op (VDOM (RAN.PDuration duration) _) = case duration of 
  Year_ -> \p q -> show <$> (op <$> (parseNumber p) <*> ((mul millisecondsPerYear) <$> (parseNumber q)))
  Week_ -> \p q -> show <$> (op <$> (parseNumber p) <*> ((mul millisecondsPerWeek) <$> (parseNumber q)))
  Day_ -> \p q -> show <$> (op <$> (parseNumber p) <*> ((mul millisecondsPerDay) <$> (parseNumber q)))
  Hour_ -> \p q -> show <$> (op <$> (parseNumber p) <*> ((mul millisecondsPerHour) <$> (parseNumber q)))
  Minute_ -> \p q -> show <$> (op <$> (parseNumber p) <*> ((mul millisecondsPerMinute) <$> (parseNumber q)))
  Second_ -> \p q -> show <$> (op <$> (parseNumber p) <*> ((mul 1000.0) <$> (parseNumber q)))
  MilliSecond_ -> \p q -> show <$> (op <$> (parseNumber p) <*> (parseNumber q))

mapDurationOperator :: Partial => FunctionName -> Domain -> (String -> String ~~> String)
mapDurationOperator AddF = mapDurationOperator_ add
mapDurationOperator SubtractF = mapDurationOperator_ sub

millisecondsPerMinute :: Number
millisecondsPerMinute = 60000.0
millisecondsPerHour :: Number
millisecondsPerHour = 60.0 * millisecondsPerMinute
millisecondsPerDay :: Number
millisecondsPerDay = 24.0 * millisecondsPerHour
millisecondsPerWeek :: Number
millisecondsPerWeek = 7.0 * millisecondsPerDay
-- NOTE: this is an approximation!
millisecondsPerMonth :: Number
millisecondsPerMonth = 365.0 / 12.0 * millisecondsPerDay
millisecondsPerYear :: Number
millisecondsPerYear = 365.0 * millisecondsPerDay

wrapNumericOperator :: (Number -> Number -> Number) -> (String -> String ~~> String)
wrapNumericOperator g p q = show <$> (g <$> (parseNumber p) <*> (parseNumber q))
-- wrapNumericOperator g p q = do
--   q' <- parseNumber q
--   p' <- parseNumber p
--   r <- pure $ g p' q'
--   pure $ show r

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
getPropertyFunction = expandDefaultNamespaces >=> getPropertyType >=> getterFromPropertyType

pushCurrentObject :: (String ~~> String) -> (String ~~> String)
pushCurrentObject f roleId = do
  -- save contextId in it under the name currentobject
  lift $ lift (addBinding "currentobject" [roleId])
  -- apply f to roleId
  f roleId

-- | From a PropertyType, retrieve or construct a function to get values for that Property from a Role instance.
-- | Caches the result.
-- | The resulting function only works as expected when applied to an instance of the rol on which the property is defined!
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
    getter <- unsafeCoerce $ PC.calculation p >>= compileFunction >>= pure <<< (withFrame_ <<< pushCurrentObject)
    void $ pure $ propertyGetterCacheInsert id (unsafeCoerce getter) functional mandatory
    pure (unsafeCoerce getter)
  Just g -> pure g

getPropertyValues :: PropertyType -> (RoleInstance ~~> Value)
getPropertyValues pt rid = (lift $ lift $ getterFromPropertyType pt) >>= \getter -> getter rid

getHiddenFunction :: QueryFunctionDescription -> MP HiddenFunction
getHiddenFunction = unsafeCoerce $ compileFunction

-- | From a string that represents either a Calculated or an Enumerated property,
-- | for a given abstract datatype of roles, retrieve the values from a role instance.
getDynamicPropertyGetter :: String -> ADT EnumeratedRoleType -> MP (RoleInstance ~~> Value)
getDynamicPropertyGetter p adt = do
  (pt :: PropertyType) <- getPropertyType p
  getDynamicPropertyGetter_ pt adt  

getDynamicPropertyGetter_ :: PropertyType -> ADT EnumeratedRoleType -> MP (RoleInstance ~~> Value)
getDynamicPropertyGetter_ pt adt = do
  case pt of 
    CP _ -> do 
      -- We MUST explore whether the property is defined locally for the current ADT EnumeratedRoleType.
      -- This is because the calculation starts at that role type.
      allProps <- allLocallyRepresentedProperties adt
      -- Special case for the 'property of last resort' that is inserted in serialised perspectives for roles without properties.
      if (isJust $ elemIndex pt allProps) || pt == (CP $ CalculatedPropertyType roleWithId)
        then getterFromPropertyType pt
        else pure (binding >=> f)
    ENP eprop -> pure $ getPropertyFromTelescope eprop
  where
    f :: RoleInstance ~~> Value
    f bnd = do
      (bndType :: EnumeratedRoleType) <- lift $ lift $ roleType_ bnd
      getter <- lift $ lift $ getDynamicPropertyGetter_ pt (ST bndType)
      getter bnd

-- | Get a property on a chain of EnumeratedRole instances that are filled by each other.
-- | Steps through the chain until the bottom is reached.
-- | For every level in the chain that could have a value for the property, we push an assumption - until we find a value.
-- | The function [getDynamicPropertyGetter](Perspectives.Query.UnsafeCompiler.html#t:getDynamicPropertyGetter)
-- | will compute the Values for a PropertyType (Enumerated or Calculated).
getPropertyFromTelescope :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getPropertyFromTelescope pn r = ArrayT $ (lift $ try $ getPerspectRol r) >>=
  handlePerspectRolError' "getPropertyFromTelescope" []
    \((PerspectRol{properties, binding: bnd, pspType:roleType}) :: PerspectRol) -> do
      allProps <- lift $ allLocallyRepresentedProperties (ST roleType)
      if isJust $ elemIndex (ENP pn) allProps
        -- We can use either the alias name or the original name for the assumption, as we use both when looking up correlation identifiers.
        then do 
          case (OBJ.lookup (unwrap pn) properties) of
            -- Even though the property is represented on this role, we choose to search the rest of the chain.
            Nothing -> getItFromTheFiller bnd
            Just p -> do 
              tell $ ArrayWithoutDoubles [Property r pn]
              pure p
        else do
          -- We must take aliases of the actual role type into account. An alias is another name for an Aspect Property that 
          -- has been added to the role.
          aliases <- catchError (lift $ propertyAliases roleType)
            \e -> pure OBJ.empty
          case OBJ.lookup (unwrap pn) aliases of
            Just aliasPropertyName -> do 
              -- Property values may have been stored under their alias name on the role instance.
              -- Check whether we have a value.
              case (OBJ.lookup (unwrap aliasPropertyName) properties) of
                -- Even though the property is represented on this role, we choose to search the rest of the chain.
                Nothing -> getItFromTheFiller bnd
                Just p -> do 
                  tell $ ArrayWithoutDoubles [Property r pn]
                  pure p
            -- The property is not represented on this role; neither directly, nor as alias. Continue 
            -- the search on the binding after adding a Filler Assumption.
            Nothing -> getItFromTheFiller bnd
  where
    getItFromTheFiller :: Maybe RoleInstance -> AssumptionTracking (Array Value)
    getItFromTheFiller bnd = do 
      tell $ ArrayWithoutDoubles [Filler r]
      case bnd of
        -- No binding yet; we're done.
        Nothing -> do 
          tell $ ArrayWithoutDoubles [Property r pn]
          pure []
        -- Search further with the original name. The alias was defined just for this role type.
        Just b -> do 
          runArrayT $ getPropertyFromTelescope pn b


-- | Builds, in compile time, a composition of `binding` and `getProperty` that will retrieve the value from the first 
-- | instance on the chain on which that property has been declared. 
-- NOTE that the implementation critically depends on the actual instance. That is not correct. It should take 
-- the roleType(s) from the ADT.
getDynamicEnumeratedPropertyGetter :: EnumeratedPropertyType -> ADT EnumeratedRoleType -> RoleInstance ~~> Value
getDynamicEnumeratedPropertyGetter eprop adt rid = do
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
      getProperty destination rid
    Nothing -> do
      allProps <- lift $ lift $ allLocallyRepresentedProperties adt
      if isJust $ elemIndex (ENP eprop) allProps
        then do 
          getProperty eprop rid
        else (binding >=> f) rid
  where
    f :: RoleInstance ~~> Value
    f bnd = do
      (bndType :: EnumeratedRoleType) <- lift $ lift $ roleType_ bnd
      getDynamicEnumeratedPropertyGetter eprop (ST bndType) bnd

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

getFillerTypeRecursively :: ADT RoleInContext -> RoleInstance ~~> RoleInstance
getFillerTypeRecursively adt r = do 
  adtDnf <- lift $ lift $ (toConjunctiveNormalForm_ adt)
  ArrayT $ (lift $ try $ getPerspectRol r) >>=
    handlePerspectRolError' "binding" [] (depthFirst adtDnf)
  where
  depthFirst :: CNF RoleInContext -> PerspectRol -> AssumptionTracking (Array RoleInstance)
  depthFirst adtDnf role = do
    tell $ ArrayWithoutDoubles [Filler $ rol_id role]
    case rol_binding role of
        Nothing -> pure []
        Just b -> do 
          bRole <- lift $ getPerspectRol b
          roleDnf <- lift (getEnumeratedRole (rol_pspType bRole) >>= pure <<< _.completeType <<< unwrap)
          -- adtDnf -> roleDnf
          -- e.g. adtDnf is an aspect of roleDnf or fills it.
          if roleDnf `equalsOrSpecialises_` adtDnf
            then pure [b]
            else depthFirst adtDnf bRole

-- | Just the fillers that come from instances of a particular ContextType.
bindingInContext :: ContextType -> ADT RoleInContext -> RoleInstance ~~> RoleInstance
bindingInContext cType adt r = ArrayT do
  (fillers :: Array RoleInstance) <- runArrayT $ getFillerTypeRecursively adt r
  filterA
    (\filler -> (lift $ try $ getPerspectRol filler) >>=
      handlePerspectRolError' "bindingInContext" false
        \(role :: PerspectRol) -> do
          fillerContextType <-  lift ((rol_context role) ##>> contextType)
          pure (eq cType fillerContextType))
    fillers


-- | We look for a public role in the (type of the) ContextInstance.
-- | We then arbitrarily take the publicUrl of the first public role to have one.
-- | Notice that we return the url to which resources are saved; not the url of a particular resource
getPublicUrl :: ContextInstance -> MonadPerspectives (Maybe String)
getPublicUrl ctxt = do
  ctype <- contextType_ ctxt
  publicRoles <- ctype ###= publicUserRole
  murls <- for publicRoles getUrl
  case head $ catMaybes murls of 
    Nothing -> pure Nothing
    Just url -> pure $ Just url
  where
  getUrl :: RoleType -> MonadPerspectives (Maybe String)
  getUrl (CR _) = pure Nothing
  getUrl (ENR r) = do
    EnumeratedRole {publicUrl} <- getEnumeratedRole r
    case publicUrl of 
      Just (Q qfd) -> do
        urlComputer <- context2propertyValue qfd
        murl <- ctxt ##> urlComputer
        case murl of 
          Just (Value url) -> pure $ Just $ ensureTerminalSlash url
          Nothing ->  throwError (error $ "Cannot compute a URL to publish to for this user role type and context instance: " <> show r <> " ('" <> show ctxt <> "')")
      _ -> pure Nothing

ensureTerminalSlash :: String -> String
ensureTerminalSlash s = case STRING.lastIndexOf (STRING.Pattern "/") s of
  Nothing -> s <> "/"
  Just i -> if STRING.length s == i + 1
    then s
    else s <> "/"