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

module Perspectives.Query.Inversion where

import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, head)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (endsWithSegments)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription, Range, RoleInContext(..), roleInContext2Role)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..), isFunctionalFunction, isMandatoryFunction)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, PropertyType, RoleType(..))
import Perspectives.Types.ObjectGetters (isRelational_)
import Prelude (class Monoid, class Semigroup, bind, pure, ($), (<$>), (<>))

-- | For each type of function that appears as a single step in a query, we compute the inverse step.
invertFunction :: Domain -> QueryFunction -> Range -> PhaseThree (Maybe QueryFunction)
invertFunction dom qf ran = case qf of
  DataTypeGetter f -> case f of
    -- If we have the external role, use `DataTypeGetter ExternalRoleF`. That is, if the range is a Context.
    ContextF -> if isExternalRole dom
      then pure $ Just $ DataTypeGetter ExternalRoleF 
      else do 
        et <- unsafePartial $ domain2RoleType dom
        -- If the role is relational, produce another getter!
        relationalP <- lift $ lift $ isRelational_ et
        if relationalP
          then pure $ Just $ DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF (unwrap et)
          else pure $ Just $ RolGetter $ ENR et
    -- FillerF is the `filledBy` step. So we have filled `filledBy` filler.
    -- Its inversion is filler `fills` filled, or: filler FilledF filled.
    -- We must qualify FilledF with the type that is filled.
    -- That is the domain, here!
    FillerF -> case dom of
      (RDOM (ST (RoleInContext{context,role}))) -> pure $ Just $ FilledF role context
      (RDOM (UET (RoleInContext{context,role}))) -> pure $ Just $ FilledF role context
      otherwise -> pure $ Nothing
    ExternalRoleF -> pure $ Just $ DataTypeGetter ContextF
    -- Identity steps add nothing to the query and can be left out.
    IdentityF -> pure $ Nothing

    -- An expression like `step >>= sum` is compiled as an SQD with DataTypeGetter as constructor for QueryFunction.
    -- These function descriptions have the same domain as range. In general, the domain will be a VDOM, as the
    -- sequence functions apply to Values, EXCEPT for CountF. We can count anything.
    -- In the compiled AffectedContextQuery we wish to ignore a step like this. We accomplish that by constructing
    -- a Value2Role QueryFunction.
    MinimumF -> pure $ Just $ Value2Role (unsafePartial $ domain2PropertyType dom)
    MaximumF -> pure $ Just $ Value2Role (unsafePartial $ domain2PropertyType dom)
    AddF -> pure $ Just $ Value2Role (unsafePartial $ domain2PropertyType dom)
    MultiplyF -> pure $ Just $ Value2Role (unsafePartial $ domain2PropertyType dom)

    _ -> pure $ Nothing

  DataTypeGetterWithParameter f _ -> case f of
    GetRoleInstancesForContextFromDatabaseF -> pure $ Just $ DataTypeGetter ContextF
    SpecialisesRoleTypeF -> pure $ Nothing
    FillerF -> case dom of
      (RDOM (ST (RoleInContext{context,role}))) -> pure $ Just $ FilledF role context
      otherwise -> pure $ Nothing
    -- A lot of cases will never be seen in a regular query.
    _ -> pure $ Nothing

  FilledF _ _ -> pure $ Just $ DataTypeGetter FillerF

  PropertyGetter pt -> pure $ Just $ Value2Role pt

  -- ExternalCoreContextGetter ct

  -- Catchall clause
  _ -> pure $ Nothing

  where
    -- NOTE: this is a shortcut that depends on a naming convention. It allows us to **not** make this function in MP.
    isExternalRole :: Domain -> Boolean
    isExternalRole (RDOM (ST (RoleInContext {role}))) = unwrap role `endsWithSegments` "External"
    isExternalRole (RDOM (UET (RoleInContext {role}))) = unwrap role `endsWithSegments` "External"
    isExternalRole _ = false

-- | Checks whether the QueryFunction returns just a single result.
queryFunctionIsFunctional :: QueryFunction -> ThreeValuedLogic
queryFunctionIsFunctional qf = case qf of
  DataTypeGetter f -> isFunctionalFunction f
  DataTypeGetterWithParameter f _ -> isFunctionalFunction f
  -- NOTE: we can do better by looking up the property,
  -- but that requires making this a monadic function.
  PropertyGetter _ -> Unknown
  Value2Role _ -> True
  -- NOTE: we can do better by looking up the role.
  RolGetter _ -> Unknown
  ExternalCoreRoleGetter _ -> Unknown
  ExternalCorePropertyGetter _ -> Unknown
  ExternalCoreContextGetter _ -> Unknown
  ForeignRoleGetter _ -> Unknown
  ForeignPropertyGetter _ -> Unknown
  VariableLookup _ -> Unknown
  BindVariable _ -> Unknown
  BindResultFromCreatingAssignment _ -> True
  AssignmentOperator _ -> Unknown
  WithFrame -> Unknown
  TypeGetter _ -> Unknown
  UnaryCombinator _ -> Unknown
  BinaryCombinator _ -> Unknown
  Constant _ _ -> True
  RoleIndividual _ -> True
  ContextIndividual _ -> True
  _ -> Unknown

-- | Checks whether the QueryFunction always returns a result.
queryFunctionIsMandatory :: QueryFunction -> ThreeValuedLogic
queryFunctionIsMandatory qf = case qf of
  DataTypeGetter f -> isMandatoryFunction f
  DataTypeGetterWithParameter f _ -> isMandatoryFunction f
  -- NOTE: we can do better by looking up the property,
  -- but that requires making this a monadic function.
  PropertyGetter _ -> Unknown
  Value2Role _ -> True
  -- NOTE: we can do better by looking up the role.
  RolGetter _ -> Unknown
  ExternalCoreRoleGetter _ -> Unknown
  ExternalCorePropertyGetter _ -> Unknown
  ExternalCoreContextGetter _ -> Unknown
  ForeignRoleGetter _ -> Unknown
  ForeignPropertyGetter _ -> Unknown
  VariableLookup _ -> Unknown
  BindVariable _ -> Unknown
  BindResultFromCreatingAssignment _ -> True
  AssignmentOperator _ -> Unknown
  WithFrame -> Unknown
  TypeGetter _ -> Unknown
  UnaryCombinator _ -> Unknown
  BinaryCombinator _ -> Unknown
  Constant _ _ -> True
  RoleIndividual _ -> True
  ContextIndividual _ -> True
  _ -> Unknown

-- TODO. #22 This function is a stub: it selects arbitrarily the first EnumeratedRoleType from SUMS and PRODUCTS
domain2RoleType :: Partial => Domain -> PhaseThree EnumeratedRoleType
-- domain2RoleType (RDOM (ST (RoleInContext {role}))) = pure role
domain2RoleType (RDOM adt) = fromJust <$> domain2RoleType' adt
  where
  domain2RoleType' :: Partial => ADT RoleInContext -> PhaseThree (Maybe EnumeratedRoleType)
  domain2RoleType' (ST a) = pure $ Just $ roleInContext2Role a
  domain2RoleType' (UET a) = pure $ Just $ roleInContext2Role a
  domain2RoleType' (SUM adts) = do
    (roles :: Array EnumeratedRoleType) <- catMaybes <$> traverse domain2RoleType' adts
    pure $ head roles
  domain2RoleType' (PROD adts) = do
    (roles :: Array EnumeratedRoleType) <- catMaybes <$> traverse domain2RoleType' adts
    pure $ head roles

domain2PropertyType :: Partial => Domain -> PropertyType
domain2PropertyType (VDOM _ (Just pt)) = pt

-- | Paths is the general representation of the result of invertFunction. It holds a main path (the first member)
-- | and an array of secondary paths.
data Paths = Paths Path (Array Path)

-- | A Path is a series of function descriptions.
type Path = Array QueryFunctionDescription

-- | Combine two Paths according to composition. The paths on the left precede those on the right.
composePaths :: Paths -> Paths -> Paths
composePaths (Paths mp1 subs1) (Paths mp2 subs2) = Paths (mp1 <> mp2) (subs1 <> (((<>) mp1) <$> subs2))

instance pathsSemiGroup :: Semigroup Paths where
  append = composePaths

instance pathsMonoid :: Monoid Paths where
  mempty = Paths [] []

mkPaths :: QueryFunctionDescription -> Paths
mkPaths q = Paths [q] []

-- | Combine two Paths according to filtering. The main Path of the first Paths argument is the source of the filter
-- | and represents the main Path. The second Paths argument represents the filter. All paths in this argument
-- | end up as secondary paths in the result.
filterPaths :: Paths -> Paths -> Paths
filterPaths p1@(Paths mp _) p2 = let
  Paths x subs = composePaths p1 p2
  in Paths mp (cons x subs)

-- | Add two Paths, where we choose the **second** main path to be the main
-- | path of the result. This is not completely arbitrary, for a Let* is compiled as a
-- | sequence of its bindings and its body. It is fitting that the body should be the main path.
sumPaths :: Paths -> Paths -> Paths
sumPaths (Paths mp1 subs1) (Paths mp2 subs2) = Paths mp2 (subs1 <> [mp1] <> subs2)

allPaths :: Paths -> Array Path
allPaths (Paths mp1 subs1) = cons mp1 subs1
