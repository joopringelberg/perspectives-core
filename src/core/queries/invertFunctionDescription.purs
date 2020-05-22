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

module Perspectives.Query.Inversion where

import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (endsWithSegments)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, domain, functional, mandatory, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType, RoleType(..))
import Prelude (class Monoid, class Semigroup, ($), (<$>), (<>))

-- | For each type of function that appears as a single step in a query, we compute the inverse step.
invertFunction :: Domain -> QueryFunction -> Range -> Maybe QueryFunction
invertFunction dom qf ran = case qf of
  DataTypeGetter f -> case f of
    -- If we have the external role, use `DataTypeGetter ExternalRoleF`. That is, if the range is a Context.
    ContextF -> if isExternalRole dom
      then Just $ DataTypeGetter ExternalRoleF
      else Just $ RolGetter $ ENR (unsafePartial $ domain2RoleType dom)
    BindingF -> case ran of
      (RDOM EMPTY) -> Nothing
      otherwise -> Just $ DataTypeGetterWithParameter GetRoleBindersF (unwrap $ unsafePartial $ domain2RoleType dom)
    ExternalRoleF -> Just $ DataTypeGetter ContextF
    IdentityF -> Just $ DataTypeGetter IdentityF

    -- An expression like `step >>= sum` is compiled as an SQD with DataTypeGetter as constructor for QueryFunction.
    -- These function descriptions have the same domain as range. In general, the domain will be a VDOM, as the
    -- sequence functions apply to Values, EXCEPT for CountF. We can count anything.
    -- In the compiled AffectedContextQuery we wish to ignore a step like this. We accomplish that by constructing
    -- a Value2Role QueryFunction.
    MinimumF -> Just $ Value2Role (unsafePartial $ domain2PropertyType dom)
    MaximumF -> Just $ Value2Role (unsafePartial $ domain2PropertyType dom)
    AddF -> Just $ Value2Role (unsafePartial $ domain2PropertyType dom)
    MultiplyF -> Just $ Value2Role (unsafePartial $ domain2PropertyType dom)

    _ -> Nothing

  DataTypeGetterWithParameter f _ -> case f of
    GetRoleBindersF -> Just $ DataTypeGetter BindingF
    -- A lot of cases will never be seen in a regular query.
    _ -> Nothing

  PropertyGetter pt -> Just $ Value2Role pt

  -- Catchall clause
  _ -> Nothing

  where
    -- NOTE: this is a shortcut that depends on a naming convention. It allows us to **not** make this function in MP.
    isExternalRole :: Domain -> Boolean
    isExternalRole (RDOM (ST (EnumeratedRoleType n))) = n `endsWithSegments` "External"
    isExternalRole _ = false

inversionIsFunctional :: QueryFunction -> ThreeValuedLogic
inversionIsFunctional f = Unknown

inversionIsMandatory :: QueryFunction -> ThreeValuedLogic
inversionIsMandatory f = Unknown

domain2RoleType :: Partial => Domain -> EnumeratedRoleType
domain2RoleType (RDOM (ST e)) = e

domain2PropertyType :: Partial => Domain -> PropertyType
domain2PropertyType (VDOM _ (Just pt)) = pt

-- | Create a QueryFunctionDescription with composition.
compose :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
compose f1 f2 = BQD
  (domain f1)
  (BinaryCombinator ComposeF)
  f1
  f2
  (range f2)
  (and (functional f1)(functional f2))
  (and (mandatory f1)(mandatory f2))

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
