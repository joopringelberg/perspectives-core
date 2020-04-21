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

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, cons, fold, foldr, last, reverse, uncons)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.Identifiers (endsWithSegments)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), Range, domain, functional, mandatory, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (contextOfADT, getCalculation, getRole)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Utilities (prettyPrint)
import Prelude (class Monoid, class Semigroup, append, bind, mempty, pure, ($), (<$>), (<*>), (<>), (>=>), (>>=), (<<<), show)

-- | Compute from the description of a query function a series of paths.
-- | Consider the description to be a tree, then the paths run from the leaves to the root.
-- | We use this function to compute contexts whose rules should be re-run when processing a Delta.MonadPerspectives
invertFunctionDescription :: QueryFunctionDescription -> MP (Array QueryFunctionDescription)
invertFunctionDescription qfd = do
  paths <- invertFunctionDescription_ qfd
  traverse invert (allPaths paths) >>= pure <<< catMaybes
  where
    invert :: Array QueryFunctionDescription -> MonadPerspectives (Maybe QueryFunctionDescription)
    invert path = do
      mPath <- addContextToPropertyQuery path
      case mPath of
        Nothing -> pure Nothing
        Just augmentedPath -> case uncons augmentedPath of
          Nothing -> pure Nothing
          -- Now invert the steps.
          Just {head, tail} -> pure $ Just $ foldr compose head (reverse tail)

    -- | If the array of steps ends with Value2Role, and does not start with context, prepend context before inverting.
    addContextToPropertyQuery :: Path -> MonadPerspectives (Maybe Path)
    addContextToPropertyQuery path = case last path of
      Nothing -> pure Nothing
      Just (SQD _ (Value2Role _) _ _ _) -> case uncons path of
        Nothing -> pure Nothing
        Just {head: (SQD _ (DataTypeGetter ContextF) _ _ _), tail} -> pure $ Just path
        Just {head, tail} -> case domain head of
          (RDOM adt) -> do
            -- The first step has a domain that is a role. Compute the context of that role.
            (contextAdt :: ADT ContextType) <- contextOfADT adt
            pure $ Just $ cons (SQD (domain head) (DataTypeGetter ContextF) (CDOM contextAdt) True True) path
          otherwise -> pure $ Just path
      otherwise -> pure $ Just path

-- | Invert each step, but keep the same order of steps.
invertFunctionDescription_ :: QueryFunctionDescription -> MP Paths

invertFunctionDescription_ (SQD dom (Constant _ _) ran _ _) = pure mempty

invertFunctionDescription_ (SQD dom (RolGetter rt) ran _ _) = case rt of
  ENR _ -> pure $ mkPaths (SQD ran (DataTypeGetter ContextF) dom (inversionIsFunctional (RolGetter rt)) (inversionIsMandatory (RolGetter rt)))
  CR r -> do
    calc <- (getRole >=> getCalculation) rt
    invertFunctionDescription_ calc

invertFunctionDescription_ (SQD dom (PropertyGetter (CP prop)) ran _ _) = do
  calc <- (getCalculatedProperty >=> calculation) prop
  invertFunctionDescription_ calc

-- For now, ignore externe functions.
-- TODO. Draai de argumenten om.
invertFunctionDescription_ (MQD dom (ExternalCorePropertyGetter functionName) args ran _ _) = traverse invertCalculation args >>= pure <<< fold

invertFunctionDescription_ (MQD dom (ExternalCoreRoleGetter functionName) args ran _ _) =  traverse invertCalculation args >>= pure <<< fold

invertFunctionDescription_ (SQD dom (DataTypeGetter CountF) ran _ _) = pure mempty

invertFunctionDescription_ (SQD dom f ran _ _) = do
  minvertedF <- pure $ invertFunction dom f ran
  case minvertedF of
    Nothing -> pure mempty
    Just invertedF -> pure $ mkPaths (SQD ran invertedF dom (inversionIsFunctional f) (inversionIsMandatory f))

invertFunctionDescription_ (UQD dom f qfd1 ran _ _) = invertFunctionDescription_ qfd1

invertFunctionDescription_ (BQD dom (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _)= append <$> invertFunctionDescription_ qfd1 <*> invertFunctionDescription_ qfd2

-- At a filter expression, the path splits in two.
-- One path leads through the source.
-- The other path leads through the source **and then** through the criterium.
-- We compose a path from source and criterium and invert that.
invertFunctionDescription_ (BQD dom (BinaryCombinator FilterF) source criterium ran _ _) = filterPaths <$> (invertFunctionDescription_ source) <*> (invertFunctionDescription_ criterium)

-- For all other functions, we just sum their paths.
invertFunctionDescription_ (BQD dom (BinaryCombinator f) qfd1 qfd2 ran _ _) = sumPaths <$> (invertFunctionDescription_ qfd1) <*> (invertFunctionDescription_ qfd2)

-- catchall
invertFunctionDescription_ qfd = throwError (error $ "Missing case in invertFunctionDescription_ for: " <> prettyPrint qfd)

invertCalculation :: Calculation -> MP Paths
invertCalculation (Q qfd) = invertFunctionDescription_ qfd
invertCalculation (S s) = throwError (error $ "invertCalculation: Step should have been compiled: " <> show s)

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
  -- Variable lookup implies variable binding elsewhere. We've traced the path back in the binding, so we ignore it here.
  VariableLookup _ -> Nothing

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
  append = sumPaths

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
