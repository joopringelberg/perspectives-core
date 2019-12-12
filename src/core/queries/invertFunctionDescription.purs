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

import Control.Monad.Writer (WriterT, tell)
import Data.Array (cons, unsnoc)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (endsWithSegments)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, domain, functional, mandatory, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Prelude (class Monad, class Monoid, class Semigroup, bind, discard, pure, unit, void, ($), (<$>), (<>))

-- | The `path` parameter describes a query to the root of the original query.
invertFunctionDescription :: forall m. Monad m => Partial => QueryFunctionDescription -> Array QueryFunctionDescription -> Boolean -> WriterT (Array QueryFunctionDescription) m (Maybe QueryFunctionDescription)

invertFunctionDescription (SQD dom (Constant _ _) ran _ _) path isMainPath = pure Nothing

invertFunctionDescription (SQD dom f ran _ _) path isMainPath = do
  minvertedF <- pure $ invertFunction dom f ran
  case minvertedF of
    Nothing -> pure Nothing
    Just invertedF -> pure $ Just (SQD ran invertedF dom (inversionIsFunctional f) (inversionIsMandatory f))

invertFunctionDescription (UQD dom f qfd1 ran _ _) path isMainPath = invertFunctionDescription qfd1 path isMainPath

-- The right step is a Simple Query or has a Value domain. We've arrived at the end of the composition.
-- Invert both steps, add to the path, apply foldr compose to it.
-- In any composition, if the left step itself is a composition, we deal with a sub-expression in parentheses.
-- This breaks the default right-association. On the end of such a path, we should not `tell` the result.
-- If the left step is a filter, we should provide it with the path.
invertFunctionDescription (BQD dom (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) path isMainPath | terminalStep qfd2 = do
  -- minvertedLeft <- invertFunctionDescription qfd1 [] (if isComposition qfd1 then false else isMainPath)
  minvertedLeft <- if isFilter qfd1
    then invertFunctionDescription qfd1 path (if isComposition qfd1 then false else isMainPath)
    else invertFunctionDescription qfd1 [] (if isComposition qfd1 then false else isMainPath)
  case minvertedLeft of
    -- Nothing means a constant expression. There is no valid composition of a constant and a step.
    Nothing -> pure Nothing
    Just invertedLeft -> do
      void $ pushQuery (cons invertedLeft path) isMainPath
      minvertedRight <- invertFunctionDescription qfd2 [] isMainPath
      case minvertedRight of
        -- Nothing means a constant expression. The compiler reduces the composition to that constant. There is no path.
        Nothing -> pure Nothing
        Just invertedRight -> pushQuery (cons invertedRight $ cons invertedLeft path) isMainPath

-- The non-terminal step of Compose.
-- If the left step is itself a composition, it represents a sub-path that is left-associated rather than the
-- default right-association applied to paths. The recursive call is made with isMainPath = false.
-- Push a query based on the path augmented with the left step.
invertFunctionDescription (BQD dom (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) path isMainPath = do
  -- minvertedLeft <- invertFunctionDescription qfd1 [] (if isComposition qfd1 then false else isMainPath)
  if isFilter qfd1
    then do
      minvertedLeft <- invertFunctionDescription qfd1 path (if isComposition qfd1 then false else isMainPath)
      case minvertedLeft of
        Nothing -> pure Nothing
        Just invertedLeft -> do
          void $ pushQuery path isMainPath
          invertFunctionDescription qfd2 [invertedLeft] isMainPath
    else do
      minvertedLeft <- invertFunctionDescription qfd1 [] (if isComposition qfd1 then false else isMainPath)
      case minvertedLeft of
        Nothing -> pure Nothing
        Just invertedLeft -> do
          void $ pushQuery (cons invertedLeft path) isMainPath
          invertFunctionDescription qfd2 (cons invertedLeft path) isMainPath

-- At a filter expression, the path splits in two.
-- One path leads through the source.
-- The other path leads through the source **and then** through the criterium.
-- We compose a path from source and criterium and invert that.
-- If the source is the terminal of a path, we should compose an inverted query from it and the path and `tell` it.
-- Het probleem is dat bij de aanroep het pad leeg is, omdat het filter in de linkertak van een compose zit.
-- filter geeft het pad van de source terug.
invertFunctionDescription (BQD dom (BinaryCombinator FilterF) source criterium ran _ _) path isMainPath = do
  void $ invertFunctionDescription (compose source criterium) path isMainPath
  if terminalStep source
    then do
      minvertedSource <- invertFunctionDescription source path isMainPath
      case minvertedSource of
        Nothing -> pure Nothing
        Just invertedSource -> pushQuery (cons invertedSource path) isMainPath
    else invertFunctionDescription source path isMainPath

-- We can ignore all other binary functions. Just trace paths back to the root from both branches.
-- The results will not be used.
invertFunctionDescription (BQD dom (BinaryCombinator f) qfd1 qfd2 ran _ _) path isMainPath = do
  void $ invertFunctionDescription qfd1 path isMainPath
  invertFunctionDescription qfd2 path isMainPath

pushQuery :: forall m. Monad m => Array QueryFunctionDescription -> Boolean -> WriterT (Array QueryFunctionDescription) m (Maybe QueryFunctionDescription)
pushQuery path' isMainPath = do
  {init, last} <- pure $ unsafePartial $ fromJust $ unsnoc path'
  result <- pure $ foldr compose last init
  if isMainPath
    then tell [result]
    else pure unit
  pure $ Just result


invertFunction :: Domain -> QueryFunction -> Range -> Maybe QueryFunction
invertFunction dom qf ran = case qf of
  DataTypeGetter f -> case f of
    -- If we have the external role, use `DataTypeGetter ExternalRoleF`. That is, if the range is a Context.
    ContextF -> if isExternalRole dom
      then Just $ DataTypeGetter ExternalRoleF
      else Just $ RolGetter $ ENR (unsafePartial $ domain2RoleType dom)
    BindingF -> Just $ DataTypeGetterWithParameter GetRoleBindersF (unwrap $ unsafePartial $ domain2RoleType dom)
    ExternalRoleF -> Just $ DataTypeGetter ContextF
    IdentityF -> Just $ DataTypeGetter IdentityF

    -- An expression like `step >>= sum` is compiled as an SQD with DataTypeGetter as constructor for QueryFunction.
    -- These function descriptions have the same domain as range. Invariably, the domain will be a VDOM, as the
    -- sequence functions only apply to Values.
    -- In the compiled AffectedContextQuery we wish to ignore a step like this. We accomplish that by constructing
    -- a Value2Role QueryFunction.
    CountF -> Just $ Value2Role
    MinimumF -> Just $ Value2Role
    MaximumF -> Just $ Value2Role
    AddF -> Just $ Value2Role
    MultiplyF -> Just $ Value2Role

    _ -> Nothing

  DataTypeGetterWithParameter f _ -> case f of
    GetRoleBindersF -> Just $ DataTypeGetter BindingF
    -- A lot of cases will never be seen in a regular query.
    _ -> Nothing

  RolGetter rt -> Just $ DataTypeGetter ContextF
  PropertyGetter pt -> Just Value2Role
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


terminalStep :: QueryFunctionDescription -> Boolean
terminalStep (SQD _ _ _ _ _) = true
terminalStep qfd = case domain qfd of
  CDOM _ -> false
  RDOM _ -> false
  VDOM _ _ -> true

compose :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
compose f1 f2 = BQD
  (domain f1)
  (BinaryCombinator ComposeF)
  f1
  f2
  (range f2)
  (and (functional f1)(functional f2))
  (and (mandatory f1)(mandatory f2))

isComposition :: QueryFunctionDescription -> Boolean
isComposition (BQD _ (BinaryCombinator ComposeF) _ _ _ _ _) = true
isComposition _ = false

isFilter :: QueryFunctionDescription -> Boolean
isFilter (BQD _ (BinaryCombinator FilterF) _ _ _ _ _) = true
isFilter _ = false

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
