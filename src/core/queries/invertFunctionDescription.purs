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
import Perspectives.CoreTypes (MP)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, domain, functional, mandatory, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, RoleType(..))
import Prelude (bind, discard, pure, unit, void, ($))

-- | The `path` parameter describes a query to the root of the original query.
invertFunctionDescription :: Partial => QueryFunctionDescription -> Array QueryFunctionDescription -> Boolean -> WriterT (Array QueryFunctionDescription) MP (Maybe QueryFunctionDescription)

invertFunctionDescription (SQD dom (Constant _ _) ran _ _) path isMainPath = pure Nothing

invertFunctionDescription (SQD dom f ran _ _) path isMainPath = pure $ Just (SQD ran (invertFunction dom f ran) dom (inversionIsFunctional f) (inversionIsMandatory f))

invertFunctionDescription (UQD dom f qfd1 ran _ _) path isMainPath = invertFunctionDescription qfd1 path isMainPath

-- The right step is a Simple Query or has a Value domain. We've arrived at the end of the composition.
-- Invert both steps, add to the path, apply foldr compose to it.
invertFunctionDescription (BQD dom (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) path isMainPath | terminalStep qfd2 = do
  minvertedLeft <- invertFunctionDescription qfd1 [] false
  case minvertedLeft of
    -- Nothing means a constant expression. There is no valid composition of a constant and a step.
    Nothing -> pure Nothing
    Just invertedLeft -> do
      minvertedRight <- invertFunctionDescription qfd2 [] isMainPath
      case minvertedRight of
        -- Nothing means a constant expression. The compiler reduces the composition to that constant. There is no path.
        Nothing -> pure Nothing
        Just invertedRight -> do
          {init, last} <- pure $ unsafePartial $ fromJust $ unsnoc $ cons invertedRight $ cons invertedLeft path
          result <- pure $ foldr compose last init
          if isMainPath
            then tell [result]
            else pure unit
          pure $ Just result

  where
    compose :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
    compose f1 f2 = BQD
      (domain f1)
      (BinaryCombinator ComposeF)
      f1
      f2
      (range f2)
      (and (functional f1)(functional f2))
      (and (mandatory f1)(mandatory f2))

-- The non-terminal step of Compose.
invertFunctionDescription (BQD dom (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) path isMainPath = do
  minvertedLeft <- invertFunctionDescription qfd1 [] false
  case minvertedLeft of
    Nothing -> pure Nothing
    Just invertedLeft -> invertFunctionDescription qfd2 (cons invertedLeft path) isMainPath

-- For a filtered query, we return the filtered result and ignore the condition.
invertFunctionDescription (BQD dom (BinaryCombinator FilterF) qfd1 qfd2 ran _ _) path isMainPath = do
  void $ invertFunctionDescription qfd2 path isMainPath
  invertFunctionDescription qfd1 path isMainPath

-- We can ignore all other binary functions. Just trace paths back to the root from both branches.
-- The results will not be used.
invertFunctionDescription (BQD dom (BinaryCombinator f) qfd1 qfd2 ran _ _) path isMainPath = do
  void $ invertFunctionDescription qfd1 path isMainPath
  invertFunctionDescription qfd2 path isMainPath

invertFunction :: Domain -> QueryFunction -> Range -> QueryFunction
invertFunction dom qf ran = case qf of
  DataTypeGetter f -> case f of
    ContextF -> RolGetter $ ENR (unsafePartial $ domain2RoleType dom)
    BindingF -> DataTypeGetterWithParameter GetRoleBindersF (unwrap $ unsafePartial $ domain2RoleType dom)
    ExternalRoleF -> DataTypeGetter ContextF
    IdentityF -> DataTypeGetter IdentityF

    _ -> Identity

  RolGetter rt -> DataTypeGetter ContextF
  PropertyGetter pt -> Value2Role

  -- Catchall clause
  _ -> Identity

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
  VDOM _ -> true
