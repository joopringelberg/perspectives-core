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

module Perspectives.Assignment.DependencyTracking where

-- | The ActionAssumptionRegister is indexed by the two elements of an Assumption, in order.
import Data.Array (catMaybes)
import Data.Array (cons, delete) as Arr
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (ActionInstance(..), Assumption, MP)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, delete, ensure, keys, modify, new, peek, poke)
import Perspectives.PerspectivesState (actionAssumptionCache, actionInstanceCache)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..))
import Prelude (class Eq, Unit, const, flip, join, unit, ($), (<#>), (<$>), (&&), eq, map, bind, pure, discard, void)


-- | Creates reciprocal entries in the ActionAssumptionCache and the ActionInstanceCache.
-- | When the same ActionInstance is cached again but with different Assumptions,
-- | both caches are updated correspondingly.
cacheActionInstanceDependencies :: ActionInstance -> Array Assumption -> MP Unit
cacheActionInstanceDependencies act@(ActionInstance c a) as = do
  void $ cacheActionInstanceSupports act as
  for_ as (cacheActionInstanceDependency act)

  where
    cacheActionInstanceSupports :: ActionInstance -> Array Assumption -> MP (GLStrMap (Array Assumption))
    cacheActionInstanceSupports (ActionInstance c' a') as' = do
      actionInstanceCache' <- actionInstanceCache
      pure $ poke (ensure actionInstanceCache' (unwrap c') (new unit)) (unwrap a') as'

    cacheActionInstanceDependency :: ActionInstance -> Assumption -> MP (GLStrMap (Array ActionInstance))
    cacheActionInstanceDependency ai (Tuple r p) = do
      actionAssumptionCache' <- actionAssumptionCache
      pure $ modify (ensure actionAssumptionCache' r (new unit)) p (Arr.cons ai) []

-- | Retrieve the assumptions that underly the application of an Action to a ContextInstance.
retrieveActionInstanceSupports :: ActionInstance -> MP (Maybe (Array Assumption))
retrieveActionInstanceSupports (ActionInstance c a) = do
  actionInstanceCache' <- actionInstanceCache
  pure $ join $ flip peek (unwrap a) <$> (peek actionInstanceCache' (unwrap c))

-- | Retrieve the ActionInstances that depend an an Assumption.
retrieveAssumptionActionInstances :: Assumption -> MP (Maybe (Array ActionInstance))
retrieveAssumptionActionInstances (Tuple r p) = do
  actionAssumptionCache' <- actionAssumptionCache
  pure $ join $ flip peek p <$> (peek actionAssumptionCache' r)

-- | Given just a ContextInstance, find all Assumptions that reference an ActionInstance with that
-- | ContextInstance and remove those ActionInstances.
removeContextInstanceDependencies :: ContextInstance -> MP Unit
removeContextInstanceDependencies (ContextInstance c) = do
  actionInstanceCache' <- actionInstanceCache
  actionAssumptionCache' <- actionAssumptionCache
  pure $ case delete actionInstanceCache' c of
    Nothing -> unit
    (Just (asm :: GLStrMap (Array Assumption))) -> const unit $ keys asm <#> \at -> let
      actionInstance = ActionInstance (ContextInstance c) (ActionType at) in
      -- remove this ActionInstance from each of the assumptions.
      case peek asm at of
        Nothing -> unit
        (Just (as :: Array Assumption)) -> const unit $ as <#> \((Tuple r p) :: Assumption) ->
          case peek actionAssumptionCache' r of
            Nothing -> unit
            (Just x) -> const unit (modify x p (Arr.delete actionInstance) [])

-- | Given an array of assumptions, find all ActionInstances that depend on at least one of them.
actionInstancesDependingOn :: Array Assumption -> MP (Array ActionInstance)
actionInstancesDependingOn as = do
  (r :: Array (Maybe (Array ActionInstance))) <- traverse retrieveAssumptionActionInstances as
  pure $ join (catMaybes r)

actionTypesForContextInstance :: ContextInstance -> MP (Maybe (Array ActionType))
actionTypesForContextInstance c = do
  actionInstanceCache' <- actionInstanceCache
  pure $ (map ActionType) <$> (keys <$> peek actionInstanceCache' (unwrap c))
