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
module Perspectives.Parsing.Arc.AspectInference  where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, filter, null, singleton, some, uncons)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Traversable (for)
import Foreign.Object (values)
import Perspectives.DomeinCache (modifyEnumeratedRoleInDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder)
import Perspectives.Identifiers (startsWithSegments)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF, withDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.QueryTypes (roleInContext2Role)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Identifiable (identifier_)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (binding)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Perspectives.Types.ObjectGetters (isMandatory_, isRelational_)

-- Modify EnumeratedRoles by inferring the following from their aspects:
--    * If an Aspect is relational, make the role relational, too.
--    * If an Aspect is mandatory, make the role relational, too.
--    * Add the restrictions on fillers from Aspects to that of the role.
inferFromAspectRoles :: PhaseThree Unit
inferFromAspectRoles = do
  df@{id} <- lift $ State.gets _.dfr
  withDomeinFile
    id
    (DomeinFile df)
    (inferFromAspectRoles' df)
  where
    inferFromAspectRoles' :: DomeinFileRecord -> PhaseThree Unit
    inferFromAspectRoles' df@{id, namespace, enumeratedRoles} = do
      -- We have to execute in topological order, so aspects are handled before they are applied.
      enumeratedRoles' <- executeInTopologicalOrder
        identifier_
        -- Only count aspects defined in this namespace as dependencies for the sorting!
        (filter (flip startsWithSegments namespace) <<< (map (unwrap <<< roleInContext2Role) <<< _.roleAspects <<< unwrap))
        (values enumeratedRoles)
        (inferCardinality >=> inferMandatoriness {->=> inferBinding-} >=> lift <<< lift <<< modifyEnumeratedRoleInDomeinFile id)
      (DomeinFile dfr') <- lift $ lift $ getDomeinFile id
      modifyDF \_ -> dfr'

    inferCardinality :: EnumeratedRole -> PhaseThree EnumeratedRole
    inferCardinality r@(EnumeratedRole{functional, roleAspects}) = do
      isFunctional <- null <$> evalStateT (some aspectIsRelational) (roleInContext2Role <$> roleAspects) <|> pure functional
      pure $ over EnumeratedRole (\rr -> rr {functional = isFunctional}) r

    aspectIsRelational :: StateT (Array EnumeratedRoleType) PhaseThree EnumeratedRoleType
    aspectIsRelational = do
      maspect <- State.gets uncons
      case maspect of 
        Nothing -> throwError $ singleton (Custom "Not found") 
        Just {head, tail} -> do
          State.put tail
          -- fail if head is functional; only pass through when head is relational.
          (lift $ lift $ lift $ isRelational_ head) >>= guard
          pure head

    inferMandatoriness :: EnumeratedRole -> PhaseThree EnumeratedRole
    inferMandatoriness r@(EnumeratedRole{mandatory, roleAspects}) = do
      isMandatory <- not <<< null <$> evalStateT (some aspectIsMandatory) (roleInContext2Role <$> roleAspects) <|> pure mandatory
      pure $ over EnumeratedRole (\rr -> rr {mandatory = isMandatory}) r

    aspectIsMandatory :: StateT (Array EnumeratedRoleType) PhaseThree EnumeratedRoleType
    aspectIsMandatory = do
      maspect <- State.gets uncons
      case maspect of 
        Nothing -> throwError $ singleton (Custom "Not found") 
        Just {head, tail} -> do
          State.put tail
          -- fail if head is optional; only pass through when head is mandatory.
          (lift $ lift $ lift $ isMandatory_ head) >>= guard
          pure head

    -- TODO: beslis of en hoe de binding restrictie geexpandeerd wordt. Nu laten we het bij de declared binding. Momenteel wordt inferBinding niet toegepast.
    -- The restriction on role fillers is the SUM of the restrictions of the aspects (including that modelled with the role itself)
    -- Assuming we've inferred bindings for all aspects _before_ we infer bindings for the role itself, we just have to deal 
    -- with the direct aspects.
    inferBinding :: EnumeratedRole -> PhaseThree EnumeratedRole
    inferBinding r = lift $ lift $ do
      ownBinding <- binding r
      -- An aspect may have no binding restrictions.
      completeBinding <- catMaybes <<< cons ownBinding <$> for (roleInContext2Role <$> (unwrap r).roleAspects) (getEnumeratedRole >=> binding)
      if null completeBinding
        then pure r
        else pure $ over EnumeratedRole (\rr -> rr {binding = Just $ SUM completeBinding}) r
