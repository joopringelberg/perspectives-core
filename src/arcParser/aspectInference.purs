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
import Data.Array (null, singleton, some, uncons)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Traversable (for)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF, withDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (roleInContext2Role)
import Perspectives.Representation.Class.Role (roleAspects)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Perspectives.Types.ObjectGetters (isMandatory_, isRelational_)

-- Modify EnumeratedRoles by inferring the following from their aspects:
--    * If an Aspect is relational, make the role relational, too.
inferFromAspectRoles :: PhaseThree Unit
inferFromAspectRoles = do
  df@{_id} <- lift $ State.gets _.dfr
  withDomeinFile
    _id
    (DomeinFile df)
    (inferFromAspectRoles' df)
  where
    inferFromAspectRoles' :: DomeinFileRecord -> PhaseThree Unit
    inferFromAspectRoles' df@{enumeratedRoles} = do
      enumeratedRoles' <- for enumeratedRoles (inferCardinality >=> inferMandatoriness)
      modifyDF \dfr -> dfr { enumeratedRoles = enumeratedRoles'}

    inferCardinality :: EnumeratedRole -> PhaseThree EnumeratedRole
    inferCardinality r@(EnumeratedRole{functional}) = do
      aspects <- lift $ lift (roleAspects r)
      isFunctional <- null <$> evalStateT (some aspectIsRelational) (roleInContext2Role <$> aspects) <|> pure functional
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
    inferMandatoriness r@(EnumeratedRole{mandatory}) = do
      aspects <- lift $ lift (roleAspects r)
      isMandatory <- not <<< null <$> evalStateT (some aspectIsMandatory) (roleInContext2Role <$> aspects) <|> pure mandatory
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
