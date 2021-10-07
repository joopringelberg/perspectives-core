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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.
-- END LICENSE

-- | This module contains functions to create a JSON structure from a Role instance,
-- | that contains all properties and values stored in that role.
-- | These functions are used in the API to move information to the client.

module Perspectives.Queries.RolesAndProperties where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Perspectives.ContextAndRole (rol_properties)
import Perspectives.CoreTypes (MonadPerspectives, (##=), type (~~>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Query.UnsafeCompiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Simple.JSON (writeJSON)

type RoleWithProperties =
  { roleId :: String
  , propertyValues :: Object (Array String)
  }

type RolesWithProperties' = Array RoleWithProperties

newtype RolesWithProperties = RolesWithProperties String
derive instance newtypeRolesWithProperties :: Newtype RolesWithProperties _

getRoleWithProperties :: RoleInstance -> MonadPerspectives RoleWithProperties
getRoleWithProperties rid = do
  role <- getPerspectRol rid
  pure { roleId: (unwrap rid), propertyValues: (map unwrap <$> rol_properties role) }

getRolesWithProperties' :: String -> ContextInstance -> MonadPerspectives RolesWithProperties'
getRolesWithProperties' rt cid = do
  getter <- getRoleFunction rt
  roleIds <- cid ##= getter
  traverse getRoleWithProperties roleIds

getRolesWithProperties :: String ->  (ContextInstance ~~> RolesWithProperties)
getRolesWithProperties rt cid = ArrayT do
  r <- lift $ getRolesWithProperties' rt cid
  pure $ RolesWithProperties <<< writeJSON <$> r
