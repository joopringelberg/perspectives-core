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

-- | The properties of a Role’s binding are as accessible as if they were the Role’s own
-- | properties. The modeller may add a View to an Action that includes any of these binding
-- | properties. And this applies to the binding of the binding, recursively.
-- |
-- | This module addresses the issue: how do we make sure that changes to such properties are
-- | distributed properly?
-- |
-- | For an explanatory text, see: https://joopringelberg.github.io/perspectives-documentation/Perspectives%20on%20bindings.pdf

module Perspectives.Query.PropertyGetter where

import Data.Array (elemIndex, head, catMaybes)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MonadPerspectives, type (~~>))
import Perspectives.Instances.ObjectGetters (getProperty, binding) as IOG
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, binding)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..))
import Prelude (pure, bind, ($), (>>=), (>=>))

type PropertyGetter = RoleInstance ~~> Value

-- TODO. Cache.

-- TODO. These functions seem to be a double of those in  Perspectives.Instances.GetPropertyOnRoleGraph. 
getDynamicPropertyGetter :: EnumeratedPropertyType -> ADT EnumeratedRoleType -> MonadPerspectives (Maybe PropertyGetter)
getDynamicPropertyGetter enp enr@(ST role) = do
  props <- allLocallyRepresentedProperties enr
  if isJust $ elemIndex (ENP enp) props
    then pure $ Just $ IOG.getProperty enp
    else do
      bnd <- getEnumeratedRole role >>= binding
      g <- getDynamicPropertyGetter enp bnd
      case g of
        Nothing -> pure Nothing
        Just g' -> pure $ Just (IOG.binding >=> g')

getDynamicPropertyGetter enp enr@(PROD roles) = do
  getters <- traverse (getDynamicPropertyGetter enp) roles
  case head $ catMaybes getters of
    Nothing -> pure Nothing
    Just g -> pure $ Just g

getDynamicPropertyGetter enp enr@(SUM roles) = case head roles of
  Nothing -> pure Nothing
  Just r -> getDynamicPropertyGetter enp r

getDynamicPropertyGetter _ _ = pure Nothing
