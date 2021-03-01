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

module Perspectives.ObjectGetterLookup where

import Data.Maybe (Maybe)
import Foreign.Object (Object, empty, insert, lookup)
import Perspectives.CoreTypes (RoleGetter, PropertyValueGetter)
import Prelude ((<$>))

type ComputedFunction f = {func :: f, functional :: Boolean, mandatory :: Boolean}

type RoleGetterCache = Object (ComputedFunction RoleGetter)

type PropertyValueGetterCache = Object (ComputedFunction PropertyValueGetter)

roleGetterCache :: RoleGetterCache
roleGetterCache = empty

propertyValueGetterCache :: PropertyValueGetterCache
propertyValueGetterCache = empty

lookupGetterByName :: forall f. Object (ComputedFunction f) -> String -> Maybe f
lookupGetterByName cache name = _.func <$> lookup name cache

lookupRoleGetterByName :: String -> Maybe RoleGetter
lookupRoleGetterByName = lookupGetterByName roleGetterCache

lookupPropertyValueGetterByName :: String -> Maybe PropertyValueGetter
lookupPropertyValueGetterByName = lookupGetterByName propertyValueGetterCache

getterCacheInsert :: forall f. Object (ComputedFunction f) -> String -> f -> Boolean -> Boolean -> Object(ComputedFunction f)
getterCacheInsert cache name getter functional mandatory = insert name {func: getter, functional, mandatory} cache

roleGetterCacheInsert :: String -> RoleGetter -> Boolean -> Boolean -> RoleGetterCache
roleGetterCacheInsert = getterCacheInsert roleGetterCache

propertyGetterCacheInsert :: String -> PropertyValueGetter -> Boolean -> Boolean -> PropertyValueGetterCache
propertyGetterCacheInsert = getterCacheInsert propertyValueGetterCache

isGetterFunctional :: forall f. Object (ComputedFunction f) -> String -> Maybe Boolean
isGetterFunctional cache name = _.functional <$> lookup name cache

isRoleGetterFunctional :: String -> Maybe Boolean
isRoleGetterFunctional = isGetterFunctional roleGetterCache

isPropertyGetterFunctional :: String -> Maybe Boolean
isPropertyGetterFunctional = isGetterFunctional propertyValueGetterCache

isGetterMandatory :: forall f. Object (ComputedFunction f) -> String -> Maybe Boolean
isGetterMandatory cache name = _.mandatory <$> lookup name cache

isRoleGetterMandatory :: String -> Maybe Boolean
isRoleGetterMandatory = isGetterFunctional roleGetterCache

isPropertyGetterMandatory :: String -> Maybe Boolean
isPropertyGetterMandatory = isGetterFunctional propertyValueGetterCache
