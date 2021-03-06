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

module Perspectives.Representation.Verbs where

import Data.Array (difference, elemIndex, intersect, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (isJust, isNothing)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (class Eq, class Ord, class Show, compare, not, show, ($))

-----------------------------------------------------------
-- ROLEVERB
-----------------------------------------------------------

data RoleVerb =
    Remove            -- Remove a single instance
  | Delete            -- Remove all instances
  | Create            -- Create an instance
  | CreateAndFill     -- CreateAndFill <RoleType> with <roleExpr>
  | Fill              -- <functionalRoleExpr> with <functionalRoleExpr>
  | Unbind            -- <roleExpr> from <RoleType>, i.e. remove all binders of type <RoleType>
  | RemoveFiller      -- <functionalRoleExpr> from <functionalRoleExpr>
  | Move              -- Move an instance from one context to another.

derive instance genericRepRoleVerb :: Generic RoleVerb _
instance encodeRoleVerb :: Encode RoleVerb where
  encode = genericEncode defaultOptions
instance decodeRoleVerb :: Decode RoleVerb where
  decode = genericDecode defaultOptions
instance showRoleVerb :: Show RoleVerb where
  show = genericShow
instance eqRoleVerb :: Eq RoleVerb where
  eq = genericEq

-----------------------------------------------------------
-- PROPERTYVERB
-----------------------------------------------------------

data PropertyVerb =
    Consult
  | RemovePropertyValue   -- Remove a single value.
  | DeleteProperty        -- Remove all values.
  | AddPropertyValue      -- Add a single value.
  | SetPropertyValue      -- Replace all values.

derive instance genericRepPropertyVerb :: Generic PropertyVerb _
instance encodePropertyVerb :: Encode PropertyVerb where
  encode = genericEncode defaultOptions
instance decodePropertyVerb :: Decode PropertyVerb where
  decode = genericDecode defaultOptions
instance showPropertyVerb :: Show PropertyVerb where
  show = genericShow
instance eqPropertyVerb :: Eq PropertyVerb where
  eq = genericEq
instance ordPropertyVerb :: Ord PropertyVerb where
  compare pv1 pv2 = compare (show pv1) (show pv2)

-----------------------------------------------------------
-- ROLEVERBLIST
-----------------------------------------------------------
data RoleVerbList = All | Including (Array RoleVerb) | Excluding (Array RoleVerb)

derive instance genericRoleVerbList :: Generic RoleVerbList _
instance encodeRoleVerbList :: Encode RoleVerbList where encode = genericEncode defaultOptions
instance decodeRoleVerbList :: Decode RoleVerbList where decode = genericDecode defaultOptions
instance showRoleVerbList :: Show RoleVerbList where show = genericShow
derive instance eqRoleVerbList :: Eq RoleVerbList

hasVerb :: RoleVerb -> RoleVerbList -> Boolean
hasVerb v All = true
hasVerb v (Including vl) = isJust $ elemIndex v vl
hasVerb v (Excluding vl) = isNothing $ elemIndex v vl

hasAllVerbs :: Array RoleVerb -> RoleVerbList -> Boolean
hasAllVerbs vs All = true
hasAllVerbs vs (Including vl) = null $ difference vs vl
hasAllVerbs vs (Excluding vl) = null $ intersect vs vl

hasOneOfTheVerbs :: Array RoleVerb -> RoleVerbList -> Boolean
hasOneOfTheVerbs vs All = true
hasOneOfTheVerbs vs (Including vl) = not $ null $ intersect vs vl
hasOneOfTheVerbs vs (Excluding vl) = not $ null $ difference vs vl
