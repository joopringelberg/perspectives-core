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

import Data.Ordering

import Data.Array (difference, elemIndex, intersect, null)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (isJust, isNothing)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (class Eq, class Ord, class Semigroup, class Show, compare, not, show, ($), (<>), (<<<))
import Simple.JSON (class WriteForeign, write)

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
instance writeForeignRoleVerb :: WriteForeign RoleVerb where
  writeImpl = write <<< show
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

instance writeForeignPropertyVerb :: WriteForeign PropertyVerb where writeImpl = write <<< show

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

allPropertyVerbs :: Array PropertyVerb
allPropertyVerbs = [Consult, RemovePropertyValue, DeleteProperty, AddPropertyValue, SetPropertyValue]

-----------------------------------------------------------
-- ROLEVERBLIST
-----------------------------------------------------------
data RoleVerbList = All | Including (Array RoleVerb) | Excluding (Array RoleVerb)

derive instance genericRoleVerbList :: Generic RoleVerbList _
instance encodeRoleVerbList :: Encode RoleVerbList where encode = genericEncode defaultOptions
instance decodeRoleVerbList :: Decode RoleVerbList where decode = genericDecode defaultOptions
instance showRoleVerbList :: Show RoleVerbList where show = genericShow
derive instance eqRoleVerbList :: Eq RoleVerbList

instance ordRoleVerbList :: Ord RoleVerbList where
  compare All _ = GT
  compare _ All = LT
  compare l1 l2 = let
      v1 = roleVerbList2Verbs l1
      v2 = roleVerbList2Verbs l2
    in
      if null $ difference v1 v2
        -- v1 subset v2
        then LT
        else if null $ difference v2 v1
          then GT
          else EQ

instance semigroupRoleVerbList :: Semigroup RoleVerbList where
  append l1 l2 = Including ((roleVerbList2Verbs l1) <> (roleVerbList2Verbs l2))

hasVerb :: RoleVerb -> RoleVerbList -> Boolean
hasVerb v All = true
hasVerb v (Including vl) = isJust $ elemIndex v vl
hasVerb v (Excluding vl) = isNothing $ elemIndex v vl

hasAllVerbs :: Array RoleVerb -> RoleVerbList -> Boolean
hasAllVerbs vs All = true
hasAllVerbs vs (Including vl) = null $ difference vs vl
hasAllVerbs vs (Excluding vl) = null $ intersect vs vl

roleVerbList2Verbs :: RoleVerbList -> Array RoleVerb
roleVerbList2Verbs All = allVerbs
roleVerbList2Verbs (Excluding excluded) = difference allVerbs excluded
roleVerbList2Verbs (Including v) = v

allVerbs :: Array RoleVerb
allVerbs = [Remove, Delete, Create, CreateAndFill, Fill, Unbind, RemoveFiller, Move]

hasOneOfTheVerbs :: Array RoleVerb -> RoleVerbList -> Boolean
hasOneOfTheVerbs vs All = true
hasOneOfTheVerbs vs (Including vl) = not $ null $ intersect vs vl
hasOneOfTheVerbs vs (Excluding vl) = not $ null $ difference vs vl
