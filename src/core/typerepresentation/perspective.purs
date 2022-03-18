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

module Perspectives.Representation.Perspective where

import Data.Array (concat, difference, findIndex, foldl, fromFoldable, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List (findIndex) as LST
import Data.Map (Map, values, fromFoldable) as MAP
import Data.Maybe (isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription, RoleInContext(..), range)
import Perspectives.Representation.ADT (ADT, commonLeavesInADT)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), isElementOf, overlapsPSet)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType, StateIdentifier)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..), RoleVerbList, hasAllVerbs, hasOneOfTheVerbs, hasVerb)
import Prelude (class Eq, class Ord, class Show, ($), (&&), (<>), (<#>))
import Simple.JSON (class WriteForeign, write)

-----------------------------------------------------------
-- PERSPECTIVE
-----------------------------------------------------------
-- | As we do not treat Perspectives as entities that can be stored separately
-- | as a type like Roles and Contexts, we have no need for a newtype identification.
type PerspectiveId = String

newtype Perspective = Perspective PerspectiveRecord

type PerspectiveRecord =
  { id :: PerspectiveId
  , object :: QueryFunctionDescription
  , displayName :: String
  -- The RoleTypes of the object of the perspective.
  , roleTypes :: Array RoleType
  , isEnumerated :: Boolean
  , roleVerbs :: EncodableMap StateSpec RoleVerbList
	, propertyVerbs :: EncodableMap StateSpec (Array PropertyVerbs)
	, actions :: EncodableMap StateSpec (Object Action)
  , selfOnly :: Boolean
  , isSelfPerspective :: Boolean
  }

derive instance genericRepPerspective :: Generic Perspective _

instance showPerspective :: Show Perspective where
  show = genericShow

derive instance eqPerspective :: Eq Perspective

derive instance newtypePerspective :: Newtype Perspective _

instance encodePerspective :: Encode Perspective where
  encode = genericEncode defaultOptions

instance decodePerspective :: Decode Perspective where
  decode = genericDecode defaultOptions

data StateSpec =
  ContextState StateIdentifier
  | SubjectState StateIdentifier
  | ObjectState StateIdentifier

derive instance genericStateSpec :: Generic StateSpec _
instance showStateSpec :: Show StateSpec where show = genericShow
derive instance eqStateSpec :: Eq StateSpec
instance encodeStateSpec :: Encode StateSpec where encode = genericEncode defaultOptions
instance decodeStateSpec :: Decode StateSpec where decode = genericDecode defaultOptions
instance ordStateSpec :: Ord StateSpec where compare = genericCompare

stateSpec2StateIdentifier :: StateSpec -> StateIdentifier
stateSpec2StateIdentifier (ContextState s) = s
stateSpec2StateIdentifier (SubjectState s) = s
stateSpec2StateIdentifier (ObjectState s) = s

-----------------------------------------------------------
-- ACCESSORS
-----------------------------------------------------------
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
objectOfPerspective :: Partial => Perspective -> ADT RoleInContext
objectOfPerspective (Perspective {object}) = case range object of
  RDOM adt -> adt

-- | Disregarding state, returns true iff the perspective lets the user apply the
-- | verb to the property.
perspectiveSupportsPropertyForVerb :: Perspective -> PropertyType -> PropertyVerb -> Boolean
perspectiveSupportsPropertyForVerb (Perspective {propertyVerbs}) property verb = find $ MAP.values $ unwrap propertyVerbs
  where
    find :: List (Array PropertyVerbs) -> Boolean
    find pvs = isJust $ LST.findIndex
      (\(pva :: Array PropertyVerbs) -> isJust $ findIndex
        (\(PropertyVerbs pset pverbs) -> isElementOf property pset && (isElementOf verb pverbs))
        pva)
      pvs

-- | Disregarding state, returns true iff the perspective lets the user apply *some*
-- | verb to the property.
perspectiveSupportsProperty :: Perspective -> PropertyType -> Boolean
perspectiveSupportsProperty (Perspective {propertyVerbs}) property = find $ MAP.values $ unwrap propertyVerbs
  where
    find :: List (Array PropertyVerbs) -> Boolean
    find pvs = isJust $ LST.findIndex
      (\(pva :: Array PropertyVerbs) -> isJust $ findIndex
        (\(PropertyVerbs pset pverbs) -> isElementOf property pset)
        pva)
      pvs

-- | The object of the perspective must cover the given ADT in the sense that its
-- | EnumeratedRoleTypes form a superset of those of the ADT, those nodes being
-- | the types that occur on each path through the ADT tree (the 'union' of the paths, as it were).
-- | <perspective> `isPerspectiveOnADT` <adt>
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
isPerspectiveOnADT :: Partial => Perspective -> ADT RoleInContext -> Boolean
isPerspectiveOnADT p adt = null (commonLeavesInADT adt `difference` (commonLeavesInADT $ objectOfPerspective p))

-- | Regardless of state, does the perspective allow for the RoleVerb?
perspectiveSupportsRoleVerb :: Perspective -> RoleVerb -> Boolean
perspectiveSupportsRoleVerb (Perspective{roleVerbs}) verb = isJust $ LST.findIndex
  (\rvs -> hasVerb verb rvs)
  (MAP.values $ unwrap roleVerbs)

perspectiveSupportsRoleVerbs :: Perspective -> Array RoleVerb -> Boolean
perspectiveSupportsRoleVerbs (Perspective{roleVerbs}) verbs = isJust $ LST.findIndex
  (\rvs -> hasAllVerbs verbs rvs)
  (MAP.values $ unwrap roleVerbs)

perspectiveSupportsOneOfRoleVerbs :: Perspective -> Array RoleVerb -> Boolean
perspectiveSupportsOneOfRoleVerbs (Perspective{roleVerbs}) verbs = isJust $ LST.findIndex
  (\rvs -> hasOneOfTheVerbs verbs rvs)
  (MAP.values $ unwrap roleVerbs)

-----------------------------------------------------------
-- PROPERTYVERBS
-----------------------------------------------------------
-- NOTE: we might replace (ExplicitSet PropertyType) with RelevantProperties.
data PropertyVerbs = PropertyVerbs (ExplicitSet PropertyType) (ExplicitSet PropertyVerb)
derive instance genericPropertyVerbs :: Generic PropertyVerbs _
instance showPropertyVerbs :: Show PropertyVerbs where show = genericShow
derive instance eqPropertyVerbs :: Eq PropertyVerbs
instance encodePropertyVerbs :: Encode PropertyVerbs where encode = genericEncode defaultOptions
instance decodePropertyVerbs :: Decode PropertyVerbs where decode = genericDecode defaultOptions

instance writeForeignPropertyVerbs :: WriteForeign PropertyVerbs where
  writeImpl (PropertyVerbs props verbs) = write { properties: write props, verbs: write verbs}

-----------------------------------------------------------
-- MODIFICATIONSUMMARY
-----------------------------------------------------------
-- | The ModificationSummary is used to indicate in an InvertedQuery whether the
-- | locus of its attachment can be modified by the user according to a Perspective.
-- | This is regardless of state! We use this aspect of InvertedQueries to check on the
-- | completeness of synchronization.
type ModificationSummary =
  { modifiesRoleInstancesOf :: Array RoleInContext
  , modifiesRoleBindingOf :: Array RoleInContext
  -- TODO Refine this to PropertyInRole = PropertyInRole { role :: EnumeratedRoleType, property :: PropertyType}
  , modifiesPropertiesOf :: MAP.Map EnumeratedRoleType (ExplicitSet EnumeratedPropertyType)
  }

newtype PropertyInRole = PropertyInRole { role :: EnumeratedRoleType, property :: PropertyType}

-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
createModificationSummary :: Partial => Perspective -> ModificationSummary
createModificationSummary p@(Perspective{roleVerbs, propertyVerbs}) =
  { modifiesRoleInstancesOf: if perspectiveSupportsOneOfRoleVerbs p [Remove, Delete, Create, CreateAndFill]
    then commonLeavesInADT $ objectOfPerspective p
    else []
  , modifiesRoleBindingOf: if perspectiveSupportsOneOfRoleVerbs p [CreateAndFill, Fill, Unbind, RemoveFiller]
    then commonLeavesInADT $ objectOfPerspective p
    else []
  -- , modifiesPropertiesOf: foldl
  --     (\modifiableProperties (PropertyVerbs props verbs) -> if overlapsPSet (PSet [RemovePropertyValue, DeleteProperty, AddPropertyValue, SetPropertyValue]) verbs
  --       then props <> modifiableProperties
  --       else modifiableProperties)
  --     Empty
  --     (concat $ fromFoldable $ values $ unwrap propertyVerbs)
  , modifiesPropertiesOf: let
      (props :: ExplicitSet EnumeratedPropertyType) = foldl
          (\(modifiableProperties :: ExplicitSet EnumeratedPropertyType) (PropertyVerbs props verbs) -> if overlapsPSet (PSet [RemovePropertyValue, DeleteProperty, AddPropertyValue, SetPropertyValue]) verbs
            then (props <#> case _ of ENP pt -> pt) <> modifiableProperties
            else modifiableProperties)
          Empty
          (concat $ fromFoldable $ MAP.values $ unwrap propertyVerbs)
      in MAP.fromFoldable $ (commonLeavesInADT $ objectOfPerspective p) <#> \(RoleInContext{role}) -> Tuple role props
  }
