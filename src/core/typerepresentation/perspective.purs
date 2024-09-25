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

import Data.Array (concat, cons, elemIndex, findIndex, foldl, fromFoldable, length, null)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List (findIndex) as LST
import Data.Map (Map, values, fromFoldable) as MAP
import Data.Maybe (isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription, RoleInContext(..), range)
import Perspectives.Representation.ADT (ADT, commonLeavesInADT)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), intersectionPset, isElementOf, overlapsPSet)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType, StateIdentifier)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..), RoleVerbList, allPropertyVerbs, hasAllVerbs, hasOneOfTheVerbs, hasVerb)
import Prelude (class Eq, class Ord, class Show, bind, flip, not, pure, ($), (&&), (<#>), (<$>), (<>), (>), (||), (/=))
import Simple.JSON (class ReadForeign, class WriteForeign, read', write, writeImpl)

-----------------------------------------------------------
-- PERSPECTIVE
-----------------------------------------------------------
-- | As we do not treat Perspectives as entities that can be stored separately
-- | as a type like Roles and Contexts, we have no need for a newtype identification.
type PerspectiveId = String

newtype Perspective = Perspective PerspectiveRecord

type PerspectiveRecord =
  { id :: PerspectiveId
  -- This is underconstrained: the domain of the QueryFunctionDescription should be a CDOM (ADT ContextType),
  -- its range an RDOM (ADT RoleInContext).
  , object :: QueryFunctionDescription
  , displayName :: String
  -- The RoleTypes of the object of the perspective.
  , roleTypes :: Array RoleType
  , isEnumerated :: Boolean
  , roleVerbs :: EncodableMap StateSpec RoleVerbList
  , propertyVerbs :: EncodableMap StateSpec (Array PropertyVerbs)
  , actions :: EncodableMap StateSpec (Object Action)
  -- selfonly is just for a user's perspective on his own role. So the object of this perspective
  -- must be a user role and the perspective _is of_ that user role, too.
  , selfOnly :: Boolean
  -- AuthorOnly means: the perspective is just for the peer himself. 
  -- The object of this perspective must be a user role; however, the perspective may be that of another user role.
  -- The net result is that if the object is a multi-user role, any changes falling within the perspective 
  -- are _only ever shared with the instance on whom the changes are made_.
  , authorOnly :: Boolean
  , isSelfPerspective :: Boolean
  , automaticStates :: Array StateIdentifier
  }

derive instance genericRepPerspective :: Generic Perspective _

instance showPerspective :: Show Perspective where
  show = genericShow

derive instance eqPerspective :: Eq Perspective

derive instance newtypePerspective :: Newtype Perspective _

instance WriteForeign Perspective where
  writeImpl (Perspective r) = writeImpl r

instance ReadForeign Perspective where
  readImpl f = Perspective <$> read' f

data StateSpec =
  ContextState StateIdentifier
  | SubjectState StateIdentifier
  | ObjectState StateIdentifier

derive instance genericStateSpec :: Generic StateSpec _
instance showStateSpec :: Show StateSpec where show = genericShow
derive instance eqStateSpec :: Eq StateSpec

instance ordStateSpec :: Ord StateSpec where compare = genericCompare

instance WriteForeign StateSpec where
  writeImpl (ContextState stateIdentifier) = writeImpl { constructor: "ContextState", stateIdentifier}
  writeImpl (SubjectState stateIdentifier) = writeImpl { constructor: "SubjectState", stateIdentifier}
  writeImpl (ObjectState stateIdentifier) = writeImpl { constructor: "ObjectState", stateIdentifier}

instance ReadForeign StateSpec where
  readImpl f = do 
    {constructor, stateIdentifier} :: {constructor :: String, stateIdentifier :: StateIdentifier} <- read' f
    unsafePartial case constructor of 
      "ContextState" -> pure $ ContextState stateIdentifier
      "SubjectState" -> pure $ SubjectState stateIdentifier
      "ObjectState" -> pure $ ObjectState stateIdentifier

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

perspectiveSupportsPropertyVerb :: Perspective -> PropertyVerb -> Boolean
perspectiveSupportsPropertyVerb (Perspective {propertyVerbs}) propertyVerb = find $ MAP.values $ unwrap propertyVerbs
  where
    find :: List (Array PropertyVerbs) -> Boolean
    find pvs = isJust $ LST.findIndex
      (\(pva :: Array PropertyVerbs) -> isJust $ findIndex
        (\(PropertyVerbs _ pverbs) -> isElementOf propertyVerb pverbs)
        pva)
      pvs

perspectiveSupportsPropertyWithOneofVerbs :: Perspective -> PropertyType -> ExplicitSet PropertyVerb -> Boolean
perspectiveSupportsPropertyWithOneofVerbs (Perspective {propertyVerbs}) property pVerbAlternatives = find $ MAP.values $ unwrap propertyVerbs
  where
    find :: List (Array PropertyVerbs) -> Boolean
    find pvs = isJust $ LST.findIndex
      (\(pva :: Array PropertyVerbs) -> isJust $ findIndex
        (\(PropertyVerbs pset pverbs) -> Empty /= (intersectionPset [pVerbAlternatives, pverbs]) && isElementOf property pset)
        pva)
      pvs

-- | Regardless of state, does the perspective allow for the RoleVerb?
perspectiveSupportsRoleVerb :: Perspective -> RoleVerb -> Boolean
perspectiveSupportsRoleVerb (Perspective{roleVerbs}) verb = isJust $ LST.findIndex
  (\rvs -> hasVerb verb rvs)
  (MAP.values $ unwrap roleVerbs)

perspectiveSupportsRoleVerbs :: Perspective -> Array RoleVerb -> Boolean
perspectiveSupportsRoleVerbs (Perspective{roleVerbs}) verbs = null verbs || (isJust $ LST.findIndex
  (\rvs -> hasAllVerbs verbs rvs)
  (MAP.values $ unwrap roleVerbs))

-- | True iff there is at least one state in which the RoleVerb is supported.
perspectiveSupportsOneOfRoleVerbs :: Perspective -> Array RoleVerb -> Boolean
perspectiveSupportsOneOfRoleVerbs (Perspective{roleVerbs}) verbs = isJust $ LST.findIndex
  (\rvs -> hasOneOfTheVerbs verbs rvs)
  (MAP.values $ unwrap roleVerbs)

perspectiveMustBeSynchronized :: Perspective -> Boolean
perspectiveMustBeSynchronized p@(Perspective {roleVerbs, propertyVerbs, authorOnly}) = not authorOnly &&
  perspectiveSupportsOneOfRoleVerbs p [Remove, Delete, RemoveContext, DeleteContext] ||
  perspectiveSupportsPropertyVerb p Consult

-----------------------------------------------------------
-- PROPERTYVERBS
-----------------------------------------------------------
-- NOTE: we might replace (ExplicitSet PropertyType) with RelevantProperties.
data PropertyVerbs = PropertyVerbs (ExplicitSet PropertyType) (ExplicitSet PropertyVerb)
derive instance genericPropertyVerbs :: Generic PropertyVerbs _
instance showPropertyVerbs :: Show PropertyVerbs where show = genericShow
derive instance eqPropertyVerbs :: Eq PropertyVerbs

instance writeForeignPropertyVerbs :: WriteForeign PropertyVerbs where
  writeImpl (PropertyVerbs props verbs) = write { properties: write props, verbs: write verbs}

instance ReadForeign PropertyVerbs where
  readImpl f = do 
    {properties, verbs} :: {properties :: ExplicitSet PropertyType, verbs :: ExplicitSet PropertyVerb} <- read' f
    pure $ PropertyVerbs properties verbs

-- Pair each PropertyType will all PropertyVerbs available for it.
expandPropertyVerbs :: Array PropertyType -> PropertyVerbs -> Array (Tuple PropertyType (Array PropertyVerb))
expandPropertyVerbs allProps (PropertyVerbs props verbs) = let
  (verbs' :: Array PropertyVerb) = expandVerbs verbs
  in (flip Tuple verbs') <$> expandPropSet allProps props

expandVerbs ::  ExplicitSet PropertyVerb -> Array PropertyVerb
expandVerbs Universal = allPropertyVerbs
expandVerbs Empty = []
expandVerbs (PSet as) = as

expandPropSet :: Array PropertyType -> ExplicitSet PropertyType -> Array PropertyType
expandPropSet allProps Universal = allProps
expandPropSet _ Empty = []
expandPropSet _ (PSet as) = as

addProperty :: Partial => PropertyVerbs -> PropertyType -> PropertyVerbs
addProperty (PropertyVerbs (PSet props) verbs) prop = PropertyVerbs (PSet (cons prop props)) verbs

isMutatingVerbSet :: ExplicitSet PropertyVerb -> Boolean
isMutatingVerbSet s = case expandVerbs s of
  x | length x > 1 -> true
  x | isJust $ elemIndex Consult x -> false
  _ -> true

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
  { modifiesRoleInstancesOf: if perspectiveSupportsOneOfRoleVerbs p [Remove, RemoveContext, Delete, DeleteContext, Create, CreateAndFill]
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
