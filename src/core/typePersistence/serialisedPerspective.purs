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

-- | A perspective is serialised for a given context instance, for a particular role.
-- | It contains all instances of that role.
-- | The serialisation is for a given (nested) state of the context and each of its
-- | instances. If state changes, so may the role verbs, properties and their verbs
-- | and actions.
-- Implementation note: these structures are serialised to a JSON string with writeJSON.

module Perspectives.TypePersistence.PerspectiveSerialisation.Data where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype) 
import Foreign.Object (Object)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType, PropertyType, RoleKind)

type SerialisedPerspective' =
  {
  ----
  ---- Type level properties
  ----
  id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  -- The RoleType having the Perspective.
  , userRoleType :: String
  -- The RoleType of the object of the Perspective.
  , roleType :: Maybe String
  , roleKind :: Maybe RoleKind
  , contextType :: ContextType
  -- The keys are the ContextType names, the values their translations that will be shown on screen.
  , contextTypesToCreate :: Object String
  , identifyingProperty :: String
  ----
  ---- Instance properties
  ----
  -- The context instance from which we compute the object; the context of the user role.
  , contextInstance :: ContextInstance
  -- If the object of the perspective is outside the context of the user role, this is the context of that object.
  , contextIdToAddRoleInstanceTo :: Maybe ContextInstance
  , roleInstances :: Object RoleInstanceWithProperties
  ----
  ---- State dependent properties
  ----
  -- RoleVerbs
  , verbs :: Array String
  -- All properties that are available given Context and Subject state,
  -- unified with all properties that are available given the Object states of
  -- instances. In a table, we should create a column for each.
  , properties :: Object SerialisedProperty
  -- the keys are the action names as they occur in the model.
  -- the values are the translations in the currentLanguage.
  , actions :: Object String
  }

-- | SerialisedProperty is state-independent.
type SerialisedProperty =
  { id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  , range :: String
  , constrainingFacets :: PropertyFacets
  }

type PropertyFacets =
  { minLength :: Maybe Int
  , maxLength :: Maybe Int
  , pattern :: Maybe {regex :: String, label :: String}
  , whiteSpace :: Maybe String
  , enumeration :: Maybe (Array String)
  , maxInclusive :: Maybe String
  , maxExclusive :: Maybe String
  , minInclusive :: Maybe String
  , minExclusive :: Maybe String
  , totalDigits :: Maybe Int
  , fractionDigits :: Maybe Int
  }

newtype SerialisedPerspective = SerialisedPerspective String
derive instance newtypeSerialisedPerspective :: Newtype SerialisedPerspective _

type RoleInstanceWithProperties =
  { roleId :: String
  , objectStateBasedRoleVerbs :: Array String
  -- keys are the string representation of PropertyType,
  -- so this map can be read as one from PropertyType to PropertyVerbs, too.
  -- Note that a perspective may have more properties than that this instance
  -- has values for, due to object state.
  , propertyValues :: Object ValuesWithVerbs
  -- The keys are action names, the values their translation.
  , actions :: Object String
  -- This member is not needed on the client side, but we need it to
  -- compile a complete list of SerialisedProperties.
  , objectStateBasedProperties :: Array PropertyType
  -- The url of this roleInstance if there is a public perspective on it.
  -- If it is a ContextRole, the url of its filler.
  , publicUrl :: Maybe String
  , filler :: Maybe RoleInstance
  }

type ValuesWithVerbs =
  { values :: Array String
  , propertyVerbs :: Array String
  }
