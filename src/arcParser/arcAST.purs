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

module Perspectives.Parsing.Arc.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (Statements)
import Perspectives.Representation.Context (ContextKind)
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.Sentence (Sentence)
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleKind, RoleType)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerbList)

--------------------------------------------------------------------------------
---- CONTEXT
--------------------------------------------------------------------------------
newtype ContextE = ContextE
  { id :: String
  , kindOfContext :: ContextKind
  , contextParts :: List ContextPart
  , pos :: ArcPosition}

type Prefix = String
type ModelName = String

data ContextPart =
  RE RoleE |
  -- User RoleE |
  STATE StateE |
  CE ContextE |
  PREFIX Prefix ModelName |
  ContextAspect String ArcPosition |
  IndexedContext String ArcPosition

--------------------------------------------------------------------------------
---- ROLE
--------------------------------------------------------------------------------
newtype RoleE = RoleE
  { id :: String
  , kindOfRole :: RoleKind
  , roleParts :: List RolePart
  , pos :: ArcPosition}

-- TODO: het verschil tussen conjunctie en disjunctie bij FilledByAttribute.
data RolePart =
  PE PropertyE |
  SQP (List StateQualifiedPart) |
  VE ViewE |
  FunctionalAttribute Boolean |
  MandatoryAttribute Boolean |
  UnlinkedAttribute |
  FilledByAttribute String |
  Calculation Step |
  RoleAspect String ArcPosition |
  IndexedRole String ArcPosition |
  ROLESTATE StateE

--------------------------------------------------------------------------------
---- STATE
--------------------------------------------------------------------------------
newtype StateE = StateE
  { id :: StateSpecification
  , condition :: Step
  , stateParts :: List StateQualifiedPart
  , subStates :: List StateE
  }
--------------------------------------------------------------------------------
---- PROPERTY
--------------------------------------------------------------------------------
newtype PropertyE = PropertyE
  { id :: String
  , range :: Maybe Range
  , propertyParts :: List PropertyPart
  , pos :: ArcPosition
  }

data PropertyPart =
    FunctionalAttribute' Boolean
  | MandatoryAttribute' Boolean
  | Calculation' Step
  | Ran Range

--------------------------------------------------------------------------------
---- STATEQUALIFIEDPART
--------------------------------------------------------------------------------
-- | In all these types, "subject" (and "user" in NotificationE) may be both qualified and unqualified (this happens when used as a reference).
-- | However, it is an error for subject to be insufficiently qualified for it to be unique in the model.
data StateQualifiedPart =
  R RoleVerbE |
  P PropertyVerbE |
  AC ActionE |
  N NotificationE |
  AE AutomaticEffectE |
  SUBSTATE StateE

--------------------------------------------------------------------------------
---- ROLEVERB
--------------------------------------------------------------------------------
-- Ends up in Perspective, identified by subject and object.
newtype RoleVerbE = RoleVerbE
  { subject :: RoleIdentification
  , object :: RoleIdentification
  , state :: StateSpecification
  , roleVerbs :: RoleVerbList
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- PROPERTYVERB
--------------------------------------------------------------------------------
-- Ends up in Perspective, identified by subject and object.
newtype PropertyVerbE = PropertyVerbE
  { subject :: RoleIdentification
  , object :: RoleIdentification
  , state :: StateSpecification
  , propertyVerbs :: List PropertyVerb
  , propsOrView :: PropsOrView
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- ACTION
--------------------------------------------------------------------------------
-- Ends up in Perspective, identified by subject and object.
newtype ActionE = ActionE
  { id :: String
  , subject :: RoleIdentification
  , object :: RoleIdentification
  , state :: StateSpecification
  , effect :: Statements
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- NOTIFICATION
--------------------------------------------------------------------------------
-- Ends up in State, identified by the fully qualified name in StateTransitionE.
newtype NotificationE = NotificationE
  { user :: RoleIdentification
  , transition :: StateTransitionE
  , message :: Sentence
  -- , level :: NotificationLevel
  , object :: Maybe RoleIdentification
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- AUTOMATICEFFECT
--------------------------------------------------------------------------------
-- Ends up in State, identified by the fully qualified name in StateTransitionE.
newtype AutomaticEffectE = AutomaticEffectE
  { subject :: RoleIdentification
  , object :: Maybe RoleIdentification
  , transition :: StateTransitionE
  , effect :: Statements
  , start :: ArcPosition
  , end :: ArcPosition
  }

data StateTransitionE = Entry StateSpecification | Exit StateSpecification

data PropsOrView = AllProperties | Properties (List String) | View String
derive instance genericPropsOrView :: Generic PropsOrView _
instance eqPropsOrView :: Eq PropsOrView where eq = genericEq

--------------------------------------------------------------------------------
---- VIEW
--------------------------------------------------------------------------------
newtype ViewE = ViewE
  { id :: String
  , viewParts :: List String
  , pos :: ArcPosition}

--------------------------------------------------------------------------------
---- ROLEIDENTIFICATION
--------------------------------------------------------------------------------
data RoleIdentification =
  ExplicitRole ContextType RoleType ArcPosition |
  ImplicitRole ContextType Step

derive instance genericRoleIdentification :: Generic RoleIdentification _
instance eqRoleIdentification :: Eq RoleIdentification where eq = genericEq
instance showRoleIdentification :: Show RoleIdentification where show = genericShow

--------------------------------------------------------------------------------
---- STATESPECIFICATION
--------------------------------------------------------------------------------
type StateLocalName = String

data StateSpecification =
	  ContextState ContextType (Maybe SegmentedPath)
	| SubjectState RoleIdentification (Maybe SegmentedPath)
	| ObjectState RoleIdentification (Maybe SegmentedPath)

type SegmentedPath = String

derive instance genericStateSpecification :: Generic StateSpecification _
instance eqStateSpecification :: Eq StateSpecification where eq = genericEq
instance showStateSpecification :: Show StateSpecification where show = genericShow

data StateKind = CState | SState | OState
--------------------------------------------------------------------------------
---- INSTANCES
--------------------------------------------------------------------------------
-- We are only interested in ordering RE dataconstructors.
instance eqContextPart :: Eq ContextPart where
  eq (RE r1) (RE r2) = eq r1 r1
  eq _ _ = false

instance eqRoleE :: Eq RoleE where
  eq (RoleE{id:id1}) (RoleE{id:id2}) = id1 == id2

derive instance genericContextE :: Generic ContextE _
instance showContextE :: Show ContextE where show = genericShow

derive instance genericContextElement :: Generic ContextPart _
instance showContextElement :: Show ContextPart where show x = genericShow x

derive instance genericRoleE :: Generic RoleE _
instance showRoleE :: Show RoleE where show = genericShow

derive instance genericRoleElement :: Generic RolePart _
instance showRoleElement :: Show RolePart where show = genericShow

derive instance genericStateE :: Generic StateE _
instance showStateE :: Show StateE where show s = genericShow s

derive instance genericStateQualifiedPart :: Generic StateQualifiedPart _
instance showStateQualifiedPart :: Show StateQualifiedPart where show = genericShow

derive instance genericNotificationE :: Generic NotificationE _
instance showNotificationE :: Show NotificationE where show = genericShow

derive instance genericStateTransitionE :: Generic StateTransitionE _
instance showStateTransitionE :: Show StateTransitionE where show = genericShow

derive instance genericAutomaticEffectE :: Generic AutomaticEffectE _
instance showAutomaticEffectE :: Show AutomaticEffectE where show = genericShow

derive instance genericRoleVerbE :: Generic RoleVerbE _
instance showRoleVerbE :: Show RoleVerbE where show = genericShow

derive instance genericPropertyVerbE :: Generic PropertyVerbE _
instance showPropertyVerbE :: Show PropertyVerbE where show = genericShow

derive instance genericPropertyE :: Generic PropertyE _
instance showPropertyE :: Show PropertyE where show = genericShow

derive instance genericPropertyElement :: Generic PropertyPart _
instance showPropertyElement :: Show PropertyPart where show = genericShow

derive instance genericPropOrView :: Generic PropsOrView _
instance showPropOrView :: Show PropsOrView where show = genericShow

derive instance genericActionElement :: Generic ActionE _
instance showActionElement :: Show ActionE where show = genericShow

derive instance genericViewElement :: Generic ViewE _
instance showViewElement :: Show ViewE where show = genericShow
