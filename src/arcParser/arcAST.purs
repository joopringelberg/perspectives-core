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

module Perspectives.Parsing.Arc.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Perspectives.Parsing.Arc.Expression.AST (Assignment, LetStep, Step)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.Action (Verb)
import Perspectives.Representation.Context (ContextKind)
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.TypeIdentifiers (RoleKind)

newtype ContextE = ContextE
  { id :: String
  , kindOfContext :: ContextKind
  , contextParts :: List ContextPart
  , pos :: ArcPosition}

type Prefix = String
type ModelName = String

data ContextPart = RE RoleE | CE ContextE | PREFIX Prefix ModelName | ContextAspect String ArcPosition | IndexedContext String ArcPosition

newtype RoleE = RoleE
  { id :: String
  , kindOfRole :: RoleKind
  , roleParts :: List RolePart
  , pos :: ArcPosition}

type FunctionName = String
type ComputedType = String

-- TODO: het verschil tussen conjunctie en disjunctie bij FilledByAttribute.
data RolePart = PE PropertyE | PRE PerspectiveE | VE ViewE | FunctionalAttribute Boolean | MandatoryAttribute Boolean | FilledByAttribute String | Calculation Step | ForUser String | RoleAspect String ArcPosition | Computation FunctionName (List Step) ComputedType | IndexedRole String ArcPosition

newtype PropertyE = PropertyE
  { id :: String
  , range :: Maybe Range
  , propertyParts :: List PropertyPart
  , pos :: ArcPosition
  }

data PropertyPart = FunctionalAttribute' Boolean | MandatoryAttribute' Boolean | Calculation' Step

newtype PerspectiveE = PerspectiveE
  { id :: String
  , perspectiveParts :: List PerspectivePart
  , pos :: ArcPosition}

data PerspectivePart = Object String | DefaultView String | Act ActionE | Rule RuleE

newtype ActionE = ActionE
  { id :: String
  , verb :: Verb
  , actionParts :: List ActionPart
  , pos :: ArcPosition
  }

data ActionPart = IndirectObject String | SubjectView String | ObjectView String | IndirectObjectView String | Condition Step | AssignmentPart Assignment | LetPart LetStep

data RuleE = RuleE Step Assignment

newtype ViewE = ViewE
  { id :: String
  , viewParts :: List String
  , pos :: ArcPosition}

derive instance genericContextE :: Generic ContextE _
instance showContextE :: Show ContextE where show = genericShow

derive instance genericContextElement :: Generic ContextPart _
instance showContextElement :: Show ContextPart where show x = genericShow x

derive instance genericRoleE :: Generic RoleE _
instance showRoleE :: Show RoleE where show = genericShow

derive instance genericRoleElement :: Generic RolePart _
instance showRoleElement :: Show RolePart where show = genericShow

derive instance genericPropertyE :: Generic PropertyE _
instance showPropertyE :: Show PropertyE where show = genericShow

derive instance genericPropertyElement :: Generic PropertyPart _
instance showPropertyElement :: Show PropertyPart where show = genericShow

derive instance genericPerspectiveElement :: Generic PerspectiveE _
instance showPerspectiveElement :: Show PerspectiveE where show = genericShow

derive instance genericPerspectivePart :: Generic PerspectivePart _
instance showPerspectivePart :: Show PerspectivePart where show = genericShow

derive instance genericRuleElement :: Generic RuleE _
instance showRuleElement :: Show RuleE where show = genericShow

derive instance genericActionElement :: Generic ActionE _
instance showActionElement :: Show ActionE where show = genericShow

derive instance genericActionPart :: Generic ActionPart _
instance showActionPart :: Show ActionPart where show = genericShow

derive instance genericViewElement :: Generic ViewE _
instance showViewElement :: Show ViewE where show = genericShow
