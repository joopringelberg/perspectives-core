module Perspectives.Parsing.Arc.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.Context (ContextKind)
import Perspectives.Representation.EnumeratedProperty (Range)
import Perspectives.Representation.TypeIdentifiers (RoleKind)

newtype ContextE = ContextE
  { id :: String
  , kindOfContext :: ContextKind
  , contextParts :: List ContextPart
  , pos :: ArcPosition}

data ContextPart = RE RoleE | CE ContextE

newtype RoleE = RoleE
  { id :: String
  , kindOfRole :: RoleKind
  , roleParts :: List RolePart
  , pos :: ArcPosition}

-- TODO: het verschil tussen conjunctie en disjunctie bij binding.
data RolePart = PE PropertyE | PRE PerspectiveE | VE ViewE | FunctionalAttribute Boolean | MandatoryAttribute Boolean | FilledByAttribute String | Calculation String | ForUser String

newtype PropertyE = PropertyE
  { id :: String
  , range :: Range
  , propertyParts :: List PropertyPart
  , pos :: ArcPosition
  }

data PropertyPart = FunctionalAttribute' Boolean | MandatoryAttribute' Boolean | Calculation' String

newtype PerspectiveE = PerspectiveE
  { id :: String
  , perspectiveParts :: List PerspectivePart
  , pos :: ArcPosition}

data PerspectivePart = Object String | DefaultView String | Act ActionE

newtype ActionE = ActionE
  { id :: String
  , verb :: String
  , actionParts :: List ActionPart
  , pos :: ArcPosition
  }

data ActionPart = IndirectObject String | SubjectView String | ObjectView String | IndirectObjectView String

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

derive instance genericActionElement :: Generic ActionE _
instance showActionElement :: Show ActionE where show = genericShow

derive instance genericActionPart :: Generic ActionPart _
instance showActionPart :: Show ActionPart where show = genericShow

derive instance genericViewElement :: Generic ViewE _
instance showViewElement :: Show ViewE where show = genericShow
