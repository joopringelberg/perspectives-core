module Perspectives.Parsing.Arc.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Perspectives.Representation.EnumeratedProperty (Range)
import Perspectives.Representation.TypeIdentifiers (RoleKind)

newtype ContextE = ContextE
  { id :: String
  , kindOfContext :: ContextKind
  , contextParts :: List ContextPart}

data ContextKind = Domain | Case | Party | Activity | State

data ContextPart = RE RoleE | CE ContextE

newtype RoleE = RoleE
  { id :: String
  , kindOfRole :: RoleKind
  , roleParts :: List RolePart}

data RolePart = PE PropertyE | PRE PerspectiveE | VE ViewE | FA FunctionalAttribute | MA MandatoryAttribute | FBA FilledByAttribute

newtype FunctionalAttribute = FunctionalAttribute Boolean

newtype MandatoryAttribute = MandatoryAttribute Boolean

newtype FilledByAttribute = FilledByAttribute String

newtype PropertyE = PropertyE
  { id :: String
  , range :: Range
  , propertyParts :: List PropertyPart
  }

data PropertyPart = FA' FunctionalAttribute | MA' MandatoryAttribute

newtype PerspectiveE = PerspectiveE
  { object :: String
  , perspectiveParts :: List ActionE}

newtype ActionE = ActionE
  { id :: String
  , subject :: String
  , verb :: String
  , object :: String
  , indirectObject :: String}

newtype ViewE = ViewE
  { id :: String
  , viewParts :: List String}

derive instance genericContextE :: Generic ContextE _
instance showContextE :: Show ContextE where show = genericShow

derive instance genericContextKind :: Generic ContextKind _
instance showContextKind :: Show ContextKind where show = genericShow
derive instance eqContextKind :: Eq ContextKind

derive instance genericContextElement :: Generic ContextPart _
instance showContextElement :: Show ContextPart where show x = genericShow x

derive instance genericRoleE :: Generic RoleE _
instance showRoleE :: Show RoleE where show = genericShow

derive instance genericRoleElement :: Generic RolePart _
instance showRoleElement :: Show RolePart where show = genericShow

derive instance genericFunctionalAttribute :: Generic FunctionalAttribute _
instance showFunctionalAttribute :: Show FunctionalAttribute where show = genericShow

derive instance genericMandatoryAttribute :: Generic MandatoryAttribute _
instance showMandatoryAttribute :: Show MandatoryAttribute where show = genericShow

derive instance genericeFilledByAttribute :: Generic FilledByAttribute _
instance showFilledByAttribute :: Show FilledByAttribute where show = genericShow

derive instance genericPropertyE :: Generic PropertyE _
instance showPropertyE :: Show PropertyE where show = genericShow

derive instance genericPropertyElement :: Generic PropertyPart _
instance showPropertyElement :: Show PropertyPart where show = genericShow

derive instance genericPerspectiveElement :: Generic PerspectiveE _
instance showPerspectiveElement :: Show PerspectiveE where show = genericShow

derive instance genericActionElement :: Generic ActionE _
instance showActionElement :: Show ActionE where show = genericShow

derive instance genericViewElement :: Generic ViewE _
instance showViewElement :: Show ViewE where show = genericShow
