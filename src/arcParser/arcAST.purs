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

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Foreign (unsafeToForeign) 
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (Statements)
import Perspectives.Persistent.PublicStore (PublicStore)
import Perspectives.Repetition (Duration, Repeater)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.Context (ContextKind)
import Perspectives.Representation.ExplicitSet (ExplicitSet)
import Perspectives.Representation.Range (Range)
 
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleKind, RoleType, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerbList)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Simple.JSON (class ReadForeign, class WriteForeign, read', readJSON', writeImpl, writeJSON)

--------------------------------------------------------------------------------
---- CONTEXT
--------------------------------------------------------------------------------
newtype ContextE = ContextE
  { id :: String
  , kindOfContext :: ContextKind
  , public :: Maybe PublicStore
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
  IndexedContext String ArcPosition |
  CSQP (List StateQualifiedPart) |
  AspectRole String RoleKind ArcPosition

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
  FilledBySpecifications FilledBySpecification |
  Calculation Step Boolean |
  RoleAspect String ArcPosition (Maybe PropertyMapping) |
  IndexedRole String ArcPosition |
  ROLESTATE StateE |
  Screen ScreenE |
  PublicUrl Step

data FilledBySpecification = Alternatives (NonEmptyList FilledByAttribute) | Combination (NonEmptyList FilledByAttribute)
data FilledByAttribute = FilledByAttribute String ContextType
--------------------------------------------------------------------------------
---- PROPERTYMAPPING
--------------------------------------------------------------------------------
newtype PropertyMapping = PropertyMapping (List (Tuple String String))

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
  , propertyFacets :: List PropertyFacet
  , pos :: ArcPosition
  }

data PropertyPart =
    FunctionalAttribute' Boolean
  | MandatoryAttribute' Boolean
  | SelfonlyAttribute
  | AuthoronlyAttribute
  -- The Boolean indicates whether the calculation can be considered to be functional.
  | Calculation' Step Boolean
  | Ran Range

data PropertyFacet =
  MinLength Int
  | MaxLength Int
  | Pattern RegExP String
  | WhiteSpace WhiteSpaceRegime
  | Enumeration (Array String)
  | MaxInclusive String
  | MinInclusive String
  | MaxExclusive String
  | MinExclusive String
  | TotalDigits Int
  | FractionDigits Int

instance WriteForeign PropertyFacet where
  writeImpl pf = case pf of
    (MinLength i) -> writeImpl { constructor: "MinLength", args: writeJSON i}
    (MaxLength i) -> writeImpl { constructor: "MaxLength", args: writeJSON i}
    (Pattern regExp s) -> writeImpl {constructor: "Pattern", args: writeJSON {regExp, s}}
    WhiteSpace wsr -> writeImpl {constructor: "WhiteSpace", args: writeJSON wsr}
    Enumeration ss -> writeImpl {constructor: "Enumeration", args: writeJSON ss}
    MaxInclusive s -> writeImpl {constructor: "MaxInclusive", args: writeJSON s}
    MinInclusive s -> writeImpl {constructor: "MinInclusive", args: writeJSON s}
    MaxExclusive s -> writeImpl {constructor: "MaxExclusive", args: writeJSON s}
    MinExclusive s -> writeImpl {constructor: "MinExclusive", args: writeJSON s}
    TotalDigits i -> writeImpl {constructor: "TotalDigits", args: writeJSON i}
    FractionDigits i -> writeImpl {constructor: "FractionDigits", args: writeJSON i}

instance ReadForeign PropertyFacet where
  readImpl f = do
    {constructor, args} :: {constructor :: String, args :: String} <- read' f
    unsafePartial case constructor of 
      "MinLength" -> MinLength <$> readJSON' args
      "MaxLength" -> MaxLength <$> readJSON' args
      "Pattern" -> do
        {regExp, s} :: {regExp :: RegExP, s :: String} <- readJSON' args
        pure $ Pattern regExp s
      "WhiteSpace" -> WhiteSpace <$> readJSON' args
      "Enumeration" -> Enumeration <$> readJSON' args
      "MaxInclusive" -> MaxInclusive <$> readJSON' args
      "MinInclusive" -> MinInclusive <$> readJSON' args
      "MaxExclusive" -> MaxExclusive <$> readJSON' args
      "MinExclusive" -> MinExclusive <$> readJSON' args
      "TotalDigits" -> TotalDigits <$> readJSON' args
      "FractionDigits" -> FractionDigits <$> readJSON' args

data WhiteSpaceRegime =
  -- No normalization is done, the value is not changed (this is the behavior required by [XML 1.0 (Second Edition)] for element content)
  Preserve
  -- All occurrences of #x9 (tab), #xA (line feed) and #xD (carriage return) are replaced with #x20 (space)
  | Replace
  -- After the processing implied by replace, contiguous sequences of #x20's are collapsed to a single #x20, and leading and trailing #x20's are removed.
  | Collapse

instance WriteForeign WhiteSpaceRegime where writeImpl = unsafeToForeign <<< show
instance ReadForeign WhiteSpaceRegime where readImpl = enumReadForeign
--------------------------------------------------------------------------------
---- STATEQUALIFIEDPART
--------------------------------------------------------------------------------
-- | In all these types, "subject" (and "user" in NotificationE) may be both qualified and unqualified (this happens when used as a reference).
-- | However, it is an error for subject to be insufficiently qualified for it to be unique in the model.
data StateQualifiedPart =
  R RoleVerbE |
  P PropertyVerbE |
  AC ActionE |
  CA ContextActionE |
  SO SelfOnly |
  PO AuthorOnly |
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
  , propertyVerbs :: ExplicitSet PropertyVerb
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
---- CONTEXTACTION
--------------------------------------------------------------------------------
newtype ContextActionE = ContextActionE
  { id :: String
  , subject :: RoleIdentification
  , object :: ContextType
  , state :: StateSpecification
  , effect :: Statements
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- SELFONLY
--------------------------------------------------------------------------------
-- Ends up in Perspective, identified by subject and object.
newtype SelfOnly = SelfOnly
  { subject :: RoleIdentification
  , object :: RoleIdentification
  , state :: StateSpecification
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- AUTHORONLY
--------------------------------------------------------------------------------
-- Ends up in Perspective, identified by subject and object.
newtype AuthorOnly = AuthorOnly
  { subject :: RoleIdentification
  , object :: RoleIdentification
  , state :: StateSpecification
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
  , message :: SentenceE
  -- , level :: NotificationLevel
  , object :: Maybe RoleIdentification
  , startMoment :: Maybe Duration
  , endMoment :: Maybe Duration
  , repeats :: Repeater
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- SENTENCE
--------------------------------------------------------------------------------
newtype SentenceE = SentenceE {parts :: Array SentencePartE, sentence :: String}
data SentencePartE =
    HRpart String
  | CPpart Step

--------------------------------------------------------------------------------
---- AUTOMATICEFFECT
--------------------------------------------------------------------------------
-- Ends up in State, identified by the fully qualified name in StateTransitionE.
newtype AutomaticEffectE = AutomaticEffectE
  { subject :: RoleIdentification
  , object :: Maybe RoleIdentification
  , transition :: StateTransitionE
  , effect :: Statements
  , startMoment :: Maybe Duration
  , endMoment :: Maybe Duration
  , repeats :: Repeater
  , start :: ArcPosition
  , end :: ArcPosition
  }

data StateTransitionE = Entry StateSpecification | Exit StateSpecification

data PropsOrView = AllProperties | Properties (List String) | View String
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

roleIdentification2context :: RoleIdentification -> ContextType
roleIdentification2context (ExplicitRole ct _ _) = ct
roleIdentification2context (ImplicitRole ct _) = ct

derive instance genericRoleIdentification :: Generic RoleIdentification _
instance eqRoleIdentification :: Eq RoleIdentification where eq = genericEq
instance showRoleIdentification :: Show RoleIdentification where 
  show (ExplicitRole _ rt _) = roletype2string rt 
  show (ImplicitRole _ s) = "a query"
instance prettyPrintRoleIdentification :: PrettyPrint RoleIdentification where
  prettyPrint' tab (ExplicitRole ct rt pos) = tab <> "ExplicitRole " <> show ct <> " " <> show rt
  prettyPrint' tab (ImplicitRole ct step) = tab <> "ImplicitRole " <> show ct <> "\n" <> (prettyPrint' (tab <> "  ") step)

--------------------------------------------------------------------------------
---- STATESPECIFICATION
--------------------------------------------------------------------------------
type StateLocalName = String

-- | A StateSpecification identifies a base which represents either a context- or role type,
-- | and a path of segments that identify a substate of the (root state of) that type.
-- | NOTE: since we allow reference to aspect states, the SegmentedPath may be a qualified name, too (possibly a prefixed name).
data StateSpecification =
    ContextState ContextType (Maybe SegmentedPath)
  | SubjectState RoleIdentification (Maybe SegmentedPath)
  | ObjectState RoleIdentification (Maybe SegmentedPath)

type SegmentedPath = String

derive instance genericStateSpecification :: Generic StateSpecification _
instance eqStateSpecification :: Eq StateSpecification where eq = genericEq
instance showStateSpecification :: Show StateSpecification where 
  show (ContextState ctype mpath) = "context state " <> unwrap ctype <> showPath mpath
  show (SubjectState r mpath) = "subject state " <> show r <> showPath mpath
  show (ObjectState r mpath) = "subject state " <> show r <> showPath mpath

showPath :: Maybe String -> String
showPath = maybe "" (append "$")
--------------------------------------------------------------------------------
---- SCREEN
--------------------------------------------------------------------------------
newtype ScreenE = ScreenE
  { title :: Maybe String
  , tabs :: Maybe (List TabE)
  , rows :: Maybe (List RowE)
  , columns :: Maybe (List ColumnE)
  , subject :: RoleIdentification
  , context :: ContextType
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- SCREENELEMENT
--------------------------------------------------------------------------------
data ScreenElement =
    RowElement RowE
  | ColumnElement ColumnE
  | TableElement TableE
  | FormElement FormE
  | MarkDownElement MarkDownE
  | ChatElement ChatE

--------------------------------------------------------------------------------
---- TAB, ROW, COLUMN
--------------------------------------------------------------------------------
-- | The String is the tab title. 
-- | When the Boolean is true, it is the default tab.
data TabE = TabE String Boolean (List ScreenElement)
newtype RowE = RowE (List ScreenElement)
newtype ColumnE = ColumnE (List ScreenElement)

--------------------------------------------------------------------------------
---- FORM
--------------------------------------------------------------------------------
newtype FormE = FormE WidgetCommonFields

type WidgetCommonFields =
  { title :: Maybe String
  -- Only the ExplicitRole constructor is allowed!
  , perspective :: RoleIdentification
  , propsOrView :: PropsOrView
  -- Must be a subset of the propertyVerbs of the perspective
  , propertyVerbs :: ExplicitSet PropertyVerb
  -- Must be a subset of the roleVerbs of the perspective
  , roleVerbs :: Maybe RoleVerbList
  , start :: ArcPosition
  , end :: ArcPosition
  }

--------------------------------------------------------------------------------
---- TABLE
--------------------------------------------------------------------------------
newtype TableE = TableE WidgetCommonFields

--------------------------------------------------------------------------------
---- MARKDOWN
--------------------------------------------------------------------------------
data MarkDownE = 
  MarkDownConstant {text :: Step, condition :: Maybe Step, context :: ContextType, start :: ArcPosition, end :: ArcPosition} |
  MarkDownPerspective {widgetFields :: WidgetCommonFields, condition :: Maybe String, start :: ArcPosition, end :: ArcPosition} |
  MarkDownExpression {text :: Step, condition :: Maybe Step, context :: ContextType, start :: ArcPosition, end :: ArcPosition} 

--------------------------------------------------------------------------------
---- CHAT
--------------------------------------------------------------------------------
newtype ChatE = ChatE 
  { chatRole :: RoleIdentification
  , messagesProperty :: String
  , mediaProperty :: String
  , start :: ArcPosition
  , end :: ArcPosition
  }

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

derive instance Generic FilledBySpecification _
instance Show FilledBySpecification where show = genericShow

derive instance Generic FilledByAttribute _
instance Show FilledByAttribute where show = genericShow

derive instance Generic PropertyMapping _
instance Show PropertyMapping where show = genericShow

derive instance genericStateE :: Generic StateE _
instance showStateE :: Show StateE where show s = genericShow s

derive instance genericStateQualifiedPart :: Generic StateQualifiedPart _
instance showStateQualifiedPart :: Show StateQualifiedPart where show = genericShow

derive instance genericNotificationE :: Generic NotificationE _
instance showNotificationE :: Show NotificationE where show = genericShow

derive instance Generic SentenceE _
instance Show SentenceE where show = genericShow

derive instance Generic SentencePartE _
instance Show SentencePartE where show = genericShow

derive instance genericStateTransitionE :: Generic StateTransitionE _
instance showStateTransitionE :: Show StateTransitionE where show = genericShow

derive instance genericAutomaticEffectE :: Generic AutomaticEffectE _
instance showAutomaticEffectE :: Show AutomaticEffectE where show = genericShow

derive instance genericRoleVerbE :: Generic RoleVerbE _
instance showRoleVerbE :: Show RoleVerbE where show = genericShow
derive instance newtypeRoleVerbE :: Newtype RoleVerbE _

derive instance genericPropertyVerbE :: Generic PropertyVerbE _
instance showPropertyVerbE :: Show PropertyVerbE where show = genericShow
derive instance newtypePropertyVerbE :: Newtype PropertyVerbE _

derive instance genericSelfOnly :: Generic SelfOnly _
instance showSelfOnly :: Show SelfOnly where show = genericShow

derive instance Generic AuthorOnly _
instance Show AuthorOnly where show = genericShow

derive instance genericPropertyE :: Generic PropertyE _
instance showPropertyE :: Show PropertyE where show = genericShow

derive instance genericPropertyElement :: Generic PropertyPart _
instance showPropertyElement :: Show PropertyPart where show = genericShow

derive instance genericPropertyFacet :: Generic PropertyFacet _
instance showPropertyFacet :: Show PropertyFacet where show = genericShow

derive instance genericWhiteSpaceRegime :: Generic WhiteSpaceRegime _
instance showWhiteSpaceRegime :: Show WhiteSpaceRegime where show = genericShow

derive instance genericPropOrView :: Generic PropsOrView _
instance showPropOrView :: Show PropsOrView where show = genericShow

derive instance genericActionElement :: Generic ActionE _
instance showActionElement :: Show ActionE where show = genericShow

derive instance genericContextActionElement :: Generic ContextActionE _
instance showContextActionElement :: Show ContextActionE where show = genericShow

derive instance genericViewElement :: Generic ViewE _
instance showViewElement :: Show ViewE where show = genericShow

derive instance genericScreen :: Generic ScreenE _
instance showScreen :: Show ScreenE where show = genericShow

derive instance genericScreenElement :: Generic ScreenElement _
instance showScreenElement :: Show ScreenElement where
  show e = genericShow e

derive instance genericTabE :: Generic TabE _
instance showTabE :: Show TabE where show = genericShow

derive instance genericRow :: Generic RowE _
instance showRow :: Show RowE where show = genericShow

derive instance genericColumn :: Generic ColumnE _
instance showColumn :: Show ColumnE where show = genericShow

derive instance genericTable :: Generic TableE _
instance showTable :: Show TableE where show = genericShow
derive instance newtypeTableE :: Newtype TableE _

derive instance genericForm :: Generic FormE _
instance showForm :: Show FormE where show = genericShow

derive instance Generic MarkDownE _
instance Show MarkDownE where show = genericShow

derive instance Generic ChatE _
instance Show ChatE where show = genericShow
derive instance Newtype ChatE _
