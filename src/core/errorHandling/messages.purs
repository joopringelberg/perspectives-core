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

module Perspectives.Parsing.Messages where

-- | A Perspectives sourcefile (text or diagram) will be parsed in two passes.
-- | The resulting internal representation of types is type-checked.
-- | During all three phases of transformation, errors may be detected.
-- | This module defines the structure and kind of these errors.

import Control.Monad.Except (ExceptT, throwError) as Except
import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Parsing.Arc.Expression.AST (PureLetStep(..), Step)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (LetStep(..))
import Perspectives.Query.QueryTypes (Domain, QueryFunctionDescription, RoleInContext, Range)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Range (Range) as RAN
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, CalculatedRoleType, ContextType, DomeinFileId, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType, StateIdentifier, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Utilities (prettyPrint)
import Prelude (class Eq, class Show, show, (<<<), (<>))

-- | A Perspectives sourcefile (text or diagram) will be parsed in two passes.
-- | The resulting internal representation of types is type-checked.
-- | During all three phases of transformation, errors may be detected.
-- | This module defines the structure and kind of these errors.

-----------------------------------------------------------
-- COLLECTING ERRORS WHILE TRANSFORMING PERSPECTIVES SOURCES
-----------------------------------------------------------
-- | A type for Perspectives errors
-- | Each error should have an ArcPosition.
data PerspectivesError
  = DefaultPrototype ContextType ContextType
    | CyclicAspects ArcPosition ContextType
    | WrongRoleKind RoleType RoleKind RoleKind
    -- | MissingForUser ArcPosition String
    | MissingRoleForPropertyAssignment ArcPosition ArcPosition
    | NotWellFormedName ArcPosition String
    | RoleMissingInContext ArcPosition String String
    | UnknownContext ArcPosition String
    | UnknownRole ArcPosition String
    | UnknownState ArcPosition String
    | UnknownProperty ArcPosition String String
    | UnknownView ArcPosition String
    | NotAViewOfObject ArcPosition String
    | NotUniquelyIdentifying ArcPosition String (Array String)
    | UnknownElementaryQueryStep
    | IncompatibleQueryArgument ArcPosition Domain Step
    | ContextHasNoRole (ADT ContextType) String ArcPosition ArcPosition
    | RoleHasNoProperty (ADT EnumeratedRoleType) String ArcPosition ArcPosition
    | RoleHasNoEnumeratedProperty (ADT EnumeratedRoleType) String ArcPosition ArcPosition
    | UniversalRoleHasNoParts
    | RoleHasNoBinding ArcPosition (ADT EnumeratedRoleType)
    | RoleCannotHaveBinding ArcPosition ArcPosition String
    | IncompatibleDomainsForJunction Domain Domain
    | RoleDoesNotBind ArcPosition RoleType (ADT RoleInContext)
    | LocalRoleDoesNotBind ArcPosition ArcPosition String (ADT RoleInContext)
    | IncompatibleComposition ArcPosition Range Domain
    | IncompatibleDomains ArcPosition ArcPosition
    | TypesCannotBeCompared ArcPosition Range Range
    | NotABoolean ArcPosition
    | WrongTypeForOperator ArcPosition (Array RAN.Range) Domain
    | MissingValueForAssignment ArcPosition ArcPosition
    | ArgumentMustBeSequenceFunction ArcPosition
    | UnknownVariable ArcPosition String
    | NotALetWithAssignment PureLetStep
    | NotAPureLet LetStep
    | CannotCreateCalculatedRole CalculatedRoleType ArcPosition ArcPosition
    | CannotCreateCalculatedProperty CalculatedPropertyType ArcPosition ArcPosition
    | CannotModifyCalculatedProperty String ArcPosition ArcPosition
    | NotAContextDomain QueryFunctionDescription Domain ArcPosition ArcPosition
    | NotARoleDomain Domain ArcPosition ArcPosition
    | ValueExpressionNotAllowed Domain ArcPosition ArcPosition
    | NotAStringDomain QueryFunctionDescription ArcPosition ArcPosition
    | DomainTypeRequired String Range ArcPosition ArcPosition
    | NotAContextRole ArcPosition ArcPosition
    | NotFunctional ArcPosition ArcPosition Step
    | MaybeNotFunctional ArcPosition ArcPosition Step
    | WrongPropertyRange ArcPosition ArcPosition RAN.Range RAN.Range
    | NotAPropertyRange  ArcPosition ArcPosition RAN.Range
    | WrongNumberOfArguments ArcPosition ArcPosition String Int Int
    | UnknownExternalFunction ArcPosition ArcPosition String
    | NotAnEffect ArcPosition ArcPosition String
    | NotAFunction ArcPosition ArcPosition String
    | CannotFindContextType ArcPosition ArcPosition String
    | NoPropertyTypeWithValue ArcPosition ArcPosition
    | CurrentObjectNotAllowed ArcPosition ArcPosition
    | CurrentSubjectNotAllowed ArcPosition ArcPosition
    | FillerRestrictionNotAnAspectSubtype ArcPosition ArcPosition String String
    | RecursiveDefinition String
    | ExpressionsShouldBeFunctional Boolean Boolean ArcPosition
    | NoMonths ArcPosition
    | NotARootContext ArcPosition ArcPosition ContextType
    | PropertyCannotBeCalculated String ArcPosition ArcPosition
    | NotASelfPerspective ArcPosition ArcPosition

    | UnauthorizedForProperty String RoleType RoleType PropertyType PropertyVerb (Maybe ArcPosition) (Maybe ArcPosition)
    | UnauthorizedForRole String RoleType RoleType (Array RoleVerb) (Maybe ArcPosition) (Maybe ArcPosition)
    | UnauthorizedForContext String RoleType ContextType

    | MissingPerspective
    | UserHasNoPerspective RoleType RoleType ArcPosition ArcPosition
    | PerspectiveCannotBeAuthorOnly String ArcPosition EnumeratedRoleType ArcPosition EnumeratedPropertyType
    | SelfOnlyNeedsTwoRoles String ArcPosition EnumeratedPropertyType
    | SelfOnlyShouldBeAuthorOnly String ArcPosition EnumeratedPropertyType
    | StateDoesNotExist StateIdentifier ArcPosition ArcPosition

    | RolErrorBoundary String String
    | ContextErrorBoundary String String
    | TypeErrorBoundary String String
    | DomeinFileErrorBoundary String String
    | ApiErrorBoundary String
    | RuleErrorBoundary String String
    | ParserError String ArcPosition
    | MissingObject ArcPosition ArcPosition
    | NoCalculatedAspect ArcPosition String

    | DomeinFileIdIncompatible DomeinFileId DomeinFileId ArcPosition
    | PerspectivesFileFormatError String String

    -- Screens
    | ScreenForUserRoleOnly ArcPosition ArcPosition
    | WidgetCardinalityMismatch ArcPosition ArcPosition
    | MarkDownExpressionMustBeFunctional ArcPosition ArcPosition
    | UnknownMarkDownConditionProperty ArcPosition ArcPosition String RoleType

    | Custom String

derive instance eqPerspectivesError :: Eq PerspectivesError

instance showPerspectivesError :: Show PerspectivesError where
  show (DefaultPrototype expected given) = "Invalid type for DefaultPrototype. Expected: '" <> unwrap expected <> "' but found '" <> unwrap given <> "'."
  show (CyclicAspects pos c) = "Context '" <> unwrap c <> "' has cyclic aspects: " <> show pos
  show (WrongRoleKind roletype expected found) = "Role '" <> show roletype <> "' has kind '" <> show found <> "' but should have kind '" <> show expected<> "'."
  -- show (MissingForUser pos localBotName) = "(MissingForUser) The BotRole '" <> localBotName <> "' should have a 'ForUser' clause: " <> show pos
  show (MissingRoleForPropertyAssignment start end) = "(MissingRoleForPropertyAssignment) The role for this property assignment is not specified (between " <> show start <> "and " <> show end <> ")."
  show (NotWellFormedName pos name) = "(NotWellFormedName) The name '" <> name <> "' is not well-formed (it cannot be expanded to a fully qualified name): " <> show pos
  show (RoleMissingInContext pos localRoleName ctxt) = "(RoleMissingInContext) The local role name '" <> localRoleName <> "' cannot be found in the context: '" <> ctxt <> "', at: " <> show pos
  show (UnknownRole pos qname) = "(UnknownRole) The role '" <> qname <> "' is not defined, at: " <> show pos
  show (UnknownState pos qname) = "(UnknownState) The state '" <> qname <> "' is not defined, at: " <> show pos
  show (UnknownProperty pos qname roleType) = "(UnknownProperty) The property '" <> qname <> "' is not defined for role '" <> roleType <> "', at: " <> show pos
  show (UnknownContext pos qname) = "(UnknownContext) The context '" <> qname <> "' is not defined, at: " <> show pos
  show (UnknownView pos qname) = "(UnknownView) The view '" <> qname <> "' is not defined, at: " <> show pos
  show (NotAViewOfObject pos qname) = "(NotAViewOfObject) The view '" <> qname <> "' is not a view of the current object, at: " <> show pos
  show (NotUniquelyIdentifying pos lname alts) = "(NotUniquelyIdentifying) The local name '" <> lname <> "' does not uniquely identify a resource. Choose one from: " <> show alts <> ", at: " <> show pos <> ", or choose a resource from another context."
  show (Custom s) = s
  show (ParserError message pos) = "(ParserError) " <> message <> show pos
  -- TODO: Als extra kunnen we de Constructors hieronder voorzien van ArcPosition.
  show (MissingObject start end) = "(MissingObject) The expression contains a reference to the 'currentobject' variable but there is no current object in scope (between " <> show start <> " and " <> show end <> ")"
  show (UnknownElementaryQueryStep) = "(UnknownElementaryQueryStep) This step is unknown"
  show (IncompatibleQueryArgument pos dom step) = "(IncompatibleQueryArgument) Cannot get " <> show step <> " from " <> show dom <> ", at: " <> show pos
  show (NoCalculatedAspect start calculatedRoleName) = "(NoCalculatedAspect) The role '" <> calculatedRoleName <> "' is implied to be an aspect (by providing an explicit context type) but CalculatedRoles cannot be used as an Aspect."
  show (DomeinFileIdIncompatible manifestDfid sourceDfid start) = "(DomeinFileIdIncompatible) The manifest is for " <> show manifestDfid <> " while the arc source mentions " <> show sourceDfid <> "."
  show (PerspectivesFileFormatError pfileString errorString) = "(PerspectivesFileFormatError) This string cannot be parsed as a valid PFile value: '" <> pfileString <> "', because: " <> errorString
  show (ScreenForUserRoleOnly start end) = "(ScreenForUserRoleOnly) Only a user role may contain a screen definition!"
  show (WidgetCardinalityMismatch start end) = "(WidgetCardinalityMismatch) The cardinality of the Widget and the Role do not suit each other (between " <> show start <> " and " <> show end <> ")."
  show (MarkDownExpressionMustBeFunctional start end) = "(MarkDownExpressionMustBeFunctional) The expression used here to supply MarkDown must be functional! Between " <> show start <> " and " <> show end <> ")."
  show (UnknownMarkDownConditionProperty start end propname roletype) = "(UnknownMarkDownConditionProperty) The property " <> propname <> " is not a property of the role " <> show roletype <> "! Between " <> show start <> " and " <> show end <> ")." 
  show (ContextHasNoRole ctype qn start end) = "(ContextHasNoRole) The Context-type '" <> show ctype <> "' has no enumerated role with the name '" <> qn <> "' (it may have a calculated role but that cannot be used here). Between " <> show start <> " and " <> show end
  show (RoleHasNoProperty rtype qn start end) = "(RoleHasNoProperty) The Role-type '" <> show rtype <> "' has no property with the name '" <> qn <> "' (between " <> show start <> " and " <> show end <> ")."
  show (RoleHasNoEnumeratedProperty rtype qn start end) = "(RoleHasNoEnumeratedProperty) The Role-type '" <> show rtype <> "' has no enumerated property with the name '" <> qn <> "' (between " <> show start <> " and " <> show end <> "). It may have a calculated property, but we cannot use that in this situation."
  show UniversalRoleHasNoParts = "(UniversalRoleHasNoParts) 'NoBinding' gives no access to properties, aspects, binding, etc."
  show (RoleHasNoBinding pos rtype) = "(RoleHasNoBinding) The role '" <> show rtype <> "' has no binding. If it is a Sum-type, one of its members may have no binding " <> show pos
  show (RoleCannotHaveBinding start end roletype) = "(RoleCannotHaveBinding) External Roles cannot have a binding. " <> roletype <> ", from " <> show start <> " to " <> show end
  show (IncompatibleDomainsForJunction dom1 dom2) = "(IncompatibleDomainsForJunction) These two domains cannot be joined in a disjunction of conjunction: '" <> show dom1 <> "', '" <> show dom2 <> "'."
  show (RoleDoesNotBind pos rtype adt) = "(RoleDoesNotBind) The role '" <> show rtype <> "' does not bind roles of type '" <> show adt <> "' (it may have an aspect that is stricter than its own filler restricion!), at: " <> show pos
  show (LocalRoleDoesNotBind start end lname adt) = "(LocalRoleDoesNotBind) The roles that name '" <> lname <> "' (from " <> show start <> " to " <> show end <> ") can be matched to, do not bind '" <> show adt <> "'."
  show (IncompatibleComposition pos left right) = "(IncompatibleComposition) The result of the left operand (" <> show left <> ") and the argument of the right operand (" <> show right <> ") are incompatible."
  show (IncompatibleDomains pos1 pos2) = "(IncompatibleDomains) The result of the expression at " <> show pos1 <> " and " <> show pos2 <> " have incompatible domains (both must be contexts, roles or values)."
  show (TypesCannotBeCompared pos left right) = "(TypesCannotBeCompared) The result of the left operand (" <> show left <> ") and of the right operand (" <> show right <> ") are incompatible in: " <> show pos
  show (NotABoolean pos) = "(NotABoolean) The expression starting at " <> show pos <> " does not result in a boolean value."
  show (WrongTypeForOperator pos allowedOps dom) = "(WrongTypeForOperator) This operator requires its arguments to be (one of): " <> show allowedOps <> ", but was given: " <> show dom <> ", at position " <> show pos
  show (MissingValueForAssignment start end) = "(MissingValueForAssignment) This assignment statement needs a value expression on the right: from " <> show start <> " to " <> show end
  show (ArgumentMustBeSequenceFunction pos) = "(ArgumentMustBeSequenceFunction) The right operand of '>>=' must be a monoidal function such as sum, product, minimum, or maximum, at: " <> show pos
  show (UnknownVariable pos varName) = "(UnknownVariable) The variable '" <> varName <> "' is not known at position: " <> show pos
  show (NotALetWithAssignment (PureLetStep{start, end})) = "(NotALetWithAssignment) This let*-expression does not have an assignment in its body, hence is of no use in a rule. From " <> show start <> " to " <> show end
  show (NotAPureLet (LetStep{start, end})) = "(NotAPureLet) This let*-expression has an assignment in its body but it is used in a pure expression, so its body should be a pure expression, too. From " <> show start <> " to " <> show end
  show (CannotCreateCalculatedRole cr start end) = "(CannotCreateCalculatedRole) Can not create an instance of a calculated role (" <> show cr <> ") between: " <> show start <> " and: " <> show end
  show (CannotCreateCalculatedProperty pt start end) = "(CannotCreateCalculatedProperty) Can not change the value of a property that is calculated, between: " <> show start <> " and: " <> show end
  show (CannotModifyCalculatedProperty props start end) = "(CannotModifyCalculatedProperty) Can not change the value these calculated properties (" <> props <> "), between: " <> show start <> " and: " <> show end
  show (NotARoleDomain dom start end) = "(NotARoleDomain) This expression should have a role type: " <> show dom <> ", between " <> show start <> " and " <> show end
  show (ValueExpressionNotAllowed dom start end) = "(ValueExpressionNotAllowed) This expression has type: " <> show dom <> " but a Value is not allowed here " <> showPosition start end
  show (NotAStringDomain qfd start end) = "(NotAStringDomain) This expression should have a string type: " <> show qfd <> ", between " <> show start <> " and " <> show end
  show (DomainTypeRequired domains dom start end) = "(DomainTypeRequired) This expression should result in a " <> domains <> ", but instead is a " <> show dom <> showPosition start end
  show (NotAContextRole start end) = "(NotAContextRole) All role types in this expression should be context roles (binding the external role of a context), between " <> show start <> " and " <> show end
  show (NotAContextDomain qfd dom start end) = "(NotAContextDomain) This expression:\n" <> prettyPrint qfd <> "\nshould return a context type, but has instead: " <> show dom <> ", between " <> show start <> " and " <> show end
  show (NotFunctional start end qfd) = "(NotFunctional) This expression is not a single value, between " <> show start <> " and " <> show end
  show (MaybeNotFunctional start end qfd) = "(MaybeNotFunctional) This expression might not be a single value, between " <> show start <> " and " <> show end
  show (WrongPropertyRange start end expected received) = "(WrongPropertyRange) Expected the range '" <> show expected <> "', got '" <> show received <> "', between " <> show start <> " and " <> show end
  show (NotAPropertyRange start end expected) = "(NotAPropertyRange) Expression does not yield a property value. Expected the range '" <> show expected <> "', between " <> show start <> " and " <> show end
  show (WrongNumberOfArguments start end functionName nrExpected nrGiven) = "(WrongNumberOfArguments) The function '" <> functionName <> "' expects " <> show nrExpected <> " arguments but received " <> show nrGiven <> ", between " <> show start <> " and " <> show end
  show (UnknownExternalFunction start end functionName) = "(UnknownExternalFunction) The external function name '" <> functionName <> "' is unknown, between " <> show start <> " and " <> show end
  show (NotAnEffect start end functionName) = "(NotAnEffect) The external function '" <> functionName <> "' is not an effect but a function. Use 'callExternal' rather than 'callEffect'."
  show (NotAFunction start end functionName) = "(NotAFunction) The external function '" <> functionName <> "' is not a function but an effect. Use 'callEffect' rather than 'callExternal'."
  show (CannotFindContextType start end ctype) = "(CannotFindContextType) The context type name '" <> ctype <> "' cannot be resolved, between " <> show start <> " and " <> show end
  show (NoPropertyTypeWithValue start end) = "(NoPropertyTypeWithValue) The property value computed by this expression is not associated with a Property Type (this throws up a problem in combination with, for example, `modelname`)"
  show (CurrentObjectNotAllowed start end) = "(CurrentObjectNotAllowed) The variable `currentobject` is illegal in the expression between " <> show start <> " and " <> show end <> "."
  show (CurrentSubjectNotAllowed start end) = "(CurrentSubjectNotAllowed) The variable `currentsubject` is illegal in the expression between " <> show start <> " and " <> show end <> "."
  show (FillerRestrictionNotAnAspectSubtype rolePos aspectPos roleName aspectName) = "(FillerRestrictionNotAnAspectSubtype) The role " <> roleName <> " (at " <> show rolePos <> ") has a value for the filledBy clause that is not a subtype of that of its aspect " <> aspectName <> "(at " <> show aspectPos <> ")."
  show (RecursiveDefinition s) = "(RecursiveDefinition) " <> s
  show (ExpressionsShouldBeFunctional left right pos) = "(ExpressionsShouldBeFunctional) The cardinality of the left and right term at " <> show pos <> " should both be functional. The left is " <> showCardinality left <> " and the right is " <> showCardinality right <> "."
    where 
      showCardinality :: Boolean -> String 
      showCardinality b = if b then "functional" else "relational"
  show (NoMonths pos) = "(NoMonths) It is not allowed to subtract months from or add to a date. Try days or weeks instead."
  show (NotARootContext start end qualifiedContextTypeIdentifier) = "(NotARootContext) " <> (unwrap qualifiedContextTypeIdentifier) <> " is not a RootContext but you try to create it without filling a role with it. Consider using 'create_ context <ContextType> bound to <roleExpression>' between " <> show start <> " and " <> show end <> "."
  show (PropertyCannotBeCalculated prop start end) = "(PropertyCannotBeCalculated) This property cannot be calculated " <> showPosition start end
  show (NotASelfPerspective start end) = "(NotASelfPerspective) `selfOnly` is restricted to self-perspectives: a perspective for a user role on that user role. Consider `authorOnly` instead: it may be what you need " <> showPosition start end
  show (UnauthorizedForProperty author userRole role property verb start end) = "(UnauthorizedForProperty) User " <> author <> " in role " <> show userRole <> " has no perspective on role " <> show role <> " that includes " <> show verb <> " for property " <> show property <> maybeShowPosition start end
  show (UnauthorizedForRole author userRole role verbs mstart mend) = "(UnauthorizedForRole) User " <> author <> " in role " <> show userRole <> " has no perspective on role " <> show role <> " that includes at least one of " <> show verbs <> maybeShowPosition mstart mend
  show (UnauthorizedForContext author userRole contextType) = "(UnauthorizedForContext) User " <> author <> " in role " <> show userRole <> " has no perspective on context " <> show contextType


  show MissingPerspective = "(MissingPerspective) This should be inside a perspective expression."
  show (UserHasNoPerspective subject object start end) = "(UserHasNoPerspective) User " <> roletype2string subject <> " has no perspective on " <> roletype2string object <> " (between " <> show start <> " and " <> show end <> ")."
  show (PerspectiveCannotBeAuthorOnly userRoleName userRoleStart (EnumeratedRoleType objectRole) objectRoleStart (EnumeratedPropertyType prop)) = "(PerspectiveCannotBeAuthorOnly) `authoronly` on property '" <> prop <> "' of object '" <> objectRole <> "' (starting on " <> show objectRoleStart <> "), \
    \ is without meaning because user '" <> userRoleName <> "' (from " <> show userRoleStart <> ") is the only userrole with a perspective on it and the userrole is functional"
  show (SelfOnlyNeedsTwoRoles userRoleName userRoleStart (EnumeratedPropertyType prop)) = "(SelfOnlyNeedsTwoRoles) `selfonly` on property '" <> prop <> "' of user '" <> userRoleName <> "' (from " <> show userRoleStart <> ") \
    \ is meaningless because this users' perspective is the only perspective on it (consider `authoronly` instead)."
  show (SelfOnlyShouldBeAuthorOnly userRoleName userRoleStart (EnumeratedPropertyType prop)) = "(SelfOnlyShouldBeAuthorOnly) `selfonly` on property '" <> prop <> "' of user '" <> userRoleName <> "' (from " <> show userRoleStart <> ") \
    \ is meaningless because either '" <> userRoleName <> "' is functional or there is no other user role that can change the property. In both cases `authoronly` is more appropriate."
  show (StateDoesNotExist stateId start end) = "(StateDoesNotExist) The state '" <> show stateId <> "' is not modelled (between " <> show start <> " and " <> show end <> ")."

  show (RolErrorBoundary boundaryName err) = "(RolErrorBoundary) ErrorBoundary in '" <> boundaryName <> "' for PerspectRol (" <> err <> ")"
  show (ContextErrorBoundary boundaryName err) = "(ContextErrorBoundary) ErrorBoundary in '" <> boundaryName <> "' for PerspectContext (" <> err <> ")"
  show (TypeErrorBoundary boundaryName err) = "(TypeErrorBoundary) ErrorBoundary in '" <> boundaryName <> "' for type (" <> err <> ")"
  show (DomeinFileErrorBoundary boundaryName err) = "(DomeinFileErrorBoundary) ErrorBoundary in '" <> boundaryName <> "' for DomeinFile (" <> err <> ")"
  show (ApiErrorBoundary m) = "(ApiErrorBoundary) An error occurred while processing an API request: " <> show m
  show (RuleErrorBoundary ruleName m) = "(RuleErrorBoundary) An error occurred while running rule " <> ruleName <> ": " <> show m

maybeShowPosition :: Maybe ArcPosition -> Maybe ArcPosition -> String
maybeShowPosition (Just start) (Just end) = "(between " <> show start <> " and " <> show end <> ")."
maybeShowPosition _ _ = "."

showPosition :: ArcPosition -> ArcPosition -> String
showPosition start end = "(between " <> show start <> " and " <> show end <> ")."

-- | A type for accumulating multiple `PerspectivesErrors`s.
type MultiplePerspectivesErrors = Array PerspectivesError

-- | An error monad, used in this library to encode possible failures when
-- | checking a Perspectives model data.
-- |
-- | The `Alt` instance for `Except` allows us to accumulate errors,
-- | unlike `Either`, which preserves only the last error.
type PF = Except.ExceptT MultiplePerspectivesErrors MonadPerspectives

-- | Throws a failure error in `F`.
fail :: forall a. PerspectivesError -> PF a
fail = Except.throwError <<< singleton
