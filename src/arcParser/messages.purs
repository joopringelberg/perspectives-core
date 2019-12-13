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

module Perspectives.Parsing.Messages where

-- | A Perspectives sourcefile (text or diagram) will be parsed in two passes.
-- | The resulting internal representation of types is type-checked.
-- | During all three phases of transformation, errors may be detected.
-- | This module defines the structure and kind of these errors.

import Control.Monad.Except (ExceptT, throwError)
import Data.List.Lazy.NonEmpty (singleton)
import Data.List.Lazy.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Parsing.Arc.Expression.AST (LetStep(..), PureLetStep(..), Step)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Query.QueryTypes (Domain, Range)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Range (Range) as RAN
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, CalculatedRoleType, ContextType, EnumeratedRoleType, RoleKind, RoleType)
import Prelude (class Eq, class Show, (<>), show, (<<<))

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
    | MissingForUser ArcPosition String
    | MissingObject ArcPosition String
    | NotWellFormedName ArcPosition String
    | RoleMissingInContext ArcPosition String String
    | UnknownContext ArcPosition String
    | UnknownRole ArcPosition String
    | UnknownProperty ArcPosition String
    | UnknownView ArcPosition String
    | NotUniquelyIdentifying ArcPosition String (Array String)
    | UnknownElementaryQueryStep
    | IncompatibleQueryArgument ArcPosition Domain Step
    | ContextHasNoRole (ADT ContextType) String
    | RoleHasNoProperty (ADT EnumeratedRoleType) String
    | RoleHasNoBinding ArcPosition (ADT EnumeratedRoleType)
    | IncompatibleDomainsForJunction Domain Domain
    | RoleDoesNotBind ArcPosition RoleType (ADT EnumeratedRoleType)
    | LocalRoleDoesNotBind ArcPosition ArcPosition String (ADT EnumeratedRoleType)
    | IncompatibleComposition ArcPosition Range Domain
    | TypesCannotBeCompared ArcPosition Range Range
    | NotABoolean ArcPosition
    | WrongTypeForOperator ArcPosition (Array RAN.Range)
    | MissingValueForAssignment ArcPosition ArcPosition
    | ArgumentMustBeSequenceFunction ArcPosition
    | UnknownVariable ArcPosition String
    | NotALetWithAssignment PureLetStep
    | NotAPureLet LetStep
    | CannotCreateCalculatedRole CalculatedRoleType ArcPosition ArcPosition
    | CannotCreateCalculatedProperty CalculatedPropertyType ArcPosition ArcPosition
    | NotAContextDomain Domain ArcPosition ArcPosition
    | NotARoleDomain Domain ArcPosition ArcPosition
    | NotFunctional ArcPosition ArcPosition Step
    | MaybeNotFunctional ArcPosition ArcPosition Step
    | WrongPropertyRange ArcPosition ArcPosition RAN.Range RAN.Range
    | NotAPropertyRange  ArcPosition ArcPosition RAN.Range

    | ParserError String ArcPosition
    | Custom String

derive instance eqPerspectivesError :: Eq PerspectivesError

instance showPerspectivesError :: Show PerspectivesError where
  show (DefaultPrototype expected given) = "Invalid type for DefaultPrototype. Expected: '" <> unwrap expected <> "' but found '" <> unwrap given <> "'."
  show (CyclicAspects pos c) = "Context '" <> unwrap c <> "' has cyclic aspects: " <> show pos
  show (WrongRoleKind roletype expected found) = "Role '" <> show roletype <> "' has kind '" <> show found <> "' but should have kind '" <> show expected<> "'."
  show (MissingForUser pos localBotName) = "(MissingForUser) The BotRole '" <> localBotName <> "' should have a 'ForUser' clause: " <> show pos
  show (MissingObject pos localPerspectiveName) = "(MissingObject) The perspective '" <> localPerspectiveName <> "' should have an 'ObjectRef' clause: " <> show pos
  show (NotWellFormedName pos name) = "(NotWellFormedName) The name '" <> name <> "' is not well-formed (it cannot be expanded to a fully qualified name): " <> show pos
  show (RoleMissingInContext pos localRoleName ctxt) = "(RoleMissingInContext) The local role name '" <> localRoleName <> "' cannot be found in the context: '" <> ctxt <> "', at: " <> show pos
  show (UnknownRole pos qname) = "(UnknownRole) The role '" <> qname <> "' is not defined, at: " <> show pos
  show (UnknownProperty pos qname) = "(UnknownProperty) The property '" <> qname <> "' is not defined, at: " <> show pos
  show (UnknownContext pos qname) = "(UnknownContext) The context '" <> qname <> "' is not defined, at: " <> show pos
  show (UnknownView pos qname) = "(UnknownView) The view '" <> qname <> "' is not defined, at: " <> show pos
  show (NotUniquelyIdentifying pos lname alts) = "(NotUniquelyIdentifying) The local name '" <> lname <> "' does not uniquely identify a resource. Choose one from: " <> show alts <> ", at: " <> show pos
  show (Custom s) = s
  show (ParserError message pos) = "(ParserError) " <> message <> show pos
  -- TODO: Als extra kunnen we de Constructors hieronder voorzien van ArcPosition.
  show (UnknownElementaryQueryStep) = "(UnknownElementaryQueryStep) This step is unknown"
  show (IncompatibleQueryArgument pos dom step) = "(IncompatibleQueryArgument) Cannot get " <> show step <> " from " <> show dom <> ", at: " <> show pos
  show (ContextHasNoRole ctype qn) = "(ContextHasNoRole) The Context-type '" <> show ctype <> "' has no enumerated role with the name '" <> qn <> "' (it may have a calculated role but that cannot be used here)."
  show (RoleHasNoProperty rtype qn) = "(RoleHasNoProperty) The Role-type '" <> show rtype <> "' has no property with the name '" <> qn <> "'."
  show (RoleHasNoBinding pos rtype) = "(RoleHasNoBinding) The role '" <> show rtype <> "' has no binding. If it is a Sum-type, one of its members may have no binding, at: " <> show pos
  show (IncompatibleDomainsForJunction dom1 dom2) = "(IncompatibleDomainsForJunction) These two domains cannot be joined in a disjunction of conjunction: '" <> show dom1 <> "', '" <> show dom2 <> "'."
  show (RoleDoesNotBind pos rtype adt) = "(RoleDoesNotBind) The role '" <> show rtype <> "' does not bind roles of type '" <> show adt <> "'"
  show (LocalRoleDoesNotBind start end lname adt) = "(LocalRoleDoesNotBind) The roles that name '" <> lname <> "' (from " <> show start <> " to " <> show end <> ") can be matched to, do not bind '" <> show adt <> "'."
  show (IncompatibleComposition pos left right) = "(IncompatibleComposition) The result of the left operand (" <> show left <> ") and the argument of the right operand (" <> show right <> ") are incompatible."
  show (TypesCannotBeCompared pos left right) = "(TypesCannotBeCompared) The result of the left operand (" <> show left <> ") and of the right operand (" <> show right <> ") are incompatible in: " <> show pos
  show (NotABoolean pos) = "(NotABoolean) The expression starting at " <> show pos <> " does not result in a boolean value."
  show (WrongTypeForOperator pos allowedOps) = "(WrongTypeForOperator) This operator requires its arguments to be (one of): " <> show allowedOps
  show (MissingValueForAssignment start end) = "(MissingValueForAssignment) This assignment statement needs a value expression on the right: from " <> show start <> " to " <> show end
  show (ArgumentMustBeSequenceFunction pos) = "(ArgumentMustBeSequenceFunction) The right operand of '>>=' must be a monoidal function such as sum, product, minimum, or maximum, at: " <> show pos
  show (UnknownVariable pos varName) = "(UnknownVariable) The variable '" <> varName <> "' is not known at position: " <> show pos
  show (NotALetWithAssignment (PureLetStep{start, end})) = "(NotALetWithAssignment) This let*-expression does not have an assignment in its body, hence is of no use in a rule. From " <> show start <> " to " <> show end
  show (NotAPureLet (LetStep{start, end})) = "(NotAPureLet) This let*-expression has an assignment in its body but it is used in a pure expression, so its body should be a pure expression, too. From " <> show start <> " to " <> show end
  show (CannotCreateCalculatedRole cr start end) = "(CannotCreateCalculatedRole) Can not create an instance of a calculated role (" <> show cr <> ") between: " <> show start <> " and: " <> show end
  show (CannotCreateCalculatedProperty pt start end) = "(CannotCreateCalculatedProperty) Can not change the value of a property that is calculated, between: " <> show start <> " and: " <> show end
  show (NotARoleDomain dom start end) = "(NotARoleDomain) This expression should have a role type: " <> show dom <> ", between " <> show start <> " and " <> show end
  show (NotAContextDomain dom start end) = "(NotAContextDomain) This expression should have a context type: " <> show dom <> ", between " <> show start <> " and " <> show end
  show (NotFunctional start end qfd) = "(NotFunctional) This expression is not a single value, between " <> show start <> " and " <> show end
  show (MaybeNotFunctional start end qfd) = "(MaybeNotFunctional) This expression might not be a single value, between " <> show start <> " and " <> show end
  show (WrongPropertyRange start end expected received) = "(WrongPropertyRange) Expected the range '" <> show expected <> "', got '" <> show received <> "', between " <> show start <> " and " <> show end
  show (NotAPropertyRange start end expected) = "(NotAPropertyRange) Expression does not yield a property value. Expected the range '" <> show expected <> "', between " <> show start <> " and " <> show end


-- | A type for accumulating multiple `PerspectivesErrors`s.
type MultipleErrors = NonEmptyList PerspectivesError

-- | An error monad, used in this library to encode possible failures when
-- | checking a Perspectives model data.
-- |
-- | The `Alt` instance for `Except` allows us to accumulate errors,
-- | unlike `Either`, which preserves only the last error.
type PF = ExceptT MultipleErrors MonadPerspectives

-- | Throws a failure error in `F`.
fail :: forall a. PerspectivesError -> PF a
fail = throwError <<< singleton
