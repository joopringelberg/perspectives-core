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
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Query.QueryTypes (Domain)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, RoleKind, RoleType)
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
    | UnknownRole ArcPosition String
    | UnknownProperty ArcPosition String
    | UnknownView ArcPosition String
    | NotUniquelyIdentifying ArcPosition String (Array String)
    | Custom String
    | UnknownElementaryQueryStep
    | IncompatibleQueryArgument ArcPosition Domain Step
    | ContextHasNoRole (ADT ContextType) String
    | RoleHasNoProperty (ADT EnumeratedRoleType) String
    | RoleHasNoBinding ArcPosition (ADT EnumeratedRoleType)
    | IncompatibleDomainsForJunction Domain Domain
    | RoleDoesNotBind ArcPosition RoleType (ADT EnumeratedRoleType)

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
  show (UnknownView pos qname) = "(UnknownProperty) The view '" <> qname <> "' is not defined, at: " <> show pos
  show (NotUniquelyIdentifying pos lname alts) = "(NotUniquelyIdentifying) The local name '" <> lname <> "' does not uniquely identify a resource. Choose one from: " <> show alts <> ", at: " <> show pos
  show (Custom s) = s
  -- TODO: Als extra kunnen we de Constructors hieronder voorzien van ArcPosition.
  show (UnknownElementaryQueryStep) = "(UnknownElementaryQueryStep) This step is unknown"
  show (IncompatibleQueryArgument pos dom step) = "(IncompatibleQueryArgument) Cannot get " <> show step <> " from " <> show dom <> ", at: " <> show pos
  show (ContextHasNoRole ctype qn) = "(ContextHasNoRole) The Context-type '" <> show ctype <> "' has no role with the name '" <> qn <> "'."
  show (RoleHasNoProperty rtype qn) = "(RoleHasNoProperty) The Role-type '" <> show rtype <> "' has no property with the name '" <> qn <> "'."
  show (RoleHasNoBinding pos rtype) = "(RoleHasNoBinding) The role '" <> show rtype <> "' has no binding. If it is a Sum-type, one of its members may have no binding, at: " <> show pos
  show (IncompatibleDomainsForJunction dom1 dom2) = "(IncompatibleDomainsForJunction) These two domains cannot be joined in a disjunction of conjunction: '" <> show dom1 <> "', '" <> show dom2 <> "'."
  show (RoleDoesNotBind pos rtype adt) = "(RoleDoesNotBind) The role '" <> show rtype <> "' does not bind roles of type '" <> show adt <> "'"

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
