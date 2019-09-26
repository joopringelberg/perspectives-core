module Perspectives.Parsing.Arc.Simple where

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Tuple (Tuple(..))
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, colon, reserved)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (pure, (*>), bind, ($), (==), (>>=), (<<<))
import Text.Parsing.Indent (withBlock)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (try)

contextE :: IP ContextPart
contextE = withBlock
  (\(Tuple uname knd) elements -> CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements})
  context_
  (defer (\_ -> contextPart))
  where
    context_ :: IP (Tuple String ContextKind)
    context_ = reserved "Context" *> colon *> do
      knd <- contextKind
      uname <- colon *> arcIdentifier
      pure (Tuple uname knd)

    contextKind :: IP ContextKind
    contextKind = (reserved "Domain" *> pure Domain)
      <|> (reserved "Case" *> pure Case)
      <|> reserved "Party" *> pure Party
      <|> reserved "Activity" *> pure Activity
      <|> reserved "State" *> pure State

    contextPart :: IP ContextPart
    contextPart = defer (\_ -> contextE) <|> roleE

domain :: IP ContextE
domain = do
  r <- contextE
  case r of
    (CE d@(ContextE {kindOfContext})) -> if kindOfContext == Domain then pure d else fail "The kind of the context must be 'Domain'"
    otherwise -> fail "Domain cannot be a role"

roleE :: IP ContextPart
roleE = withBlock
  (\(Tuple uname knd) elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements})
  role_
  rolePart
  where
    role_ :: IP (Tuple String RoleKind)
    role_ = (reserved "Role" <|> reserved "Agent") *> colon *> do
      knd <- roleKind
      uname <- colon *> arcIdentifier
      pure (Tuple uname knd)

    roleKind :: IP RoleKind
    roleKind = (reserved "RoleInContext" *> pure RoleInContext)
      <|> reserved "ContextRole" *> pure ContextRole
      <|> reserved "ExternalRole" *> pure ExternalRole
      <|> reserved "UserRole" *> pure UserRole
      <|> reserved "BotRole" *> pure BotRole
      <|> fail "Unknown kind of role"

    rolePart :: IP RolePart
    rolePart = propertyE <|> perspectiveE <|> viewE <|> functionalAttribute <|> mandatoryAttribute <|> filledByAttribute

    functionalAttribute :: IP RolePart
    functionalAttribute = reserved "Functional" *> colon *> boolean >>= pure <<< FunctionalAttribute

    mandatoryAttribute :: IP RolePart
    mandatoryAttribute = reserved "Mandatory" *> colon *> boolean >>= pure <<< MandatoryAttribute

    filledByAttribute :: IP RolePart
    filledByAttribute = reserved "FilledBy" *> colon *> arcIdentifier >>= pure <<< FilledByAttribute

propertyE :: IP RolePart
propertyE = withBlock
  (\(Tuple uname ran) parts -> PE $ PropertyE {id: uname, range: ran, propertyParts: parts})
  property_
  propertyPart
  where
    property_ :: IP (Tuple String Range)
    property_ = reserved "Property" *> colon *> do
      ran <- range
      uname <- colon *> arcIdentifier
      pure (Tuple uname ran)

    propertyPart :: IP PropertyPart
    propertyPart = functionalAttribute <|> mandatoryAttribute

    functionalAttribute :: IP PropertyPart
    functionalAttribute = reserved "Functional" *> colon *> boolean >>= pure <<< FunctionalAttribute'

    mandatoryAttribute :: IP PropertyPart
    mandatoryAttribute = reserved "Mandatory" *> colon *> boolean >>= pure <<< MandatoryAttribute'

    range :: IP Range
    range = reserved "BooleanProperty" *> pure PBool
      <|> reserved "NumberProperty" *> pure PNumber
      <|> reserved "StringProperty" *> pure PString
      <|> reserved "DateTimeProperty" *> pure PDate

perspectiveE :: IP RolePart
perspectiveE = withBlock
  (\uname parts -> PRE $ PerspectiveE {id: uname, perspectiveParts: parts})
  perspective_
  (object <|> defaultView <|> actionE)
  where
    perspective_ :: IP String
    perspective_ = reserved "Perspective" *> colon *> reserved "Perspective" *> colon *> arcIdentifier

    object :: IP PerspectivePart
    object = try (reserved "ObjectRef" *> colon *> arcIdentifier >>= pure <<< Object)

    defaultView :: IP PerspectivePart
    defaultView = try (reserved "View" *> colon *> reserved "DefaultObjectViewRef" *> colon *> arcIdentifier >>= pure <<< DefaultView)

viewE :: IP RolePart
viewE = withBlock
  (\uname refs -> VE $ ViewE {id: uname, viewParts: refs})
  (reserved "View" *> colon *> reserved "View" *> colon *> arcIdentifier)
  (reserved "Property" *> colon *> reserved "PropertyRef" *> colon *> arcIdentifier)

actionE :: IP PerspectivePart
actionE = withBlock
  (\(Tuple uname verb) parts -> Act $ ActionE {id: uname, verb: verb, actionParts: parts})
  actionE_
  (indirectObjectView <|> indirectObject <|> objectView <|> subjectView)
  where
    actionE_ :: IP (Tuple String String)
    actionE_ = do
      verb <- reserved "Action" *> colon *> arcIdentifier
      uname <- colon *> arcIdentifier
      pure $ Tuple uname verb

    indirectObject :: IP ActionPart
    indirectObject = reserved "IndirectObjectRef" *> colon *> arcIdentifier >>= pure <<< IndirectObject

    -- If not a subjectView, the parser backtracks to objectView, indirectObjectView or indirectObject. The latter
    -- obviously fails, but the others do so too because "View" has already been consumed.
    -- Hence we need to wrap the parser in `try`, to make it backtrack over "View".
    subjectView :: IP ActionPart
    subjectView = try (reserved "View" *> colon *> reserved "SubjectViewRef" *> colon *> arcIdentifier >>= pure <<< SubjectView)

    objectView :: IP ActionPart
    objectView = try (reserved "View" *> colon *> reserved "ObjectViewRef" *> colon *> arcIdentifier >>= pure <<< ObjectView)

    indirectObjectView :: IP ActionPart
    indirectObjectView = try (reserved "View" *> colon *> reserved "IndirectObjectViewRef" *> colon *> arcIdentifier >>= pure <<< IndirectObjectView)

boolean :: IP Boolean
boolean = (reserved "True" *> pure true) <|> (reserved "False" *> pure false)
