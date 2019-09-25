module Perspectives.Parsing.Arc.Simple where

import Control.Alt (void, (<|>))
import Control.Lazy (defer)
import Data.Tuple (Tuple(..))
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextKind(..), ContextPart(..), FilledByAttribute(..), FunctionalAttribute(..), MandatoryAttribute(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, colon, reserved)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (pure, (*>), bind, ($), (==), discard, (>>=), (<<<))
import Text.Parsing.Indent (sameLine, withBlock)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.String (whiteSpace)

domain :: IP ContextE
domain = do
  r <- contextE
  case r of
    (CE d@(ContextE {kindOfContext})) -> if kindOfContext == Domain then pure d else fail "The kind of the context must be 'Domain'"
    otherwise -> fail "Domain cannot be a role"

contextE :: IP ContextPart
contextE = withBlock
  (\(Tuple uname knd) elements -> CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements})
  context_
  (defer (\_ -> contextPart))
  where
    context_ :: IP (Tuple String ContextKind)
    context_ = sameLine *> reserved "Context" *> colon *> do
      knd <- contextKind
      uname <- colon *> arcIdentifier
      void whiteSpace
      pure (Tuple uname knd)

    contextKind :: IP ContextKind
    contextKind = (reserved "Domain" *> pure Domain)
      <|> (reserved "Case" *> pure Case)
      <|> reserved "Party" *> pure Party
      <|> reserved "Activity" *> pure Activity
      <|> reserved "State" *> pure State

    contextPart :: IP ContextPart
    contextPart = defer (\_ -> contextE) <|> roleE

roleE :: IP ContextPart
roleE = withBlock
  (\(Tuple uname knd) elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements})
  role_
  rolePart
  where
    role_ :: IP (Tuple String RoleKind)
    role_ = sameLine *> reserved "Role" *> colon *> do
      knd <- roleKind
      uname <- colon *> arcIdentifier
      pure (Tuple uname knd)

    roleKind :: IP RoleKind
    roleKind = (reserved "Role" *> pure RoleInContext)
      <|> reserved "ContextRole" *> pure ContextRole
      <|> reserved "ExternalRole" *> pure ExternalRole
      <|> reserved "UserRole" *> pure UserRole
      <|> reserved "BotRole" *> pure BotRole
      <|> fail "Unknown kind of role"

    rolePart :: IP RolePart
    rolePart = propertyE <|> perspectiveE <|> viewE <|> functionalAttribute <|> mandatoryAttribute <|> filledByAttribute

    functionalAttribute :: IP RolePart
    functionalAttribute = reserved "NonFunctional" *> colon *> boolean >>= pure <<< FA <<< FunctionalAttribute

    mandatoryAttribute :: IP RolePart
    mandatoryAttribute = reserved "Mandatory" *> colon *> boolean >>= pure <<< MA <<< MandatoryAttribute


propertyE :: IP RolePart
propertyE = withBlock
  (\(Tuple uname ran) parts -> PE $ PropertyE {id: uname, range: ran, propertyParts: parts})
  property_
  propertyPart
  where
    property_ :: IP (Tuple String Range)
    property_ = sameLine *> reserved "Property" *> colon *> do
      ran <- range
      uname <- colon *> arcIdentifier
      pure (Tuple uname ran)

    propertyPart :: IP PropertyPart
    propertyPart = functionalAttribute <|> mandatoryAttribute

    functionalAttribute :: IP PropertyPart
    functionalAttribute = reserved "NonFunctional" *> colon *> boolean >>= pure <<< FA' <<< FunctionalAttribute

    mandatoryAttribute :: IP PropertyPart
    mandatoryAttribute = reserved "Mandatory" *> colon *> boolean >>= pure <<< MA' <<< MandatoryAttribute

    range :: IP Range
    range = reserved "BooleanProperty" *> pure PBool
      <|> reserved "NumberProperty" *> pure PNumber
      <|> reserved "StringProperty" *> pure PString
      <|> reserved "DateTimeProperty" *> pure PDate

perspectiveE :: IP RolePart
perspectiveE = fail "perspectiveE is not yet implemented."

viewE :: IP RolePart
viewE = withBlock
  (\uname refs -> VE $ ViewE {id: uname, viewParts: refs})
  (sameLine *> reserved "View" *> colon *> reserved "View" *> colon *> arcIdentifier)
  (reserved "Property" *> colon *> arcIdentifier)

boolean :: IP Boolean
boolean = (reserved "True" *> pure true) <|> (reserved "False" *> pure true)

filledByAttribute :: IP RolePart
filledByAttribute = reserved "FilledBy" *> colon *> arcIdentifier >>= pure <<< FBA <<< FilledByAttribute
