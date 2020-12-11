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

module Perspectives.Parsing.TransferFile where

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved, colon)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition, IP, getPosition)
import Perspectives.Representation.Action (Verb(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (bind, pure, ($), (*>), (<*), (<<<), (==), (>>=))
import Text.Parsing.Indent (withBlock)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (try)

-- TODO: handle Aspects

contextE :: IP ContextPart
contextE = withBlock
  (\{uname, knd, pos} elements -> CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements, pos: pos})
  context_
  (defer (\_ -> contextPart))
  where
    context_ :: IP (Record (uname :: String, knd :: ContextKind, pos :: ArcPosition))
    context_ = reserved "Context" *> colon *> do
      knd <- contextKind <* colon
      pos <- getPosition
      uname <- arcIdentifier
      pure {uname, knd, pos}

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
  (\{uname, knd, pos} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements, pos: pos})
  role_
  rolePart
  where
    role_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition))
    role_ = (reserved "Role" <|> reserved "Agent") *> colon *> do
      knd <- roleKind <* colon
      pos <- getPosition
      uname <- arcIdentifier
      pure {uname, knd, pos}

    roleKind :: IP RoleKind
    roleKind = (reserved "RoleInContext" *> pure RoleInContext)
      <|> reserved "ContextRole" *> pure ContextRole
      <|> reserved "ExternalRole" *> pure ExternalRole
      <|> reserved "UserRole" *> pure UserRole
      <|> reserved "BotRole" *> pure BotRole
      <|> fail "Unknown kind of role"

    rolePart :: IP RolePart
    rolePart = propertyE <|> perspectiveE <|> viewE <|> functionalAttribute <|> mandatoryAttribute <|> filledByAttribute <|> calculation <|> forUser

    functionalAttribute :: IP RolePart
    functionalAttribute = reserved "Functional" *> colon *> boolean >>= pure <<< FunctionalAttribute

    mandatoryAttribute :: IP RolePart
    mandatoryAttribute = reserved "Mandatory" *> colon *> boolean >>= pure <<< MandatoryAttribute

    filledByAttribute :: IP RolePart
    filledByAttribute = reserved "FilledBy" *> colon *> arcIdentifier >>= pure <<< FilledByAttribute

    calculation :: IP RolePart
    calculation = reserved "Calculation" *> colon *> step >>= pure <<< Calculation

    forUser :: IP RolePart
    forUser = reserved "ForUser" *> colon *> arcIdentifier >>= pure <<< ForUser

propertyE :: IP RolePart
propertyE = withBlock
  (\{uname, ran, pos} parts -> PE $ PropertyE {id: uname, range: Just ran, propertyParts: parts, pos: pos})
  property_
  propertyPart
  where
    property_ :: IP (Record (uname :: String, ran :: Range, pos :: ArcPosition))
    property_ = reserved "Property" *> colon *> do
      ran <- range <* colon
      pos <- getPosition
      uname <- arcIdentifier
      pure {uname, ran, pos}

    propertyPart :: IP PropertyPart
    propertyPart = functionalAttribute <|> mandatoryAttribute <|> calculation

    functionalAttribute :: IP PropertyPart
    functionalAttribute = reserved "Functional" *> colon *> boolean >>= pure <<< FunctionalAttribute'

    mandatoryAttribute :: IP PropertyPart
    mandatoryAttribute = reserved "Mandatory" *> colon *> boolean >>= pure <<< MandatoryAttribute'

    calculation :: IP PropertyPart
    calculation = reserved "Calculation" *> colon *> step >>= pure <<< Calculation'

    range :: IP Range
    range = reserved "BooleanProperty" *> pure PBool
      <|> reserved "NumberProperty" *> pure PNumber
      <|> reserved "StringProperty" *> pure PString
      <|> reserved "DateTimeProperty" *> pure PDate

perspectiveE :: IP RolePart
perspectiveE = withBlock
  (\(Tuple uname pos) parts -> PRE $ PerspectiveE {id: uname, perspectiveParts: parts, pos: pos})
  perspective_
  (object <|> defaultView <|> actionE)
  where
    perspective_ :: IP (Tuple String ArcPosition)
    perspective_ = reserved "Perspective" *> colon *> reserved "Perspective" *> colon *>
      do
        pos <- getPosition
        uname <- arcIdentifier
        pure (Tuple uname pos)

    object :: IP PerspectivePart
    object = try (reserved "ObjectRef" *> colon *> step >>= pure <<< Object)

    defaultView :: IP PerspectivePart
    defaultView = try (reserved "View" *> colon *> reserved "DefaultObjectViewRef" *> colon *> arcIdentifier >>= pure <<< DefaultView)

viewE :: IP RolePart
viewE = withBlock
  (\(Tuple uname pos) refs -> VE $ ViewE {id: uname, viewParts: refs, pos: pos})
  (reserved "View" *> colon *> reserved "View" *> colon *> do
    pos <- getPosition
    uname <- arcIdentifier
    pure (Tuple uname pos))
  (reserved "Property" *> colon *> reserved "PropertyRef" *> colon *> arcIdentifier)

actionE :: IP PerspectivePart
actionE = withBlock
  (\{uname, verb, pos} parts -> Act $ ActionE {id: uname, verb: constructVerb verb, actionParts: parts, pos: pos})
  actionE_
  (indirectObjectView <|> indirectObject <|> objectView <|> subjectView)
  where
    actionE_ :: IP (Record (uname :: String, verb :: String, pos :: ArcPosition))
    actionE_ = do
      verb <- reserved "Action" *> colon *> arcIdentifier
      pos <- getPosition <* colon
      uname <- arcIdentifier
      pure {uname, verb, pos}

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

    constructVerb :: String -> Verb
    constructVerb v = case v of
      "Consult" -> Consult
      "Create" -> Create
      "Change" -> Change
      "Delete" -> Delete
      s -> Custom s

boolean :: IP Boolean
boolean = (reserved "True" *> pure true) <|> (reserved "False" *> pure false)
