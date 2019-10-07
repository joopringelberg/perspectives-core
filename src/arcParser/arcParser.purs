module Perspectives.Parsing.Arc where

import Control.Alt (map, (<|>))
import Control.Lazy (defer)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..), PerspectiveE(..), PropertyE(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, stringUntilNewline, reserved, colon)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition, IP, getPosition)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (bind, pure, ($), (*>), (<<<), (==), (>>=), (<*), discard, (<>))
import Text.Parsing.Indent (withBlock)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe, try, (<?>))

contextE :: IP ContextPart
contextE = try $ withBlock
  (\{uname, knd, pos} elements -> CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements, pos: pos})
  context_
  (defer (\_ -> contextPart))
  where
    context_ :: IP (Record (uname :: String, knd :: ContextKind, pos :: ArcPosition))
    context_ = do
      knd <- contextKind <* colon
      pos <- getPosition
      uname <- arcIdentifier
      pure {uname, knd, pos}

    contextKind :: IP ContextKind
    contextKind = (reserved "domain" *> pure Domain)
      <|> (reserved "case" *> pure Case)
      <|> reserved "party" *> pure Party
      <|> reserved "activity" *> pure Activity
      <|> reserved "state" *> pure State

    contextPart :: IP ContextPart
    contextPart = defer (\_ -> contextE) <|> roleE

domain :: IP ContextE
domain = do
  r <- contextE
  case r of
    (CE d@(ContextE {kindOfContext})) -> if kindOfContext == Domain then pure d else fail "The kind of the context must be 'Domain'"
    otherwise -> fail "Domain cannot be a role"

roleE :: IP ContextPart
roleE = try $ withBlock
  (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
  role_
  rolePart
  where
    role_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    role_ = botRole_ <|> calculatedRole_ <|> otherRole_

    otherRole_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    otherRole_ = try do
      knd <- (roleKind <* colon) <?> "one of 'thing', 'properties', 'context', 'user' or 'bot'"
      pos <- getPosition
      uname <- arcIdentifier
      attributes <- roleAttributes <?> "([not] mandatory, [not] functional)"
      -- parse filledBy
      filledBy' <- filledBy
      pure {uname, knd, pos, parts: filledBy' <> attributes}

    botRole_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    botRole_ = try do
      pos <- getPosition
      reserved "bot"
      forUser <- reserved "for" *> arcIdentifier
      pure {uname: "", knd: BotRole, pos, parts: Cons (ForUser forUser) Nil}

    calculatedRole_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    calculatedRole_ = try do
      knd <- roleKind <* colon
      pos <- getPosition
      uname <- arcIdentifier
      calculation <- reserved "=" *> stringUntilNewline >>= pure <<< Calculation
      pure {uname, knd, pos, parts: Cons calculation Nil }

    roleAttributes :: IP (List RolePart)
    roleAttributes = token.parens do
      ma <- mandatory
      fu <- token.comma *> functional
      pure (Cons ma (Cons fu Nil))
        where
          mandatory :: IP RolePart
          mandatory = do
            maybeNegated <- optionMaybe (reserved "not")
            reserved "mandatory"
            case maybeNegated of
              Nothing -> pure $ MandatoryAttribute true
              otherwise -> pure $ MandatoryAttribute false

          functional :: IP RolePart
          functional = do
            maybeNegated <- optionMaybe (reserved "not")
            reserved "functional"
            case maybeNegated of
              Nothing -> pure $ FunctionalAttribute true
              otherwise -> pure $ FunctionalAttribute false

    filledBy :: IP (List RolePart)
    filledBy = option Nil (reserved "filledBy" *> colon *> token.commaSep arcIdentifier >>= pure <<< map FilledByAttribute)
    -- filledBy = option Nil (reserved "filledBy" *> colon *> arcIdentifier >>= pure <<< FilledByAttribute >>= pure <<< singleton)

    roleKind :: IP RoleKind
    roleKind = (reserved "thing" *> pure RoleInContext)
      <|> reserved "properties" *> pure ExternalRole
      <|> reserved "context" *> pure ContextRole
      <|> reserved "user" *> pure UserRole
      <|> reserved "bot" *> pure BotRole
      <|> fail "Unknown kind of role"

    rolePart :: IP RolePart
    rolePart = propertyE <|> perspectiveE <|> viewE

propertyE :: IP RolePart
propertyE = do
  pos <- getPosition
  pure $ PE $ PropertyE {id: "", range: PNumber, propertyParts: Nil, pos}

perspectiveE :: IP RolePart
perspectiveE = do
  pos <- getPosition
  pure $ PRE $ PerspectiveE {id: "", perspectiveParts: Nil, pos}

viewE :: IP RolePart
viewE = do
  pos <- getPosition
  pure $ VE $ ViewE {id: "", viewParts: Nil, pos}
