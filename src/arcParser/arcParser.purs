module Perspectives.Parsing.Arc where

import Control.Alt (map, (<|>))
import Control.Lazy (defer)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, stringUntilNewline, reserved, colon)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition, IP, getPosition)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.Action (Verb(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (bind, discard, join, pure, ($), (*>), (<*), (<<<), (<>), (==), (>>=))
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
      reserved "bot" <* colon
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
propertyE = calculatedProperty <|> enumeratedProperty
  where

    calculatedProperty :: IP RolePart
    calculatedProperty = try do
      pos <- getPosition
      uname <- reserved "property" *> colon *> arcIdentifier
      token.reservedOp "="
      calc <- stringUntilNewline
      pure $ PE $ PropertyE
        { id: uname
        , range: Nothing
        , propertyParts: Cons (Calculation' calc) Nil, pos: pos}


    enumeratedProperty :: IP RolePart
    enumeratedProperty = try do
      pos <- getPosition
      uname <- reserved "property" *> colon *> arcIdentifier
      attributes <- propertyAttributes <?> "([not] mandatory, [not] functional, PString | PNumber | PBool | PDate)"
      pure $ PE $ PropertyE
        { id: uname
        , range: Just $ fst attributes
        , propertyParts: snd attributes
        , pos: pos}

    propertyAttributes :: IP (Tuple Range (List PropertyPart))
    propertyAttributes = token.parens do
      ma <- mandatory
      fu <- token.comma *> functional
      ran <- token.comma *> range
      pure $ Tuple ran (Cons ma (Cons fu Nil))
        where
          mandatory :: IP PropertyPart
          mandatory = do
            maybeNegated <- optionMaybe (reserved "not")
            reserved "mandatory"
            case maybeNegated of
              Nothing -> pure $ MandatoryAttribute' true
              otherwise -> pure $ MandatoryAttribute' false

          functional :: IP PropertyPart
          functional = do
            maybeNegated <- optionMaybe (reserved "not")
            reserved "functional"
            case maybeNegated of
              Nothing -> pure $ FunctionalAttribute' true
              otherwise -> pure $ FunctionalAttribute' false

          range :: IP Range
          range = reserved "Boolean" *> pure PBool
            <|> reserved "Number" *> pure PNumber
            <|> reserved "String" *> pure PString
            <|> reserved "DateTime" *> pure PDate

viewE :: IP RolePart
viewE = try do
  pos <- getPosition
  uname <- reserved "view" *> colon *> arcIdentifier
  refs <- token.parens (token.commaSep arcIdentifier)
  pure $ VE $ ViewE {id: uname, viewParts: refs, pos: pos}

-- | Parses four forms, because both the default view on the Object and the actions involved are optional:
-- |  `perspective on: Guest`
-- |  `perspective on: Guest (ViewOnGuest)`
-- |  `perspective on: Guest: Consult, Change`
-- |  `perspective on: Guest (ViewOnGuest): Consult, Change`
-- | Each of the forms above can be combined with specialised actions:
-- |      Consult with ViewOnGuest
-- |      Change with AnotherView`
perspectiveE :: IP RolePart
perspectiveE = try do
  pos <- getPosition
  withBlock
    (\parts otherActions -> PRE $ PerspectiveE {id: "", perspectiveParts: parts <> otherActions, pos: pos})
    perspective_
    actionE
  where
    perspective_ :: IP (List PerspectivePart)
    perspective_ = do
      (object :: PerspectivePart) <- reserved "perspective" *> reserved "on" *> colon *> arcIdentifier >>= pure <<< Object
      (maybeDefaultView :: Maybe PerspectivePart) <- optionMaybe (token.parens $ arcIdentifier >>= pure <<< DefaultView)
      (actions :: List PerspectivePart) <- option Nil (colon *> token.commaSep minimalAction')
      case maybeDefaultView of
        Nothing -> pure $ Cons object actions
        Just v -> pure $ Cons v (Cons object actions)

    minimalAction' :: IP PerspectivePart
    minimalAction' = do
      pos <- getPosition
      verbName <- arcIdentifier
      pure $ Act $ ActionE {id: "", verb: constructVerb verbName, actionParts: Nil, pos}

constructVerb :: String -> Verb
constructVerb v = case v of
  "Consult" -> Consult
  "Create" -> Create
  "Change" -> Change
  "Delete" -> Delete
  s -> Custom s

-- | Parses the following forms:
-- |  Consult with ViewOnGuest
-- |  Consult with ViewOnGuest
-- |    subjectView: AnotherView
-- |  Bind with AnotherView
-- |    indirectObject: AnotherRole (ViewOnAnotherRole)
actionE :: IP PerspectivePart
actionE = try $ withBlock
  (\{pos, verb, view} parts -> Act $ ActionE {id: "", verb, actionParts: Cons view (join parts), pos})
  actionE_
  (subjectView <|> indirectObject)
  where
    actionE_ :: IP {pos :: ArcPosition, verb :: Verb, view :: ActionPart}
    actionE_ = try do
      pos <- getPosition
      verb <- arcIdentifier >>= pure <<< constructVerb
      view <- reserved "with" *> arcIdentifier >>= pure <<< ObjectView
      pure {pos, verb, view}

    subjectView :: IP (List ActionPart)
    subjectView = do
      pos <- getPosition
      viewName <- reserved "subjectView" *> colon *> arcIdentifier
      pure $ Cons (SubjectView viewName) Nil

    indirectObject :: IP (List ActionPart)
    indirectObject = do
      pos <- getPosition
      indirectObjectName <- reserved "indirectObject" *> colon *> arcIdentifier
      indirectObjectView <- option Nil (token.parens (arcIdentifier >>= pure <<< singleton <<< IndirectObjectView))
      pure $ Cons (IndirectObject indirectObjectName) indirectObjectView
