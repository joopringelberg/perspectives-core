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

module Perspectives.Parsing.Arc where

import Control.Alt (map, void, (<|>))
import Data.Array (elemIndex)
import Data.List (List(..), singleton, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Expression (assignment, letStep, startOf, step)
import Perspectives.Parsing.Arc.Expression.AST (PureLetStep(..), Step(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved, colon, lowerCaseName)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition, IP, arcPosition2Position, entireBlock, getPosition, nextLine, withEntireBlock)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.Action (Verb(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (bind, discard, join, pure, ($), (*>), (<*), (<<<), (<>), (==), (>>=), (<$>), (<*>))
import Text.Parsing.Indent (withPos)
import Text.Parsing.Parser (fail, failWithPosition)
import Text.Parsing.Parser.Combinators (lookAhead, option, optionMaybe, try, (<?>))

contextE :: IP ContextPart
contextE = try $ withEntireBlock
  (\{uname, knd, pos} elements -> CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements, pos: pos})
  context_
  contextPart
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

    isContextKind :: String -> Boolean
    isContextKind s = isJust (elemIndex s ["domain", "case", "party", "activity", "state"])

    contextPart :: IP ContextPart
    contextPart = do
      keyword <- lookAhead $ lowerCaseName <* colon
      case keyword of
        "use" -> useE
        "aspect" -> aspectE
        "indexed" -> indexedE
        ctxt | isContextKind ctxt -> contextE
        role | isRoleKind role -> roleE
        otherwise -> fail "unknown keyword for a context."

    aspectE :: IP ContextPart
    aspectE = do
      void $ reserved "aspect" *> colon
      pos <-getPosition
      aspect <- arcIdentifier
      pure $ ContextAspect aspect pos

    indexedE :: IP ContextPart
    indexedE = do
      void $ reserved "indexed" *> colon
      pos <- getPosition
      indexedName <- arcIdentifier
      pure $ IndexedContext indexedName pos

domain :: IP ContextE
domain = do
  r <- token.whiteSpace *> contextE
  case r of
    (CE d@(ContextE {kindOfContext})) -> if kindOfContext == Domain then pure d else fail "The kind of the context must be 'Domain'"
    otherwise -> fail "Domain cannot be a role"

useE :: IP ContextPart
useE = do
  prefix <- reserved "use" *> colon *> lowerCaseName
  void $ reserved "for"
  pos <- getPosition
  modelName <- arcIdentifier
  if isQualifiedWithDomein modelName
    then pure $ PREFIX prefix modelName
    else failWithPosition ("(NotWellFormedName) The name '" <> modelName <> "' is not well-formed (it cannot be expanded to a fully qualified name)") (arcPosition2Position pos)

roleE :: IP ContextPart
roleE = try $ withEntireBlock
  (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
  role_
  rolePart
  where
    role_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    role_ = do
      pos <- getPosition
      knd <- (roleKind <* colon) <?> "'thing', 'external', 'context', 'user' or 'bot'"
      case knd of
        BotRole -> botRole_ pos
        ExternalRole -> externalRole_ pos
        otherwise -> do
          uname <- arcIdentifier
          isCalculated <- (lookAhead $ optionMaybe (reserved "="))
          case isCalculated of
            Nothing -> otherRole_ pos knd uname
            (Just _) -> calculatedRole_ pos knd uname

    rolePart :: IP RolePart
    rolePart = do
      typeOfPart <- lookAhead (reserved' "property" <|> reserved' "perspective" <|> reserved' "view" <|> reserved' "aspect" <|> reserved' "indexed")
      case typeOfPart of
        "property" -> propertyE
        "perspective" -> perspectiveE
        "aspect" -> aspectE
        "indexed" -> indexedE
        _ -> viewE

    aspectE :: IP RolePart
    aspectE = do
      void $ reserved "aspect" *> colon
      pos <- getPosition
      aspect <- arcIdentifier
      pure $ RoleAspect aspect pos

    indexedE :: IP RolePart
    indexedE = do
      void $ reserved "indexed" *> colon
      pos <- getPosition
      indexedName <- arcIdentifier
      pure $ IndexedRole indexedName pos

    otherRole_ :: ArcPosition -> RoleKind -> String -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    otherRole_ pos knd uname = try do
      attributes <- roleAttributes <?> "([not] mandatory, [not] functional)"
      -- parse filledBy
      filledBy' <- filledBy
      pure {uname, knd, pos, parts: filledBy' <> attributes}

    botRole_ :: ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    botRole_ pos = try do
      forUser <- reserved "for" *> arcIdentifier
      pure {uname: "", knd: BotRole, pos, parts: Cons (ForUser forUser) Nil}

    externalRole_ :: ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    externalRole_ pos = pure {uname: "External", knd: ExternalRole, pos, parts: Nil}

    calculatedRole_ :: ArcPosition -> RoleKind -> String -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    calculatedRole_ pos knd uname = try do
      -- calculation <- reserved "=" *> stringUntilNewline >>= pure <<< Calculation
      calculation <- reserved "=" *> step >>= pure <<< Calculation
      pure {uname, knd, pos, parts: Cons calculation Nil }

    roleAttributes :: IP (List RolePart)
    roleAttributes = option Nil $ token.parens do
      ma <- mandatory
      fu <- token.comma *> functional
      munlinked <- optionMaybe (token.comma *> reserved "unlinked")
      case munlinked of
        Nothing -> pure (Cons ma (Cons fu Nil))
        Just _ -> pure (Cons ma (Cons fu (Cons UnlinkedAttribute Nil)))
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
    filledBy = (option Nil (reserved "filledBy" *> colon *> token.commaSep arcIdentifier >>= pure <<< map FilledByAttribute)) <* (nextLine <?> "valid filledBy clause")

    roleKind :: IP RoleKind
    roleKind = (reserved "thing" *> pure RoleInContext)
      <|> reserved "external" *> pure ExternalRole
      <|> reserved "context" *> pure ContextRole
      <|> reserved "user" *> pure UserRole
      <|> reserved "bot" *> pure BotRole
      <|> fail "Unknown kind of role"

isRoleKind :: String -> Boolean
isRoleKind s = isJust (elemIndex s ["thing", "external", "context", "user", "bot"])

propertyE :: IP RolePart
propertyE = try do
  pos <- getPosition
  uname <- reserved "property" *> colon *> arcIdentifier
  -- _ <- lookAhead (token.reservedOp "=" <|> (token.reservedOp "(")) <?> "property attributes in parentheses, or '=' followed by a calculation."
  isCalculated <- optionMaybe (token.reservedOp "=")
  case isCalculated of
    Nothing -> enumeratedProperty pos uname
    otherwise -> calculatedProperty pos uname
  -- (calculatedProperty pos uname) <|> (enumeratedProperty pos uname)

  where

    calculatedProperty :: ArcPosition -> String -> IP RolePart
    calculatedProperty pos uname = try do
      -- token.reservedOp "="
      calc <- step
      pure $ PE $ PropertyE
        { id: uname
        , range: Nothing
        , propertyParts: Cons (Calculation' calc) Nil, pos: pos}


    enumeratedProperty :: ArcPosition -> String -> IP RolePart
    enumeratedProperty pos uname = try do
      attributes <- option (Tuple PString Nil) propertyAttributes
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
            reserved "mandatory" <?> "[not] mandatory"
            case maybeNegated of
              Nothing -> pure $ MandatoryAttribute' true
              otherwise -> pure $ MandatoryAttribute' false

          functional :: IP PropertyPart
          functional = do
            maybeNegated <- optionMaybe (reserved "not")
            reserved "functional" <?> "[not] functional"
            case maybeNegated of
              Nothing -> pure $ FunctionalAttribute' true
              otherwise -> pure $ FunctionalAttribute' false

          range :: IP Range
          range = (reserved "Boolean" *> pure PBool
            <|> reserved "Number" *> pure PNumber
            <|> reserved "String" *> pure PString
            <|> reserved "DateTime" *> pure PDate) <?> "Boolean, Number, String or DateTime"

viewE :: IP RolePart
viewE = try do
  pos <- getPosition
  uname <- reserved "view" *> colon *> arcIdentifier
  refs <- token.parens (token.commaSep1 arcIdentifier)
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
  withEntireBlock
    (\parts otherActions -> PRE $ PerspectiveE {id: "", perspectiveParts: parts <> otherActions, pos: pos})
    perspective_
    (actionE <|> ruleE)
  where
    perspective_ :: IP (List PerspectivePart)
    perspective_ = do
      (object :: PerspectivePart) <- reserved "perspective" *> reserved "on" *> colon *> step >>= pure <<< Object
      (mdefaultView :: Maybe PerspectivePart) <- optionMaybe (token.parens $ arcIdentifier >>= pure <<< DefaultView)
      pos <- getPosition
      (actions :: List PerspectivePart) <- option Nil (colon *> token.commaSep minimalAction')
      case mdefaultView of
        Nothing -> pure $ Cons object actions
        (Just defaultView) -> pure $ Cons defaultView (Cons object actions)

    minimalAction' :: IP PerspectivePart
    minimalAction' = mkActionFromVerb <$> getPosition <*> token.identifier

mkActionFromVerb :: ArcPosition -> String -> PerspectivePart
mkActionFromVerb pos v = Act $ ActionE {id: "", verb: constructVerb v, actionParts: Nil, pos}

constructVerb :: String -> Verb
constructVerb v = case v of
  "Consult" -> Consult
  "Create" -> Create
  "Change" -> Change
  "Delete" -> Delete
  "CreateAndBindContext" -> CreateAndBindContext
  "Bind" -> Bind
  s -> Custom s

-- | Parses the following forms:
-- |  Consult with ViewOnGuest
-- |  Consult with ViewOnGuest
-- |    subjectView: AnotherView
-- |  Bind with AnotherView
-- |    indirectObject: AnotherRole (ViewOnAnotherRole)
-- |  Consult
-- |    indirectObject: AnotherRole
-- |  Consult
-- |    if Prop1 < 10
actionE :: IP PerspectivePart
actionE = try $ withEntireBlock
  (\{pos, verb, mview} parts -> Act $ ActionE {id: "", verb, actionParts: case mview of
    Nothing -> join parts
    (Just view) ->  Cons view (join parts), pos})
  actionE_
  actionParts
  where
    actionE_ :: IP {pos :: ArcPosition, verb :: Verb, mview :: Maybe ActionPart}
    actionE_ = try do
      pos <- getPosition
      verb <- (arcIdentifier <?> "Consult, Change, Create, Delete, Bind, CreateAndBindContext or a valid identifier") >>= pure <<< constructVerb
      mview <- optionMaybe (reserved "with" *> arcIdentifier >>= pure <<< ObjectView)
      pure {pos, verb, mview}

    actionParts :: IP (List ActionPart)
    actionParts = do
      lookAhead (reserved "subjectView" <|> reserved "indirectObject" <|> reserved "if") <?> "if, subjectView or indirectObject"
      subjectView <|> indirectObject <|> condition

    subjectView :: IP (List ActionPart)
    subjectView = do
      pos <- getPosition
      viewName <- reserved "subjectView" *> colon *> arcIdentifier
      pure $ Cons (SubjectView viewName) Nil

    indirectObject :: IP (List ActionPart)
    indirectObject = do
      pos <- getPosition
      indirectObjectName <- reserved "indirectObject" *> colon *> token.identifier
      indirectObjectView <- option Nil (token.parens (arcIdentifier >>= pure <<< singleton <<< IndirectObjectView))
      pure $ Cons (IndirectObject indirectObjectName) indirectObjectView

    condition :: IP (List ActionPart)
    condition = do
      pos <- getPosition
      expr <- reserved "if" *> step
      pure $ Cons (Condition expr) Nil

ruleE :: IP PerspectivePart
ruleE = withPos (do
  lhs <- ruleE_
  (letWithAssignment lhs <|> assignmentList lhs) <?> "one or more assignment expressions")
  -- TODO. IK SNAP niet waarom dit faalt. De positie in het resultaat van de LetStep (bijvoorbeeld) is rechts
  -- van die van de if. Waarom dan niet indented?
  -- indented *> (letWithAssignment lhs <|> assignmentList lhs)

ruleE_ :: IP {pos :: ArcPosition, condition :: Step}
ruleE_ = do
  pos <- getPosition
  cond <- reserved "if" *> step <* reserved "then"
  pure {pos: pos, condition: cond}

-- | A rule is a special format for a Bot action:
-- |   if Wens >> Bedrag > 10 then
-- |     SomeProp = false
-- |     SomeOtherProp =+ "aap"
assignmentList :: {pos :: ArcPosition, condition :: Step} -> IP PerspectivePart
assignmentList {pos, condition} = try do
  assignments <- entireBlock assignment
  pure $ Act $ ActionE {id: "", verb: Change, actionParts: Cons (Condition condition) (map AssignmentPart assignments), pos}

-- | A rule may also have a let* expression for its right hand side:
-- |  if Wens >> Bedrag > 10 then
-- |    let*
-- |      a <- <expression>
-- |    in
-- |      SomeProp = a
letWithAssignment :: {pos :: ArcPosition, condition :: Step} -> IP PerspectivePart
letWithAssignment {pos, condition} = try do
  ltst <- letStep
  case ltst of
    (Let lt) -> pure $ Act $ ActionE {id: "", verb: Change, actionParts: ((Condition condition) : (LetPart lt) : Nil), pos}
    (PureLet (PureLetStep{body})) -> failWithPosition "assignment expression" (arcPosition2Position (startOf body))
    otherwise -> fail "let* expression"

reserved' :: String -> IP String
reserved' name = token.reserved name *> pure name
