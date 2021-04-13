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
import Control.Lazy (fix)
import Data.Array (elemIndex, fromFoldable)
import Data.Either (Either(..))
import Data.List (List(..), many, singleton, (:), concat)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ActionE(..), ContextE(..), ContextPart(..), AutomaticEffectE(..), NotificationE(..), PropertyE(..), PropertyPart(..), PropertyVerbE(..), PropsOrView(..), RoleE(..), RolePart(..), RoleVerbE(..), StateQualifiedPart(..), ViewE(..), StateE(..))
import Perspectives.Parsing.Arc.Expression (assignment, letWithAssignment, step)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved, colon, lowerCaseName)
import Perspectives.Parsing.Arc.IndentParser (IP, entireBlock, getArcParserState, getPosition, isIndented, nextLine, protectObject, protectOnEntry, protectOnExit, protectSubject, setObject, setOnEntry, setOnExit, setSubject, withArcParserState, withEntireBlock)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.State (NotificationLevel(..), StateIdentifier(..), unwrapStateIdentifier)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..), RoleVerbList(..))
import Prelude (bind, discard, pure, ($), (*>), (<$>), (<*), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.Indent (block, checkIndent, indented, sameOrIndented, withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (lookAhead, option, optionMaybe, try, (<?>))

contextE :: IP ContextPart
contextE = try $ withPos do
  {uname, knd, pos} <- context_
  elements <- isIndented >>= if _
    then withArcParserState (AllStates $ qname knd uname)
      (withPos do
        uses <- many $ checkIndent *> useE
        ind <- checkIndent *> indexedE
        aspects <- many $ checkIndent *> aspectE
        states <- nestedStates
        rolesAndContexts <- many $ checkIndent *> (contextE <|> roleE)
        pure $ (ind : (states : (uses <> aspects <> rolesAndContexts))))
    else pure Nil
  pure $ CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements, pos: pos}

  where
    nestedStates :: IP ContextPart
    nestedStates = do
      start <- getPosition
      void $ reserved "states" *> colon
      entryNotifications <- onEntryKeywords *> isIndented >>= if _
        then concat <$> entireBlock notificationE
        else pure Nil
      exitNotifications <- onExitKeywords *> isIndented >>= if _
        then concat <$> entireBlock notificationE
        else pure Nil
      states <- many $ checkIndent *> stateE
      pure $ STATE $ StateE
        { id: ""
        , condition: Simple $ Value start PBool "true"
        , stateParts: entryNotifications <> exitNotifications
        , subStates: states
        }

    qname :: ContextKind -> String -> String
    qname knd cname = if knd == Domain then "model:" <> cname else cname

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
    isContextKind s = isJust (elemIndex s ["domain", "case", "party", "activity"])

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
    otherwise -> fail "Domain must be a context"

useE :: IP ContextPart
useE = do
  prefix <- reserved "use" *> colon *> lowerCaseName
  void $ reserved "for"
  pos <- getPosition
  modelName <- arcIdentifier
  if isQualifiedWithDomein modelName
    then pure $ PREFIX prefix modelName
    else fail ("(NotWellFormedName) The name '" <> modelName <> "' is not well-formed (it cannot be expanded to a fully qualified name)")

roleE :: IP ContextPart
roleE =
  lookAhead (reserved "user") *> protectSubject roleE'
  <|>
  lookAhead (reserved "thing") *> protectObject roleE'
  <|>
  lookAhead (reserved "context") *> protectObject roleE'
  <|>
  roleE'
  where
    roleE' :: IP ContextPart
    roleE' = try $ withEntireBlock
      (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
      role_
      rolePart

    role_ :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    role_ = do
      pos <- getPosition
      knd <- (roleKind <* colon) <?> "'thing', 'external', 'context' or 'user'"
      case knd of
        ExternalRole -> externalRole_ pos
        otherwise -> do
          uname <- arcIdentifier
          isCalculated <- (lookAhead $ optionMaybe (reserved "="))
          case isCalculated of
            Nothing -> otherRole_ pos knd uname
            (Just _) -> calculatedRole_ pos knd uname

    rolePart :: IP RolePart
    rolePart = do
      typeOfPart <- lookAhead (reserved' "property" <|> perspectiveOnKeywords <|> perspectiveOfKeywords <|> reserved' "view" <|> reserved' "aspect" <|> reserved' "indexed" <|> inStateKeywords <|> onEntryKeywords <|> onExitKeywords)
      case typeOfPart of
        "property" -> propertyE
        "perspectiveOn" -> SQP <$> perspectiveOn
        "perspectiveOf" -> SQP <$> perspectiveOf
        "aspect" -> aspectE
        "indexed" -> indexedE
        "inState" -> SQP <$> inState
        "onEntry" -> SQP <$> onEntryE
        "onExit" -> SQP <$> onExitE
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
      -- Add either subject or object to state.
      {object, state} <- getArcParserState
      void $ case knd of
        UserRole -> setSubject (unwrapStateIdentifier state <> uname)
        otherwise -> setObject (Simple $ ArcIdentifier pos uname)
      attributes <- roleAttributes <?> "([not] mandatory, [not] functional)"
      -- parse filledBy
      filledBy' <- filledBy
      pure {uname, knd, pos, parts: filledBy' <> attributes}

    externalRole_ :: ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    externalRole_ pos = pure {uname: "External", knd: ExternalRole, pos, parts: Nil}

    calculatedRole_ :: ArcPosition -> RoleKind -> String -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    calculatedRole_ pos knd uname = try do
      -- Add either subject or object to state.
      {object, state} <- getArcParserState
      void $ case knd of
        UserRole -> setSubject (unwrapStateIdentifier state <> uname)
        otherwise -> setObject (Simple $ ArcIdentifier pos uname)
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

stateE :: IP StateE
stateE = do
  id <- reserved "state" *> colon *> token.identifier
  withArcParserState (State_ id)
    do
      condition <- sameOrIndented *> step
      stateParts <- indented *> (concat <$> block (onEntryE <|> onExitE <|> perspectiveOn <|> perspectiveOf))
      subStates <- indented *> many stateE
      pure $ StateE {id, condition, stateParts, subStates}

perspectiveOn :: IP (List StateQualifiedPart)
perspectiveOn = try $ withPos do
  pos <- getPosition
  (stp :: Step) <- perspectiveOnKeywords *> step
  protectObject do
    setObject stp
    isIndented >>= if _
      then concat <$> entireBlock perspectivePart
      else pure Nil

perspectiveOf :: IP (List StateQualifiedPart)
perspectiveOf = try $ withPos do
  pos <- getPosition
  (subject :: String) <- perspectiveOnKeywords *> arcIdentifier
  protectSubject do
    setSubject subject
    isIndented >>= if _
      then concat <$> entireBlock perspectivePart
      else pure Nil

perspectivePart :: IP (List StateQualifiedPart)
perspectivePart = fix \p ->
      (singleton <<< R <$> roleVerbs )
  <|> (singleton <<< P <$> propertyVerbs )
  <|> inState
  <|> onEntryE
  <|> onExitE
  <|> actionE

inState :: IP (List StateQualifiedPart)
inState = do
  stateId <- State_ <$> (inStateKeywords *> arcIdentifier)
  -- We can use withArcParserState here. It will concatenate the local
  -- state identifier to the state identifier provided by the surrounding context.
  -- The assumption being that we do not refer to states out of the context that the
  -- user or role is defined in.
  withArcParserState stateId
    (isIndented >>= if _
      then concat <$> entireBlock (
           (singleton <<< R <$> roleVerbs)
        <|> (singleton <<< P <$> propertyVerbs)
        <|> perspectiveOn
        <|> perspectiveOf
        <|> actionE
        )
      else pure Nil)

inStateKeywords :: IP String
inStateKeywords = reserved "in" *> reserved "state" *> colon *> pure "inState"

onExitKeywords :: IP String
onExitKeywords = reserved "on" *> reserved "exit" *> colon *> pure "inState"

onEntryKeywords :: IP String
onEntryKeywords = reserved "on" *> reserved "entry" *> colon *> pure "inState"

perspectiveOfKeywords :: IP String
perspectiveOfKeywords = reserved "perspective" *> reserved "of" *> colon *> pure "perspectiveOf"

perspectiveOnKeywords :: IP String
perspectiveOnKeywords = reserved "perspective" *> reserved "on" *> colon *> pure "perspectiveOn"

onEntryE :: IP (List StateQualifiedPart)
onEntryE = do
  stateTrans <- onEntryKeywords *> arcIdentifier
  -- The transition will be fully qualified.
  protectOnEntry do
    setOnEntry stateTrans
    isIndented >>= if _
      then concat <$> entireBlock (notificationE <|> automaticEffectE)
      else pure Nil

onExitE :: IP (List StateQualifiedPart)
onExitE = do
  stateTrans <- onExitKeywords *> arcIdentifier
  -- The transition will be fully qualified.
  protectOnExit do
    setOnExit stateTrans
    isIndented >>= if _
      then concat <$> entireBlock (notificationE <|> automaticEffectE)
      else pure Nil

automaticEffectE :: IP (List StateQualifiedPart)
automaticEffectE = do
  start <- getPosition
  reserved "do"
  isIndented >>= if _
    then do
      effect <- Left <$> entireBlock assignment <|> Right <$> letWithAssignment
      end <- getPosition
      {subject, object, onEntry, onExit} <- getArcParserState
      case subject, onEntry, onExit of
        Just sb, Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE {subject: sb, object, transition, effect, start, end}
        Just sb, Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE {subject: sb, object, transition, effect, start, end}
        Nothing, _, _ -> fail "A subject is required"
        _, Nothing, Nothing -> fail "A state transition is required"
        _, Just _, Just _ -> fail "State transition inside state transition is not allowed"
    else pure Nil

notificationE :: IP (List StateQualifiedPart)
notificationE = do
  start <- getPosition
  reserved "notify"
  -- User role either specified here or taken from state.
  usr <- optionMaybe arcIdentifier
  level <- notificationLevel
  end <- getPosition
  {subject, onEntry, onExit} <- getArcParserState
  case usr of
    Just user -> case onEntry, onExit of
      Just transition, Nothing -> pure $ singleton $ N $ NotificationE {user, transition, level, start, end}
      Nothing, Just transition -> pure $ singleton $ N $ NotificationE {user, transition, level, start, end}
      Nothing, Nothing -> fail "A state transition is required"
      _, _ -> fail "State transition inside state transition is not allowed"
    Nothing -> case subject, onEntry, onExit of
      Just u, Just transition, Nothing -> pure $ singleton $ N $ NotificationE {user: u, transition, level, start, end}
      Just u, Nothing, Just transition -> pure $ singleton $ N $ NotificationE {user: u, transition, level, start, end}
      Nothing, _, _ -> fail "A subject is required"
      _, Nothing, Nothing -> fail "A state transition is required"
      _, Just _, Just _ -> fail "State transition inside state transition is not allowed"

  where
    notificationLevel :: IP NotificationLevel
    notificationLevel = do
      v <- token.identifier
      case v of
        "Alert" -> pure Alert
        _ -> fail "Not a notification level"

-- only Consult
-- only Consult
-- only Consult, Create
-- only Consult,
--    Create
-- all except Delete, Remove
-- etc.
roleVerbs :: IP RoleVerbE
roleVerbs = do
  -- subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      (rv :: RoleVerbList) <- option All roleVerbList
      end <- getPosition
      pure $ RoleVerbE {subject: s, object: o, state, roleVerbs: rv, start, end}
    _, _ -> fail "User role and object of perspective must be given"

propertyVerbs :: IP PropertyVerbE
propertyVerbs = do
  -- subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      -- <PropertyVerb>+: (view <View> | <Property>+)
      start <- getPosition
      (pv :: List PropertyVerb) <- option allPropertyVerbs lotsOfVerbs
      (propsOrView :: PropsOrView) <- option AllProperties
        (colon *> reserved "view" *> (View <$> arcIdentifier)) <|>
        (colon *> (Properties <$> lotsOfProperties))
      end <- getPosition
      pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView, start, end}
    _, _ -> fail "User role and object of perspective must be given"
  where
    lotsOfVerbs :: IP (List PropertyVerb)
    lotsOfVerbs = Cons <$> propertyVerb <*> (many (sameOrIndented *> token.comma *> propertyVerb))

    allPropertyVerbs :: List PropertyVerb
    allPropertyVerbs = (RemovePropertyValue : (DeleteProperty : (AddPropertyValue : (SetPropertyValue : Nil))))

    lotsOfProperties :: IP (List String)
    lotsOfProperties = Cons <$> arcIdentifier <*> (many (sameOrIndented *> token.comma *> arcIdentifier))

roleVerbList :: IP RoleVerbList
roleVerbList
 =  (reserved "only" *> (Including <<< fromFoldable <$> lotsOfVerbs)) <|>
    (reserved "except" *> (Excluding <<< fromFoldable <$> lotsOfVerbs))
  where
    lotsOfVerbs :: IP (List RoleVerb)
    lotsOfVerbs = Cons <$> roleVerb <*> (many (sameOrIndented *> token.comma *> roleVerb))

roleVerb :: IP RoleVerb
roleVerb = do
  v <- token.identifier
  case v of
    "Remove" -> pure Remove
    "Delete" -> pure Delete
    "Create" -> pure Create
    "CreateAndFill" -> pure CreateAndFill
    "Fill" -> pure Fill
    "Unbind" -> pure Unbind
    "RemoveFiller" -> pure RemoveFiller
    "Move" -> pure Move
    _ -> fail "Not a role verb"

propertyVerb :: IP PropertyVerb
propertyVerb = do
  v <- token.identifier
  case v of
    "RemovePropertyValue" -> pure RemovePropertyValue
    "DeleteProperty" -> pure DeleteProperty
    "AddPropertyValue" -> pure AddPropertyValue
    "SetPropertyValue" -> pure SetPropertyValue
    _ -> fail "Not a property verb"

actionE :: IP (List StateQualifiedPart)
actionE = do
  start <- getPosition
  id <- reserved "action" *> token.identifier <* colon
  {subject, state, object} <- getArcParserState
  case subject, object of
    Nothing, _ -> fail "User role is not specified"
    _, Nothing -> fail "object of perspective must be given"
    Just s, Just o -> do
      effect <- Left <$> entireBlock assignment <|> Right <$> letWithAssignment
      end <- getPosition
      pure $ singleton $ AC $ ActionE {id, subject: s, object: o, state, effect, start, end}

reserved' :: String -> IP String
reserved' name = token.reserved name *> pure name
