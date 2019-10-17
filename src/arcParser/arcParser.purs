module Perspectives.Parsing.Arc where

import Control.Alt (map, void, (<|>))
import Data.Array (elemIndex)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved, colon, lowerCaseName)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition, IP, getPosition, withEntireBlock, nextLine)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.Action (Verb(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Prelude (bind, discard, join, pure, ($), (*>), (<*), (<<<), (<>), (==), (>>=))
import Text.Parsing.Parser (fail, failWithPosition)
import Text.Parsing.Parser.Combinators (lookAhead, option, optionMaybe, try, (<?>))
import Unsafe.Coerce (unsafeCoerce)

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
        ctxt | isContextKind ctxt -> contextE
        role | isRoleKind role -> roleE
        otherwise -> fail "unknown keyword for a context."

    aspectE :: IP ContextPart
    aspectE = do
      void $ reserved "aspect" *> colon
      pos <-getPosition
      aspect <- arcIdentifier
      pure $ ContextAspect aspect pos

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
    else failWithPosition ("(NotWellFormedName) The name '" <> modelName <> "' is not well-formed (it cannot be expanded to a fully qualified name)") (unsafeCoerce pos)

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
          isComputed <- lookAhead $ optionMaybe (try (reserved "=" *> reserved "apicall"))
          case isComputed of
            (Just _) -> computedRole_ pos knd uname
            Nothing -> do
              isCalculated <- (lookAhead $ optionMaybe (reserved "="))
              case isCalculated of
                Nothing -> otherRole_ pos knd uname
                (Just _) -> calculatedRole_ pos knd uname

    rolePart :: IP RolePart
    rolePart = do
      typeOfPart <- lookAhead ((reserved' "property" <|> reserved' "perspective" <|> reserved' "view" <|> reserved' "aspect") <?> "property, perspectives, aspect or view")
      case typeOfPart of
        "property" -> propertyE
        "perspective" -> perspectiveE
        "aspect" -> aspectE
        _ -> viewE

    aspectE :: IP RolePart
    aspectE = do
      void $ reserved "aspect" *> colon
      pos <- getPosition
      aspect <- arcIdentifier
      pure $ RoleAspect aspect pos

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

    computedRole_ :: ArcPosition -> RoleKind -> String -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    computedRole_ pos knd uname = try do
      functionName <- reserved "=" *> reserved "apicall" *> token.stringLiteral
      computedType <- reserved "returns" *> colon *> arcIdentifier
      pure {uname, knd, pos, parts: Cons (Computation functionName computedType) Nil }

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
  _ <- lookAhead (token.reservedOp "=" <|> (token.reservedOp "(")) <?> "property attributes in parentheses, or '=' followed by a calculation."
  (calculatedProperty pos uname) <|> (enumeratedProperty pos uname)

  where

    calculatedProperty :: ArcPosition -> String -> IP RolePart
    calculatedProperty pos uname = try do
      token.reservedOp "="
      calc <- step
      pure $ PE $ PropertyE
        { id: uname
        , range: Nothing
        , propertyParts: Cons (Calculation' calc) Nil, pos: pos}


    enumeratedProperty :: ArcPosition -> String -> IP RolePart
    enumeratedProperty pos uname = try do
      attributes <- propertyAttributes
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
    actionE
  where
    perspective_ :: IP (List PerspectivePart)
    perspective_ = do
      (object :: PerspectivePart) <- reserved "perspective" *> reserved "on" *> colon *> arcIdentifier >>= pure <<< Object
      (mdefaultView :: Maybe PerspectivePart) <- optionMaybe (token.parens $ arcIdentifier >>= pure <<< DefaultView)
      (actions :: List PerspectivePart) <- option Nil (colon *> token.commaSep minimalAction')
      case mdefaultView of
        Nothing -> pure $ Cons object actions
        (Just defaultView) -> pure $ Cons defaultView (Cons object actions)

    minimalAction' :: IP PerspectivePart
    minimalAction' = do
      pos <- getPosition
      verbName <- token.identifier
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
-- |  Consult
-- |    indirectObject: AnotherRole
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
      verb <- (arcIdentifier <?> "Consult, Change, Create, Delete or a valid identifier") >>= pure <<< constructVerb
      mview <- optionMaybe (reserved "with" *> arcIdentifier >>= pure <<< ObjectView)
      pure {pos, verb, mview}

    actionParts :: IP (List ActionPart)
    actionParts = do
      lookAhead (reserved "subjectView" <|> reserved "indirectObject") <?> "subjectView or indirectObject"
      subjectView <|> indirectObject

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

reserved' :: String -> IP String
reserved' name = token.reserved name *> pure name
