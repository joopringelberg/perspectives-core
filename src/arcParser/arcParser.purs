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
import Data.Array (find, fromFoldable)
import Data.List (List(..), concat, filter, many, some, null, singleton, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextE(..), ContextPart(..), NotificationE(..), PropertyE(..), PropertyPart(..), PropertyVerbE(..), PropsOrView(..), RoleE(..), RoleIdentification(..), RolePart(..), RoleVerbE(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), ViewE(..))
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved, lowerCaseName)
import Perspectives.Parsing.Arc.IndentParser (IP, arcPosition2Position, entireBlock, entireBlock1, getArcParserState, getCurrentContext, getCurrentState, getObject, getPosition, getStateIdentifier, getSubject, inSubContext, isIndented, isNextLine, nestedBlock, protectObject, protectOnEntry, protectOnExit, protectSubject, setObject, setOnEntry, setOnExit, setSubject, withArcParserState, withEntireBlock)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement (assignment, letWithAssignment)
import Perspectives.Parsing.Arc.Statement.AST (Statements(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.Sentence (SentencePart(..), Sentence(..))
import Perspectives.Representation.State (NotificationLevel(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleKind(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..), RoleVerbList(..))
import Prelude (bind, discard, flip, not, pure, show, ($), (&&), (*>), (<$>), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.Indent (block1, checkIndent, sameOrIndented, withPos)
import Text.Parsing.Parser (fail, failWithPosition)
import Text.Parsing.Parser.Combinators (between, lookAhead, option, optionMaybe, sepBy, try, (<?>))
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)

contextE :: IP ContextPart
contextE = withPos do
  -- log "contextE"
  knd <- contextKind
  pos <- getPosition
  uname <- arcIdentifier
  -- ook hier geldt: als op de volgende regel en geïndenteerd, dan moet de deelparser slagen en het hele blok consumeren.
  isIndented' <- isIndented
  isNextLine' <- isNextLine
  elements <- if isIndented' && isNextLine'
    then inSubContext uname
      (getCurrentContext >>= \subContext ->
        withArcParserState (ContextState subContext Nothing)
            -- The state identifier is fully qualified.
            -- The parser starts with StateIdentifier ""
            -- `domain` results in StateIdentifier model:<ModelName>
            -- Each context results in: StateIdentifier model:ModelName$<ContextName>
            (withPos do
              uses <- many $ checkIndent *> useE
              ind <- option Nil $ checkIndent *> contextIndexedE
              aspects <- many $ checkIndent *> contextAspectE
              states <- scanIdentifier >>= case _ of
                "state" -> singleton <<< STATE <$> stateE
                _ -> pure Nil
              -- log "in ContextE, past looking for a state"
              rolesAndContexts <- if null uses && null ind && null aspects && null states
                then entireBlock1 contextPart
                else entireBlock contextPart
              pure $ ind <> states <> (uses <> aspects <> rolesAndContexts)))
    else pure Nil
  -- Notice: uname is unqualified.
  pure $ CE $ ContextE { id: uname, kindOfContext: knd, contextParts: elements, pos: pos}

  where

    contextPart :: IP ContextPart
    contextPart = do
      keyword <- scanIdentifier
      case keyword of
        "domain" -> contextE
        "case" -> contextE
        "party" -> contextE
        "activity" -> contextE
        "thing" -> lookAhead (reservedIdentifier *> arcIdentifier) >>=
          explicitObjectState >>=
            flip withArcParserState thingRoleE
        "user" -> lookAhead (reservedIdentifier *> arcIdentifier) >>=
          explicitSubjectState >>=
            flip withArcParserState userRoleE
        "context" -> lookAhead (reservedIdentifier *> arcIdentifier) >>=
          explicitObjectState >>=
            flip withArcParserState contextRoleE
        "external" -> explicitObjectState "External" >>=
          flip withArcParserState externalRoleE
        _ -> fail "Expected: domain, case, party, activity; or thing, user, context, external"

    explicitSubjectState :: String -> IP StateSpecification
    explicitSubjectState segments = getCurrentContext >>= \ctxt@(ContextType ccontext) -> getPosition >>= \pos -> pure $ SubjectState (ExplicitRole ctxt (EnumeratedRoleType $ ccontext <> "$" <> segments) pos) Nothing

    explicitObjectState :: String -> IP StateSpecification
    explicitObjectState segments = getCurrentContext >>= \ctxt@(ContextType ccontext) -> getPosition >>= \pos -> pure $ ObjectState (ExplicitRole ctxt (EnumeratedRoleType $ ccontext <> "$" <> segments) pos) Nothing

    contextKind :: IP ContextKind
    contextKind = (reserved "domain" *> pure Domain
      <|> reserved "case" *> pure Case
      <|> reserved "party" *> pure Party
      <|> reserved "activity" *> pure Activity) <?> "domain, case, party or activity"

    contextAspectE :: IP ContextPart
    contextAspectE = do
      void $ reserved "aspect"
      pos <-getPosition
      aspect <- arcIdentifier
      pure $ ContextAspect aspect pos

    contextIndexedE :: IP (List ContextPart)
    contextIndexedE = do
      void $ reserved "indexed"
      pos <- getPosition
      indexedName <- arcIdentifier
      pure $ singleton $ IndexedContext indexedName pos

    useE :: IP ContextPart
    useE = do
      prefix <- reserved "use" *> lowerCaseName
      void $ reserved "for"
      pos <- getPosition
      modelName <- arcIdentifier
      if isQualifiedWithDomein modelName
        then pure $ PREFIX prefix modelName
        else fail ("(NotWellFormedName) The name '" <> modelName <> "' is not well-formed (it cannot be expanded to a fully qualified name)")


domain :: IP ContextE
domain = do
  r <- token.whiteSpace *> contextE
  case r of
    (CE d@(ContextE {kindOfContext})) -> if kindOfContext == Domain then pure d else fail "The kind of the context must be 'Domain'"
    otherwise -> fail "Domain must be a context"

-- Either fails with consumed=false if the input stream does not start with "user",
-- or fails with consumed=true,
-- or succeeds with consumed=true.
userRoleE :: IP ContextPart
userRoleE = protectSubject $ withEntireBlock
  (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
  user_
  rolePart
  where
    user_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    user_ = do
      -- log "in user_"
      pos <- getPosition
      kind <- reserved "user" *> pure UserRole
      uname <- arcIdentifier
      -- We've added the role name to the state before running userRoleE.
      ct@(ContextType ctxt) <- getCurrentContext
      setSubject (ExplicitRole ct ( EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
      -- userRoleE cannot fail in the last line.
      ((calculatedRole_  uname kind pos) <|> (enumeratedRole_ uname kind pos))

thingRoleE :: IP ContextPart
thingRoleE = protectObject $ withEntireBlock
  (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
  thing_
  rolePart
  where
    thing_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    thing_ = do
      pos <- getPosition
      kind <- reserved "thing" *> pure RoleInContext
      uname <- arcIdentifier
      ct@(ContextType ctxt) <- getCurrentContext
      setObject (ExplicitRole ct ( EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
      ((calculatedRole_  uname kind pos) <|> (enumeratedRole_ uname kind pos))

contextRoleE :: IP ContextPart
contextRoleE = protectObject $ withEntireBlock
  (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
  context_
  rolePart
  where
    context_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    context_ = do
      pos <- getPosition
      kind <- reserved "context" *> pure ContextRole
      uname <- arcIdentifier
      ct@(ContextType ctxt) <- getCurrentContext
      setObject (ExplicitRole ct ( EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
      enumeratedRole_ uname kind pos

externalRoleE :: IP ContextPart
externalRoleE = protectObject $ withEntireBlock
  (\{uname, knd, pos, parts} elements -> RE $ RoleE {id: uname, kindOfRole: knd, roleParts: elements <> parts, pos: pos})
  external_
  rolePart
  where
    external_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
    external_ = do
      pos <- getPosition
      knd <- reserved "external" *> pure ExternalRole
      ct@(ContextType ctxt) <- getCurrentContext
      setObject (ExplicitRole ct ( EnumeratedRoleType (ctxt <> "$External")) pos)
      pure {uname: "External", knd, pos, parts: Nil}

calculatedRole_ :: String -> RoleKind -> ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
calculatedRole_ uname knd pos = do
  calculation <- reserved "=" *> step >>= pure <<< Calculation
  pure {uname, knd, pos, parts: Cons calculation Nil }

enumeratedRole_ :: String -> RoleKind -> ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart))
enumeratedRole_ uname knd pos = do
  attributes <- option Nil roleAttributes
  filledBy' <- filledBy
  pure {uname, knd, pos, parts: attributes <> filledBy'}
  where
    -- We cannot use token.commaSep or sebBy to separate the role attributes;
    -- it will cause looping as long as it is used
    -- both in userRoleE and thingRoleE. I do not understand why.
    roleAttributes :: IP (List RolePart)
    roleAttributes = token.parens ((mandatory <|> functional <|> unlinked) `sepBy` token.symbol ",")

    mandatory :: IP RolePart
    mandatory = (reserved "mandatory" *> (pure (MandatoryAttribute true)))

    functional :: IP RolePart
    functional = (reserved "functional" *> (pure (FunctionalAttribute true)))

    unlinked :: IP RolePart
    unlinked = (reserved "unlinked" *> (pure UnlinkedAttribute))

-- | This parser always succeeds.
-- | If it detects no filledBy clause, it leaves consumed state as it is and returns Nil.
filledBy :: IP (List RolePart)
filledBy = (option Nil $
  (reserved "filledBy" *> token.commaSep arcIdentifier >>= pure <<< map FilledByAttribute)
  )

rolePart :: IP RolePart
rolePart = do
  (Tuple first second) <- twoReservedWords
  case first, second of
    "perspective", "on" -> SQP <$> perspectiveOn
    "perspective", "of" -> SQP <$> perspectiveOf
    "in", "state" -> SQP <$> inState
    "on", "entry" -> SQP <$> onEntryE
    "on", "exit" -> SQP <$> onExitE
    "state", _ -> ROLESTATE <$> stateE
    "aspect", _ -> aspectE
    "indexed", _ -> indexedE
    "property", _ -> propertyE
    "view", _ -> viewE
    _, _ -> fail "Expected: perspective (on/of), in state, on (entry/exit), state, aspect, indexed, property, view"

aspectE :: IP RolePart
aspectE = do
  void $ reserved "aspect"
  pos <- getPosition
  aspect <- arcIdentifier
  pure $ RoleAspect aspect pos

indexedE :: IP RolePart
indexedE = do
  void $ reserved "indexed"
  pos <- getPosition
  indexedName <- arcIdentifier
  pure $ IndexedRole indexedName pos

propertyE :: IP RolePart
propertyE = do
  pos <- getPosition
  uname <- reserved "property" *> arcIdentifier
  (calculatedProperty pos uname) <|> (enumeratedProperty pos uname)
  where
    calculatedProperty :: ArcPosition -> String -> IP RolePart
    calculatedProperty pos uname = do
      token.reservedOp "="
      calc <- step
      pure $ PE $ PropertyE
        { id: uname
        , range: Nothing
        , propertyParts: Cons (Calculation' calc) Nil, pos: pos}

    -- This parser always succeeds, either with or without consuming part of the input stream.
    enumeratedProperty :: ArcPosition -> String -> IP RolePart
    enumeratedProperty pos uname = do
      attributes <- option Nil propertyAttributes
      ran <- pure $ (unsafePartial case _ of Ran r -> r) <$> find (case _ of
        Ran r -> true
        _ -> false) attributes
      pure $ PE $ PropertyE
        { id: uname
        , range: ran
        , propertyParts: filter (case _ of
            Ran _ -> false
            _ -> true) attributes
        , pos: pos}

    -- the opening parenthesis functions as the recognizer: when found, the rest of the stream **must** start on
    -- a valid property attribute specification.
    propertyAttributes :: IP (List PropertyPart)
    propertyAttributes = token.parens ((mandatory <|> functional <|> range) `sepBy` token.symbol ",")
        where
          mandatory :: IP PropertyPart
          mandatory = (reserved "mandatory" *> (pure (MandatoryAttribute' true)))

          functional :: IP PropertyPart
          functional = (reserved "functional" *> (pure (FunctionalAttribute' true)))

          range :: IP PropertyPart
          range = (reserved "Boolean" *> (pure $ Ran PBool)
            <|> reserved "Number" *> (pure $ Ran PNumber)
            <|> reserved "String" *> (pure $ Ran PString)
            <|> reserved "DateTime" *> (pure $ Ran PDate)) <?> "Boolean, Number, String or DateTime"

viewE :: IP RolePart
viewE = do
  pos <- getPosition
  uname <- reserved "view" *> arcIdentifier
  refs <- sameOrIndented *> (token.parens (token.commaSep1 arcIdentifier))
  pure $ VE $ ViewE {id: uname, viewParts: refs, pos: pos}

-- | Nothing `appendSegment` s = Just s
-- | Just m `appendSegment` s = Just m$s
appendSegment :: Maybe String -> String -> Maybe String
appendSegment Nothing s = Just s
appendSegment (Just m) s = Just (m <> "$" <> s)

addSubState :: StateSpecification -> String -> StateSpecification
-- append the local substate name to the segments we (maybe) already have.
addSubState (ContextState (ContextType cid) s) localSubStateName = ContextState
  (ContextType cid)
  (s `appendSegment` localSubStateName)
-- append the local substate name to the segments we maybe already have.
addSubState (SubjectState roleIdent s) localSubStateName = SubjectState roleIdent (s `appendSegment` localSubStateName)
addSubState (ObjectState roleIdent s) localSubStateName = ObjectState roleIdent (s `appendSegment` localSubStateName)

stateE :: IP StateE
stateE = withPos do
  id <- reserved "state" *> token.identifier
  -- log "stateE"
  -- Can be ContextState, SubjectState and ObjectState.
  {state} <- getArcParserState
  withArcParserState (state `addSubState` id)
    do
      -- In IP, defined as an IntendParser on top of StateT ArcParserState Identity, we now have a
      -- new ArcParserState object with member 'state' being StateIdentifier "{previousStateIdentifier}$id".
      condition <- sameOrIndented *> reserved "=" *> step
      allParts <- concat <$> nestedBlock statePart
      stateParts <- pure $ filter (case _ of
        SUBSTATE _ -> false
        _ -> true) allParts
      subStates <- pure $ (unsafePartial case _ of SUBSTATE s -> s) <$> filter (case _ of
        SUBSTATE _ -> true
        _ -> false) allParts
      nestedStateId <- getStateIdentifier
      pure $ StateE {id: nestedStateId, condition, stateParts, subStates}
  where
    statePart :: IP (List StateQualifiedPart)
    statePart = do
      (Tuple first second) <- twoReservedWords
      -- log ("statePart: " <> first <> ", " <> second)
      case first, second of
        "state", _ -> singleton <<< SUBSTATE <$> stateE
        "on", "entry" -> onEntryE
        "on", "exit" -> onExitE
        "perspective", "on" -> perspectiveOn
        "perspective", "of" -> perspectiveOf
        _, _ -> fail ("Expected: on (entry/exit), perspective (on/of) (but found '" <> first <> "'.)")

-- We need to use `try` because this parser will consume "perspective" from the
-- expression "perspective of".
perspectiveOn :: IP (List StateQualifiedPart)
perspectiveOn = try $ withPos do
  -- log "perspectiveOn"
  pos <- getPosition
  (stp :: Step) <- perspectiveOnKeywords *> step
  protectObject do
    ctxt <- getCurrentContext
    setObject (ImplicitRole ctxt stp)
    -- If 'perspectivePart' fails, nestedBlock will fail and thus perspectiveOn will fail.
    concat <$> nestedBlock perspectivePart
    -- In contrast, with the non-determinate expression outcommented below,
    -- if perspectivePart fails, perspectiveOn succeeds.
    -- option Nil (indented' *> (concat <$> entireBlock perspectivePart))

-- We need to use `try` because this parser will consume "perspective" from the
-- expression "perspective of".
perspectiveOf :: IP (List StateQualifiedPart)
perspectiveOf = try $ withPos do
  -- log "perspectiveOf"
  pos <- getPosition
  (subject :: Step) <- perspectiveOfKeywords *> step
  protectSubject do
    ctxt <- getCurrentContext
    setSubject (ImplicitRole ctxt subject)
    concat <$> nestedBlock perspectivePart

perspectivePart :: IP (List StateQualifiedPart)
perspectivePart = do
  (Tuple first second) <- twoReservedWords
  case first, second of
    "view", _ -> singleton <<< P <$> propertyVerbs
    "props", _ -> singleton <<< P <$> propertyVerbs
    "only", _ -> singleton <<< R <$> roleVerbs
    "except", _ -> singleton <<< R <$> roleVerbs
    "all", "roleverbs" -> singleton <<< R <$> roleVerbs
    "in", "state" -> inState
    "on", "entry" -> onEntryE
    "on", "exit" -> onExitE
    "action", _ -> actionE
    "perspective", "on" -> perspectiveOn
    "perspective", "of" -> perspectiveOf
    _, _ -> fail "Expected: view, props, only, except, all, in, on, action, perspective"

-- | Looking ahead, find at least one reserved identifier, two if possible.
-- | If only one is found, returns Tuple <theword> "".
-- | If none is found, returns Tuple "" "".
twoReservedWords :: IP (Tuple String String)
twoReservedWords = option (Tuple "" "") $ lookAhead (Tuple <$> reservedIdentifier <*> (option "" reservedIdentifier))

inState :: IP (List StateQualifiedPart)
inState = do
  stateId <- inStateKeywords *> arcIdentifier
  mspecifier <- optionMaybe (reserved "of" *> (reserved' "subject" <|> reserved' "object" <|> reserved' "context"))
  currentState <- getCurrentState
  stateSpec <- case mspecifier of
    Nothing -> pure $ addSubState currentState stateId
    Just "object" -> ObjectState <$> getObject <*> pure (Just stateId)
    Just "subject" -> SubjectState <$> getSubject <*> pure (Just stateId)
    Just "context" -> ContextState <$> getCurrentContext <*> pure (Just stateId)
    -- This case will never occur.
    _ -> fail "This will never occur"
  -- We can use withArcParserState here. It will concatenate the local
  -- state identifier to the state identifier provided by the surrounding context.
  -- The assumption being that we do not refer to states out of the context that the
  -- user or role is defined in.
  withArcParserState stateSpec
    (concat <$> nestedBlock statePart)
  where
    statePart :: IP (List StateQualifiedPart)
    statePart = do
      (Tuple first second) <- twoReservedWords
      -- log ("statePart: " <> first <> ", " <> second)
      case first, second of
        "view", _ -> singleton <<< P <$> propertyVerbs
        "props", _ -> singleton <<< P <$> propertyVerbs
        "only", _ -> singleton <<< R <$> roleVerbs
        "except", _ -> singleton <<< R <$> roleVerbs
        "all", "roleverbs" -> singleton <<< R <$> roleVerbs
        "perspective", "on" -> perspectiveOn
        "perspective", "of" -> perspectiveOf
        "action", _ -> actionE
        _, _ -> fail ("Expected: view, props, only, except, all roleverbs, perspective (on/of), action expected (but found '" <> first <> "'.)")

inStateKeywords :: IP String
inStateKeywords = reserved "in" *> reserved "state" *> pure "inState"

onExitKeywords :: IP String
onExitKeywords = reserved "on" *> reserved "exit" *> pure "onExit"

onEntryKeywords :: IP String
onEntryKeywords = reserved "on" *> reserved "entry" *> pure "onEntry"

perspectiveOfKeywords :: IP String
perspectiveOfKeywords = reserved "perspective" *> reserved "of" *> pure "perspectiveOf"

perspectiveOnKeywords :: IP String
perspectiveOnKeywords = reserved "perspective" *> reserved "on" *> pure "perspectiveOn"

-- | Parses
-- |    on entry
-- |    on entry: <stateName>
onEntryE :: IP (List StateQualifiedPart)
onEntryE = do
  void $ onEntryKeywords
  stateSpec <- optionMaybe (reserved "of") >>= case _ of
    Just _ -> (subjectState <|> objectState <|> contextState <|> subState) <?> "one of subject, object or context or a state name"
    Nothing -> getCurrentState
  protectOnEntry do
    setOnEntry stateSpec
    concat <$> nestedBlock stateTransitionPart

subState :: IP StateSpecification
subState = addSubState <$> getCurrentState <*> arcIdentifier

subjectState :: IP StateSpecification
subjectState = reserved "subject" *> reserved "state" *> do
  {subject} <- getArcParserState
  case subject of
    Nothing -> fail "A subject is required for 'in state X of subject"
    Just s -> do
      msubState <- optionMaybe arcIdentifier
      pure $ SubjectState s msubState

objectState :: IP StateSpecification
objectState = reserved "object" *> reserved "state" *> do
  {object} <- getArcParserState
  case object of
    Nothing -> fail "An object is required for 'in state X of object'"
    Just s -> do
      msubState <- optionMaybe arcIdentifier
      pure $ ObjectState s msubState

contextState :: IP StateSpecification
contextState = reserved "context" *> reserved "state" *> do
  {currentContext} <- getArcParserState
  msubState <- optionMaybe arcIdentifier
  pure $ ContextState currentContext msubState

onExitE :: IP (List StateQualifiedPart)
onExitE = do
  void $ onExitKeywords
  stateSpec <- (optionMaybe (reserved "of") >>= case _ of
    Just _ -> (subjectState <|> objectState <|> contextState <|> subState) <?> "one of subject, object or context or a state name"
    Nothing -> getCurrentState)
  protectOnExit do
    setOnExit stateSpec
    concat <$> nestedBlock stateTransitionPart

stateTransitionPart :: IP (List StateQualifiedPart)
stateTransitionPart = do
  keyword <- scanIdentifier
  case keyword of
    "do" -> automaticEffectE
    "notify" -> notificationE
    _ -> fail "Expected: do, notify"

automaticEffectE :: IP (List StateQualifiedPart)
automaticEffectE = do
  start <- getPosition
  reserved "do"
  -- User role either specified here or taken from state.
  usr <- optionMaybe (reserved "for" *> step)
  isIndented >>= if _
    then do
      keyword <- scanIdentifier
      effect <- case keyword of
        "letE" -> fail "letE does not allow assignment operators, so this will not have an effect. Did you mean 'letA'?"
        "letA" -> Let <$> letWithAssignment
        _ -> Statements <<< fromFoldable <$> nestedBlock assignment <?> "an indented block of assignments."
      end <- getPosition
      {subject, object, onEntry, onExit, currentContext} <- getArcParserState
      -- log ("automaticEffectE: object = " <> show object)
      case usr of
        Nothing -> case subject, onEntry, onExit of
          Just sb, Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE {subject: sb, object, transition, effect, start, end}
          Just sb, Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE {subject: sb, object, transition, effect, start, end}
          Nothing, _, _ -> failWithPosition "A subject is required" (arcPosition2Position start)
          _, Nothing, Nothing -> fail "A state transition is required"
          _, Just _, Just _ -> fail "State transition inside state transition is not allowed"
        Just subject -> case Just subject, onEntry, onExit of
          Just sb, Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE {subject: ImplicitRole currentContext sb, object, transition, effect, start, end}
          Just sb, Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE {subject: ImplicitRole currentContext sb, object, transition, effect, start, end}
          Nothing, _, _ -> failWithPosition "A subject is required" (arcPosition2Position start)
          _, Nothing, Nothing -> fail "A state transition is required"
          _, Just _, Just _ -> fail "State transition inside state transition is not allowed"
    else pure Nil

notificationE :: IP (List StateQualifiedPart)
notificationE = do
  start <- getPosition
  reserved "notify"
  -- User role either specified here or taken from state.
  usr <- optionMaybe step
  message <- sentenceE
  end <- getPosition
  {subject, onEntry, onExit, currentContext, object} <- getArcParserState
  case usr of
    Just user -> case onEntry, onExit of
      Just transition, Nothing -> pure $ singleton $ N $ NotificationE {user: ImplicitRole currentContext user, transition, message, object, start, end}
      Nothing, Just transition -> pure $ singleton $ N $ NotificationE {user: ImplicitRole currentContext user, transition, message, object, start, end}
      Nothing, Nothing -> fail "A state transition is required"
      _, _ -> fail "State transition inside state transition is not allowed"
    Nothing -> case subject, onEntry, onExit of
      Just u, Just transition, Nothing -> pure $ singleton $ N $ NotificationE {user: u, transition, message, object, start, end}
      Just u, Nothing, Just transition -> pure $ singleton $ N $ NotificationE {user: u, transition, message, object, start, end}
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

-- | roleVerbs =
-- |	only ( <RoleVerb> {, <roleVerb>}+ )
-- |	|
-- |	excluding ( <RoleVerb> {, <roleVerb>}+ )
-- |  |
-- |  all roleverbs
roleVerbs :: IP RoleVerbE
roleVerbs = do
  -- log "roleVerbs"
  -- subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      (rv :: RoleVerbList) <- roleVerbList
      end <- getPosition
      -- log "finishing roleVerbs"
      pure $ RoleVerbE {subject: s, object: o, state, roleVerbs: rv, start, end}
    Nothing, Nothing -> fail "User role and object of perspective must be given"
    Nothing, (Just _) -> fail "User role must be given"
    (Just _), Nothing -> fail "Object of perspective must be given"
  where
    roleVerbList :: IP RoleVerbList
    roleVerbList
     =  inclusive <|> exclusive <|> (reserved "all" *> reserved "roleverbs" *> pure All) <?> "only, except, all roleverbs"
      where
        inclusive :: IP RoleVerbList
        inclusive = do
          -- log "inclusive"
          (reserved "only" *> (Including <<< fromFoldable <$> lotsOfVerbs))

        exclusive :: IP RoleVerbList
        exclusive = do
          -- log "exclusive"
          (reserved "except" *> (Excluding <<< fromFoldable <$> lotsOfVerbs))

        lotsOfVerbs :: IP (List RoleVerb)
        lotsOfVerbs = do
          -- log "lotsOfVerbs"
          token.parens (roleVerb `sepBy` token.symbol ",") <?> "a list of role verbs between parenthesis."

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

-- view <ArcIdentifier> [: (<PropertyVerb+)]
-- props (<ArcIdentifier>) [: (<PropertyVerb+)]
propertyVerbs :: IP PropertyVerbE
propertyVerbs = basedOnView <|> basedOnProps
  where
    basedOnView :: IP PropertyVerbE
    basedOnView = do
      -- log "basedOnView"
      -- subject and object must be present.
      {subject, object, state} <- getArcParserState
      case subject, object of
        Just s, Just o -> do
          -- view <ArcIdentifier> [: (<PropertyVerb+)]
          start <- getPosition
          view <- reserved "view" *> (View <$> arcIdentifier)
          (pv :: List PropertyVerb) <- option allPropertyVerbs lotsOfVerbs
          end <- getPosition
          pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView: view, start, end}
        _, _ -> fail "User role and object of perspective must be given"
    basedOnProps :: IP PropertyVerbE
    basedOnProps = do
      -- log "basedOnProps"
      -- subject and object must be present.
      {subject, object, state} <- getArcParserState
      case subject, object of
        Just s, Just o -> do
          -- props (<ArcIdentifier>) [: (<PropertyVerb+)]
          start <- getPosition
          view <- reserved "props" *> option AllProperties (Properties <$> lotsOfProperties)
          (pv :: List PropertyVerb) <- option allPropertyVerbs lotsOfVerbs
          end <- getPosition
          pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView: view, start, end}
        _, _ -> fail "User role and object of perspective must be given"

    lotsOfVerbs :: IP (List PropertyVerb)
    lotsOfVerbs = token.parens (propertyVerb `sepBy` token.symbol ",")

    allPropertyVerbs :: List PropertyVerb
    allPropertyVerbs = (RemovePropertyValue : (DeleteProperty : (AddPropertyValue : (SetPropertyValue : Nil))))

    lotsOfProperties :: IP (List String)
    lotsOfProperties = token.parens (arcIdentifier `sepBy` token.symbol ",")

propertyVerb :: IP PropertyVerb
propertyVerb = do
  v <- token.identifier
  case v of
    "RemovePropertyValue" -> pure RemovePropertyValue
    "DeleteProperty" -> pure DeleteProperty
    "AddPropertyValue" -> pure AddPropertyValue
    "SetPropertyValue" -> pure SetPropertyValue
    "Consult" -> pure Consult
    _ -> fail "Not a property verb"

actionE :: IP (List StateQualifiedPart)
actionE = do
  start <- getPosition
  id <- reserved "action" *> token.identifier
  {subject, state, object} <- getArcParserState
  case subject, object of
    Nothing, _ -> fail "User role is not specified"
    _, Nothing -> fail "object of perspective must be given"
    Just s, Just o -> do
      effect <- Statements <$> fromFoldable <$> block1 assignment <|> Let <$> letWithAssignment
      end <- getPosition
      pure $ singleton $ AC $ ActionE {id, subject: s, object: o, state, effect, start, end}

sentenceE :: IP Sentence
sentenceE = do
  start <- getPosition
  parts <- between (char '"') (char '"') (many (sentencePart <|> exprPart))
  _ <- whiteSpace
  end <- getPosition
  pure $ Sentence (fromFoldable parts)
  where
    sentencePart :: IP SentencePart
    sentencePart = do
      chars <- some (satisfy (\c -> not (c == '{') && not (c == '"')))
      pure $ HR $ fromCharArray $ fromFoldable chars

    exprPart :: IP SentencePart
    exprPart = CP <<< S <$> between (token.symbol "{") (token.symbol "}") step

reserved' :: String -> IP String
reserved' name = token.reserved name *> pure name

-- | This parser always succeeds with a string.
-- | If no reserved word is found, restores the parser state (uses try internally).
scanIdentifier :: IP String
scanIdentifier = option "" (lookAhead reservedIdentifier)
