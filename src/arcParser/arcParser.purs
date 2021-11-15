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
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextE(..), ContextPart(..), NotificationE(..), PropertyE(..), PropertyPart(..), PropertyVerbE(..), PropsOrView(..), RoleE(..), RoleIdentification(..), RolePart(..), RoleVerbE(..), SelfOnly(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), ViewE(..))
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved, lowerCaseName)
import Perspectives.Parsing.Arc.IndentParser (IP, arcPosition2Position, containsTab, entireBlock, entireBlock1, getArcParserState, getCurrentContext, getCurrentState, getObject, getPosition, getStateIdentifier, getSubject, inSubContext, isIndented, isNextLine, nestedBlock, protectObject, protectOnEntry, protectOnExit, protectSubject, setObject, setOnEntry, setOnExit, setSubject, withArcParserState, withEntireBlock)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement (assignment, letWithAssignment, twoReservedWords)
import Perspectives.Parsing.Arc.Statement.AST (Statements(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.Sentence (SentencePart(..), Sentence(..))
import Perspectives.Representation.State (NotificationLevel(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..), RoleVerbList(..))
import Prelude (bind, discard, flip, not, pure, ($), (&&), (*>), (<$>), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.Indent (checkIndent, sameOrIndented, withPos)
import Text.Parsing.Parser (fail, failWithPosition)
import Text.Parsing.Parser.Combinators (between, lookAhead, option, optionMaybe, sepBy, try, (<?>))
import Text.Parsing.Parser.String (char, satisfy)

-- | SEMI-BNF NOTATION
-- | We use a syntax to describe the grammar of the Perspectives Language that is derived from BNF:
-- |
-- | <production> -> refers to a production rule.
-- | [...] -> whatever is between brackets, zero or one time (optional)
-- | <production>* -> zero or more times the production.
-- | <production>+ -> one or more times the production.
-- |
-- | Grouping constructs:
-- | {...} -> whatever is between brackets, exactly once.
-- | {...}* -> whatever is between brackets, zero or more times
-- | {...}+ -> whatever is between brackets, one or more times
-- | {<p> | <q>} -> <p> or <q> (grouped for convenience or to disambiguate)
-- |
-- | (<p> <q>) -> <p> and <q> between parentheses, i.e. the parenthesis are part of the production.
-- | NOTE: parenthesis have no special meaning in this grammar syntax. Hence '(' and ')' are perfectly ordinary parts of a production.
-- |
-- | <p> | <q> -> <p> or alternatively <q>
-- |
-- | '{' -> the literal character "{"
-- |
-- | state -> the literal keyword "state". We don't write keywords between double quotes.
-- |
-- | NOTE: double quotes have no special meaning in this grammar syntax. Hence " is a perfectly ordinary part of a production.


-- | context =
-- | 	<contextKind> <ident>
-- | 		use <lowercaseName> for <ident>
-- | 		indexed <ident>
-- | 		aspect <ident>+
-- | 		state <state>
-- | 		{<context> | <role>}+
contextE :: IP ContextPart
contextE = withPos do
  -- | log "contextE"
  knd <- contextKind
  pos <- getPosition
  uname <- arcIdentifier
  -- | ook hier geldt: als op de volgende regel en geïndenteerd, dan moet de deelparser slagen en het hele blok consumeren.
  isIndented' <- isIndented
  isNextLine' <- isNextLine
  contextParts <- if isIndented' && isNextLine'
    then inSubContext uname
      (getCurrentContext >>= \subContext ->
        withArcParserState (ContextState subContext Nothing)
            -- | The state identifier is fully qualified.
            -- | The parser starts with StateIdentifier ""
            -- | `domain` results in StateIdentifier model:<ModelName>
            -- | Each context results in: StateIdentifier model:ModelName$<ContextName>
            (withPos do
              uses <- many $ checkIndent *> useE
              ind <- option Nil $ checkIndent *> contextIndexedE
              aspects <- many $ checkIndent *> contextAspectE
              otherContextParts <- if null uses && null ind && null aspects
                then entireBlock1 contextPart
                else entireBlock contextPart
              -- The parts nested below `on entry` or `on exit`:
              stateQualifiedParts <- pure $ filter (case _ of
                CSQP _ -> true
                _ -> false) otherContextParts
              -- All substates
              subStates <- pure $ (unsafePartial \(STATE s) -> s) <$> filter (case _ of
                STATE _ -> true
                _ -> false) otherContextParts
              -- Nested contexts and roles.
              rolesAndContexts <- pure $ filter (case _ of
                CSQP _ -> false
                STATE _ -> false
                _ -> true) otherContextParts
              -- The root state of this context. Every context has a root state; it may be empty.
              state <- pure $ STATE $ StateE
                { id: (ContextState subContext Nothing)
                , condition: Simple $ Value pos PBool "true"
                , stateParts: concat (map (unsafePartial \(CSQP l) -> l) stateQualifiedParts)
                , subStates
                }
              pure $ (state : (ind <>
                uses <>
                aspects <>
                rolesAndContexts))))
    else pure Nil
  -- | Notice: uname is unqualified.
  pure $ CE $ ContextE { id: uname, kindOfContext: knd, contextParts, pos: pos}

  where

    contextPart :: IP ContextPart
    contextPart = do
      keyword <- scanIdentifier
      case keyword of
        -- TODO voeg "state" en "on entry" en "on exit" toe.
        "domain" -> contextE
        "case" -> contextE
        "party" -> contextE
        "activity" -> contextE
        "thing" -> explicitObjectState >>= flip withArcParserState thingRoleE
        "user" -> explicitSubjectState >>= flip withArcParserState userRoleE
        "context" -> explicitObjectState >>= flip withArcParserState contextRoleE
        "external" -> externalRoleState >>= flip withArcParserState externalRoleE
        "state" -> STATE <$> stateE
        _ -> do
          (Tuple first second) <- twoReservedWords
          case first, second of
            "perspective", "on" -> CSQP <$> perspectiveOn
            "perspective", "of" -> CSQP <$> perspectiveOf
            _, _ -> fail "Expected: domain, case, party, activity; or thing, user, context, external, state, on entry, on exit"

    explicitSubjectState :: IP StateSpecification
    explicitSubjectState = do
      (Tuple segments isCalculated) <- lookAhead (Tuple <$> (reserved "user" *> token.identifier) <*> option false (reserved "=" *> pure true))
      ctxt@(ContextType ccontext) <- getCurrentContext
      pos <- getPosition
      if isCalculated
        then pure $ SubjectState (ExplicitRole ctxt (CR $ CalculatedRoleType $ ccontext <> "$" <> segments) pos) Nothing
        else pure $ SubjectState (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ ccontext <> "$" <> segments) pos) Nothing

    explicitObjectState :: IP StateSpecification
    explicitObjectState = do
      (Tuple segments isCalculated) <- lookAhead (Tuple <$> (reservedIdentifier *> token.identifier) <*> option false (reserved "=" *> pure true))
      ctxt@(ContextType ccontext) <- getCurrentContext
      pos <- getPosition
      if isCalculated
        then pure $ ObjectState (ExplicitRole ctxt (CR $ CalculatedRoleType $ ccontext <> "$" <> segments) pos) Nothing
        else pure $ ObjectState (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ ccontext <> "$" <> segments) pos) Nothing

    externalRoleState :: IP StateSpecification
    externalRoleState = do
      ctxt@(ContextType ccontext) <- getCurrentContext
      pos <- getPosition
      pure $ ObjectState (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ ccontext <> "$External") pos) Nothing

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
  void containsTab
  r <- token.whiteSpace *> contextE
  case r of
    (CE d@(ContextE {kindOfContext})) -> if kindOfContext == Domain then pure d else fail "The kind of the context must be 'Domain'"
    otherwise -> fail "Domain must be a context"

--------------------------------------------------------------------------------------------------
-- ROLE
--------------------------------------------------------------------------------------------------
-- | role = <calculatedRole> | <enumeratedRole> | <externalRole>
-- |
-- | calculatedRole =
-- | 	<roleKind> <ident> = <expression>
-- | 		<rolePart>*
-- |
-- | enumeratedRole =
-- | 	<roleKind> <ident> [(roleAttribute [, roleAttribute])] [filledBy <ident>]
-- | 		<rolePart>*
-- |
-- | roleKind = user | thing | context | external

-- | Either fails with consumed=false if the input stream does not start with "user",
-- | or fails with consumed=true,
-- | or succeeds with consumed=true.
userRoleE :: IP ContextPart
userRoleE = do
  -- `state` will be a role state specification.
  {state} <- getArcParserState
  protectSubject $ withEntireBlock
    (\{uname, knd, pos, parts, isEnumerated} elements -> RE $ RoleE
      { id: uname
      , kindOfRole: knd
      , roleParts: if isEnumerated
          then (createRoleState pos state elements) <> parts
          else elements <> parts
      , pos
      })
    user_
    rolePart
  where
    user_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
    user_ = do
      pos <- getPosition
      kind <- reserved "user" *> pure UserRole
      uname <- arcIdentifier
      -- | We've added the role name to the state before running userRoleE.
      ct@(ContextType ctxt) <- getCurrentContext
      isCalculated <- option false (lookAhead (reserved "=") *> pure true)
      if isCalculated
        then do
          setSubject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
          calculatedRole_ uname kind pos
        else do
          setSubject (ExplicitRole ct (ENR $ EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
          enumeratedRole_ uname kind pos

thingRoleE :: IP ContextPart
thingRoleE = do
  -- `state` will be a role state specification.
  {state} <- getArcParserState
  protectObject $ withEntireBlock
    (\{uname, knd, pos, parts, isEnumerated} elements -> RE $ RoleE
      { id: uname
      , kindOfRole: knd
      , roleParts: if isEnumerated
          then (createRoleState pos state elements) <> parts
          else elements <> parts
      , pos
      })
    thing_
    rolePart
  where
    thing_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
    thing_ = do
      pos <- getPosition
      kind <- reserved "thing" *> pure RoleInContext
      uname <- arcIdentifier
      ct@(ContextType ctxt) <- getCurrentContext
      isCalculated <- option false (lookAhead (reserved "=") *> pure true)
      if isCalculated
        then do
          setObject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
          calculatedRole_  uname kind pos
        else do
          setObject (ExplicitRole ct (ENR $ EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
          enumeratedRole_ uname kind pos

contextRoleE :: IP ContextPart
contextRoleE = do
  -- `state` will be a role state specification.
  {state} <- getArcParserState
  protectObject $ withEntireBlock
    (\{uname, knd, pos, parts, isEnumerated} elements -> RE $ RoleE
      { id: uname
      , kindOfRole: knd
      , roleParts: if isEnumerated
          then (createRoleState pos state elements) <> parts
          else elements <> parts
      , pos
      })
    context_
    rolePart
  where
    context_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
    context_ = do
      pos <- getPosition
      kind <- reserved "context" *> pure ContextRole
      uname <- arcIdentifier
      ct@(ContextType ctxt) <- getCurrentContext
      -- | Het is niet noodzakelijk een EnumeratedRole!
      isCalculated <- option false (lookAhead (reserved "=") *> pure true)
      if isCalculated
        then do
          setObject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
          calculatedRole_  uname kind pos
        else do
          setObject (ExplicitRole ct (ENR $ EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
          enumeratedRole_ uname kind pos

externalRoleE :: IP ContextPart
externalRoleE = do
  -- `state` will be a role state specification.
  {state} <- getArcParserState
  protectObject $ withEntireBlock
    (\{uname, knd, pos, parts} elements -> RE $ RoleE
      { id: uname
      , kindOfRole: knd
      , roleParts: (createRoleState pos state elements) <> parts
      , pos
      })
    external_
    rolePart
  where
    external_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
    external_ = do
      pos <- getPosition
      knd <- reserved "external" *> pure ExternalRole
      ct@(ContextType ctxt) <- getCurrentContext
      setObject (ExplicitRole ct (ENR $ EnumeratedRoleType (ctxt <> "$External")) pos)
      pure {uname: "External", knd, pos, parts: Nil, isEnumerated: true}

calculatedRole_ :: String -> RoleKind -> ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
calculatedRole_ uname knd pos = do
  calculation <- reserved "=" *> step >>= pure <<< Calculation
  pure {uname, knd, pos, parts: Cons calculation Nil, isEnumerated: false}

enumeratedRole_ :: String -> RoleKind -> ArcPosition -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
enumeratedRole_ uname knd pos = do
  attributes <- option Nil roleAttributes
  filledBy' <- filledBy
  pure {uname, knd, pos, parts: attributes <> filledBy', isEnumerated: true}
  where
    -- | We cannot use token.commaSep or sebBy to separate the role attributes;
    -- | it will cause looping as long as it is used
    -- | both in userRoleE and thingRoleE. I do not understand why.
    roleAttributes :: IP (List RolePart)
    roleAttributes = token.parens (((mandatory <|> functional <|> unlinked) <?> "mandatory, functional, unlinked.") `sepBy` token.symbol ",")

    mandatory :: IP RolePart
    mandatory = (reserved "mandatory" *> (pure (MandatoryAttribute true)))

    -- | Roles are by default functional.
    functional :: IP RolePart
    functional = (reserved "relational" *> (pure (FunctionalAttribute false)))

    unlinked :: IP RolePart
    unlinked = (reserved "unlinked" *> (pure UnlinkedAttribute))

-- | This parser always succeeds.
-- | If it detects no filledBy clause, it leaves consumed state as it is and returns Nil.
filledBy :: IP (List RolePart)
filledBy = (option Nil $
  (reserved "filledBy" *> token.commaSep arcIdentifier >>= pure <<< map FilledByAttribute)
  )

-- | rolePart =
-- | 	<perspectiveOn> |
-- | 	<perspectiveOf> |
-- | 	<inState> |
-- | 	<onEntry> |
-- | 	<onExit> |
-- | 	<state> |
-- | 	<aspect> |
-- | 	<indexed> |
-- | 	<property> |
-- | 	<view>
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

-- | Combine the RolePart elements that result from "on entry", "on exit" and "state" into a root state for the role.
-- | The elements we combine are: NotificationE, AutomaticEffectE and StateE.
createRoleState :: ArcPosition -> StateSpecification -> List RolePart -> List RolePart
createRoleState pos stateSpec roleParts = let
  -- The parts nested below `on entry` or `on exit`:
  stateQualifiedParts = filter (case _ of
    SQP _ -> true
    _ -> false) roleParts
  -- All substates
  subStates = (unsafePartial \(ROLESTATE s) -> s) <$> filter (case _ of
    ROLESTATE _ -> true
    _ -> false) roleParts
  otherParts = filter (case _ of
    SQP _ -> false
    ROLESTATE _ -> false
    _ -> true) roleParts
  state = ROLESTATE $ StateE
    { id: stateSpec
    , condition: Simple $ Value pos PBool "true"
    , stateParts: concat (map (unsafePartial \(SQP l) -> l) stateQualifiedParts)
    , subStates
    }
  in
    (state : otherParts)

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

    -- | This parser always succeeds, either with or without consuming part of the input stream.
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
    propertyAttributes = token.parens (((mandatory <|> functional <|> range) <?> "mandatory, relational or a range (Boolean, Number, String or DateTime)") `sepBy` token.symbol ",")
        where
          mandatory :: IP PropertyPart
          mandatory = (reserved "mandatory" *> (pure (MandatoryAttribute' true)))

          -- | Properties are by default functional.
          functional :: IP PropertyPart
          functional = (reserved "relational" *> (pure (FunctionalAttribute' false)))

          range :: IP PropertyPart
          range = (reserved "Boolean" *> (pure $ Ran PBool)
            <|> reserved "Number" *> (pure $ Ran PNumber)
            <|> reserved "String" *> (pure $ Ran PString)
            <|> reserved "DateTime" *> (pure $ Ran PDate))

viewE :: IP RolePart
viewE = do
  pos <- getPosition
  uname <- reserved "view" *> arcIdentifier
  refs <- sameOrIndented *> (token.parens (token.commaSep1 arcIdentifier))
  pure $ VE $ ViewE {id: uname, viewParts: refs, pos: pos}

-- Nothing `appendSegment` s = Just s
-- Just m `appendSegment` s = Just m$s
appendSegment :: Maybe String -> String -> Maybe String
appendSegment Nothing s = Just s
appendSegment (Just m) s = Just (m <> "$" <> s)

addSubState :: StateSpecification -> String -> StateSpecification
-- | append the local substate name to the segments we (maybe) already have.
addSubState (ContextState (ContextType cid) s) localSubStateName = ContextState
  (ContextType cid)
  (s `appendSegment` localSubStateName)
-- | append the local substate name to the segments we maybe already have.
addSubState (SubjectState roleIdent s) localSubStateName = SubjectState roleIdent (s `appendSegment` localSubStateName)
addSubState (ObjectState roleIdent s) localSubStateName = ObjectState roleIdent (s `appendSegment` localSubStateName)

-- | state =
-- | 	state <ident> = <expression>
-- | 		<state>*
-- | 		<onEntry>
-- | 		<onExit>
-- | 		<perspectiveOn>
-- | 		<perspectiveOf>
stateE :: IP StateE
stateE = withPos do
  id <- reserved "state" *> token.identifier
  -- | Can be ContextState, SubjectState and ObjectState.
  {state} <- getArcParserState
  withArcParserState (state `addSubState` id)
    do
      -- | In IP, defined as an IntendParser on top of StateT ArcParserState Identity, we now have a
      -- | new ArcParserState object with member 'state' being StateIdentifier "{previousStateIdentifier}$id".
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
      case first, second of
        "state", _ -> singleton <<< SUBSTATE <$> stateE
        "on", "entry" -> onEntryE
        "on", "exit" -> onExitE
        "perspective", "on" -> perspectiveOn
        "perspective", "of" -> perspectiveOf
        _, _ -> fail ("Expected: on (entry/exit), perspective (on/of) (but found '" <> first <> "'.)")

-- We need to use `try` because this parser will consume "perspective" from the
-- expression "perspective of".
-- | perspectiveOn =
-- | 	perspective on <expression>
-- | 		<roleVerbs>
-- | 		<perspectivePart>*
perspectiveOn :: IP (List StateQualifiedPart)
perspectiveOn = try $ withPos do
  pos <- getPosition
  (stp :: Step) <- perspectiveOnKeywords *> step
  protectObject do
    ctxt <- getCurrentContext
    -- If the step is a simple arc identifier step, create an explicit role.
    case stp of
      Simple (ArcIdentifier ps ident) -> setObject (ExplicitRole ctxt (ENR $ EnumeratedRoleType ident) ps)
      _ -> setObject (ImplicitRole ctxt stp)
    -- TODO. Ik denk dat hier ook de state gezet moet worden.
    -- If 'perspectivePart' fails, nestedBlock will fail and thus perspectiveOn will fail.
    concat <$> nestedBlock perspectivePart
    -- In contrast, with the non-determinate expression outcommented below,
    -- if perspectivePart fails, perspectiveOn succeeds.
    -- option Nil (indented' *> (concat <$> entireBlock perspectivePart))

-- | perspectiveOf =
-- | 	perspective of <ident>
-- | 		<perspectivePart>*
-- We need to use `try` because this parser will consume "perspective" from the
-- expression "perspective of".
perspectiveOf :: IP (List StateQualifiedPart)
perspectiveOf = try $ withPos do
  pos <- getPosition
  (subject :: String) <- perspectiveOfKeywords *> arcIdentifier
  protectSubject do
    ctxt <- getCurrentContext
    -- We cannot establish, at this point, whether the string that identifies the role we carry out the effect for
    -- is calculated or enumerated. Hence we arbitrarily choose enumerated and fix it in PhaseThree.
    setSubject (ExplicitRole ctxt (ENR $ EnumeratedRoleType subject) pos)
    concat <$> nestedBlock perspectivePart

-- | perspectivePart =
-- | 	defaults
-- | 	<propertyVerbs> |
-- | 	<roleVerbs> |
-- | 	<inState> |
-- | 	<onEntry> |
-- | 	<onExit> |
-- | 	<action> |
-- | 	<perspectiveOn> |
-- | 	<perspectiveOf>
perspectivePart :: IP (List StateQualifiedPart)
perspectivePart = do
  (Tuple first second) <- twoReservedWords
  case first, second of
    "defaults", _ -> roleAndPropertyDefaults
    "view", _ -> singleton <<< P <$> propertyVerbs
    "props", _ -> singleton <<< P <$> propertyVerbs
    "verbs", _ -> singleton <<< P <$> propertyVerbs
    "only", _ -> singleton <<< R <$> roleVerbs
    "except", _ -> singleton <<< R <$> roleVerbs
    "all", "roleverbs" -> singleton <<< R <$> roleVerbs
    "in", _ -> inState
    "on", "entry" -> onEntryE
    "on", "exit" -> onExitE
    "action", _ -> actionE
    "perspective", "on" -> perspectiveOn
    "perspective", "of" -> perspectiveOf
    "selfonly", _ -> singleton <<< SO <$> selfOnly
    one, two -> fail ("Expected: view, props, verbs, only, except, all, in, on, action, perspective, selfonly, but found: '" <> one <> "' and '" <> two <> "'.")

-- | inState =
-- |  in [{subject | object | context}] state [<ident>]
-- | 		defaults
-- | 		<propertyVerbs>
-- | 		<roleVerbs>
-- | 		<perspectiveOn>
-- | 		<perspectiveOf>
-- | 		<action>
inState :: IP (List StateQualifiedPart)
inState = do
  reserved "in"
  mspecifier <- optionMaybe (reserved' "subject" <|> reserved' "object" <|> reserved' "context")
  mstateId <- reserved "state" *> optionMaybe arcIdentifier
  currentState <- getCurrentState
  stateSpec <- case mspecifier, mstateId of
    Nothing, Just stateId -> pure $ addSubState currentState stateId
    Nothing, Nothing -> pure currentState
    Just "object", _ -> ObjectState <$> getObject <*> pure mstateId
    Just "subject", _ -> SubjectState <$> getSubject <*> pure mstateId
    Just "context", _ -> ContextState <$> getCurrentContext <*> pure mstateId
    -- This case will never occur.
    _, _ -> fail "This will never occur"
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
      case first, second of
        "defaults", _ -> roleAndPropertyDefaults
        "view", _ -> singleton <<< P <$> propertyVerbs
        "props", _ -> singleton <<< P <$> propertyVerbs
        "verbs", _ -> singleton <<< P <$> propertyVerbs
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

-- | onEntry =
-- |  on entry [of {subject | object | context} state [<ident>]]
-- |		{<notification> | <automaticEffect>}*
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

-- | onExit =
-- | 	on exit [ of {subject | object | context} state <ident>]
-- | 		{<notification> | <automaticEffect>}*
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

-- | automaticEffect =
-- | 	do
-- | 		<statement>*
-- | 		|
-- | 		letA
-- | 			{<lowercaseName> <- <expression>}*
-- | 		in
-- | 			<statement>*
automaticEffectE :: IP (List StateQualifiedPart)
automaticEffectE = do
  start <- getPosition
  reserved "do"
  -- User role either specified here or taken from state.
  usr <- optionMaybe (reserved "for" *> arcIdentifier)
  isIndented >>= if _
    then do
      keyword <- scanIdentifier
      effect <- case keyword of
        "letE" -> fail "letE does not allow assignment operators, so this will not have an effect. Did you mean 'letA'?"
        "letA" -> Let <$> letWithAssignment
        _ -> Statements <<< fromFoldable <$> nestedBlock assignment
      end <- getPosition
      {subject, object, onEntry, onExit, currentContext} <- getArcParserState
      case usr of
        Nothing -> case subject, onEntry, onExit of
          Just sb, Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE {subject: sb, object, transition, effect, start, end}
          Just sb, Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE {subject: sb, object, transition, effect, start, end}
          Nothing, _, _ -> failWithPosition "A subject is required" (arcPosition2Position start)
          _, Nothing, Nothing -> fail "A state transition is required"
          _, Just _, Just _ -> fail "State transition inside state transition is not allowed"
        Just ident -> case onEntry, onExit of
          -- We cannot establish, at this point, whether the string that identifies the role we carry out the effect for
          -- is calculated or enumerated. Hence we arbitrarily choose enumerated and fix it in PhaseThree.
          Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE {subject: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start, object, transition, effect, start, end}
          Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE {subject: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start, object, transition, effect, start, end}
          Nothing, Nothing -> fail "A state transition is required"
          _, _ -> fail "State transition inside state transition is not allowed"
    else pure Nil

-- | notification =
-- |  notify [<ident>] sentence
-- | sentence = " { <charString> | '{' <expression> '}' }* "
-- |
-- | charString = a string of any character that is not { or ".
notificationE :: IP (List StateQualifiedPart)
notificationE = do
  start <- getPosition
  reserved "notify"
  -- User role either specified here or taken from state.
  usr <- optionMaybe arcIdentifier
  message <- sentenceE
  end <- getPosition
  {subject, onEntry, onExit, currentContext, object} <- getArcParserState
  case usr of
    Nothing -> case subject, onEntry, onExit of
      Just u, Just transition, Nothing -> pure $ singleton $ N $ NotificationE {user: u, transition, message, object, start, end}
      Just u, Nothing, Just transition -> pure $ singleton $ N $ NotificationE {user: u, transition, message, object, start, end}
      Nothing, _, _ -> fail "A subject is required"
      _, Nothing, Nothing -> fail "A state transition is required"
      _, Just _, Just _ -> fail "State transition inside state transition is not allowed"
    Just ident -> case onEntry, onExit of
      -- | We cannot establish, at this point, whether the string that identifies the role we carry out the effect for
      -- | is calculated or enumerated. Hence we arbitrarily choose enumerated and fix it in PhaseThree.
      Just transition, Nothing -> pure $ singleton $ N $ NotificationE {user: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start, transition, message, object, start, end}
      Nothing, Just transition -> pure $ singleton $ N $ NotificationE {user: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start, transition, message, object, start, end}
      Nothing, Nothing -> fail "A state transition is required"
      _, _ -> fail "State transition inside state transition is not allowed"

  where
    notificationLevel :: IP NotificationLevel
    notificationLevel = do
      v <- token.identifier
      case v of
        "Alert" -> pure Alert
        _ -> fail "Not a notification level"

roleAndPropertyDefaults :: IP (List StateQualifiedPart)
roleAndPropertyDefaults = do
  reserved "defaults"
  -- subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      end <- getPosition
      pure $ (R (RoleVerbE {subject: s, object: o, state, roleVerbs: All, start, end}) :
        P (PropertyVerbE {subject: s, object: o, state, propertyVerbs: Universal, propsOrView: AllProperties, start, end}) :
          Nil)
    Nothing, Nothing -> fail "User role and object of perspective must be given"
    Nothing, (Just _) -> fail "User role must be given"
    (Just _), Nothing -> fail "Object of perspective must be given"

-- | roleVerbs =
-- |	only ( <RoleVerb> {, <roleVerb>}+ )
-- |	|
-- |	excluding ( <RoleVerb> {, <roleVerb>}+ )
-- |  |
-- |  all roleverbs
roleVerbs :: IP RoleVerbE
roleVerbs = do
  -- | subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      (rv :: RoleVerbList) <- roleVerbList
      end <- getPosition
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
          (reserved "only" *> (Including <<< fromFoldable <$> lotsOfVerbs))

        exclusive :: IP RoleVerbList
        exclusive = do
          (reserved "except" *> (Excluding <<< fromFoldable <$> lotsOfVerbs))

        lotsOfVerbs :: IP (List RoleVerb)
        lotsOfVerbs = do
          token.parens (roleVerb `sepBy` token.symbol ",") <?> "a list of role verbs between parenthesis."

roleVerb :: IP RoleVerb
roleVerb = do
  v <- token.identifier
  case v of
    "RemoveFiller" -> pure RemoveFiller
    "Remove" -> pure Remove
    "Delete" -> pure Delete
    "CreateAndFill" -> pure CreateAndFill
    "Create" -> pure Create
    "Fill" -> pure Fill
    "Unbind" -> pure Unbind
    "Move" -> pure Move
    _ -> fail "Not a role verb"

selfOnly :: IP SelfOnly
selfOnly = do
  reserved "selfonly"
  -- | subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      end <- getPosition
      pure $ SelfOnly {subject: s, object: o, state, start, end}
    Nothing, Nothing -> fail "User role and object of perspective must be given"
    Nothing, (Just _) -> fail "User role must be given"
    (Just _), Nothing -> fail "Object of perspective must be given"

-- | propertyVerbs =
-- | 	view <ident> [ ( <propertyVerb>{, <propertyVerb>}+ )]
-- | 	|
-- | 	props [(<ident> {, <ident>}+) ] [ verbs ( <propertyVerb>{, <propertyVerb>}+ )]
propertyVerbs :: IP PropertyVerbE
propertyVerbs = basedOnView <|> basedOnProps
  where
    -- view SomeView              all properties in SomeView, all verbs
    -- view SomeView (Consult)    all properties in SomeView, verb Consult
    basedOnView :: IP PropertyVerbE
    basedOnView = do
      -- | subject and object must be present.
      {subject, object, state} <- getArcParserState
      case subject, object of
        Just s, Just o -> do
          -- | view <ArcIdentifier> [: (<PropertyVerb+)]
          start <- getPosition
          view <- reserved "view" *> (View <$> arcIdentifier)
          (pv :: ExplicitSet PropertyVerb) <- option Universal lotsOfVerbs
          end <- getPosition
          pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView: view, start, end}
        _, _ -> fail "User role and object of perspective must be given"

    -- props                         all properties, all verbs
    -- props (Title)                 property Title, all verbs
    -- props (Title) verbs (Consult) property Title, verb Consult
    -- props verbs (Consult)         all properties, verb Consult
    basedOnProps :: IP PropertyVerbE
    basedOnProps = do
      -- | subject and object must be present.
      {subject, object, state} <- getArcParserState
      case subject, object of
        Just s, Just o -> do
          -- | props (<ArcIdentifier>) [: (<PropertyVerb+)]
          start <- getPosition
          props <- option AllProperties (reserved "props" *> (Properties <$> lotsOfProperties))
          (pv :: ExplicitSet PropertyVerb) <- option Universal (reserved "verbs" *> lotsOfVerbs)
          end <- getPosition
          pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView: props, start, end}
        _, _ -> fail "User role and object of perspective must be given"

    lotsOfVerbs :: IP (ExplicitSet PropertyVerb)
    lotsOfVerbs = PSet <<< fromFoldable <$> token.parens (propertyVerb `sepBy` token.symbol ",")

    lotsOfProperties :: IP (List String)
    lotsOfProperties = token.parens (arcIdentifier `sepBy` token.symbol ",")

allPropertyVerbs :: List PropertyVerb
allPropertyVerbs = (Consult : (RemovePropertyValue : (DeleteProperty : (AddPropertyValue : (SetPropertyValue : Nil)))))

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

-- | action =
-- | 	action <ident>
-- | 		<statement>*
-- | 		|
-- | 		letA
-- | 			{<lowercaseName> <- <expression>}*
-- | 		in
-- | 			<statement>*
actionE :: IP (List StateQualifiedPart)
actionE = do
  start <- getPosition
  id <- reserved "action" *> token.identifier
  {subject, state, object} <- getArcParserState
  case subject, object of
    Nothing, _ -> fail "User role is not specified"
    _, Nothing -> fail "object of perspective must be given"
    Just s, Just o -> do
      kw <- option "" (lookAhead reservedIdentifier)
      effect <- if kw == "letA"
        then Let <$> letWithAssignment
        else Statements <$> fromFoldable <$> entireBlock1 assignment
      end <- getPosition
      pure $ singleton $ AC $ ActionE {id, subject: s, object: o, state, effect, start, end}

sentenceE :: IP Sentence
sentenceE = do
  start <- getPosition
  parts <- between (char '"') (char '"') (many (sentencePart <|> exprPart))
  _ <- token.whiteSpace
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

-- This parser always succeeds with a string.
-- If no reserved word is found, restores the parser state (uses try internally).
scanIdentifier :: IP String
scanIdentifier = option "" (lookAhead reservedIdentifier)
