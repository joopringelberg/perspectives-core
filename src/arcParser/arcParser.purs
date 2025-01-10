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
import Control.Lazy (defer)
import Control.Monad.Trans.Class (lift)
import Data.Array (fromFoldable)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.JSDate (toISOString)
import Data.List (List(..), concat, filter, find, intercalate, many, null, singleton, some, (:))
import Data.List.NonEmpty (NonEmptyList, toList, singleton) as LNE
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Parsing (fail, failWithPosition)
import Parsing.Combinators (between, lookAhead, option, optionMaybe, sepBy, sepBy1, try, (<?>))
import Parsing.Indent (checkIndent, sameOrIndented, withPos)
import Parsing.String (char, satisfy)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (getFirstMatch, isModelUri)
import Perspectives.Parsing.Arc.AST (ActionE(..), AuthorOnly(..), AutomaticEffectE(..), ChatE(..), ColumnE(..), ContextActionE(..), ContextE(..), ContextPart(..), FilledByAttribute(..), FilledBySpecification(..), FormE(..), MarkDownE(..), NotificationE(..), PropertyE(..), PropertyFacet(..), PropertyMapping(..), PropertyPart(..), PropertyVerbE(..), PropsOrView(..), RoleE(..), RoleIdentification(..), RolePart(..), RoleVerbE(..), RowE(..), ScreenE(..), ScreenElement(..), SelfOnly(..), SentenceE(..), SentencePartE(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), TabE(..), TableE(..), ViewE(..), WidgetCommonFields)
import Perspectives.Parsing.Arc.AST.ReplaceIdentifiers (replaceIdentifier)
import Perspectives.Parsing.Arc.Expression (parseJSDate, propertyRange, regexExpression, step)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, boolean, email, lowerCaseName, prefixedName, qualifiedName, reserved, stringUntilNewline)
import Perspectives.Parsing.Arc.IndentParser (IP, arcPosition2Position, containsTab, entireBlock, entireBlock1, getArcParserState, getCurrentContext, getCurrentState, getObject, getPosition, getStateIdentifier, getSubject, inSubContext, isEof, isIndented, isNextLine, nestedBlock, protectObject, protectOnEntry, protectOnExit, protectSubject, sameOrOutdented', setObject, setOnEntry, setOnExit, setSubject, withArcParserState, withEntireBlock)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement (assignment, letWithAssignment, twoReservedWords)
import Perspectives.Parsing.Arc.Statement.AST (Statements(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Perspectives.Persistent.PublicStore (PublicStore(..))
import Perspectives.Repetition (Duration(..), Repeater(..))
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.State (NotificationLevel(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..), RoleVerbList(..))
import Prelude (bind, discard, flip, not, pure, show, ($), (&&), (*>), (<$>), (<*), (<*>), (<<<), (<>), (==), (>>=), (||))

contextE :: IP ContextPart
contextE = withPos do
  -- | log "contextE"
  knd <- contextKind
  pos <- getPosition
  uname <- arcIdentifier
  mpublicStore <- optionMaybe (reserved "public" *> publicStore)
  -- | ook hier geldt: als op de volgende regel en geïndenteerd, dan moet de deelparser slagen en het hele blok consumeren.
  isIndented' <- isIndented
  isNextLine' <- isNextLine
  isEndOfFile <- isEof
  contextParts <- if isIndented' && isNextLine' && not isEndOfFile
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
              enumeratedPublicDuplicates <- do
                publicRoles <- pure $ filter (case _ of
                  RE (RoleE {kindOfRole}) -> kindOfRole == Public
                  _ -> false) otherContextParts
                pure (enumeratedPublicDuplicate <$> publicRoles)
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
                enumeratedPublicDuplicates <>
                rolesAndContexts))))
    else inSubContext uname 
      (getCurrentContext >>= \subContext ->
        pure $ singleton $ STATE $ StateE
          { id: (ContextState subContext Nothing)
          , condition: Simple $ Value pos PBool "true"
          , stateParts: Nil
          , subStates: Nil
          })
  -- | Notice: uname is unqualified.
  pure $ CE $ ContextE { id: uname, kindOfContext: knd, contextParts, pos: pos, public: mpublicStore}

  where

    -- Remove the Calculation, a Screen (if any)
    enumeratedPublicDuplicate :: ContextPart -> ContextPart
    enumeratedPublicDuplicate (RE (RoleE r@({id}))) = RE $ replaceIdentifier id "Proxy" $ RoleE r 
      { roleParts = filter (case _ of 
        Calculation _ _ -> false
        Screen _ -> false
        _ -> true) r.roleParts
      , kindOfRole = PublicProxy
      }
    -- This case is redundant in the sense that we only apply enumeratedPublicDuplicate to RE parts anyway.
    enumeratedPublicDuplicate p = p

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
        "public" -> getCurrentState >>= flip withArcParserState publicRoleE
        "context" -> explicitObjectState >>= flip withArcParserState contextRoleE
        "external" -> externalRoleState >>= flip withArcParserState externalRoleE
        "state" -> STATE <$> stateE
        "aspect" -> aspectRole
        "on" -> do
          (Tuple first second) <- twoReservedWords
          case first, second of
            "on", "entry" -> CSQP <$> onEntryE
            "on", "exit" -> CSQP <$> onExitE
            _, _ -> fail "Expected 'entry' or 'exit' after 'on', "
        _ -> do
          (Tuple first second) <- twoReservedWords
          case first, second of
            "perspective", "on" -> CSQP <$> perspectiveOn
            "perspective", "of" -> CSQP <$> perspectiveOf
            _, _ -> case first of
              "" -> do
                thisWord <- stringUntilNewline
                fail ("Expected: domain, case, party, activity; or thing, user, context, external, state, on entry, on exit (but found '" <> thisWord <> "'), ")
              otherwise -> fail ("Expected: domain, case, party, activity; or thing, user, context, external, state, on entry, on exit (but found '" <> otherwise <> "'), ")

    explicitSubjectState :: IP StateSpecification
    explicitSubjectState = do
      (Tuple segments calculated) <- lookAhead (Tuple <$> (reserved "user" *> token.identifier) <*> isCalculated)
      ctxt@(ContextType ccontext) <- getCurrentContext
      pos <- getPosition
      if calculated
        then getCurrentState
        else pure $ SubjectState (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ ccontext <> "$" <> segments) pos) Nothing

    explicitObjectState :: IP StateSpecification
    explicitObjectState = do
      (Tuple segments calculated) <- lookAhead (Tuple <$> (reservedIdentifier *> token.identifier) <*> isCalculated)
      ctxt@(ContextType ccontext) <- getCurrentContext
      pos <- getPosition
      if calculated
        then getCurrentState
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
      <|> reserved "activity" *> pure Activity) <?> "domain, case, party or activity, "

    aspectRole :: IP ContextPart
    aspectRole = do
      roleKind <- reserved "aspect" *> kind
      pos <- getPosition
      roleId <- qualifiedName <|> prefixedName
      pure $ AspectRole roleId roleKind pos

      where
        kind :: IP RoleKind
        kind = reserved "user" *> pure UserRole
          <|> reserved "thing" *> pure RoleInContext
          <|> reserved "context" *> pure ContextRole


    contextAspectE :: IP ContextPart
    contextAspectE = do 
      void $ reserved "aspect"
      pos <-getPosition
      aspect <- qualifiedName <|> prefixedName
      pure $ ContextAspect aspect pos

    contextIndexedE :: IP (List ContextPart)
    contextIndexedE = do
      void $ reserved "indexed"
      pos <- getPosition
      indexedName <- arcIdentifier
      sameOrOutdented'
      pure $ singleton $ IndexedContext indexedName pos

    useE :: IP ContextPart
    useE = do
      prefix <- reserved "use" *> lowerCaseName
      void $ reserved "for"
      pos <- getPosition
      modelName <- arcIdentifier
      sameOrOutdented'
      if isModelUri modelName
        then pure $ PREFIX prefix modelName
        else fail ("(NotWellFormedName) The name '" <> modelName <> "' is not well-formed (it cannot be expanded to a fully qualified name), ")
    
    -- Returns one of a limited set of symbolic store names
    publicStore :: IP PublicStore
    publicStore = reserved "NAMESPACESTORE" *> pure NAMESPACESTORE

domain :: IP ContextE
domain = do
  void containsTab
  r <- token.whiteSpace *> contextE
  isEndOfFile <- isEof
  case r of
    (CE d@(ContextE {kindOfContext})) -> if isEndOfFile
      then case kindOfContext of
        Domain -> pure d
        otherwise -> fail "The kind of the context must be 'Domain', "
      else fail "There is text beyond the domain context that has not been parsed. Possibly you need to change its indentation: otherwise remove it. "
    otherwise -> fail "Domain must be a context, "


--------------------------------------------------------------------------------------------------
-- ROLE
--------------------------------------------------------------------------------------------------
-- | role = <calculatedRole> | <enumeratedRole> | <externalRole>
-- |
-- | calculatedRole =
-- |   <roleKind> <ident> = <expression>
-- |     <rolePart>*
-- |
-- | enumeratedRole =
-- |   <roleKind> <ident> [(roleAttribute [, roleAttribute])] [filledBy <ident>]
-- |     <rolePart>*
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
      calculated <- isCalculated
      if calculated
        then do
          setSubject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
          calculatedRole_ uname kind pos
        else do
          setSubject (ExplicitRole ct (ENR $ EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
          enumeratedRole_ uname kind pos

publicRoleE :: IP ContextPart
publicRoleE = do
  state <- publicState
  protectSubject $ withEntireBlock
    (\{uname, knd, pos, parts, isEnumerated} elements -> RE $ RoleE
      { id: uname
      , kindOfRole: knd
      -- Notice that we add a state, even though we consider a public role to be calculated.
      -- This is because we need the Enumerated duplicate (the proxy) to have a state.
      -- We'll filter it away from the Calculated version.
      , roleParts: (createRoleState pos (state) elements) <> elements <> parts
      , pos
      })
    public_
    rolePart
  where
    public_  :: IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
    public_ = do
      pos <- getPosition
      knd <- reserved "public" *> pure Public
      uname <- arcIdentifier
      -- at <expression>
      -- where <expression> should evaluate to a url.
      url <- PublicUrl <$> (reserved "at" *> step)
      ct@(ContextType ctxt) <- getCurrentContext
      setSubject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
      isFunctional <- functionalCalculation
      token.reservedOp "="
      calculation <- step
      pure {uname, knd, pos, parts: (url : (Calculation calculation isFunctional) : Nil), isEnumerated: false}

    publicState :: IP StateSpecification
    publicState = do
      segments <- lookAhead (reserved "public" *> token.identifier)
      ctxt@(ContextType ccontext) <- getCurrentContext
      pos <- getPosition
      pure $ SubjectState (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ ccontext <> "$" <> segments) pos) Nothing

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
      calculated <- isCalculated
      if calculated
        then do
          setObject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
          calculatedRole_  uname kind pos
        else do
          setObject (ExplicitRole ct (ENR $ EnumeratedRoleType (ctxt <> "$" <> uname)) pos)
          enumeratedRole_ uname kind pos

isCalculated :: IP Boolean
isCalculated = do
  hasIs <- option false (lookAhead (reserved "=") *> pure true)
  hasFunctional <- option false (lookAhead (token.parens (reserved "functional")) *> pure true)
  pure (hasIs || hasFunctional)

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
      calculated <- isCalculated
      if calculated
        then do
          setObject (ExplicitRole ct (CR $ CalculatedRoleType (ctxt <> "$" <> uname)) pos)
          calculatedRole_ uname kind pos
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

calculatedRole_ :: String -> RoleKind -> ArcPosition 
  -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
calculatedRole_ uname knd pos = do
  isFunctional <- functionalCalculation
  token.reservedOp "="
  calculation <- step
  pure {uname, knd, pos, parts: Cons (Calculation calculation isFunctional) Nil, isEnumerated: false}

-- | Calculated roles and properties are by default relational.
functionalCalculation :: IP Boolean
functionalCalculation = option false ((try $ token.parens (reserved "functional")) *> (pure true))

enumeratedRole_ :: String -> RoleKind -> ArcPosition 
  -> IP (Record (uname :: String, knd :: RoleKind, pos :: ArcPosition, parts :: List RolePart, isEnumerated :: Boolean))
enumeratedRole_ uname knd pos = do
  attributes <- option Nil roleAttributes
  filledBy' <- filledBy
  pure {uname, knd, pos, parts: maybe attributes (flip (:) attributes) filledBy', isEnumerated: true}
  where
    -- | We cannot use token.commaSep or sebBy to separate the role attributes;
    -- | it will cause looping as long as it is used
    -- | both in userRoleE and thingRoleE. I do not understand why.
    roleAttributes :: IP (List RolePart)
    roleAttributes = token.parens (((mandatory <|> functional <|> relational <|> unlinked) <?> "mandatory, functional/relational, unlinked. ") `sepBy` token.symbol ",")

    mandatory :: IP RolePart
    mandatory = (reserved "mandatory" *> (pure (MandatoryAttribute true)))

    -- | Roles are by default functional.
    functional :: IP RolePart
    functional = (reserved "functional" *> (pure (FunctionalAttribute true)))

    -- | Roles are by default functional.
    relational :: IP RolePart
    relational = (reserved "relational" *> (pure (FunctionalAttribute false)))

    unlinked :: IP RolePart
    unlinked = (reserved "unlinked" *> (pure UnlinkedAttribute))

-- | This parser always succeeds.
-- | If it detects no filledBy clause, it leaves consumed state as it is and returns Nil.
-- | filledBy SomeRole, AnotherRole
-- | Here `SomeRole` and `AnotherRole` are alternatives.
-- | filledBy SomeRole + AnotherRole
-- | Here both are required: that is, only instances that have both types are allowed to fill this role.
-- Implementation note: we cannot know, here, whether the filler is Calculated or
-- Enumerated. This will be fixed in PhaseThree.
filledBy :: IP (Maybe RolePart)
filledBy = optionMaybe (reserved "filledBy" *> 
  (
    (try (FilledBySpecifications <<< Alternatives <$> token.parens (token.commaSep1 filler)))
    <|> (try (FilledBySpecifications <<< Combination <$> token.parens (plusSep filler)))
    <|> (FilledBySpecifications <<< Alternatives <<< LNE.singleton <$> (try filler))
  ))
  where
    filler :: IP FilledByAttribute
    filler = do
      role <- arcIdentifier
      mcontext <- optionMaybe (reserved "in" *> arcIdentifier)
      case mcontext of
        Nothing -> do
          pure $ FilledByAttribute role (ContextType "")
        Just context -> pure $ FilledByAttribute role (ContextType context)
    plus :: IP String
    plus = token.symbol "+"

    plusSep :: forall a . IP a -> IP (LNE.NonEmptyList a)
    plusSep p = sepBy1 p plus


-- | rolePart =
-- |   <perspectiveOn> |
-- |   <perspectiveOf> |
-- |   <inState> |
-- |   <onEntry> |
-- |   <onExit> |
-- |   <state> |
-- |   <aspect> |
-- |   <indexed> |
-- |   <property> |
-- |   <view>
rolePart :: IP RolePart
rolePart = do
  (Tuple first second) <- twoReservedWords
  case first, second of
    "perspective", "on" -> SQP <$> perspectiveOn
    "perspective", "of" -> SQP <$> perspectiveOf
    "in", _ -> SQP <$> inState
    "on", "entry" -> SQP <$> onEntryE
    "on", "exit" -> SQP <$> onExitE
    "state", _ -> ROLESTATE <$> stateE
    "aspect", _ -> aspectE
    "indexed", _ -> indexedE
    "property", _ -> propertyE
    "view", _ -> viewE
    "action", _ -> SQP <$> actionE
    "screen", _ -> Screen <$> screenE
    _, _ -> case first of
      "" -> do
        thisWord <- stringUntilNewline
        fail ("Expected: perspective (on/of), in state, on (entry/exit), state, aspect, indexed, property, view (but found '" <> thisWord <> "'), ")
      otherwise -> fail ("Expected: perspective (on/of), in state, on (entry/exit), state, aspect, indexed, property, view (but found '" <> otherwise <> "'), ")

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
aspectE = withPos do
  void $ reserved "aspect"
  pos <- getPosition
  aspect <- arcIdentifier
  (mPropertyMapping :: Maybe PropertyMapping) <- optionMaybe ((reserved "where") *> propertyMapping)
  -- There may be no indented source beyond this, so the next line must have the same or less indentation.
  sameOrOutdented'
  pure $ RoleAspect aspect pos mPropertyMapping

propertyMapping :: IP PropertyMapping
propertyMapping = do
  -- moet geïndenteerd zijn
  PropertyMapping <$> nestedBlock mapping
  where 
    mapping :: IP ((Tuple String String))
    mapping = do
      aspectProp <- arcIdentifier
      void $ reserved "is" 
      void $ reserved "replaced"
      void $ reserved "by"
      replacingProp <- arcIdentifier
      pure $ Tuple aspectProp replacingProp


indexedE :: IP RolePart
indexedE = do
  void $ reserved "indexed"
  pos <- getPosition
  indexedName <- arcIdentifier
  sameOrOutdented'
  pure $ IndexedRole indexedName pos

propertyE :: IP RolePart
propertyE = do
  pos <- getPosition
  uname <- reserved "property" *> arcIdentifier
  (calculatedProperty pos uname) <|> (enumeratedProperty pos uname)
  where
    calculatedProperty :: ArcPosition -> String -> IP RolePart
    calculatedProperty pos uname = do
      isFunctional <- option false functionalCalculation
      token.reservedOp "="
      calc <- step
      pure $ PE $ PropertyE
        { id: uname
        , range: Nothing
        , propertyParts: Cons (Calculation' calc isFunctional) Nil, pos: pos
        , propertyFacets: Nil}

    -- | This parser always succeeds, either with or without consuming part of the input stream.
    enumeratedProperty :: ArcPosition -> String -> IP RolePart
    enumeratedProperty pos uname = do
      attributes <- option Nil propertyAttributes
      (ran :: Maybe Range) <- pure $ (unsafePartial case _ of Ran r -> r) <$> find (case _ of
        Ran r -> true
        _ -> false) attributes
      facets <- option Nil (propertyFacets (case ran of
        Just r -> r
        otherwise -> PString))
      pure $ PE $ PropertyE
        { id: uname
        , range: ran
        , propertyParts: filter (case _ of
            Ran _ -> false
            _ -> true) attributes
        , propertyFacets: facets
        , pos: pos}

    -- the opening parenthesis functions as the recognizer: when found, the rest of the stream **must** start on
    -- a valid property attribute specification.
    propertyAttributes :: IP (List PropertyPart)
    propertyAttributes = token.parens (((mandatory <|> relational <|> selfonly <|> authoronly <|> (Ran <$> propertyRange)) <?> "(optionally) mandatory, then optionally relational, then optionally either selfonly or authoronly, then (required) a range (Boolean, Number, String, DateTime, Email), ") `sepBy` token.symbol ",")
        where
          mandatory :: IP PropertyPart
          mandatory = (reserved "mandatory" *> (pure (MandatoryAttribute' true)))

          -- | Properties are by default functional.
          relational :: IP PropertyPart
          relational = (reserved "relational" *> (pure (FunctionalAttribute' false)))

          selfonly :: IP PropertyPart
          selfonly = (reserved "selfonly" *> (pure SelfonlyAttribute))

          authoronly :: IP PropertyPart
          authoronly = (reserved "authoronly" *> (pure AuthoronlyAttribute))

    propertyFacets :: Range -> IP (List PropertyFacet)
    propertyFacets r = nestedBlock (propertyFacet r)
      where

      propertyFacet :: Range -> IP PropertyFacet
      propertyFacet r'' = do
        facet <- reservedIdentifier
        case facet of
          "minLength" -> reserved "=" *> (MinLength <$> token.integer)
          "maxLength" -> reserved "=" *> (MaxLength <$> token.integer)
          "enumeration" -> reserved "=" *> (Enumeration <<< fromFoldable <$> token.parens (token.commaSep1 (unsafePartial typedValue r'')))
          "pattern" -> reserved "=" *> (Pattern <$> regexExpression <*> token.stringLiteral)
          "maxInclusive" -> reserved "=" *> (MaxInclusive <$> boundaryValue r'')
          "minInclusive" -> reserved "=" *> (MinInclusive <$> boundaryValue r'')
          kw -> fail ("Expected `minLength`, `maxLength`, `enumeration` but got: " <> kw <> ". ")

      -- Partial, because we cannot have File instances in the ARC syntax.
      typedValue :: Partial => Range -> IP String
      typedValue r' = case r' of
        PString -> token.stringLiteral
        PBool -> boolean
        PNumber -> show <$> token.integer
        PDate ->  parseDateString
        PEmail -> email

      boundaryValue :: Range -> IP String
      boundaryValue r' = case r' of
        PNumber -> show <$> token.integer
        PDate -> parseDateString
        _ -> fail "minInclusive and maxInclusive can only be applied to numbers and dates. "

      -- Returns a string formatted as YYYY-MM-DD.
      parseDateString :: IP String
      parseDateString = do
        d <- parseJSDate
        iso <- liftToIP $ toISOString d
        case getFirstMatch dateRegex iso of
          Nothing -> fail "Enter a valid date in the form `YYYY-MM-DD`. "
          Just dstring -> pure dstring

      liftToIP = lift <<< lift <<< liftEffect

      dateRegex :: Regex
      dateRegex = unsafeRegex "^(.*)T" noFlags


viewE :: IP RolePart
viewE = do
  pos <- getPosition
  uname <- reserved "view" *> arcIdentifier
  refs <- sameOrIndented *> (token.parens (token.commaSep1 arcIdentifier))
  pure $ VE $ ViewE {id: uname, viewParts: LNE.toList refs, pos: pos}

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
-- |   state <ident> = <expression>
-- |     <state>*
-- |     <onEntry>
-- |     <onExit>
-- |     <perspectiveOn>
-- |     <perspectiveOf>
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
      allParts <- option Nil (concat <$> nestedBlock statePart)
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
        "action", _ -> actionE
        "on", "entry" -> onEntryE
        "on", "exit" -> onExitE
        "perspective", "on" -> perspectiveOn
        "perspective", "of" -> perspectiveOf
        _, _ -> case first of
          "" -> do
            thisWord <- stringUntilNewline
            fail ("Expected: on (entry/exit), perspective (on/of), state, action, on entry, on exit (but found '" <> thisWord <> "'.) ")
          otherwise -> fail ("Expected: on (entry/exit), perspective (on/of), state, action, on entry, on exit (but found '" <> otherwise <> "'.) ")

-- We need to use `try` because this parser will consume "perspective" from the
-- expression "perspective of".
-- | perspectiveOn =
-- |   perspective on <expression>
-- |     <roleVerbs>
-- |     <perspectivePart>*
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
-- |   perspective of <ident>
-- |     <perspectivePart>*
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
-- |   defaults
-- |   <propertyVerbs> |
-- |   <roleVerbs> |
-- |   <inState> |
-- |   <onEntry> |
-- |   <onExit> |
-- |   <action> |
-- |   <perspectiveOn> |
-- |   <perspectiveOf>
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
    "authoronly", _ -> singleton <<< PO <$> authorOnly
    _, _ -> case first of
      "" -> do
        thisWord <- stringUntilNewline
        fail ("Expected: view, props, verbs, only, except, all, in, on, action, perspective, selfonly (but found '" <> thisWord <> "'.) ")
      otherwise -> fail ("Expected: view, props, verbs, only, except, all, in, on, action, perspective, selfonly (but found '" <> otherwise <> "'.) ")

-- | inState =
-- |  in [{subject | object | context}] state [<ident>]
-- |     defaults
-- |     <propertyVerbs>
-- |     <roleVerbs>
-- |     <perspectiveOn>
-- |     <perspectiveOf>
-- |     <action>
inState :: IP (List StateQualifiedPart)
inState = do
  reserved "in"
  mspecifier <- optionMaybe (reserved' "subject" <|> reserved' "object" <|> reserved' "context")
  mstateId <- reserved "state" *> optionMaybe arcIdentifier
  currentState <- getCurrentState
  stateSpec <- case mspecifier, mstateId of
    Nothing, Nothing -> pure currentState
    -- These may be fully qualified aspect state names. We must check later whether they belong to the object, subject or context.
    Just "object", _ -> ObjectState <$> getObject <*> pure mstateId
    Just "subject", _ -> SubjectState <$> getSubject <*> pure mstateId
    Just "context", _ -> ContextState <$> getCurrentContext <*> pure mstateId
    Nothing, Just stateId -> pure $ addSubState currentState stateId
    -- This case will never occur.
    _, _ -> fail "This will never occur. "
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
        _, _ -> case first of
          "" -> do
            thisWord <- stringUntilNewline
            fail ("Expected: view, props, only, except, all roleverbs, perspective (on/of), action (but found '" <> thisWord <> "'), ")
          otherwise -> fail ("Expected: view, props, only, except, all roleverbs, perspective (on/of), action (but found '" <> otherwise <> "'), ")

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
-- |    {<notification> | <automaticEffect>}*
onEntryE :: IP (List StateQualifiedPart)
onEntryE = do
  void $ onEntryKeywords
  stateSpec <- optionMaybe (reserved "of") >>= case _ of
    Just _ -> (subjectState <|> objectState <|> contextState <|> subState) <?> "one of subject, object or context or a state name, "
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
    Nothing -> fail "A subject is required for 'in state X of subject', "
    Just s -> do
      msubState <- optionMaybe arcIdentifier
      pure $ SubjectState s msubState

objectState :: IP StateSpecification
objectState = reserved "object" *> reserved "state" *> do
  {object} <- getArcParserState
  case object of
    Nothing -> fail "An object is required for 'in state X of object', "
    Just s -> do
      msubState <- optionMaybe arcIdentifier
      pure $ ObjectState s msubState

contextState :: IP StateSpecification
contextState = reserved "context" *> reserved "state" *> do
  {currentContext} <- getArcParserState
  msubState <- optionMaybe arcIdentifier
  pure $ ContextState currentContext msubState

-- | onExit =
-- |   on exit [ of {subject | object | context} state <ident>]
-- |     {<notification> | <automaticEffect>}*
onExitE :: IP (List StateQualifiedPart)
onExitE = do
  void $ onExitKeywords
  stateSpec <- (optionMaybe (reserved "of") >>= case _ of
    Just _ -> (subjectState <|> objectState <|> contextState <|> subState) <?> "one of subject, object or context or a state name, "
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
    _ -> fail "Expected: do, notify, "

-- | automaticEffect =
-- |   do [for <Identifier>]
-- |     <statement>*
-- |     |
-- |     letA
-- |       {<lowercaseName> <- <expression>}*
-- |     in
-- |       <statement>*
automaticEffectE :: IP (List StateQualifiedPart)
automaticEffectE = do
  start <- getPosition
  reserved "do"
  -- User role either specified here or taken from state.
  usr <- optionMaybe (reserved "for" *> arcIdentifier)
  startMoment <- optionMaybe (reserved "after" *> duration)
  endMoment <- optionMaybe (reserved "until" *> duration)
  repeats <- repeatsE
  case endMoment, repeats of
    Just _, Never -> fail "`until` must be followed by `every`. "
    _, _ -> do
      isIndented >>= if _
        then do
          keyword <- scanIdentifier
          effect <- case keyword of
            "letE" -> fail "letE does not allow assignment operators, so this will not have an effect. Did you mean 'letA'? "
            "letA" -> Let <$> letWithAssignment
            _ -> Statements <<< fromFoldable <$> nestedBlock assignment
          end <- getPosition
          {subject, object, onEntry, onExit, currentContext} <- getArcParserState
          case usr of
            Nothing -> case subject, onEntry, onExit of
              Just sb, Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE 
                { subject: sb
                , object
                , transition
                , effect
                , startMoment
                , endMoment
                , repeats
                , start
                , end}
              Just sb, Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE 
                { subject: sb
                , object
                , transition
                , effect
                , startMoment
                , endMoment
                , repeats
                , start
                , end}
              Nothing, _, _ -> failWithPosition "A subject is required" (arcPosition2Position start)
              _, Nothing, Nothing -> fail "A state transition is required, "
              
              _, Just entry, Just exit -> fail ("State transition inside state transition is not allowed: " <> show entry <> show exit <> ", ")
            Just ident -> case onEntry, onExit of
              -- We cannot establish, at this point, whether the string that identifies the role we carry out the effect for
              -- is calculated or enumerated. Hence we arbitrarily choose enumerated and fix it in PhaseThree.
              Just transition, Nothing -> pure $ singleton $ AE $ AutomaticEffectE 
                { subject: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start
                , object
                , transition
                , effect
                , startMoment
                , endMoment
                , repeats
                , start
                , end}
              Nothing, Just transition -> pure $ singleton $ AE $ AutomaticEffectE 
                { subject: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start
                , object
                , transition
                , effect
                , startMoment
                , endMoment
                , repeats
                , start
                , end}
              Nothing, Nothing -> fail "A state transition is required, "
              entry, exit -> fail ("State transition inside state transition is not allowed: " <> show entry <> show exit <> ", ")
        else pure Nil

-- [every N Milliseconds|Seconds|Minutes|Hours|Days [maximally N times]]
repeatsE :: IP Repeater
repeatsE = option Never
  do
    interval <- reserved "every" *> duration
    max <- optionMaybe (reserved "maximally" *> token.integer <* reserved "times")
    case max of
      Nothing -> pure $ Forever interval
      Just m -> pure $ RepeatFor m interval


-- | <integer> Milliseconds|Seconds|Minutes|Hours|Days
duration :: IP Duration
duration = do
  interval <- toNumber <$> token.integer
  nomination <- reservedIdentifier
  case nomination of
    "Milliseconds" -> pure $ Millisecond interval
    "Seconds" -> pure $ Second interval
    "Minutes" -> pure $ Minute interval
    "Hours" -> pure $ Hour interval
    "Days" -> pure $ Day interval
    _ -> fail "Expected `Milliseconds`, `Seconds`, `Minutes`, `Hours` or `Days`. "


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
      Just u, Just transition, Nothing -> pure $ singleton $ N $ NotificationE 
        { user: u
        , transition
        , message
        , object
        , startMoment: Nothing
        , endMoment: Nothing
        , repeats: Never
        , start
        , end}
      Just u, Nothing, Just transition -> pure $ singleton $ N $ NotificationE 
        { user: u
        , transition
        , message
        , object
        , startMoment: Nothing
        , endMoment: Nothing
        , repeats: Never
        , start
        , end}
      Nothing, _, _ -> fail "A subject is required, "
      _, Nothing, Nothing -> fail "A state transition is required, "
      _, Just _, Just _ -> fail "State transition inside state transition is not allowed, "
    Just ident -> case onEntry, onExit of
      -- | We cannot establish, at this point, whether the string that identifies the role we carry out the effect for
      -- | is calculated or enumerated. Hence we arbitrarily choose enumerated and fix it in PhaseThree.
      Just transition, Nothing -> pure $ singleton $ N $ NotificationE 
        { user: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start
          , transition
          , message
          , object
          , startMoment: Nothing
          , endMoment: Nothing
          , repeats: Never
          , start
          , end}
      Nothing, Just transition -> pure $ singleton $ N $ NotificationE 
        { user: ExplicitRole currentContext (ENR $ EnumeratedRoleType ident) start
        , transition
        , message
        , object
        , startMoment: Nothing
        , endMoment: Nothing
        , repeats: Never
        , start
        , end}
      Nothing, Nothing -> fail "A state transition is required, "
      _, _ -> fail "State transition inside state transition is not allowed, "

  where
    notificationLevel :: IP NotificationLevel
    notificationLevel = do
      v <- token.identifier
      case v of
        "Alert" -> pure Alert
        _ -> fail "Not a notification levelm "

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
    Nothing, Nothing -> fail "User role and object of perspective must be given, "
    Nothing, (Just _) -> fail "User role must be given, "
    (Just _), Nothing -> fail "Object of perspective must be given, "

-- | roleVerbs =
-- |  only ( <RoleVerb> {, <roleVerb>}+ )
-- |  |
-- |  excluding ( <RoleVerb> {, <roleVerb>}+ )
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
      sameOrOutdented'
      pure $ RoleVerbE {subject: s, object: o, state, roleVerbs: rv, start, end}
    Nothing, Nothing -> fail "User role and object of perspective must be given, "
    Nothing, (Just _) -> fail "User role must be given, "
    (Just _), Nothing -> fail "Object of perspective must be given, "
  where
    roleVerbList :: IP RoleVerbList
    roleVerbList
     =  inclusive <|> exclusive <|> (reserved "all" *> reserved "roleverbs" *> pure All) <?> "only, except, all roleverbs, "
      where
        inclusive :: IP RoleVerbList
        inclusive = do
          (reserved "only" *> (Including <<< fromFoldable <$> lotsOfVerbs))

        exclusive :: IP RoleVerbList
        exclusive = do
          (reserved "except" *> (Excluding <<< fromFoldable <$> lotsOfVerbs))

        lotsOfVerbs :: IP (List RoleVerb)
        lotsOfVerbs = do
          token.parens (roleVerb `sepBy` token.symbol ",") <?> "a list of role verbs between parenthesis, "

roleVerb :: IP RoleVerb
roleVerb = do
  v <- token.identifier
  case v of
    "RemoveFiller" -> pure RemoveFiller
    "Remove" -> pure Remove
    "RemoveContext" -> pure RemoveContext
    "Delete" -> pure Delete
    "DeleteContext" -> pure DeleteContext
    "CreateAndFill" -> pure CreateAndFill
    "Create" -> pure Create
    "Fill" -> pure Fill
    "Unbind" -> pure Unbind
    "Move" -> pure Move
    _ -> fail "Not a role verb, "

selfOnly :: IP SelfOnly
selfOnly = do
  reserved "selfonly"
  -- | subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      end <- getPosition
      sameOrOutdented'
      pure $ SelfOnly {subject: s, object: o, state, start, end}
    Nothing, Nothing -> fail "User role and object of perspective must be given, "
    Nothing, (Just _) -> fail "User role must be given, "
    (Just _), Nothing -> fail "Object of perspective must be given, "

authorOnly :: IP AuthorOnly
authorOnly = do
  reserved "authoronly"
  -- | subject and object must be present.
  {subject, object, state} <- getArcParserState
  case subject, object of
    Just s, Just o -> do
      start <- getPosition
      end <- getPosition
      sameOrOutdented'
      pure $ AuthorOnly {subject: s, object: o, state, start, end}
    Nothing, Nothing -> fail "User role and object of perspective must be given, "
    Nothing, (Just _) -> fail "User role must be given, "
    (Just _), Nothing -> fail "Object of perspective must be given, "

-- | propertyVerbs =
-- |   view <ident> [ ( <propertyVerb>{, <propertyVerb>}+ )]
-- |   |
-- |   props [(<ident> {, <ident>}+) ] [ verbs ( <propertyVerb>{, <propertyVerb>}+ )]
-- | The default has propertyVerbs = Universal and propsOrView = AllProperties!
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
          (pv :: ExplicitSet PropertyVerb) <- option Universal (reserved "verbs" *> lotsOfVerbs)
          end <- getPosition
          sameOrOutdented'
          pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView: view, start, end}
        _, _ -> fail "User role and object of perspective must be given, "

    -- props                         all properties, all verbs
    -- props (Title)                 property Title, all verbs
    -- props (Title) verbs (Consult) property Title, verb Consult
    -- props verbs (Consult)         all properties, verb Consult
    -- The default has propertyVerbs = Universal and propsOrView = AllProperties!
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
          sameOrOutdented'
          pure $ PropertyVerbE {subject: s, object: o, state, propertyVerbs: pv, propsOrView: props, start, end}
        _, _ -> fail "User role and object of perspective must be given, "

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
    _ -> fail "Not a property verb, "

-- | action =
-- |   action <ident>
-- |     <statement>*
-- |     |
-- |     letA
-- |       {<lowercaseName> <- <expression>}*
-- |     in
-- |       <statement>*
actionE :: IP (List StateQualifiedPart)
actionE = do
  start <- getPosition
  id <- reserved "action" *> token.identifier
  {subject, state, object, currentContext} <- getArcParserState
  case subject, object of
    Nothing, _ -> fail "User role is not specified, "
    Just s, Nothing -> do
      kw <- option "" (lookAhead reservedIdentifier)
      effect <- if kw == "letA"
        then Let <$> letWithAssignment
        else Statements <$> fromFoldable <$> entireBlock1 assignment
      end <- getPosition
      pure $ singleton $ CA $ ContextActionE {id, subject: s, object: currentContext, state, effect, start, end}
    Just s, Just o -> do
      kw <- option "" (lookAhead reservedIdentifier)
      effect <- if kw == "letA"
        then Let <$> letWithAssignment
        else Statements <$> fromFoldable <$> entireBlock1 assignment
      end <- getPosition
      pure $ singleton $ AC $ ActionE {id, subject: s, object: o, state, effect, start, end}

sentenceE :: IP SentenceE
sentenceE = do
  start <- getPosition
  allparts <- between (char '"') (char '"') (many (sentencePart <|> exprPart))
  _ <- token.whiteSpace
  end <- getPosition
  sentence <- pure $ intercalate " " (flip mapWithIndex allparts \i p -> case p of 
    HRpart s -> s
    CPpart _ -> "$" <> show i)
  parts <- pure $ filter (\p -> case p of
    CPpart _ -> true
    _ -> false) allparts
  pure $ SentenceE { parts: (fromFoldable parts), sentence} 
  where
    sentencePart :: IP SentencePartE
    sentencePart = do
      chars <- some (satisfy (\c -> not (c == '{') && not (c == '"')))
      pure $ HRpart $ trim $ fromCharArray $ fromFoldable chars

    exprPart :: IP SentencePartE
    exprPart = CPpart <$> (between (token.symbol "{") (token.symbol "}") step)

reserved' :: String -> IP String
reserved' name = token.reserved name *> pure name

-- This parser always succeeds with a string.
-- If no reserved word is found, restores the parser state (uses try internally).
scanIdentifier :: IP String
scanIdentifier = option "" (lookAhead reservedIdentifier)

--------------------------------------------------------------------------------
---- SCREENS
--------------------------------------------------------------------------------
screenE :: IP ScreenE
screenE = withPos do
  start <- getPosition
  reserved "screen"
  title <- optionMaybe token.stringLiteral
  keyword <- scanIdentifier
  subject <- getSubject
  context <- getCurrentContext
  case keyword of
    "tab" -> do
      tabs <- Just <$> nestedBlock tabE
      end <- getPosition
      pure $ ScreenE {title, tabs, rows: Nothing, columns: Nothing, subject, context, start, end}
    "row" -> do
      rows <- Just <$> nestedBlock rowE
      end <- getPosition
      pure $ ScreenE {title, tabs: Nothing, rows, columns: Nothing, subject, context, start, end}
    "column" -> do
      columns <- Just <$> nestedBlock columnE
      end <- getPosition
      pure $ ScreenE {title, tabs: Nothing, columns, rows: Nothing, subject, context, start, end}
    _ -> fail "Only `tab`, `row` and `column` are allowed here. "

tabE :: IP TabE
tabE = reserved "tab" *> (TabE <$> token.stringLiteral <*> option false (reserved "default" *> pure true) <*> nestedBlock (defer \_ -> screenElementE))


rowE :: IP RowE
rowE = reserved "row" *> (RowE <$> nestedBlock (defer \_ -> screenElementE))

columnE :: IP ColumnE
columnE = reserved "column" *> (ColumnE <$> nestedBlock (defer \_ -> screenElementE))

screenElementE :: IP ScreenElement
screenElementE = withPos do
  keyword <- scanIdentifier
  case keyword of
    "row" -> RowElement <$> rowE
    "column" -> ColumnElement <$> columnE
    "table" -> reserved "table" *> (TableElement <$> tableE)
    "form" -> reserved "form" *> (FormElement <$> formE)
    "markdown" -> reserved "markdown" *> (MarkDownElement <$> markdownE)
    "chat" -> reserved "chat" *> (ChatElement <$> chatE)
    -- NOTE: extend message when a new widget is added.
    _ -> fail "Only `row`, `column`, `table` and `form` are allowed here. "

widgetCommonFields :: IP WidgetCommonFields
widgetCommonFields = do
  start <- getPosition
  title <- optionMaybe token.stringLiteral
  pos <- getPosition
  roleName <- arcIdentifier
  ctxt <- getCurrentContext
  -- We cannot know, at this point, whether the role is Calculated or Enumerated.
  -- Like with the filledBy clause, we assume Enumerated and repair that later.
  perspective <- pure (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ roleName) pos)
  isIndented' <- isIndented
  protectObject do
    setObject perspective
    if isIndented'
      then withPos do
        -- The default of parser propertyVerbs has propertyVerbs = Universal and propsOrView = AllProperties!
        PropertyVerbE r <- propertyVerbs
        mroleVerbs <- optionMaybe roleVerbs
        end <- getPosition
        pure
          { title
          , perspective
          , propsOrView: r.propsOrView
          , propertyVerbs: r.propertyVerbs
          , roleVerbs: _.roleVerbs <<< unwrap <$> mroleVerbs
          , start
          , end}
      else do
        end <- getPosition
        pure
          { title
          , perspective
          , propsOrView: AllProperties
          , propertyVerbs: Universal
          , roleVerbs: Nothing
          , start
          , end}

formE :: IP FormE
formE = FormE <$> widgetCommonFields

tableE :: IP TableE
tableE = TableE <$> widgetCommonFields

markdownE :: IP MarkDownE
markdownE = do
  s <- lookAhead step
  start <- getPosition
  case s of 
    (Simple (ArcIdentifier pos id)) -> do 
      widgetFields <- widgetCommonFields
      condition <- optionMaybe (reserved "when" *> arcIdentifier)
      sameOrOutdented'
      end <- getPosition
      pure $ MarkDownPerspective {widgetFields, condition, start, end} 
    Simple (Value pos PMarkDown _) -> do 
      text <- step
      condition <- optionMaybe (reserved "when" *> step)
      context <- getCurrentContext
      sameOrOutdented'
      end <- getPosition
      pure $ MarkDownConstant {text, condition, context, start, end}
    _ -> do 
      text <- step
      condition <- optionMaybe (reserved "when" *> step)
      context <- getCurrentContext
      sameOrOutdented'
      end <- getPosition
      pure $ MarkDownExpression {text, condition, context, start, end}

chatE :: IP ChatE
chatE = do
  -- We cannot know, at this point, whether the role is Calculated or Enumerated.
  -- Like with the filledBy clause, we assume Enumerated and repair that later.
  ctxt <- getCurrentContext
  start <- getPosition
  roleName <- arcIdentifier
  chatRole <- pure (ExplicitRole ctxt (ENR $ EnumeratedRoleType $ roleName) start)
  messagesProperty <- reserved "messages" *> arcIdentifier
  mediaProperty <- reserved "media" *> arcIdentifier
  end <- getPosition
  pure $ ChatE { chatRole, messagesProperty, mediaProperty, start, end}