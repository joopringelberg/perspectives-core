module Perspectives.Parsing.Arc where

import Control.Alt (void, (<|>))
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Lens (Traversal', _Just, over, preview, set)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile, DomeinFileRecord)
import Perspectives.Parsing.Arc.Identifiers (capitalizedString, colon, reserved)
import Perspectives.Parsing.Arc.IndentParser (ArcParserState, IP, getArcParserState, getNamespace, modifyArcParserState, runIndentParser', withExtendedNamespace)
import Perspectives.Representation.Context (defaultContext, ContextRecord)
import Perspectives.Representation.EnumeratedRole (defaultEnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Prelude (Unit, bind, pure, ($), unit, discard, (*>), (<<<), (>>=), (<*))
import Text.Parsing.Indent (indented, sameLine)
import Text.Parsing.Parser (ParseError, fail)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.String (whiteSpace)

-----------------------------------------------------------
-- ParseAndCache
-----------------------------------------------------------
parseAndCache ::  String -> MonadPerspectives (Either ParseError DomeinFile)
parseAndCache text = do
  (Tuple parseResult {domeinFile}) <- runIndentParser' text domain
  case parseResult of
    (Left e) -> pure $ Left e
    (Right r) -> pure $ Right domeinFile

-----------------------------------------------------------
-- Domain
-----------------------------------------------------------
domain :: IP Unit
domain = void $ parseAType
  domain_
  -- (enumeratedRole <|> calculatedRole <|> context)
  (enumeratedRole <|> context)
  defaultContext
  (prop (SProxy :: SProxy "context"))
  (_APSdomeinFile <<< (prop (SProxy :: SProxy "contexts")))

domain_ :: IP String
domain_ = sameLine *> reserved "Context" *> colon *> reserved "Domain" *> colon *> capitalizedString

_APSdomeinFile :: Traversal' ArcParserState DomeinFileRecord
_APSdomeinFile = (prop (SProxy :: SProxy "domeinFile")) <<< _Newtype

_APScontext ::  Traversal' ArcParserState ContextRecord
_APScontext = (prop (SProxy :: SProxy "context")) <<< _Just <<< _Newtype

-----------------------------------------------------------
-- EnumeratedRole
-----------------------------------------------------------
enumeratedRole :: IP Unit
enumeratedRole = do
  id <- parseAType
    enumeratedRole_
    do
      -- TODO: let op, Calculated zal niet noodzakelijk de eerstvolgende regel zijn.
      -- Bovendien: de lookahead wordt voor elk onderdeel van het block opnieuw uitgevoerd.
      -- Dat gaat niet goed zodra isNotCalculated geparsed is als regel van het block.
      -- lookAhead isNotCalculated
      -- (isRoleFunctional <|> isRoleMandatory <|> enumeratedProperty <|> calculatedProperty <|> perspective)
      isRoleFunctional
    defaultEnumeratedRole
    (prop (SProxy :: SProxy "enumeratedRole"))
    (_APSdomeinFile <<< (prop (SProxy :: SProxy "enumeratedRoles")))
  -- now save the identifier with the Context (its parent).
  modifyArcParserState
    (over
      (_APScontext <<< (prop (SProxy :: SProxy "rolInContext")))
      (cons (ENR $ EnumeratedRoleType id)))

  pure unit

  where
    isNotCalculated :: IP Unit
    isNotCalculated = (sameLine *> reserved "Calculated" *> colon *> reserved "False") <* whiteSpace

enumeratedRole_ :: IP String
enumeratedRole_ = sameLine *> reserved "Role" *> colon *> reserved "Role" *> colon *> capitalizedString

-----------------------------------------------------------
-- isRoleMandatory
-----------------------------------------------------------
-- | Parse the property 'Mandatory'. Store the result in the EnumeratedRole.
isRoleMandatory :: IP Unit
isRoleMandatory = do
  (mandry :: Boolean) <- isRoleMandatory_
  modifyArcParserState (set (prop (SProxy :: SProxy "enumeratedRole") <<< _Just <<< _Newtype <<< (prop (SProxy :: SProxy "mandatory"))) mandry)
  pure unit

isRoleMandatory_ :: IP Boolean
isRoleMandatory_ = (sameLine *> reserved "Mandatory" *> colon *> boolean) <* whiteSpace

-----------------------------------------------------------
-- isRoleFunctional
-----------------------------------------------------------
-- | Parse the property 'Functional'. Store the result in the EnumeratedRole.
isRoleFunctional :: IP Unit
isRoleFunctional = do
  (functl :: Boolean) <- isRoleFunctional_
  modifyArcParserState (set (prop (SProxy :: SProxy "enumeratedRole") <<< _Just <<< _Newtype <<< (prop (SProxy :: SProxy "functional"))) functl)
  pure unit

isRoleFunctional_ :: IP Boolean
isRoleFunctional_ = (sameLine *> reserved "Mandatory" *> colon *> boolean) <* whiteSpace

boolean :: IP Boolean
boolean = (reserved "True" *> pure true) <|> (reserved "False" *> pure true)

enumeratedProperty :: IP Unit
enumeratedProperty = fail "enumeratedProperty is not implemented"

calculatedProperty :: IP Unit
calculatedProperty = fail "calculatedProperty is not implemented"

perspective :: IP Unit
perspective = fail "perspective is not implemented"

-----------------------------------------------------------
-- CalculatedRole
-----------------------------------------------------------
-- Currently EA produces no CalculatedRoles.
calculatedRole :: IP Unit
calculatedRole = do
  id <- parseAType
    calculatedRole_
    do
      lookAhead isCalculated
      (isCalculated <|> isRoleMandatory <|> enumeratedProperty <|> calculatedProperty <|> perspective)
    defaultEnumeratedRole
    (prop (SProxy :: SProxy "enumeratedRole"))
    (_APSdomeinFile <<< (prop (SProxy :: SProxy "enumeratedRoles")))
  -- now save the identifier with the Context.
  modifyArcParserState
    (over
      (_APScontext <<< (prop (SProxy :: SProxy "rolInContext")))
      (cons (CR $ CalculatedRoleType id)))
  where
    isCalculated :: IP Unit
    isCalculated = sameLine *> reserved "Calculated" *> colon *> reserved "True"

calculatedRole_ :: IP String
calculatedRole_ = sameLine *> reserved "Role" *> colon *> reserved "Role" *> colon *> capitalizedString

context :: IP Unit
context = fail "context is not implemented"

-----------------------------------------------------------
-- PARSEATYPE
-----------------------------------------------------------
-- | This skeleton function parses an unqualified name for a Context, Role, or any other Perspectives type.
-- | It then applies a parser that should parse each of its components (built with parseAType).
-- | It stores the new type in ArcParserState and finally puts it into the DomeinFile.
-- | The first parser should provide the local (unqualified) name of the Context, Role, Action, Property, etc.
-- | The second parser should parse any of its members a single time.
-- | The first Traversal' provides a focus on the element in the ArcParserState.
-- | The second Traversal' provides a focus on the array of such elements in the DomeinFile.
parseAType :: forall a.
  IP String ->
  IP Unit ->
  (String -> String -> a) ->
  (Traversal' ArcParserState (Maybe a)) ->
  (Traversal' ArcParserState (Object a)) ->
  IP String
parseAType unqualifiedNameParser contentParser constructor getterLens overLens = do
  unqualifiedName <- unqualifiedNameParser <* whiteSpace
  withExtendedNamespace unqualifiedName
    do
      -- Create a new element of the proper type and store it in ArcParserState.
      namespace <- getNamespace
      modifyArcParserState (set getterLens (Just (constructor namespace unqualifiedName)))
      -- Flesh out the details of the new element by adding components to it.
      -- void $ (optionMaybe $ indented *> block contentParser)
      void $ (indented *> contentParser)
    -- Put the finished element into the DomeinFile.
      addXToDomeinFile
  where
    -- Take one of the currently built type artefacts and put it into the DomeinFile, in parser state.
    addXToDomeinFile :: IP String
    addXToDomeinFile = do
      s <- getNamespace
      (mdom :: Maybe a) <- getArcParserState >>= pure <<< preview (getterLens <<< _Just)
      case mdom of
        Nothing -> pure s
        (Just dom) -> modifyArcParserState (over overLens (insert s dom)) *> pure s
