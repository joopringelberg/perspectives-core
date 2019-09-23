module Perspectives.Parsing.Arc where

-----------------------------------------------------------
-- ParseAndCache
-----------------------------------------------------------
import Control.Alt (void, (<|>))
import Data.Array (many)
import Data.Either (Either(..))
import Data.Lens (Traversal', _Just, over, preview, set)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Parsing.Arc.Identifiers (capitalizedString, colon, reserved)
import Perspectives.Parsing.Arc.IndentParser (ArcParserState, IP, getArcParserState, modifyArcParserState, runIndentParser', withNamespace)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.TypeIdentifiers (ContextType(..))
import Prelude (Unit, bind, pure, ($), unit, discard, (*>), (<>), (<<<), (>>=))
import Text.Parsing.Indent (block, indented, sameLine)
import Text.Parsing.Parser (ParseError)

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
domain = do
  -- Context : Domain : <unqualifiedName>
  -- Unqualified name of the Domain. We have no prefix. Construct the model name out of it:
  -- model:<unqualifiedName>
  -- Put the model name in state for future reference.
  dname <- domeinName

  -- set the id and displayName of the Domain (the current value of context in the ArcParserState)
  modifyArcParserState (set __id (ContextType dname))
  modifyArcParserState (set _displayName dname)

  -- Now parse and collect roles and contexts (the only allowed components of a Domain).
  withNamespace
    dname
    indented *> (void $ block (many roleOrContext))

  -- Finally, put the domain into the DomeinFile.
  addXToDomeinFile _context _domainContexts dname

  pure unit
  where
    __id :: Traversal' ArcParserState ContextType
    __id = (prop (SProxy :: SProxy "context")) <<< _Just <<< _Newtype <<< (prop (SProxy :: SProxy "_id"))

    _displayName :: Traversal' ArcParserState String
    _displayName = (prop (SProxy :: SProxy "context")) <<< _Just <<< _Newtype <<< (prop (SProxy :: SProxy "displayName"))

-- A Traversal' for the contexts in the DomeinFile in the parser state.
_domainContexts :: Traversal' ArcParserState (Object Context)
_domainContexts = (prop (SProxy :: SProxy "domeinFile")) <<< _Newtype <<< (prop (SProxy :: SProxy "contexts"))

-- A Traversal' for the currently built Context out of the parser state.
_context :: Traversal' ArcParserState Context
_context = (prop (SProxy :: SProxy "context")) <<< _Just

-- Take one of the currently built type artefacts and put it into the DomeinFile, in parser state.
addXToDomeinFile :: forall a. (Traversal' ArcParserState a) -> (Traversal' ArcParserState (Object a)) -> String -> IP Unit
addXToDomeinFile getterLens overLens s = do
  (mdom :: Maybe a) <- getArcParserState >>= pure <<< preview getterLens
  case mdom of
    Nothing -> pure unit
    (Just dom) -> modifyArcParserState (over overLens (insert s dom))

domeinName :: IP String
domeinName = do
  cname <- sameLine *> reserved "Context" *> colon *> reserved "Domain" *> colon *> capitalizedString
  domein <- capitalizedString
  pure $ "model:" <> domein

roleOrContext :: IP Unit
roleOrContext = enumeratedRole <|> calculatedRole <|> context

enumeratedRole :: IP Unit
enumeratedRole = pure unit

calculatedRole :: IP Unit
calculatedRole = pure unit

context :: IP Unit
context = pure unit
