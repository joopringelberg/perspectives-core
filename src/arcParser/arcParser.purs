module Perspectives.Parsing.Arc where

-----------------------------------------------------------
-- ParseAndCache
-----------------------------------------------------------
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser', IP)
import Perspectives.Parsing.Arc.Token (token)
import Prelude (Unit, bind, pure, ($), unit, discard, (*>))
import Text.Parsing.Indent (sameLine)
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
  -- Context : Domain :
  sameLine *> reserved "Context" *> colon *> reserved "Domain" *> colon

  pure unit

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------

reservedOp ::  String -> IP Unit
reservedOp = token.reservedOp

reserved ::  String -> IP Unit
reserved = token.reserved

colon :: IP Unit
colon = reservedOp ":"
