module Perspectives.Parsing.Arc.Comments where

import Data.Array (many) as AR
import Data.String.CodeUnits (fromCharArray)
import Perspectives.EntiteitAndRDFAliases (Comment)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Prelude (bind, discard, pure, (*>), (/=), (<*), (<$>))
import Text.Parsing.Indent (sameLine)
import Text.Parsing.Parser.Combinators (option)
import Text.Parsing.Parser.String (string) as STRING
import Text.Parsing.Parser.String (satisfy, whiteSpace)

inLineComment ::  IP (Array Comment)
inLineComment = option [] do
  sameLine
  _ <- (STRING.string "--")
  chars <- AR.many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure [fromCharArray chars]

manyOneLineComments ::  IP (Array Comment)
manyOneLineComments = AR.many
    (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace)))
