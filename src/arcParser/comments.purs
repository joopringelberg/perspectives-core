-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

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
