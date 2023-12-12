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
module Perspectives.Parsing.Arc.Expression.RegExP where

import Prelude

import Data.Either (Either(..))
import Data.String.Regex (Regex, flags, regex, source)
import Data.String.Regex.Flags (RegexFlags(..), RegexFlagsRec)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

newtype RegExP = RegExP Regex
instance showRegExP :: Show RegExP where show (RegExP r) = show r
instance eqRegExP :: Eq RegExP where eq (RegExP r1) (RegExP r2) = eq (show r1) (show r2)
instance ordRegExP :: Ord RegExP where compare (RegExP r1) (RegExP r2) = compare (show r1) (show r2)

type RegExPRecord = {source :: String, flags :: RegexFlagsRec}

instance writeForeignRegExp :: WriteForeign RegExP where
  writeImpl (RegExP r) = let
    (RegexFlags flagsRecord) = flags r
    in
    writeImpl ({ source: (source r), flags: flagsRecord} :: RegExPRecord)

instance readForeignRegExp :: ReadForeign RegExP where
  readImpl f = do
    ({source, flags} :: RegExPRecord) <- read' f
    parseResult <- pure (regex source (RegexFlags flags))
    case parseResult of
      Left e -> fail (ForeignError ("could not construct regex from source '" <> source <> "' and flags '" <> show flags <> "'. "))
      Right r -> pure $ RegExP r
