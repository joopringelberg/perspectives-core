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

module Perspectives.Sync.SignedDelta where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

-- | The author is the instance of sys:PerspectivesSystem$User who signed the delta.
newtype SignedDelta = SignedDelta {author :: String, encryptedDelta :: String}

derive instance genericRepSignedDelta :: Generic SignedDelta _

instance showSignedDelta :: Show SignedDelta where
  show = genericShow

instance encodeSignedDelta :: Encode SignedDelta where
  encode = genericEncode defaultOptions

instance decodeSignedDelta :: Decode SignedDelta where
  decode = genericDecode defaultOptions

instance eqSignedDelta :: Eq SignedDelta where
  eq = genericEq

derive instance newtypeSignedDelta :: Newtype SignedDelta _

instance prettyPrintSignedDelta :: PrettyPrint SignedDelta where
  prettyPrint' t (SignedDelta r) = "SignedDelta " <> prettyPrint' (t <> "  ") r
