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

-- | An AffectedContextCalculation is the combination of a QueryFunctionDescription and Maybe the compilation
-- | of that description. However, the Purescript type compiler cannot handle the full type of such functions
-- | in the places where we want to use it (CalculatedProperty, CalculatedRole).
-- | For that reason, we use a type HiddenFunction. We will unsafely coerce that type to the function we like
-- | when we feel we can do it.
-- | Notice that we will actually never encode such values: we replace them with Nothing in the act.

module Perspectives.AffectedContextCalculation where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.TypeIdentifiers (ActionType)

newtype AffectedContextCalculation = AffectedContextCalculation {description :: QueryFunctionDescription, compilation :: (Maybe HiddenFunction), action :: ActionType}

derive instance genericAffectedContextCalculation :: Generic AffectedContextCalculation _

instance showAffectedContextCalculation :: Show AffectedContextCalculation where
  show = genericShow

instance eqAffectedContextCalculation :: Eq AffectedContextCalculation where
  eq = genericEq

instance encodeAffectedContextCalculation :: Encode AffectedContextCalculation where
  encode (AffectedContextCalculation {description, action}) = genericEncode defaultOptions (AffectedContextCalculation {description, compilation: Nothing, action})

instance decodeAffectedContextCalculation :: Decode AffectedContextCalculation where
  decode = genericDecode defaultOptions
