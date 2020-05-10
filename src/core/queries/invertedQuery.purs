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

-- | An InvertedQuery is the combination of a QueryFunctionDescription, Maybe the compilation
-- | of that description and the UserRole types that have a perspective on the query end result.
-- | However, the Purescript type compiler cannot handle the full type of such functions
-- | in the places where we want to use it (CalculatedProperty, CalculatedRole).
-- | For that reason, we use a type HiddenFunction. We will unsafely coerce that type to the function we like
-- | when we feel we can do it.
-- | Notice that we will actually never encode such values: we replace them with Nothing in the act.

module Perspectives.InvertedQuery where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.TypeIdentifiers (RoleType)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

newtype InvertedQuery = InvertedQuery {description :: QueryFunctionDescription, compilation :: (Maybe HiddenFunction), userTypes :: Array RoleType}

derive instance genericInvertedQuery :: Generic InvertedQuery _
derive instance newtypeInvertedQuery :: Newtype InvertedQuery _

instance showInvertedQuery :: Show InvertedQuery where
  show = genericShow

instance eqInvertedQuery :: Eq InvertedQuery where
  eq = genericEq

instance encodeInvertedQuery :: Encode InvertedQuery where
  encode (InvertedQuery {description, userTypes}) = genericEncode defaultOptions (InvertedQuery {description, compilation: Nothing, userTypes})

instance decodeInvertedQuery :: Decode InvertedQuery where
  decode = genericDecode defaultOptions

instance prettyPrintInvertedQuery :: PrettyPrint InvertedQuery where
  prettyPrint' t (InvertedQuery{description, userTypes}) = "InvertedQuery " <> prettyPrint' (t <> "  ") description <> show userTypes
