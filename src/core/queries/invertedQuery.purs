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

import Data.Array (cons, findIndex, modifyAt, union)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Partial.Unsafe (unsafePartial)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.TypeIdentifiers (RoleType)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

newtype InvertedQuery = InvertedQuery {description :: QueryWithAKink, compilation :: (Maybe HiddenFunction), userTypes :: Array RoleType}

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

equalDescriptions :: InvertedQuery -> InvertedQuery -> Boolean
equalDescriptions (InvertedQuery{description:d1}) (InvertedQuery{description:d2}) = d1 == d2

addUserTypes :: Array RoleType -> InvertedQuery -> InvertedQuery
addUserTypes t (InvertedQuery r@{userTypes}) = InvertedQuery r {userTypes = union userTypes t}

addInvertedQuery :: InvertedQuery -> Array InvertedQuery -> Array InvertedQuery
addInvertedQuery q@(InvertedQuery{userTypes}) qs = case findIndex (equalDescriptions q) qs of
  Nothing -> cons q qs
  Just i -> unsafePartial $ fromJust $ modifyAt i (addUserTypes userTypes) qs

--------------------------------------------------------------------------------------------------------------
---- QUERYWITHAKINK
--------------------------------------------------------------------------------------------------------------
-- | A QueryWithAKink represents a query as seen from a specific station (context or role) that is visited by some
-- | original query. The forwards part describes a query that will run from the station to its original query's end;
-- | the backwards part is a query that will run from the station to the original queries beginning.
data QueryWithAKink = ZQ (Maybe QueryFunctionDescription) (Maybe QueryFunctionDescription)

forwards :: QueryWithAKink -> Maybe QueryFunctionDescription
forwards (ZQ _ forward) = forward

backwards :: QueryWithAKink -> Maybe QueryFunctionDescription
backwards (ZQ backward _) = backward

derive instance genericQueryWithAKink :: Generic QueryWithAKink _

instance showQueryWithAKink :: Show QueryWithAKink where
  show = genericShow

instance prettyPrintQueryWithAKink :: PrettyPrint QueryWithAKink where
  prettyPrint' tab (ZQ bw fw) = "QueryWithAKink\n" <> (prettyPrint' (tab <> "  ") bw) <> (prettyPrint' (tab <> "  ") fw)

instance eqQueryWithAKink :: Eq QueryWithAKink where
  eq = genericEq

instance encodeQueryWithAKink :: Encode QueryWithAKink where
  encode = genericEncode defaultOptions

instance decodeQueryWithAKink :: Decode QueryWithAKink where
  decode = genericDecode defaultOptions
