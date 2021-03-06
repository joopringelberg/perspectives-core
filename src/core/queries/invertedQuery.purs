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

-- | An InvertedQuery is the combination of a QueryFunctionDescription, Maybe the backwardsCompiled
-- | of that description and the UserRole types that have a perspective on the query end result.
-- | However, the Purescript type compiler cannot handle the full type of such functions
-- | in the places where we want to use it (CalculatedProperty, CalculatedRole).
-- | For that reason, we use a type HiddenFunction. We will unsafely coerce that type to the function we like
-- | when we feel we can do it.
-- | Notice that we will actually never encode such values: we replace them with Nothing in the act.

module Perspectives.InvertedQuery where

import Prelude

import Data.Array (cons, null, union)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Query.QueryTypes (QueryFunctionDescription, isContextDomain, isRoleDomain, range)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType, StateIdentifier)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

-----------------------------------------------------------
-- INVERTEDQUERY
-----------------------------------------------------------
newtype InvertedQuery = InvertedQuery
  { description :: QueryWithAKink
  , backwardsCompiled :: (Maybe HiddenFunction)
  , forwardsCompiled :: (Maybe HiddenFunction)
  -- TODO dit kan er maar één zijn.
  , users :: Array RoleType
  -- Yield PerspectiveObject InvertedQueryResult data only in one of these states:
  , states :: Array StateIdentifier
  , statesPerProperty :: EncodableMap PropertyType (Array StateIdentifier)
  , selfOnly :: Boolean
}

derive instance genericInvertedQuery :: Generic InvertedQuery _
derive instance newtypeInvertedQuery :: Newtype InvertedQuery _

instance showInvertedQuery :: Show InvertedQuery where
  show = genericShow

instance eqInvertedQuery :: Eq InvertedQuery where
  eq = genericEq

instance encodeInvertedQuery :: Encode InvertedQuery where
  encode = genericEncode defaultOptions

instance decodeInvertedQuery :: Decode InvertedQuery where
  decode = genericDecode defaultOptions

instance prettyPrintInvertedQuery :: PrettyPrint InvertedQuery where
  prettyPrint' t (InvertedQuery{description, users, states, statesPerProperty}) =
    "\nInvertedQuery " <> prettyPrint' (t <> "  ") description <>
    ("\n" <> t <> "    users:") <> prettyPrint' (t <> "    ") users <>
    ("\n" <> t <> "    states:") <> prettyPrint' (t <> "    ") states <>
    ("\n" <> t <> "    statesPerProperty:") <> prettyPrint' (t <> "    ") statesPerProperty

equalDescriptions :: InvertedQuery -> InvertedQuery -> Boolean
equalDescriptions (InvertedQuery{description:d1}) (InvertedQuery{description:d2}) = d1 == d2

-- | If we find an existing inverted query with the same description, we compare states and users.
-- | If the new query specifies the same users, we add its states;
-- | if it specifies the same states, we add its users.
addInvertedQuery :: InvertedQuery -> Array InvertedQuery -> Array InvertedQuery
addInvertedQuery q qs = cons q qs

-- TODO. Dit voegt InvertedQueries samen die apart moeten blijven.
-- addInvertedQuery q@(InvertedQuery{users, states}) qs = case findIndex (equalDescriptions q) qs of
--   Nothing -> cons q qs
--   Just i -> case unsafePartial $ fromJust $ index qs i of
--     InvertedQuery{users:users1, states:states1} -> if ((length $ (union users users1)) == length users)
--       then unsafePartial $ fromJust $ modifyAt i (\(InvertedQuery qr) -> InvertedQuery $ qr {states = union states states1}) qs
--       else if ((length $ (union states states1)) == length states)
--         then unsafePartial $ fromJust $ modifyAt i (\(InvertedQuery qr) -> InvertedQuery $ qr {users = union users users1}) qs
--         else qs

isStateQuery :: InvertedQuery -> Boolean
isStateQuery (InvertedQuery{users}) = null users

-- | This is a Partial function. Do not apply when the description has Nothing for its
-- | backwards part.
shouldResultInContextStateQuery :: Partial => InvertedQuery -> Boolean
shouldResultInContextStateQuery (InvertedQuery{description, users}) = null users &&
  (isContextDomain $ range $ fromJust $ backwards description)

-- | This is a Partial function. Do not apply when the description has Nothing for its
-- | backwards part.
shouldResultInRoleStateQuery :: Partial => InvertedQuery -> Boolean
shouldResultInRoleStateQuery (InvertedQuery{description, users}) = null users &&
  (isRoleDomain $ range $ fromJust $ backwards description)

backwardsQueryResultsInRole ::  Partial => InvertedQuery -> Boolean
backwardsQueryResultsInRole (InvertedQuery{description}) = (isRoleDomain $ range $ fromJust $ backwards description)

backwardsQueryResultsInContext ::  Partial => InvertedQuery -> Boolean
backwardsQueryResultsInContext (InvertedQuery{description}) = (isContextDomain $ range $ fromJust $ backwards description)

shouldResultInPerspectiveObject :: InvertedQuery -> Boolean
shouldResultInPerspectiveObject (InvertedQuery{users}) = not $ null users

-----------------------------------------------------------
-- UserPropsAndVerbs
-----------------------------------------------------------
newtype UserPropsAndVerbs = UserPropsAndVerbs {user :: RoleType, properties :: RelevantProperties}

derive instance genericUserProps :: Generic UserPropsAndVerbs _

instance encodeUserProps :: Encode UserPropsAndVerbs where
  encode = genericEncode defaultOptions

instance decodeUserProps :: Decode UserPropsAndVerbs where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- RELEVANTPROPERTIES
-----------------------------------------------------------
-- NOTE: We might replace this with ExplicitSet PropertyType.
data RelevantProperties = All | Properties (Array PropertyType)

derive instance genericRelevantProperties :: Generic RelevantProperties _

instance encodeRelevantProperties :: Encode RelevantProperties where
  encode = genericEncode defaultOptions

instance decodeRelevantProperties :: Decode RelevantProperties where
  decode = genericDecode defaultOptions

instance showRelevantProperties :: Show RelevantProperties where
  show = genericShow

instance eqRelevantProperties :: Eq RelevantProperties where
  eq = genericEq

instance semigroupRelevantProperties :: Semigroup RelevantProperties where
  append All _ = All
  append _ All = All
  append (Properties p1) (Properties p2) = Properties (union p1 p2)

instance monoidRelevantProperties :: Monoid RelevantProperties where
  mempty = Properties []

isEmpty :: RelevantProperties -> Boolean
isEmpty All = false
isEmpty (Properties ps) = null ps
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
  prettyPrint' tab (ZQ bw fw) = "QueryWithAKink\n" <> ((tab <> "  ") <> "backwards:" <> prettyPrint' (tab <> "  ") bw) <> (tab <> "  ") <> "forwards:" <> (prettyPrint' (tab <> "  ") fw)

instance eqQueryWithAKink :: Eq QueryWithAKink where
  eq = genericEq

instance encodeQueryWithAKink :: Encode QueryWithAKink where
  encode = genericEncode defaultOptions

instance decodeQueryWithAKink :: Decode QueryWithAKink where
  decode = genericDecode defaultOptions
