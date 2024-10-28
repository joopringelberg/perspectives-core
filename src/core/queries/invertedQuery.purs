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

import Data.Array (cons, delete, null, union, elemIndex)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, lookup) as MAP
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object, insert, lookup)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), RoleInContext(..), isContextDomain, isRoleDomain, range)
import Perspectives.Representation.ExplicitSet (ExplicitSet, isElementOf) 
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType, StateIdentifier)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

-----------------------------------------------------------
-- INVERTEDQUERY
-----------------------------------------------------------
newtype InvertedQuery = InvertedQuery
  { description :: QueryWithAKink
  , backwardsCompiled :: (Maybe HiddenFunction)
  , forwardsCompiled :: (Maybe HiddenFunction)
  -- TODO #9 There is only a single user in InvertedQuery.
  , users :: Array RoleType
  -- True iff the user can modify the structural element where the InvertedQuery is attached.
  , modifies :: Boolean
  -- Yield PerspectiveObject InvertedQueryResult data only in one of these states:
  , states :: Array StateIdentifier
  , statesPerProperty :: EncodableMap PropertyType (Array StateIdentifier)
  , selfOnly :: Boolean
  , authorOnly :: Boolean
}

derive instance genericInvertedQuery :: Generic InvertedQuery _
derive instance newtypeInvertedQuery :: Newtype InvertedQuery _

instance showInvertedQuery :: Show InvertedQuery where
  show = genericShow

instance eqInvertedQuery :: Eq InvertedQuery where
  eq = genericEq

derive newtype instance WriteForeign InvertedQuery
derive newtype instance ReadForeign InvertedQuery

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

lookupInvertedQueries :: String -> Object (Array InvertedQuery) -> Array (InvertedQuery)
lookupInvertedQueries s obj = maybe [] identity (lookup s obj)

deleteInvertedQueryIndexedByContext :: InvertedQuery -> ContextType -> Object (Array InvertedQuery) -> Object (Array InvertedQuery)
deleteInvertedQueryIndexedByContext q cType qs = case lookup (unwrap cType) qs of
  Nothing -> qs
  Just x -> insert (unwrap cType) (delete q x) qs

-- | Add an InvertedQuery to (the inverted queries of) an EnumeratedRole type, indexed with ContextType.
-- | Their first step is `context`.
addInvertedQueryIndexedByContext ::
  InvertedQuery ->
  ContextType ->
  Object (Array InvertedQuery) ->
  Array RoleInContext ->
  EnumeratedRoleType ->
  Object (Array InvertedQuery)
addInvertedQueryIndexedByContext q cType qs modifiesRoleInstancesOf role = let
  q' = if isJust $ elemIndex (RoleInContext{context: cType, role}) modifiesRoleInstancesOf
    then over InvertedQuery (\qr -> qr {modifies=true}) q
    else q
  in case lookup (unwrap cType) qs of
    Nothing -> insert (unwrap cType) [q'] qs
    Just x -> insert (unwrap cType) (cons q' x) qs

addInvertedQueryToPropertyIndexedByRole ::
  InvertedQuery ->
  EnumeratedRoleType ->
  Object (Array InvertedQuery) ->
  MAP.Map EnumeratedRoleType (ExplicitSet EnumeratedPropertyType) ->
  EnumeratedPropertyType ->
  Object (Array InvertedQuery)
addInvertedQueryToPropertyIndexedByRole q eroleType qs modifiesPropertiesOf propertyType = let
  q' = case MAP.lookup eroleType modifiesPropertiesOf of
    Nothing -> q
    Just props -> if isElementOf propertyType props
      then over InvertedQuery (\qr -> qr {modifies=true}) q
      else q
  in case lookup (unwrap eroleType) qs of
    Nothing -> insert (unwrap eroleType) [q'] qs
    Just x -> insert (unwrap eroleType) (cons q' x) qs

deleteInvertedQueryFromPropertyTypeIndexedByRole ::
  InvertedQuery ->
  EnumeratedRoleType ->
  Object (Array InvertedQuery) ->
  Object (Array InvertedQuery)
deleteInvertedQueryFromPropertyTypeIndexedByRole q eroleType qs = case lookup (unwrap eroleType) qs of
  Nothing -> qs
  Just queries -> insert (unwrap eroleType) (delete q queries) qs

-- | Add an InvertedQuery to (the inverted queries of) a Context type, indexed with an EnumeratedRoleType.
-- | Their first step is `role`.
addInvertedQueryIndexedByRole ::
  InvertedQuery ->
  EnumeratedRoleType ->
  Object (Array InvertedQuery) ->
  Array RoleInContext ->
  ContextType ->
  Object (Array InvertedQuery)
addInvertedQueryIndexedByRole q eroleType qs modifiesRoleInstancesOf context = let
  q' = if isJust $ elemIndex (RoleInContext {context, role: eroleType}) modifiesRoleInstancesOf
    then over InvertedQuery (\qr -> qr {modifies=true}) q
    else q
  in case lookup (unwrap eroleType) qs of
    Nothing -> insert (unwrap eroleType) [q'] qs
    Just x -> insert (unwrap eroleType) (cons q' x) qs

deleteInvertedQueryIndexedByRole :: InvertedQuery -> EnumeratedRoleType -> Object (Array InvertedQuery) -> Object (Array InvertedQuery)
deleteInvertedQueryIndexedByRole q eroleType qs = case lookup (unwrap eroleType) qs of
  Nothing -> qs
  Just x -> insert (unwrap eroleType) (delete q x) qs

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

startsWithFilter :: InvertedQuery -> Boolean
startsWithFilter (InvertedQuery{description}) = startsWithFilter' description
  where
    -- | True iff the first backwards step is a FilterF operation.
    startsWithFilter' :: QueryWithAKink -> Boolean
    startsWithFilter' (ZQ Nothing _) = false
    startsWithFilter' (ZQ fd _) = case fd of
      Just (UQD _ FilterF _ _ _ _) -> true
      Just (BQD _ (BinaryCombinator ComposeF) first _ _ _ _) -> case first of
        (UQD _ FilterF _ _ _ _) -> true
        _ -> false
      _ -> false

-----------------------------------------------------------
-- UserPropsAndVerbs
-----------------------------------------------------------
newtype UserPropsAndVerbs = UserPropsAndVerbs {user :: RoleType, properties :: RelevantProperties}

derive instance genericUserProps :: Generic UserPropsAndVerbs _

-----------------------------------------------------------
-- RELEVANTPROPERTIES
-----------------------------------------------------------
-- NOTE: We might replace this with ExplicitSet PropertyType.
data RelevantProperties = All | Properties (Array PropertyType)

derive instance genericRelevantProperties :: Generic RelevantProperties _

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
-- | original query. The forwards (second) part describes a query that will run from the station to its original query's end;
-- | the backwards (first) part is a query that will run from the station to the original queries beginning.
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

instance WriteForeign QueryWithAKink where
  writeImpl (ZQ bw fw) = 
    writeImpl {constructor: "ZQ", bw, fw}

instance ReadForeign QueryWithAKink where
  readImpl f = do 
    {bw, fw} :: {bw :: Maybe QueryFunctionDescription, fw :: Maybe QueryFunctionDescription} <- read' f
    pure $ ZQ bw fw