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

module Perspectives.Query.QueryTypes where

-- | A description of a queryfunction. Such a description
-- | consists of the origin (Domain) and destination of the querypath, and a description of the function that computes
-- | the destination from the origin.

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (QueryFunction)
import Perspectives.Representation.Range (Range) as RAN
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, PropertyType)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

-- | A description of a calculation with its domain and range.
-- | The last two members represent whether the described function is functional and whether it is mandatory.
data QueryFunctionDescription =
  SQD Domain QueryFunction Range ThreeValuedLogic ThreeValuedLogic |
  UQD Domain QueryFunction QueryFunctionDescription Range ThreeValuedLogic ThreeValuedLogic|
  BQD Domain QueryFunction QueryFunctionDescription QueryFunctionDescription Range ThreeValuedLogic ThreeValuedLogic |
  MQD Domain QueryFunction (Array QueryFunctionDescription) Range ThreeValuedLogic ThreeValuedLogic

---------------------------------------------------------------------------------------------------------------------
---- TRAVERSING
---------------------------------------------------------------------------------------------------------------------
-- | Traverse a QueryFunctionDescription with a function that modifies each individual QueryFunctionDescription.
traverseQfd :: forall f. Monad f => (QueryFunctionDescription -> f QueryFunctionDescription) -> QueryFunctionDescription -> f QueryFunctionDescription
traverseQfd f q@(SQD _ _ _ _ _) = f q
traverseQfd f q@(UQD dom qf qfd ran fun man) = traverseQfd f qfd >>= \qfd' -> f (UQD dom qf qfd' ran fun man)
traverseQfd f q@(BQD dom qf qfd1 qfd2 ran fun man) = do
  qfd1' <- traverseQfd f qfd1
  qfd2' <- traverseQfd f qfd2
  f (BQD dom qf qfd1' qfd2' ran fun man)
traverseQfd f q@(MQD dom qf qfds ran fun man) = traverse (traverseQfd f) qfds >>= \qfds' -> f (MQD dom qf qfds' ran fun man)

---------------------------------------------------------------------------------------------------------------------
---- SELECTING PARTS
---------------------------------------------------------------------------------------------------------------------

range :: QueryFunctionDescription -> Range
range (SQD _ _ r _ _) = r
range (UQD _ _ _ r _ _) = r
range (BQD _ _ _ _ r _ _) = r
range (MQD _ _ _ r _ _) = r

roleRange :: Partial => QueryFunctionDescription -> ADT EnumeratedRoleType
roleRange r = case range r of
  RDOM et -> et

domain :: QueryFunctionDescription -> Range
domain (SQD d _ _ _ _) = d
domain (UQD d _ _ _ _ _) = d
domain (BQD d _ _ _ _ _ _) = d
domain (MQD d _ _ _ _ _) = d

functional :: QueryFunctionDescription -> ThreeValuedLogic
functional (SQD _ _ _ f _) = f
functional (UQD _ _ _ _ f _) = f
functional (BQD _ _ _ _ _ f _) = f
functional (MQD _ _ _ _ f _) = f

mandatory :: QueryFunctionDescription -> ThreeValuedLogic
mandatory (SQD _ _ _ _ f ) = f
mandatory (UQD _ _ _ _ _ f) = f
mandatory (BQD _ _ _ _ _ _ f ) = f
mandatory (MQD _ _ _ _ _ f) = f

queryFunction :: QueryFunctionDescription -> QueryFunction
queryFunction (SQD _ f _ _ _) = f
queryFunction (UQD _ f _ _ _ _) = f
queryFunction (BQD _ f _ _ _ _ _) = f
queryFunction (MQD _ f _ _ _ _) = f

derive instance genericRepQueryFunctionDescription :: Generic QueryFunctionDescription _

instance eqQueryFunctionDescription :: Eq QueryFunctionDescription where
  eq d1@(SQD _ _ _ _ _ ) d2@(SQD _ _ _ _ _ ) = genericEq d1 d2
  eq d1@(UQD _ _ _ _ _ _) d2@(UQD _ _ _ _ _ _) = genericEq d1 d2
  eq d1@(BQD _ _ _ _ _ _ _) d2@(BQD _ _ _ _ _ _ _) = genericEq d1 d2
  eq _ _ = false

instance encodeQueryFunctionDescription :: Encode QueryFunctionDescription where
  encode q = genericEncode defaultOptions q

instance decodeQueryFunctionDescription :: Decode QueryFunctionDescription where
  decode q = genericDecode defaultOptions q

instance showQueryFunctionDescription :: Show QueryFunctionDescription where
  show q = genericShow q

-- | The QueryCompilerEnvironment contains the domain of the queryStep. It also holds
-- | an array of variables that have been declared.
data Domain = RDOM (ADT EnumeratedRoleType) | CDOM (ADT ContextType) | VDOM RAN.Range (Maybe PropertyType)

type Range = Domain

instance encodeDomain :: Encode Domain where
  encode = genericEncode defaultOptions

instance decodeDomain :: Decode Domain where
  decode = genericDecode defaultOptions


sumOfDomains :: Domain -> Domain -> Maybe Domain
sumOfDomains (RDOM a) (RDOM b) = Just (RDOM (SUM [a, b]))
sumOfDomains (CDOM a) (CDOM b) = Just (CDOM (SUM [a, b]))
sumOfDomains _ _ = Nothing

productOfDomains :: Domain -> Domain -> Maybe Domain
productOfDomains (RDOM a) (RDOM b) = Just (RDOM (PROD [a, b]))
productOfDomains (CDOM a) (CDOM b) = Just (CDOM (PROD [a, b]))
productOfDomains _ _ = Nothing

domain2roleType :: Partial => Domain -> ADT EnumeratedRoleType
domain2roleType (RDOM r) = r

domain2contextType :: Partial => Domain -> ADT ContextType
domain2contextType (CDOM c) = c

derive instance genericDomain :: Generic Domain _

instance showDomain :: Show Domain where
  show = genericShow

instance eqDomain :: Eq Domain where
  eq (VDOM r1 _) (VDOM r2 _) = eq r1 r2
  eq d1 d2 = genericEq d1 d2

instance qfdPrettyPrint :: PrettyPrint QueryFunctionDescription where
  prettyPrint' tab (SQD dom qf ran man fun) = "SQD" <> newline
    <> tab <> show dom <> newline
    <> tab <> show qf <> newline
    <> tab <> show ran <> newline
    <> tab <> show man <> newline
    <> tab <> show fun <> newline
  prettyPrint' tab (UQD dom qf qfd ran man fun) = "UQD" <> newline
    <> tab <> show dom <> newline
    <> tab <> show qf <> newline
    <> tab <> (prettyPrint' (tab <> ind) qfd)
    <> tab <> show ran <> newline
    <> tab <> show man <> newline
    <> tab <> show fun <> newline
  prettyPrint' tab (BQD dom qf qfd1 qfd2 ran man fun) = "BQD" <> newline
    <> tab <> show dom <> newline
    <> tab <> show qf <> newline
    <> tab <> (prettyPrint' (tab <> ind) qfd1)
    <> tab <> (prettyPrint' (tab <> ind) qfd2)
    <> tab <> show ran <> newline
    <> tab <> show man <> newline
    <> tab <> show fun <> newline
  prettyPrint' tab (MQD dom qf qfds ran man fun) = "MQD" <> newline
    <> tab <> show dom <> newline
    <> tab <> show qf <> newline
    <> tab <> show qfds <> newline
    <> tab <> show ran <> newline
    <> tab <> show man <> newline
    <> tab <> show fun <> newline

newline :: String
newline = "\n"

ind :: String
ind = "  "

-----------------------
data Calculation = S Step | Q QueryFunctionDescription

derive instance genericRepCalculation :: Generic Calculation _
instance encodeCalculation :: Encode Calculation where
  encode = genericEncode defaultOptions
instance decodeCalculation :: Decode Calculation where
  decode = genericDecode defaultOptions
instance showCalculation :: Show Calculation where
  show = genericShow
instance eqCalculation :: Eq Calculation where
  eq = genericEq
