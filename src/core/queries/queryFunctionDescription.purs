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

module Perspectives.Query.QueryTypes where

-- | A description of a queryfunction. Such a description
-- | consists of the origin (Domain) and destination of the querypath, and a description of the function that computes
-- | the destination from the origin.

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Traversable (foldMap, traverse)
import Foreign (ForeignError(..), fail, unsafeFromForeign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range) as RAN
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, PropertyType)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write, writeImpl)

-----------------------------------------------------------------------------------------
---- QUERYFUNCTIONDESCRIPTION
-----------------------------------------------------------------------------------------

-- | A description of a calculation with its domain and range.
-- | The last two members represent whether the described function is functional and whether it is mandatory.
data QueryFunctionDescription =
    SQD Domain QueryFunction Range ThreeValuedLogic ThreeValuedLogic
  | UQD Domain QueryFunction QueryFunctionDescription Range ThreeValuedLogic ThreeValuedLogic
  | BQD Domain QueryFunction QueryFunctionDescription QueryFunctionDescription Range ThreeValuedLogic ThreeValuedLogic
  | MQD Domain QueryFunction (Array QueryFunctionDescription) Range ThreeValuedLogic ThreeValuedLogic

instance writeForeignQFD :: WriteForeign QueryFunctionDescription where
  writeImpl (SQD dom qf ran fun man) = unsafeToForeign
    { constructor: unsafeToForeign "SQD"
    , domain: write dom
    , function: write qf
    , range: write ran
    , isFunctional: write fun
    , isMandatory: write man}
  writeImpl (UQD dom qf arg ran fun man) = unsafeToForeign
    { constructor: unsafeToForeign "UQD"
    , domain: write dom
    , function: write qf
    , singleArg: write arg
    , range: write ran
    , isFunctional: write fun
    , isMandatory: write man}
  writeImpl (BQD dom qf arg1 arg2 ran fun man) = unsafeToForeign
    { constructor: unsafeToForeign "BQD"
    , domain: write dom
    , function: write qf
    , firstArg: write arg1
    , secondArg: write arg2
    , range: write ran
    , isFunctional: write fun
    , isMandatory: write man}
  writeImpl (MQD dom qf args ran fun man) = unsafeToForeign
    { constructor: unsafeToForeign "MQD"
    , domain: write dom
    , function: write qf
    , arguments: write <$> args
    , range: write ran
    , isFunctional: write fun
    , isMandatory: write man}

instance readForeignQFD :: ReadForeign QueryFunctionDescription where
  readImpl f = case unsafeFromForeign f of
    {constructor: "SQD", domain, function, range, isFunctional, isMandatory} -> SQD <$> (readImpl domain) <*> (readImpl function) <*> (readImpl range) <*> (readImpl isFunctional) <*> (readImpl isMandatory)
    {constructor: "UQD", domain, function, singleArg, range, isFunctional, isMandatory} -> UQD <$> (readImpl domain) <*> (readImpl function) <*> (readImpl singleArg) <*> (readImpl range) <*> (readImpl isFunctional) <*> (readImpl isMandatory)
    {constructor: "BQD", domain, function, firstArg, secondArg, range, isFunctional, isMandatory} -> BQD <$> (readImpl domain) <*> (readImpl function) <*> (readImpl firstArg) <*> (readImpl secondArg) <*> (readImpl range) <*> (readImpl isFunctional) <*> (readImpl isMandatory)
    {constructor: "MQD", domain, function, arguments, range, isFunctional, isMandatory} ->
      MQD <$> (readImpl domain) <*> (readImpl function) <*> (traverse readImpl arguments) <*> (readImpl range) <*> (readImpl isFunctional) <*> (readImpl isMandatory)
    otherwise -> fail $ ForeignError "Expected record with constructor SQD, UQD, BQD, or MQD"

derive instance genericRepQueryFunctionDescription :: Generic QueryFunctionDescription _

instance eqQueryFunctionDescription :: Eq QueryFunctionDescription where
  eq d1@(SQD _ _ _ _ _ ) d2@(SQD _ _ _ _ _ ) = genericEq d1 d2
  eq d1@(UQD _ _ _ _ _ _) d2@(UQD _ _ _ _ _ _) = genericEq d1 d2
  eq d1@(BQD _ _ _ _ _ _ _) d2@(BQD _ _ _ _ _ _ _) = genericEq d1 d2
  eq d1@(MQD _ _ _ _ _ _ ) d2@(MQD _ _ _ _ _ _) = genericEq d1 d2
  eq _ _ = false

instance encodeQueryFunctionDescription :: Encode QueryFunctionDescription where
  -- encode q = genericEncode defaultOptions q
  encode = writeImpl

instance decodeQueryFunctionDescription :: Decode QueryFunctionDescription where
  -- decode q = genericDecode defaultOptions q
  decode = readImpl

instance showQueryFunctionDescription :: Show QueryFunctionDescription where
  show q = genericShow q

derive instance ordQueryFunctionDescription :: Ord QueryFunctionDescription
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

-- | Check whether the expression or one of its subexpressions has the requested QueryFunction.
hasQueryFunction :: QueryFunction -> QueryFunctionDescription -> Boolean
hasQueryFunction f q@(SQD _ qf _ _ _) = eq qf f
hasQueryFunction f q@(UQD _ qf qfd _ _ _) = if eq qf f then true else hasQueryFunction f qfd
hasQueryFunction f q@(BQD _ qf qfd1 qfd2 _ _ _) = if eq qf f
  then true
  else if hasQueryFunction f qfd1
    then true
    else hasQueryFunction f qfd1
hasQueryFunction f q@(MQD _ qf qfds _ _ _) = if eq qf f
  then true
  else unwrap $ foldMap (Disj <<< hasQueryFunction f) qfds

-----------------------------------------------------------------------------------------
---- BOOLEAN FUNCTIONS
-----------------------------------------------------------------------------------------
isContextDomain :: Domain -> Boolean
isContextDomain (CDOM _) = true
isContextDomain _ = false

isRoleDomain :: Domain -> Boolean
isRoleDomain (RDOM _) = true
isRoleDomain _ = false

-----------------------------------------------------------------------------------------
---- SELECTING PARTS
-----------------------------------------------------------------------------------------

range :: QueryFunctionDescription -> Range
range (SQD _ _ r _ _) = r
range (UQD _ _ _ r _ _) = r
range (BQD _ _ _ _ r _ _) = r
range (MQD _ _ _ r _ _) = r

roleRange :: Partial => QueryFunctionDescription -> ADT RoleInContext
roleRange r = case range r of
  RDOM et -> et

roleDomain :: Partial => QueryFunctionDescription -> ADT RoleInContext
roleDomain r = case domain r of
  RDOM et -> et

contextDomain :: Partial => QueryFunctionDescription -> ADT ContextType
contextDomain r = case domain r of
  CDOM ct -> ct

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

firstOperand :: QueryFunctionDescription -> Maybe QueryFunctionDescription
firstOperand (BQD _ _ f _ _ _ _) = Just f
firstOperand _ = Nothing

secondOperand :: QueryFunctionDescription -> Maybe QueryFunctionDescription
secondOperand (BQD _ _ _ s _ _ _) = Just s
secondOperand _ = Nothing

propertyOfRange :: QueryFunctionDescription -> Maybe PropertyType
propertyOfRange qfd = case range qfd of
  (VDOM _ p) -> p
  otherwise -> Nothing

-----------------------------------------------------------------------------------------
---- REPLACE DOMAIN, RANGE
-----------------------------------------------------------------------------------------
replaceDomain :: QueryFunctionDescription -> Domain -> QueryFunctionDescription
replaceDomain (SQD dom f ran fun man) d = (SQD d f ran fun man)
replaceDomain (UQD dom f qfd ran fun man) d = (UQD d f (replaceDomain qfd d) ran fun man)
replaceDomain (BQD dom f qfd1 qfd2 ran fun man) d = case f of
  (BinaryCombinator FilterF) -> (BQD d f (replaceDomain qfd1 d) qfd2 ran fun man)
  (BinaryCombinator ComposeF) -> (BQD d f (replaceDomain qfd1 d) qfd2 ran fun man)
  otherwise -> (BQD d f (replaceDomain qfd1 d) (replaceDomain qfd2 d) ran fun man)
replaceDomain (MQD dom f qfds ran fun man) d = (MQD d f (flip replaceDomain d <$> qfds) ran fun man)

replaceRange :: QueryFunctionDescription -> Domain -> QueryFunctionDescription
replaceRange (SQD dom f ran fun man) r = (SQD dom f r fun man)
replaceRange (UQD dom f qfd ran fun man) r = (UQD dom f (replaceRange qfd r) r fun man)
replaceRange (BQD dom f qfd1 qfd2 ran fun man) r = case f of
  (BinaryCombinator FilterF) -> (BQD dom f (replaceRange qfd1 r) qfd2 r fun man)
  (BinaryCombinator ComposeF) -> (BQD dom f (replaceRange qfd1 r) qfd2 r fun man)
  otherwise -> (BQD dom f (replaceRange qfd1 r) (replaceRange qfd2 r) r fun man)
replaceRange (MQD dom f qfds ran fun man) r = (MQD dom f (flip replaceRange r <$> qfds) ran fun man)

-----------------------------------------------------------------------------------------
---- ROLEINCONTEXT
-----------------------------------------------------------------------------------------
-- | An EnumeratedRoleType is lexically defined in a single ContextType.
-- | However, as an Aspect it may be a role in unlimited other ContextTypes.
-- | Therefore, on the typelevel, we represent a role domain as an EnumeratedRoleType in a ContextType.
newtype RoleInContext = RoleInContext {context :: ContextType, role :: EnumeratedRoleType}
derive instance genericRoleInContext :: Generic RoleInContext _
derive instance newtypeRoleInContext :: Newtype RoleInContext _
instance showRoleInContext :: Show RoleInContext where show = genericShow
instance eqRoleInContext :: Eq RoleInContext where eq = genericEq
instance writeForeignRoleInContext :: WriteForeign RoleInContext where
  writeImpl (RoleInContext r) = writeImpl r
instance readForeignRoleInContext :: ReadForeign RoleInContext where
  readImpl f = RoleInContext <$> readImpl f
instance encodeRoleInContext :: Encode RoleInContext where encode = writeImpl
instance decodeRoleInContext :: Decode RoleInContext where decode = readImpl
instance ordRoleInContext :: Ord RoleInContext where compare = genericCompare
instance prettyPrintRoleInContext :: PrettyPrint RoleInContext where
  prettyPrint' tab (RoleInContext r) = "PrettyPrint" <> show r

roleInContext2Role :: RoleInContext -> EnumeratedRoleType
roleInContext2Role (RoleInContext{role}) = role

roleInContext2Context :: RoleInContext -> ContextType
roleInContext2Context (RoleInContext{context}) = context

adtRoleInContext2adtEnumeratedRoleType :: ADT RoleInContext -> ADT EnumeratedRoleType
adtRoleInContext2adtEnumeratedRoleType = map roleInContext2Role

replaceContext :: ADT RoleInContext -> ContextType -> ADT RoleInContext
replaceContext adt ctxt = (over RoleInContext (\r -> r {context = ctxt})) <$> adt

adtContext2AdtRoleInContext :: ADT ContextType -> EnumeratedRoleType -> ADT RoleInContext
adtContext2AdtRoleInContext adt role = (\context -> RoleInContext {context, role}) <$> adt

-----------------------------------------------------------------------------------------
---- DOMAIN
-----------------------------------------------------------------------------------------
-- | The QueryCompilerEnvironment contains the domain of the queryStep. It also holds
-- | an array of variables that have been declared.
data Domain =
  -- The ContextType represents the embedding context type for Aspect roles that are added
  -- as such to the context type. If Nothing, use the static context type (the namespace of the Enumerated).
    -- RDOM (ADT EnumeratedRoleType) (Maybe ContextType)
    RDOM (ADT RoleInContext)
  | CDOM (ADT ContextType)
  | VDOM RAN.Range (Maybe PropertyType)
  | ContextKind
  | RoleKind

type Range = Domain

derive instance genericDomain :: Generic Domain _

instance showDomain :: Show Domain where
  show = genericShow

instance eqDomain :: Eq Domain where
  eq (VDOM r1 _) (VDOM r2 _) = eq r1 r2
  eq d1 d2 = genericEq d1 d2

instance writeForeignDomain :: WriteForeign Domain where
  -- writeImpl (RDOM adt ct) = unsafeToForeign { rdom: write adt, ctype: write ct}
  writeImpl (RDOM adt) = unsafeToForeign { rdom: write adt}
  writeImpl (CDOM adt) = unsafeToForeign { cdom: write adt}
  writeImpl (VDOM ran mprop) = unsafeToForeign { range: write ran, maybeproperty: write mprop}
  writeImpl ContextKind = unsafeToForeign "ContextKind"
  writeImpl RoleKind = unsafeToForeign "RoleKind"

instance readForeignDomain :: ReadForeign Domain where
  readImpl f =
    -- (\({rdom, ctype} :: {rdom :: ADT EnumeratedRoleType, ctype :: Maybe ContextType})-> (RDOM rdom ctype)) <$> (readImpl f)
    -- RDOM <$> (readImpl f)
    (\({rdom: adt} :: {rdom :: ADT RoleInContext}) -> (RDOM adt)) <$> (readImpl f)
    <|> (\({cdom: adt} :: {cdom :: ADT ContextType}) -> (CDOM adt)) <$> (readImpl f)
    <|> (\({range:r, maybeproperty} :: {range :: RAN.Range, maybeproperty :: Maybe PropertyType}) -> (VDOM r maybeproperty)) <$> (readImpl f)
    <|>
    case runExcept (readImpl f) of
      Left e -> throwError e
      Right k -> case k of
        "ContextKind" -> pure ContextKind
        "RoleKind" -> pure RoleKind
        otherwise ->  fail $ TypeMismatch "ContextKind, RoleKind" otherwise

instance encodeDomain :: Encode Domain where
  -- encode = writeImpl
  encode = genericEncode defaultOptions

instance decodeDomain :: Decode Domain where
  -- decode = readImpl
  decode = genericDecode defaultOptions

instance ordDomain :: Ord Domain where compare = genericCompare

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

sumOfDomains :: Domain -> Domain -> Maybe Domain
-- sumOfDomains (RDOM a ec1) (RDOM b ec2) | ec1 == ec2 = Just (RDOM (SUM [a, b]) ec1)
sumOfDomains (RDOM a) (RDOM b) = Just (RDOM (SUM [a, b]))
-- sumOfDomains (RDOM a _) (RDOM b _) = Just (RDOM (SUM [a, b]) Nothing)
sumOfDomains (CDOM a) (CDOM b) = Just (CDOM (SUM [a, b]))
sumOfDomains _ _ = Nothing

productOfDomains :: Domain -> Domain -> Maybe Domain
-- productOfDomains (RDOM a ec1) (RDOM b ec2) | ec1 == ec2 = Just (RDOM (PROD [a, b]) ec1)
-- productOfDomains (RDOM a _) (RDOM b _) = Just (RDOM (PROD [a, b]) Nothing)
productOfDomains (RDOM a) (RDOM b) = Just (RDOM (PROD [a, b]))
productOfDomains (CDOM a) (CDOM b) = Just (CDOM (PROD [a, b]))
productOfDomains _ _ = Nothing

domain2roleInContext :: Partial => Domain -> ADT RoleInContext
domain2roleInContext (RDOM r) = r

domain2roleType :: Partial => Domain -> ADT RoleInContext
domain2roleType (RDOM r) = r

domain2contextType :: Partial => Domain -> ADT ContextType
domain2contextType (CDOM c) = c

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

-----------------------------------------------------------------------------------------
---- ADT CONTEXTTYPE TO ADT ROLEINCONTEXT
-----------------------------------------------------------------------------------------
context2RoleInContextADT :: ADT ContextType -> EnumeratedRoleType -> ADT RoleInContext
context2RoleInContextADT adt role = (\context -> RoleInContext{context, role}) <$> adt
