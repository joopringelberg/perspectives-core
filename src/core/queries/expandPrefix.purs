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

module Perspectives.Query.ExpandPrefix where

import Data.Traversable (traverse)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), BinaryStep(..), ComputationStep(..), LetStep(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo, expandNamespace)
import Prelude (pure, (<$>), bind, ($))

class ContainsPrefixes s where
  expandPrefix :: s -> PhaseTwo s

instance containsPrefixesStep :: ContainsPrefixes Step where
  expandPrefix (Simple s) = Simple <$> expandPrefix s
  expandPrefix (Binary s) = Binary <$> expandPrefix s
  expandPrefix (Unary s) = Unary <$> expandPrefix s
  expandPrefix (Let s) = Let <$> expandPrefix s
  expandPrefix (PureLet s) = PureLet <$> expandPrefix s
  expandPrefix (Computation s) = Computation <$> expandPrefix s

instance containsPrefixesSimpleStep :: ContainsPrefixes SimpleStep where
  expandPrefix (ArcIdentifier pos s) = ArcIdentifier pos <$> expandNamespace s
  expandPrefix x = pure x

instance containsPrefixesVarBinding :: ContainsPrefixes VarBinding where
  expandPrefix (VarBinding s step) = VarBinding s <$> expandPrefix step

instance containsPrefixesBinaryStep :: ContainsPrefixes BinaryStep where
  expandPrefix (BinaryStep r@{left, right}) = do
    eleft <- expandPrefix left
    eright <- expandPrefix right
    pure $ BinaryStep r {left = eleft, right = eright}

instance containsPrefixesUnaryStep :: ContainsPrefixes UnaryStep where
  expandPrefix (LogicalNot pos s) = LogicalNot pos <$> expandPrefix s
  expandPrefix (Exists pos s) = Exists pos <$> expandPrefix s
  expandPrefix (Available pos s) = Available pos <$> expandPrefix s

instance containsPrefixesLetStep :: ContainsPrefixes LetStep where
  expandPrefix (LetStep r@{bindings, assignments}) = do
    ebindings <- traverse expandPrefix bindings
    eassignments <- traverse expandPrefix assignments
    pure $ LetStep r {bindings = ebindings, assignments = eassignments}

instance containsPrefixesPureLetStep :: ContainsPrefixes PureLetStep where
  expandPrefix (PureLetStep r@{bindings, body}) = do
    ebindings <- traverse expandPrefix bindings
    ebody <- expandPrefix body
    pure $ PureLetStep r {bindings = ebindings, body = ebody}

instance containsPrefixesComputationStep :: ContainsPrefixes ComputationStep where
  expandPrefix (ComputationStep r@{functionName, arguments, computedType}) = do
    efunctionName <- expandNamespace functionName
    earguments <- traverse expandPrefix arguments
    ecomputedType <- expandNamespace computedType
    pure $ ComputationStep r {functionName = efunctionName, arguments = earguments, computedType = ecomputedType}

instance containsPrefixesAssignment :: ContainsPrefixes Assignment where
  expandPrefix (Remove r@{roleExpression}) = do
    eroleExpression <- expandPrefix roleExpression
    pure $ Remove r{roleExpression = eroleExpression}
  expandPrefix (CreateRole r@{contextExpression}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    pure $ CreateRole r {contextExpression = econtextExpression}
  expandPrefix (Move r@{roleExpression, contextExpression}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    eroleExpression <- expandPrefix roleExpression
    pure $ Move r{roleExpression = eroleExpression, contextExpression = econtextExpression}
  expandPrefix (Bind r@{bindingExpression, contextExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    econtextExpression <- traverse expandPrefix contextExpression
    pure $ Bind r {bindingExpression = ebindingExpression, contextExpression = econtextExpression}
  expandPrefix (Bind_ r@{bindingExpression, binderExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    ebinderExpression <- expandPrefix binderExpression
    pure $ Bind_ r {binderExpression = ebinderExpression, bindingExpression = ebindingExpression}
  expandPrefix (Unbind r@{bindingExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    pure $ Unbind r {bindingExpression = ebindingExpression}
  expandPrefix (Unbind_ r@{bindingExpression, binderExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    ebinderExpression <- expandPrefix binderExpression
    pure $ Unbind_ r {bindingExpression = ebindingExpression, binderExpression = ebinderExpression}
  expandPrefix (DeleteRole r@{contextExpression}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    pure $ DeleteRole r{contextExpression = econtextExpression}
  expandPrefix (DeleteProperty r@{roleExpression}) = do
    eroleExpression <- traverse expandPrefix roleExpression
    pure $ DeleteProperty r{roleExpression = eroleExpression}
  expandPrefix (PropertyAssignment r@{valueExpression, roleExpression}) = do
    eroleExpression <- traverse expandPrefix roleExpression
    evalueExpression <- expandPrefix valueExpression
    pure $ PropertyAssignment r {valueExpression = evalueExpression, roleExpression = eroleExpression}
  expandPrefix (ExternalEffect r@{effectName, arguments}) = do
    eeffectName <- expandNamespace effectName
    earguments <- traverse expandPrefix arguments
    pure $ ExternalEffect r {effectName = eeffectName, arguments = earguments}
