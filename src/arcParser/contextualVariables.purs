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

-- | Functions in this module add the contextual variables "currentcontext" and "currentobject"
-- | to the lexical representation of expressions and statements, if they contain a
-- | reference to them.

module Perspectives.Parsing.Arc.ContextualVariables where

import Data.Array (catMaybes, filter, foldMap, head, last, null)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), LetABinding(..), LetStep(..), Statements(..), endOfAssignment, startOfAssignment)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.TypeIdentifiers (ContextType)
import Prelude (($), (<$>), (<>), (==), (||))

--------------------------------------------------------------------------
--- VARIABLE REFERENCES IN EXPRESSIONS
--------------------------------------------------------------------------
-- | True only if the Step itself is a reference to the named variable,
-- | or if one its constituents contain a reference to the named variable.
stepContainsVariableReference :: String -> Step -> Boolean
stepContainsVariableReference varName (Simple (Variable _ n)) = varName == n

stepContainsVariableReference varName (Binary (BinaryStep{left, right} )) = stepContainsVariableReference varName left || stepContainsVariableReference varName right

stepContainsVariableReference varName (Unary (LogicalNot _ s)) = stepContainsVariableReference varName s
stepContainsVariableReference varName (Unary (Exists _ s)) = stepContainsVariableReference varName s
stepContainsVariableReference varName (Unary (FilledBy _ s)) = stepContainsVariableReference varName s
stepContainsVariableReference varName (Unary (Fills _ s)) = stepContainsVariableReference varName s
stepContainsVariableReference varName (Unary (Available _ s)) = stepContainsVariableReference varName s

stepContainsVariableReference varName (PureLet (PureLetStep{bindings, body})) = stepContainsVariableReference varName body ||
  ala Disj foldMap
    (catMaybes $ (\(VarBinding n stp) -> if varName == n then Nothing else Just (stepContainsVariableReference varName stp)) <$> bindings)

stepContainsVariableReference varName (Computation(ComputationStep{arguments})) =
  ala Disj foldMap
    ((stepContainsVariableReference varName) <$> arguments)

stepContainsVariableReference _ _ = false

--------------------------------------------------------------------------
--- VARIABLE REFERENCES IN STATEMENTS
--------------------------------------------------------------------------
-- | True only if the Statement contains a reference to the named variable.
statementContainsVariableReference :: String -> Statements -> Boolean
statementContainsVariableReference varName (Let (LetStep{bindings, assignments})) =
  ala Disj foldMap (assignmentContainsReference varName <$> assignments)
  ||
  ala Disj foldMap
    (catMaybes $ recur <$> bindings)
  where
    recur :: LetABinding -> Maybe Boolean
    recur (Expr (VarBinding n stp)) = if varName == n then Nothing else Just (stepContainsVariableReference varName stp)
    recur (Stat n assignment) = if varName == n then Nothing else Just $ assignmentContainsReference varName assignment

statementContainsVariableReference varName (Statements stats) =
  ala Disj foldMap (assignmentContainsReference varName <$> stats)

assignmentContainsReference :: String -> Assignment -> Boolean
assignmentContainsReference varName (RemoveRole {roleExpression}) = stepContainsVariableReference varName roleExpression

assignmentContainsReference varName (RemoveContext {roleExpression}) = stepContainsVariableReference varName roleExpression

assignmentContainsReference varName (CreateRole {contextExpression}) = maybe false (stepContainsVariableReference varName) contextExpression

assignmentContainsReference varName (CreateContext {contextExpression}) =
  maybe false (stepContainsVariableReference varName) contextExpression

assignmentContainsReference varName (CreateContext_ {roleExpression}) =
  stepContainsVariableReference varName roleExpression

assignmentContainsReference varName (Move {roleExpression, contextExpression}) =
  stepContainsVariableReference varName roleExpression
  ||
  maybe false (stepContainsVariableReference varName) contextExpression

assignmentContainsReference varName (Bind {bindingExpression, contextExpression}) =
  stepContainsVariableReference varName bindingExpression
  ||
  maybe false (stepContainsVariableReference varName) contextExpression

assignmentContainsReference varName (Bind_ {bindingExpression, binderExpression}) =
  stepContainsVariableReference varName bindingExpression
  ||
  stepContainsVariableReference varName binderExpression

assignmentContainsReference varName (Unbind {bindingExpression}) =
  stepContainsVariableReference varName bindingExpression

assignmentContainsReference varName (Unbind_ {bindingExpression, binderExpression}) =
  stepContainsVariableReference varName bindingExpression
  ||
  stepContainsVariableReference varName binderExpression

assignmentContainsReference varName (DeleteRole {contextExpression}) = maybe false (stepContainsVariableReference varName) contextExpression

assignmentContainsReference varName (DeleteContext {contextExpression}) = maybe false (stepContainsVariableReference varName) contextExpression

assignmentContainsReference varName (DeleteProperty {roleExpression}) =
  maybe false (stepContainsVariableReference varName) roleExpression

assignmentContainsReference varName (PropertyAssignment {valueExpression, roleExpression}) =
  stepContainsVariableReference varName valueExpression
  ||
  maybe false (stepContainsVariableReference varName) roleExpression

assignmentContainsReference varName (CreateFile {roleExpression, contentExpression, fileNameExpression}) = 
  stepContainsVariableReference varName contentExpression
  ||
  stepContainsVariableReference varName fileNameExpression
  ||
  maybe false (stepContainsVariableReference varName) roleExpression

assignmentContainsReference varName (ExternalEffect {arguments}) =
  ala Disj foldMap $ stepContainsVariableReference varName <$> arguments

assignmentContainsReference varName (ExternalDestructiveEffect {arguments}) =
  ala Disj foldMap $ stepContainsVariableReference varName <$> arguments

--------------------------------------------------------------------------
--- CONSTRUCTING AND ADDING BINDINGS
--------------------------------------------------------------------------
type VarName = String
makeIdentityStep :: VarName -> ArcPosition -> VarBinding
makeIdentityStep varName pos = (VarBinding varName (Simple $ Identity pos))

-- The resulting step will be compiled to a QueryFunctionDescription with function TypeTimeOnlyContextF,
-- which the unsafeCompiler will ignore.
makeTypeTimeOnlyContextStep :: VarName -> ContextType -> ArcPosition -> VarBinding
makeTypeTimeOnlyContextStep varName ctype pos = (VarBinding varName (Simple $ TypeTimeOnlyContext pos (unwrap ctype)))

-- The resulting step will be compiled to a QueryFunctionDescription with function TypeTimeOnlyRoleF,
-- which the unsafeCompiler will ignore.
makeTypeTimeOnlyRoleStep :: Partial => VarName -> ADT RoleInContext -> ArcPosition -> VarBinding
makeTypeTimeOnlyRoleStep varName (ST (RoleInContext{context, role})) pos = VarBinding varName (Simple $ TypeTimeOnlyEnumeratedRole pos (unwrap context) (unwrap role))
makeTypeTimeOnlyRoleStep varName (UET (RoleInContext{context, role})) pos = VarBinding varName (Simple $ TypeTimeOnlyEnumeratedRole pos (unwrap context) (unwrap role))
makeTypeTimeOnlyRoleStep varName (SUM alternatives) pos = case head alternatives of 
  Just r -> makeTypeTimeOnlyRoleStep varName r pos


-- | Produces a step that takes the context of its origin.
makeContextStep :: VarName -> ArcPosition -> VarBinding
makeContextStep varName pos = VarBinding varName (Simple (Context pos))

addContextualBindingsToExpression :: Array VarBinding -> Step -> Step
addContextualBindingsToExpression extraBindings step = let
  necessaryBindings = filter (\(VarBinding varName _) -> stepContainsVariableReference varName step)
    extraBindings
  in
  if null necessaryBindings
    then step
    else case step of
      (PureLet (PureLetStep r@{bindings})) -> PureLet (PureLetStep r {bindings = extraBindings <> bindings})
      _ -> PureLet (PureLetStep
            { start: startOf step
            , end: endOf step
            , bindings: extraBindings
            , body: step
            })

-- | We add the standard variables, regardless of whether they actually occur in one or more expressions.
-- | This adds just a little overhead in type time and in runtime they will not occur (the unsafe compiler
-- | removes them).
addContextualBindingsToStatements :: Array VarBinding -> Statements -> Statements
addContextualBindingsToStatements extraBindings stmts = case stmts of
  Let (LetStep r@{bindings, assignments}) -> do
    Let (LetStep r {bindings = (Expr <$> extraBindings) <> bindings})
  Statements stmtArray -> do
    Let (LetStep
        { start: unsafePartial $ startOfStatements stmts
        , end: unsafePartial $ endOfStatements stmts
        , bindings: Expr <$> extraBindings
        , assignments: stmtArray
        })
  where
    startOfStatements :: Partial => Statements -> ArcPosition
    startOfStatements (Let (LetStep{start})) = start
    startOfStatements (Statements stmtArray) = startOfAssignment (fromJust $ head stmtArray)

    endOfStatements :: Partial => Statements -> ArcPosition
    endOfStatements (Let (LetStep{end})) = end
    endOfStatements (Statements stmtArray) = endOfAssignment (fromJust $ last stmtArray)
