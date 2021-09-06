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

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (StateT, execStateT, modify)
import Data.Array (catMaybes, foldMap, head, last, length, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.AST (StateKind(..), StateSpecification(..))
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), LetABinding(..), LetStep(..), Statements(..), endOfAssignment, startOfAssignment)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Prelude (class Monad, Unit, bind, discard, pure, unit, void, ($), (<$>), (<>), (==), (||), (>))

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
stepContainsVariableReference varName (Unary (Binds _ s)) = stepContainsVariableReference varName s
stepContainsVariableReference varName (Unary (BoundBy _ s)) = stepContainsVariableReference varName s
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
assignmentContainsReference varName (Remove {roleExpression}) = stepContainsVariableReference varName roleExpression

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

assignmentContainsReference varName (DeleteProperty {roleExpression}) =
  maybe false (stepContainsVariableReference varName) roleExpression

assignmentContainsReference varName (PropertyAssignment {valueExpression, roleExpression}) =
  stepContainsVariableReference varName valueExpression
  ||
  maybe false (stepContainsVariableReference varName) roleExpression

assignmentContainsReference varName (ExternalEffect {arguments}) =
  ala Disj foldMap $ stepContainsVariableReference varName <$> arguments

--------------------------------------------------------------------------
--- A MONAD TO COLLECT VARIABLE BINDINGS
--------------------------------------------------------------------------
type CollectingBindings m = StateT (Array VarBinding) m

-- | Run a computation that adds VarBinding instances to state, return
-- | an Array of those bindings (possibly empty).
collectBindings :: forall a m. Monad m => CollectingBindings m a -> m (Array VarBinding)
collectBindings x = execStateT x []

-- | In CollectingBindings, add a VarBinding to state, if it is not yet there.
addBinding :: forall m. Monad m => VarBinding -> CollectingBindings m (Array VarBinding)
addBinding vb = modify (union [vb])

--------------------------------------------------------------------------
--- ADD CONTEXTUAL VARIABLES TO AN EXPRESSION
--------------------------------------------------------------------------

addContextualVariablesToExpression :: forall m. MonadError PerspectivesError m => Step -> Maybe Step -> StateKind -> m Step
addContextualVariablesToExpression stp mobject stateKind = case stp of
  (PureLet (PureLetStep r@{bindings})) -> do
    -- TODO. Let op: we zouden óók in de bindings zelf moeten zoeken naar verwijzingen naar "currentobject" en "currentcontext".
    extraBindings <- collectBindings do
      for_ bindings \(VarBinding _ stp') -> do
        addCurrentContextForStep stp' stateKind
        addObjectForStep stp' mobject stateKind
      addCurrentContextForStep stp stateKind
      addObjectForStep stp mobject stateKind
    pure $ PureLet (PureLetStep r {bindings = extraBindings <> bindings})
  _ -> do
    bindings <- collectBindings do
      addCurrentContextForStep stp stateKind
      addObjectForStep stp mobject stateKind
    if length bindings > 0
      then pure $ PureLet (PureLetStep
        { start: startOf stp
        , end: endOf stp
        , bindings
        , body: stp
        })
      else pure stp

--
addCurrentContextForStep :: forall m. MonadError PerspectivesError m => Step -> StateKind -> CollectingBindings m Unit
addCurrentContextForStep stp stateKind = if stepContainsVariableReference "currentcontext" stp
  then case stateKind of
    CState -> void $ addBinding (VarBinding "currentcontext" (Simple $ Identity (unsafePartial $ startOf stp)))
    SState -> void $ addBinding (VarBinding "currentcontext" (Simple $ Context (unsafePartial $ startOf stp)))
    OState -> void $ addBinding (VarBinding "currentcontext" (Simple $ Context (unsafePartial $ startOf stp)))
  else pure unit

addObjectForStep :: forall m. MonadError PerspectivesError m => Step -> Maybe Step -> StateKind -> CollectingBindings m Unit
addObjectForStep stp mobject stateKind = if stepContainsVariableReference "currentobject" stp
  then case stateKind of
    CState -> case mobject of
      Nothing -> throwError (MissingObject (unsafePartial $ startOf stp) (unsafePartial $ endOf stp))
      Just obj -> void $ addBinding (VarBinding "currentobject" obj)
    SState -> case mobject of
      Nothing -> throwError (MissingObject (unsafePartial $ startOf stp) (unsafePartial $ endOf stp))
      Just obj -> void $ addBinding (VarBinding "currentobject" obj)
    OState -> void $ addBinding (VarBinding "currentobject" (Simple $ Identity (unsafePartial $ startOf stp)))
  else pure unit

--------------------------------------------------------------------------
--- ADD CONTEXTUAL VARIABLES TO STATEMENTS
--------------------------------------------------------------------------
addContextualVariablesToStatements :: forall m. MonadError PerspectivesError m => Statements -> Maybe Step -> StateKind -> m Statements
addContextualVariablesToStatements stmts mobject stateKind = case stmts of
  Let (LetStep r@{bindings, assignments}) -> do
    extraBindings <- collectBindings do
      for_ bindings \b -> case b of
        (Expr (VarBinding _ stp)) -> do
          addCurrentContextForStep stp stateKind
          addObjectForStep stp mobject stateKind
        (Stat _ assignment) -> do
          addCurrentContext (Statements [assignment])
          addObject (Statements [assignment])
      addCurrentContext stmts
      addObject stmts
    pure $ Let (LetStep r {bindings = (Expr <$> extraBindings) <> bindings})
  Statements stmtArray -> do
    bindings <- collectBindings do
      addCurrentContext stmts
      addObject stmts
    if length bindings > 0
      then pure $ Let (LetStep
        { start: unsafePartial $ startOfStatements stmts
        , end: unsafePartial $ endOfStatements stmts
        , bindings: Expr <$> bindings
        , assignments: stmtArray
        })
      else pure stmts
  where
    -- For context state, `currentcontext` is bound to the context instance the expression is applied to.
    -- For object- and subject state, it is bound to the context of the role instance the expression is applied to.
    addCurrentContext :: Statements -> CollectingBindings m Unit
    addCurrentContext stmts' = if statementContainsVariableReference "currentcontext" stmts'
      then case stateKind of
        CState -> void $ addBinding (VarBinding "currentcontext" (Simple $ Identity (unsafePartial $ startOfStatements stmts')))
        SState -> void $ addBinding (VarBinding "currentcontext" (Simple $ Context (unsafePartial $ startOfStatements stmts')))
        OState -> void $ addBinding (VarBinding "currentcontext" (Simple $ Context (unsafePartial $ startOfStatements stmts')))
      else pure unit

    -- For context state, `object` is bound to objects computed with mobject.
    -- For object state, `object` is bound to the role instance that the expression is applied to.
    -- For subject state, `object` is bound to objects computed with mobject.
    addObject :: Statements -> CollectingBindings m Unit
    addObject stmts' = if statementContainsVariableReference "currentobject" stmts'
      then case stateKind of
        CState -> case mobject of
          Nothing -> throwError (MissingObject (unsafePartial $ startOfStatements stmts') (unsafePartial $ endOfStatements stmts'))
          Just obj -> void $ addBinding (VarBinding "currentobject" obj)
        SState -> case mobject of
          Nothing -> throwError (MissingObject (unsafePartial $ startOfStatements stmts') (unsafePartial $ endOfStatements stmts'))
          Just obj -> void $ addBinding (VarBinding "currentobject" obj)
        OState -> void $ addBinding (VarBinding "currentobject" (Simple $ Identity (unsafePartial $ startOfStatements stmts')))
      else pure unit

    startOfStatements :: Partial => Statements -> ArcPosition
    startOfStatements (Let (LetStep{start})) = start
    startOfStatements (Statements stmtArray) = startOfAssignment (fromJust $ head stmtArray)

    endOfStatements :: Partial => Statements -> ArcPosition
    endOfStatements (Let (LetStep{end})) = end
    endOfStatements (Statements stmtArray) = endOfAssignment (fromJust $ last stmtArray)

stateSpec2stateKind :: StateSpecification -> StateKind
stateSpec2stateKind (ContextState _ _) = CState
stateSpec2stateKind (SubjectState _ _) = SState
stateSpec2stateKind (ObjectState _ _) = OState
