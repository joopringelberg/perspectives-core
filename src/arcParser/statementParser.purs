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

module Perspectives.Parsing.Arc.Statement where

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Maybe (isJust)
import Perspectives.Parsing.Arc.Expression (binding, step)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), AssignmentOperator(..), LetStep(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, reserved)
import Perspectives.Parsing.Arc.IndentParser (IP, getPosition, nestedBlock)
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Prelude (bind, discard, pure, ($), (*>), (<$>), (<*), (>>=), (<>))
import Text.Parsing.Indent (withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (lookAhead, option, optionMaybe, try, (<?>))

assignment :: IP Assignment
assignment = isPropertyAssignment >>= if _
  then propertyAssignment
  else roleAssignment

roleAssignment :: IP Assignment
roleAssignment = do
  keyword <- lookAhead reservedIdentifier <?> "Expected remove, createRole, move, bind, bind_, unbind, unbind_, delete, or callEffect"
  case keyword of
    "remove" -> removal
    "createRole" -> roleCreation
    "move" -> move
    "bind" -> bind'
    "bind_" -> bind_
    "unbind" -> unbind
    "unbind_" -> unbind_
    "delete" -> roleDeletion
    "callEffect" -> callEffect
    s -> fail ("Expected remove, createRole, move, bind, bind_, unbind, unbind_, delete, or callEffect but found '" <> s <> "'.")

removal :: IP Assignment
removal = do
  start <- getPosition
  roleExpression <- (reserved "remove" *> step)
  end <- getPosition
  pure $ Remove {start, end, roleExpression}

roleCreation :: IP Assignment
roleCreation = do
  start <- getPosition
  roleIdentifier <- reserved "createRole" *> arcIdentifier
  contextExpression <- optionMaybe (reserved "in" *> step)
  end <- getPosition
  pure $ CreateRole {start, end, roleIdentifier, contextExpression}

-- createContext ContextType bound to RoleType [ in <contextExpression>]
createContext :: IP Assignment
createContext = do
  start <- getPosition
  contextTypeIdentifier <- reserved "createContext" *> arcIdentifier
  roleTypeIdentifier <- reserved "bound" *> reserved "to" *> arcIdentifier
  contextExpression <- optionMaybe (reserved "in" *> step)
  end <- getPosition
  pure $ CreateContext {start, end, contextTypeIdentifier, roleTypeIdentifier, contextExpression}

-- createContext_ ContextType bound to <roleExpression>
createContext_ :: IP Assignment
createContext_ = do
  start <- getPosition
  contextTypeIdentifier <- reserved "createContext" *> arcIdentifier
  roleExpression <- reserved "bound" *> reserved "to" *> step
  end <- getPosition
  pure $ CreateContext_ {start, end, contextTypeIdentifier, roleExpression}

move :: IP Assignment
move = do
  start <- getPosition
  roleExpression <- (reserved "move" *> step)
  contextExpression <- optionMaybe (reserved "to" *> step)
  end <- getPosition
  pure $ Move {start, end, roleExpression, contextExpression}

bind' :: IP Assignment
bind' = do
  start <- getPosition
  bindingExpression <- (reserved "bind" *> step)
  roleIdentifier <- reserved "to" *> arcIdentifier
  contextExpression <- optionMaybe (reserved "in" *> step)
  end <- getPosition
  pure $ Bind {start, end, bindingExpression, roleIdentifier, contextExpression}

bind_ :: IP Assignment
bind_ = do
  start <- getPosition
  bindingExpression <- (reserved "bind_" *> step)
  binderExpression <- reserved "to" *> step
  end <- getPosition
  pure $ Bind_ {start, end, bindingExpression, binderExpression}

unbind :: IP Assignment
unbind = do
  start <- getPosition
  bindingExpression <- (reserved "unbind" *> step)
  roleIdentifier <- optionMaybe (reserved "from" *> arcIdentifier)
  end <- getPosition
  pure $ Unbind {start, end, bindingExpression, roleIdentifier}

unbind_ :: IP Assignment
unbind_ = do
  start <- getPosition
  bindingExpression <- (reserved "unbind_" *> step)
  binderExpression <- reserved "from" *> step
  end <- getPosition
  pure $ Unbind_ {start, end, bindingExpression, binderExpression}

isPropertyAssignment :: IP Boolean
isPropertyAssignment = do
  keyword <- option "" (lookAhead reservedIdentifier)
  case keyword of
    "delete" -> pure true
    otherwise -> isJust <$> optionMaybe (lookAhead (arcIdentifier *> assignmentOperator))

propertyAssignment :: IP Assignment
propertyAssignment = do
  keyword <- option "" (lookAhead reservedIdentifier)
  case keyword of
    "delete" -> propertyDeletion
    _ -> propertyAssignment'
  where
    propertyAssignment' :: IP Assignment
    propertyAssignment' = try do
      start <- getPosition
      propertyIdentifier <- arcIdentifier
      op <- assignmentOperator
      val <- step
      end <- getPosition
      roleExpression <- optionMaybe (reserved "for" *> step)
      pure $ PropertyAssignment {start, end, propertyIdentifier, operator: op, valueExpression: val, roleExpression}

    propertyDeletion :: IP Assignment
    propertyDeletion = try do
      start <- getPosition
      reserved "delete"
      reserved "property"
      propertyIdentifier <- arcIdentifier
      roleExpression <- optionMaybe (reserved "from" *> step)
      end <- getPosition
      pure $ DeleteProperty {start, end, propertyIdentifier, roleExpression}


assignmentOperator :: IP AssignmentOperator
assignmentOperator = try
  (DeleteFrom <$> (getPosition <* token.reservedOp "=-"))
  <|>
  (AddTo <$> (getPosition <* token.reservedOp "=+"))
  <|>
  ((Set <$> (getPosition <* token.reservedOp "="))
  ) <?> "=, =+, =-"

roleDeletion :: IP Assignment
roleDeletion = try do
  start <- getPosition
  reserved "delete"
  roleIdentifier <- arcIdentifier
  contextExpression <- optionMaybe (reserved "from" *> step)
  end <- getPosition
  pure $ DeleteRole {start, end, roleIdentifier, contextExpression}

callEffect :: IP Assignment
callEffect = try do
  start <- getPosition
  effectName <- reserved "callEffect" *> arcIdentifier
  arguments <- token.parens (token.commaSep step)
  end <- getPosition
  pure $ ExternalEffect {start, end, effectName, arguments: (fromFoldable arguments)}

-- letStep = do
--   lookAhead (reserved "let*")
--   start <- getPosition
--   bindings <- withEntireBlock (\_ bs -> bs) (reserved "let*") binding
--   massignments <- optionMaybe $ try $ withEntireBlock (\_ bs -> bs) (reserved "in") assignment
--   case massignments of
--     (Just assignments) -> do
--       end <- getPosition
--       pure $ Let $ LetStep {start, end, bindings: fromFoldable bindings, assignments: fromFoldable assignments}
--     Nothing -> do
--       body <- reserved "in" *> step
--       end <- getPosition
--       pure $ PureLet $ PureLetStep {start, end, bindings: fromFoldable bindings, body}

-- | A let with assignments: letA <binding>+ in <assignment>+.
letWithAssignment :: IP LetStep
letWithAssignment = withPos $ try do
  start <- getPosition
  bindings <- reserved "letA" *> nestedBlock binding
  assignments <- reserved "in" *> nestedBlock assignment
  end <- getPosition
  pure $ LetStep {start, end, bindings: fromFoldable bindings, assignments: fromFoldable assignments}