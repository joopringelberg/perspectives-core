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
import Data.List (List(..))
import Data.Maybe (isJust)
import Data.Tuple (Tuple(..))
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Expression.AST (VarBinding(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, lowerCaseName, reserved)
import Perspectives.Parsing.Arc.IndentParser (IP, getPosition, outdented')
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), AssignmentOperator(..), LetStep(..), LetABinding(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Prelude (bind, discard, pure, ($), (*>), (<$>), (<*), (>>=), (<>), (<*>))
import Text.Parsing.Indent (indented', withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (lookAhead, manyTill, option, optionMaybe, try, (<?>))

assignment :: IP Assignment
assignment = isPropertyAssignment >>= if _
  then propertyAssignment
  else roleAssignment

roleAssignment :: IP Assignment
roleAssignment = do
  keyword <- lookAhead reservedIdentifier <?> "Expected remove, createRole, move, bind, bind_, unbind, unbind_, delete, createContext, createContext_ or callEffect"
  case keyword of
    "remove" -> do
      (Tuple first second) <- twoReservedWords
      case first, second of
        "remove", "role" -> roleRemoval
        "remove", "context" -> contextRemoval
        _, _ -> fail ("Expected 'role' or 'context' after 'remove'.")
    "createRole" -> roleCreation
    "move" -> move
    "bind" -> bind'
    "bind_" -> bind_
    "unbind" -> unbind
    "unbind_" -> unbind_
    "delete" -> do
      (Tuple first second) <- twoReservedWords
      case first, second of
        "delete", "role" -> roleDeletion
        "delete", "context" -> contextDeletion
        _, _ -> fail ("Expected 'role' or 'context' after 'delete'.")
    "callEffect" -> callEffect
    "createContext" -> createContext
    "createContext_" -> createContext_
    s -> fail ("Expected remove, createRole, move, bind, bind_, unbind, unbind_, delete, or callEffect but found '" <> s <> "'.")

roleRemoval :: IP Assignment
roleRemoval = do
  start <- getPosition
  reserved "remove"
  reserved "role"
  roleExpression <- step
  end <- getPosition
  pure $ RemoveRole {start, end, roleExpression}

contextRemoval :: IP Assignment
contextRemoval = do
  start <- getPosition
  reserved "remove"
  reserved "context"
  roleExpression <- step
  end <- getPosition
  pure $ RemoveContext {start, end, roleExpression}

roleCreation :: IP Assignment
roleCreation = do
  start <- getPosition
  roleIdentifier <- reserved "createRole" *> arcIdentifier
  -- Check indentiation to prevent confusion of 'in' with the 'in' of the letA.
  contextExpression <- optionMaybe (indented' *> reserved "in" *> step)
  end <- getPosition
  pure $ CreateRole {start, end, roleIdentifier, contextExpression}

-- createContext ContextType bound to RoleType [ in <contextExpression>]
createContext :: IP Assignment
createContext = withPos do
  start <- getPosition
  contextTypeIdentifier <- reserved "createContext" *> arcIdentifier
  roleTypeIdentifier <- reserved "bound" *> reserved "to" *> arcIdentifier
  -- Check indentiation to prevent confusion of 'in' with the 'in' of the letA.
  contextExpression <- optionMaybe (indented' *> reserved "in" *> step)
  end <- getPosition
  pure $ CreateContext {start, end, contextTypeIdentifier, roleTypeIdentifier, contextExpression}

-- createContext_ ContextType bound to <roleExpression>
createContext_ :: IP Assignment
createContext_ = do
  start <- getPosition
  contextTypeIdentifier <- reserved "createContext_" *> arcIdentifier
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
  (Tuple first second) <- twoReservedWords
  case first, second of
    "delete", "property" -> pure true
    "delete", "role" -> pure false
    _, _ -> isJust <$> optionMaybe (lookAhead (arcIdentifier *> assignmentOperator))
  -- keyword <- option "" (lookAhead reservedIdentifier)
  -- case keyword of
  --   "delete" -> pure true
  --   otherwise -> isJust <$> optionMaybe (lookAhead (arcIdentifier *> assignmentOperator))

propertyAssignment :: IP Assignment
propertyAssignment = do
  keyword <- option "" (lookAhead reservedIdentifier)
  case keyword of
    "delete" -> propertyDeletion
    _ -> propertyAssignment'
  where
    propertyAssignment' :: IP Assignment
    propertyAssignment' = do
      start <- getPosition
      propertyIdentifier <- arcIdentifier
      op <- assignmentOperator
      val <- step
      end <- getPosition
      roleExpression <- optionMaybe (reserved "for" *> step)
      pure $ PropertyAssignment {start, end, propertyIdentifier, operator: op, valueExpression: val, roleExpression}

    propertyDeletion :: IP Assignment
    propertyDeletion = do
      start <- getPosition
      reserved "delete"
      reserved "property"
      propertyIdentifier <- arcIdentifier
      roleExpression <- optionMaybe (reserved "from" *> step)
      end <- getPosition
      pure $ DeleteProperty {start, end, propertyIdentifier, roleExpression}


assignmentOperator :: IP AssignmentOperator
assignmentOperator =
  (DeleteFrom <$> (getPosition <* token.reservedOp "=-"))
  <|>
  (AddTo <$> (getPosition <* token.reservedOp "=+"))
  <|>
  ((Set <$> (getPosition <* token.reservedOp "="))
  ) <?> "=, =+, =-"

roleDeletion :: IP Assignment
roleDeletion = do
  start <- getPosition
  reserved "delete"
  reserved "role"
  roleIdentifier <- arcIdentifier
  contextExpression <- optionMaybe (reserved "from" *> step)
  end <- getPosition
  pure $ DeleteRole {start, end, roleIdentifier, contextExpression}

contextDeletion :: IP Assignment
contextDeletion = do
  start <- getPosition
  reserved "delete"
  reserved "context"
  reserved "bound"
  reserved "to"
  contextRoleIdentifier <- arcIdentifier
  contextExpression <- optionMaybe (reserved "from" *> step)
  end <- getPosition
  pure $ DeleteContext {start, end, contextRoleIdentifier, contextExpression}

callEffect :: IP Assignment
callEffect = do
  start <- getPosition
  effectName <- reserved "callEffect" *> arcIdentifier
  arguments <- token.symbol "(" *>
    (((token.symbol ")") *> pure Nil)
    <|>
    do
      first <- step
      -- By using manyTill we get the error messages inside arguments to the end user.
      -- sepBy would hide them.
      rest <- manyTill (token.comma *> step) (token.symbol ")")
      pure (Cons first rest))
  end <- getPosition
  pure $ ExternalEffect {start, end, effectName, arguments: (fromFoldable arguments)}

-- | A let with assignments: letA <binding>+ in <assignment>+.
letWithAssignment :: IP LetStep
letWithAssignment = try $ withPos do
  start <- getPosition
  -- bindings <- reserved "letA" *> nestedBlock letABinding
  bindings <- reserved "letA" *> withPos (manyTill letABinding outdented')
  -- assignments <- reserved "in" *> nestedBlock assignment
  assignments <- reserved "in" *> withPos (manyTill assignment outdented')
  end <- getPosition
  pure $ LetStep {start, end, bindings: fromFoldable bindings, assignments: fromFoldable assignments}

letABinding :: IP LetABinding
letABinding = do
  varName <- (lowerCaseName <* token.reservedOp "<-") <?> "lower case name followed by <-"
  keyword <- option "" (lookAhead reservedIdentifier)
  case keyword of
    "createContext" -> Stat <$> pure varName <*> createContext
    "createRole" -> Stat <$> pure varName <*> roleCreation
    _ -> Expr <$> (VarBinding <$> pure varName <*> step)


-- | Looking ahead, find at least one reserved identifier, two if possible.
-- | If only one is found, returns Tuple <theword> "".
-- | If none is found, returns Tuple "" "".
twoReservedWords :: IP (Tuple String String)
twoReservedWords = option (Tuple "" "") $ lookAhead (Tuple <$> reservedIdentifier <*> (option "" reservedIdentifier))
