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

-- | An Abstract Syntax Tree data model for Perspectives statements.

module Perspectives.Parsing.Arc.Statement.AST where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, isNothing)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Parsing.Arc.Expression.AST (Step, VarBinding)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

data Statements = Let LetStep | Statements (Array Assignment)

newtype LetStep = LetStep {start :: ArcPosition, end :: ArcPosition, bindings:: Array LetABinding, assignments :: Array Assignment}

data LetABinding = Expr VarBinding | Stat String Assignment

data AssignmentOperator =
  Set ArcPosition
  | AddTo ArcPosition
  | DeleteFrom ArcPosition

type WithTextRange f = {start :: ArcPosition, end :: ArcPosition | f}

data Assignment =
	Remove (WithTextRange (roleExpression :: Step))
	| CreateRole (WithTextRange (roleIdentifier :: String, contextExpression :: Maybe Step))
  | CreateContext (WithTextRange (contextTypeIdentifier :: String, roleTypeIdentifier :: String, contextExpression :: Maybe Step))
  | CreateContext_ (WithTextRange (contextTypeIdentifier :: String, roleExpression :: Step))
  | Move (WithTextRange (roleExpression :: Step, contextExpression :: Maybe Step))
  | Bind (WithTextRange (bindingExpression :: Step, roleIdentifier :: String, contextExpression :: Maybe Step))
  | Bind_ (WithTextRange (bindingExpression :: Step, binderExpression :: Step))
  -- TODO: Maybe String voor roleIdentifier. Pas de parser aan.
  | Unbind (WithTextRange (bindingExpression :: Step, roleIdentifier :: Maybe String))
  | Unbind_ (WithTextRange (bindingExpression :: Step, binderExpression :: Step))
  | DeleteRole (WithTextRange (roleIdentifier :: String, contextExpression :: Maybe Step))
  | DeleteProperty (WithTextRange (propertyIdentifier :: String, roleExpression :: Maybe Step))
  | PropertyAssignment (WithTextRange (propertyIdentifier :: String, operator :: AssignmentOperator, valueExpression :: Step, roleExpression :: Maybe Step ))
  | ExternalEffect (WithTextRange (effectName :: String, arguments :: (Array Step) ) )

startOfAssignment :: Assignment -> ArcPosition
startOfAssignment (Remove{start}) = start
startOfAssignment (CreateRole{start}) = start
startOfAssignment (CreateContext{start}) = start
startOfAssignment (CreateContext_{start}) = start
startOfAssignment (Move{start}) = start
startOfAssignment (Bind{start}) = start
startOfAssignment (Bind_{start}) = start
startOfAssignment (Unbind{start}) = start
startOfAssignment (Unbind_{start}) = start
startOfAssignment (DeleteRole{start}) = start
startOfAssignment (DeleteProperty{start}) = start
startOfAssignment (PropertyAssignment{start}) = start
startOfAssignment (ExternalEffect{start}) = start

endOfAssignment :: Assignment -> ArcPosition
endOfAssignment (Remove{end}) = end
endOfAssignment (CreateRole{end}) = end
endOfAssignment (CreateContext{end}) = end
endOfAssignment (CreateContext_{end}) = end
endOfAssignment (Move{end}) = end
endOfAssignment (Bind{end}) = end
endOfAssignment (Bind_{end}) = end
endOfAssignment (Unbind{end}) = end
endOfAssignment (Unbind_{end}) = end
endOfAssignment (DeleteRole{end}) = end
endOfAssignment (DeleteProperty{end}) = end
endOfAssignment (PropertyAssignment{end}) = end
endOfAssignment (ExternalEffect{end}) = end

derive instance genericStatements :: Generic Statements _
instance showStatements :: Show Statements where show = genericShow
instance eqStatements :: Eq Statements where eq = genericEq
instance encodeStatements :: Encode Statements where
  encode = genericEncode defaultOptions
instance decodeStatements :: Decode Statements where
  decode = genericDecode defaultOptions

derive instance genericLetStep :: Generic LetStep _
instance showLetStep :: Show LetStep where show = genericShow
instance eqLetStep :: Eq LetStep where eq u1 u2 = genericEq u1 u2
derive instance ordLetStap :: Ord LetStep
instance encodeLetStep :: Encode LetStep where
  encode q = genericEncode defaultOptions q
instance decodeLetStep :: Decode LetStep where
  decode q = genericDecode defaultOptions q
instance prettyPrintLetStep :: PrettyPrint LetStep where
  prettyPrint' t (LetStep {bindings, assignments}) = "LetStep\n" <> intercalate (t <> "\n") (prettyPrint' (t <> "  ") <$> bindings) <> "\n" <> t <> "in\n" <> intercalate (t <> "\n") (prettyPrint' (t <> "  ") <$> assignments)

derive instance genericLetABinding :: Generic LetABinding _
instance showLetABinding :: Show LetABinding where show = genericShow
instance eqLetABinding :: Eq LetABinding where eq = genericEq
derive instance ordLetABinding :: Ord LetABinding
instance encodeLetABinding :: Encode LetABinding where
  encode q = genericEncode defaultOptions q
instance decodeLetABinding :: Decode LetABinding where
  decode q = genericDecode defaultOptions q
instance prettyPrintLetABinding :: PrettyPrint LetABinding where
  prettyPrint' t (Expr e) = "Expr " <> prettyPrint' t e
  prettyPrint' t (Stat varname s) = "Stat " <> varname <> " " <> prettyPrint' t s

derive instance genericAssignment :: Generic Assignment _
instance showAssignment :: Show Assignment where show = genericShow
instance eqAssignment :: Eq Assignment where eq = genericEq
derive instance ordAssignment :: Ord Assignment
instance encodeAssignment :: Encode Assignment where
  encode q = genericEncode defaultOptions q
instance decodeAssignment :: Decode Assignment where
  decode q = genericDecode defaultOptions q
instance prettyPrintAssignment :: PrettyPrint Assignment where
  prettyPrint' t (Remove {roleExpression}) = "Remove " <> prettyPrint' t roleExpression
  prettyPrint' t (CreateRole {roleIdentifier, contextExpression}) = "CreateRole " <> roleIdentifier <> " " <> prettyPrint' t contextExpression
  prettyPrint' t (CreateContext {contextTypeIdentifier, roleTypeIdentifier, contextExpression}) = let
    context = if isNothing contextExpression then "current context" else prettyPrint' t contextExpression
    in
      "CreateContext " <> contextTypeIdentifier <> " bound to " <> roleTypeIdentifier <> " in " <> context
  prettyPrint' t (CreateContext_ {contextTypeIdentifier, roleExpression}) = "CreateContext_ " <> contextTypeIdentifier <> " bound to " <> prettyPrint' t roleExpression
  prettyPrint' t (Move {roleExpression, contextExpression}) = "Move " <> prettyPrint' t roleExpression <> "\n" <> t <> prettyPrint' (t <> "  ") contextExpression
  prettyPrint' t (Bind {bindingExpression, contextExpression}) = "Bind " <> prettyPrint' t bindingExpression <> "\n" <> t <> prettyPrint' (t <> "  ") contextExpression
  prettyPrint' t (Bind_ {bindingExpression, binderExpression}) = "Bind_ " <> prettyPrint' t bindingExpression <> "\n" <> t <> prettyPrint' (t <> "  ") binderExpression
  prettyPrint' t (Unbind {bindingExpression, roleIdentifier}) = "Unbind " <> prettyPrint' t bindingExpression <> "\n" <> t <> show roleIdentifier
  prettyPrint' t (Unbind_ {bindingExpression, binderExpression}) = "Unbind_ " <> prettyPrint' t bindingExpression <> "\n" <> t <> prettyPrint' (t <> "  ") binderExpression
  prettyPrint' t (DeleteRole {roleIdentifier, contextExpression}) = "DeleteRole " <> roleIdentifier <> " " <> prettyPrint' t contextExpression
  prettyPrint' t (DeleteProperty {propertyIdentifier, roleExpression}) = "DeleteProperty " <> propertyIdentifier <> " " <> prettyPrint' t roleExpression
  prettyPrint' t (PropertyAssignment {propertyIdentifier, operator, valueExpression, roleExpression}) = "PropertyAssignment " <> propertyIdentifier <> " " <> prettyPrint' t operator <> " " <> "\n" <> t <> prettyPrint' (t <> "  ") valueExpression <> "\n" <> t <> prettyPrint' (t <> "  ") roleExpression
  prettyPrint' t (ExternalEffect {arguments}) = "ExternalEffect\n" <> intercalate ("\n" <> t) (prettyPrint' (t <> "  ") <$> arguments)

derive instance genericAssignmentOperator :: Generic AssignmentOperator _
instance showAssignmentOperator :: Show AssignmentOperator where show = genericShow
instance eqAssignmentOperator :: Eq AssignmentOperator where eq = genericEq
derive instance ordAssignmentOperator :: Ord AssignmentOperator
instance encodeAssignmentOperator :: Encode AssignmentOperator where
  encode = genericEncode defaultOptions
instance decodeAssignmentOperator :: Decode AssignmentOperator where
  decode = genericDecode defaultOptions
instance prettyPrintAssignmentOperator :: PrettyPrint AssignmentOperator where
  prettyPrint' _ (Set _) = "Set"
  prettyPrint' _ (AddTo _) = "AddTo"
  prettyPrint' _ (DeleteFrom _) = "DeleteFrom"
