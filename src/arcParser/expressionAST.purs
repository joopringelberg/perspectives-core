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

-- | An Abstract Syntax Tree data model for Perspectives expressions. The expression grammar is below.
-- |
-- | step = simpleStep | unaryStep | compoundStep | let*
-- |
-- | simpleStep = ArcIdentifier
-- |
-- | | binding
-- |
-- | | binder
-- |
-- | | context
-- |
-- | | extern
-- |
-- | | Value
-- |
-- | | variable
-- |
-- | | this
-- |
-- | | >>= SequenceFunction
-- |
-- | unaryStep =
-- |    'not' step
-- |  | 'createRole' ArcIdentifier
-- |  | 'createContext' ArcIdentifier
-- |  | 'exists' step
-- |
-- | compoundStep =
-- |    'filter' step 'with' step
-- |  | step operator step
-- |  | 'bind_' step 'in' step
-- |
-- | operator = '>>' | '==' | '<' | '>' | '<=' | '>=' | 'and' | 'or' | '+' | '-' | '*' | '/'
-- |
-- | assignment = ArcIdentifier AssignmentOperator step
-- |
-- | AssignmentOperator = '=' | '=+' | '=-'
-- |
-- | RoleName = ArcIdentifier
-- |
-- | PropertyName = ArcIdentifier
-- |
-- | Value = number | boolean | string | date
-- |
-- | SequenceFunction = 'sum' | 'count' | 'product' | 'minimum' | 'maximum'
-- |
-- |
-- | let* = 'let*' binding+ 'in' body
-- |
-- | binding = variable '<-' step
-- |
-- | body = step | assignment+
-- |
-- | variable = lowerCaseName


module Perspectives.Parsing.Arc.Expression.AST where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.QueryFunction (FunctionName) as QF
import Perspectives.Representation.Range (Range)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

-- | Step represents an Expression conforming to the grammar given above.
data Step = Simple SimpleStep | Binary BinaryStep | Unary UnaryStep | Let LetStep | PureLet PureLetStep | Computation ComputationStep

data SimpleStep =
  ArcIdentifier ArcPosition String
  | Value ArcPosition Range String
  | CreateContext ArcPosition String
  | CreateEnumeratedRole ArcPosition String
  | Binding ArcPosition
  | Binder ArcPosition String
  | Context ArcPosition
  | Extern ArcPosition
  | SequenceFunction ArcPosition QF.FunctionName
  | Identity ArcPosition
  | Variable ArcPosition String

  | TypeOfContext ArcPosition
  | RoleTypes ArcPosition
  | SpecialisesRoleType ArcPosition String

data UnaryStep =
  LogicalNot ArcPosition Step
  | Exists ArcPosition Step
  | Available ArcPosition Step

newtype BinaryStep = BinaryStep {start :: ArcPosition, end :: ArcPosition, operator :: Operator, left :: Step, right :: Step, parenthesised :: Boolean}

newtype LetStep = LetStep {start :: ArcPosition, end :: ArcPosition, bindings:: Array VarBinding, assignments :: Array Assignment}

newtype PureLetStep = PureLetStep {start :: ArcPosition, end :: ArcPosition, bindings:: Array VarBinding, body :: Step}

newtype ComputationStep = ComputationStep {functionName :: String, arguments :: Array Step, computedType :: String, start :: ArcPosition, end :: ArcPosition}

data VarBinding = VarBinding String Step

data Operator =
  Compose ArcPosition
  | Equals ArcPosition
  | NotEquals ArcPosition
  | LessThan ArcPosition
  | LessThanEqual ArcPosition
  | GreaterThan ArcPosition
  | GreaterThanEqual ArcPosition
  | LogicalAnd ArcPosition
  | LogicalOr ArcPosition
  | Add ArcPosition
  | Subtract ArcPosition
  | Divide ArcPosition
  | Multiply ArcPosition
  | Filter ArcPosition
  | Sequence ArcPosition

-- newtype Assignment = Assignment {start :: ArcPosition, end :: ArcPosition, lhs :: String, operator :: AssignmentOperator, value :: Maybe Step}

data AssignmentOperator =
  Set ArcPosition
  | AddTo ArcPosition
  | DeleteFrom ArcPosition

type WithTextRange f = {start :: ArcPosition, end :: ArcPosition | f}

data Assignment =
	Remove (WithTextRange (roleExpression :: Step))
	| CreateRole (WithTextRange (roleIdentifier :: String, contextExpression :: Maybe Step))
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

derive instance genericStep :: Generic Step _
instance showStep :: Show Step where show s = genericShow s
instance eqStep :: Eq Step where eq = genericEq
instance encodeStep :: Encode Step where
  encode = genericEncode defaultOptions
instance decodeStep :: Decode Step where
  decode = genericDecode defaultOptions
instance prettyPrintStep :: PrettyPrint Step where
  prettyPrint' t (Simple s) = prettyPrint' t s
  prettyPrint' t (Binary s) = prettyPrint' t s
  prettyPrint' t (Unary s) = prettyPrint' t s
  prettyPrint' t (Let s) = prettyPrint' t s
  prettyPrint' t (PureLet s) = prettyPrint' t s
  prettyPrint' t (Computation s) = prettyPrint' t s

derive instance genericSimpleStep :: Generic SimpleStep _
instance showSimpleStep :: Show SimpleStep where show = genericShow
instance eqSimpleStep :: Eq SimpleStep where eq = genericEq
instance encodeSimpleStep :: Encode SimpleStep where
  encode = genericEncode defaultOptions
instance decodeSimpleStep :: Decode SimpleStep where
  decode = genericDecode defaultOptions
instance prettyPrintSimpleStep :: PrettyPrint SimpleStep where
  prettyPrint' t (ArcIdentifier _ s) = "ArcIdentifier " <> s
  prettyPrint' t (CreateContext _ s) = "CreateContext " <> s
  prettyPrint' t (CreateEnumeratedRole _ s) = "CreateEnumeratedRole " <> s
  prettyPrint' t (Binding _) = "Binding"
  prettyPrint' t (Binder _ s) = "Binder " <> s
  prettyPrint' t (Context _) = "Context"
  prettyPrint' t (Extern _) = "Extern"
  prettyPrint' t (SequenceFunction _ s) = "SequenceFunction " <> show s
  prettyPrint' t (Identity _ ) = "Identity"
  prettyPrint' t (Variable _ s) = "Variable " <> s
  prettyPrint' t (Value _ range s) = "Value " <> show range <> " " <> s
  prettyPrint' t (RoleTypes _) = "RoleTypes"
  prettyPrint' t (TypeOfContext _) = "TypeOfContext"
  prettyPrint' t (SpecialisesRoleType _ s) = "SpecialisesRoleType " <> s

derive instance genericBinaryStep :: Generic BinaryStep _
instance showBinaryStep :: Show BinaryStep where show = genericShow
instance eqBinaryStep :: Eq BinaryStep where eq s1 s2 = genericEq s1 s2
instance encodeBinaryStep :: Encode BinaryStep where
  encode q = genericEncode defaultOptions q
instance decodeBinaryStep :: Decode BinaryStep where
  decode q = genericDecode defaultOptions q
instance prettyPrintBinaryStep :: PrettyPrint BinaryStep where
  prettyPrint' t (BinaryStep {operator, left, right}) = prettyPrint' t operator <> "\n" <> t <> (prettyPrint' (t <> "  ") left) <> "\n" <> t <> (prettyPrint' (t <> "  ") right)

derive instance genericUnaryStep :: Generic UnaryStep _
instance showUnaryStep :: Show UnaryStep where show = genericShow
instance eqUnaryStep :: Eq UnaryStep where eq u1 u2 = genericEq u1 u2
instance encodeUnaryStep :: Encode UnaryStep where
  encode q = genericEncode defaultOptions q
instance decodeUnaryStep :: Decode UnaryStep where
  decode q = genericDecode defaultOptions q
instance prettyPrintUnaryStep :: PrettyPrint UnaryStep where
  prettyPrint' t (LogicalNot _ s) = "LogicalNot " <> prettyPrint' t s
  prettyPrint' t (Exists _ s) = "Exists " <> prettyPrint' t s
  prettyPrint' t (Available _ s) = "Available " <> prettyPrint' t s

derive instance genericLetStep :: Generic LetStep _
instance showLetStep :: Show LetStep where show = genericShow
instance eqLetStep :: Eq LetStep where eq u1 u2 = genericEq u1 u2
instance encodeLetStep :: Encode LetStep where
  encode q = genericEncode defaultOptions q
instance decodeLetStep :: Decode LetStep where
  decode q = genericDecode defaultOptions q
instance prettyPrintLetStep :: PrettyPrint LetStep where
  prettyPrint' t (LetStep {bindings, assignments}) = "LetStep\n" <> intercalate (t <> "\n") (prettyPrint' (t <> "  ") <$> bindings) <> "\n" <> t <> "in\n" <> intercalate (t <> "\n") (prettyPrint' (t <> "  ") <$> assignments)

derive instance genericPureLetStep :: Generic PureLetStep _
instance showPureLetStep :: Show PureLetStep where show = genericShow
instance eqPureLetStep :: Eq PureLetStep where eq u1 u2 = genericEq u1 u2
instance encodePureLetStep :: Encode PureLetStep where
  encode q = genericEncode defaultOptions q
instance decodePureLetStep :: Decode PureLetStep where
  decode q = genericDecode defaultOptions q
instance prettyPrintPureLetStep :: PrettyPrint PureLetStep where
  prettyPrint' t (PureLetStep {bindings, body}) = "LetStep\n" <> intercalate (t <> "\n") (prettyPrint' (t <> "  ") <$> bindings) <> "\n" <> t <> "in\n" <> (prettyPrint' (t <> "  ") body)

derive instance genericVarBinding :: Generic VarBinding _
instance showVarBinding :: Show VarBinding where show = genericShow
instance eqVarBinding :: Eq VarBinding where eq = genericEq
instance encodeVarBinding :: Encode VarBinding where
  encode q = genericEncode defaultOptions q
instance decodeVarBinding :: Decode VarBinding where
  decode q = genericDecode defaultOptions q
instance prettyPrintVarBinding :: PrettyPrint VarBinding where
  prettyPrint' t (VarBinding name s) = name <> " = " <> prettyPrint' t s

derive instance genericOperator :: Generic Operator _
instance showOperator :: Show Operator where show = genericShow
instance eqOperator :: Eq Operator where eq = genericEq
instance encodeOperator :: Encode Operator where
  encode = genericEncode defaultOptions
instance decodeOperator :: Decode Operator where
  decode = genericDecode defaultOptions
instance prettyPrintOperator :: PrettyPrint Operator where
  prettyPrint' t (Compose _) = "Compose"
  prettyPrint' t (Equals _) = "Equals"
  prettyPrint' t (NotEquals _) = "NotEquals"
  prettyPrint' t (LessThan _) = "LessThan"
  prettyPrint' t (LessThanEqual _) = "LessThanEqual"
  prettyPrint' t (GreaterThan _) = "GreaterThan"
  prettyPrint' t (GreaterThanEqual _) = "GreaterThanEqual"
  prettyPrint' t (LogicalAnd _) = "LogicalAnd"
  prettyPrint' t (LogicalOr _) = "LogicalOr"
  prettyPrint' t (Add _) = "Add"
  prettyPrint' t (Subtract _) = "Subtract"
  prettyPrint' t (Divide _) = "Divide"
  prettyPrint' t (Multiply _) = "Multiply"
  prettyPrint' t (Filter _) = "Filter"
  prettyPrint' t (Sequence _) = "Sequence"

derive instance genericAssignment :: Generic Assignment _
instance showAssignment :: Show Assignment where show = genericShow
instance eqAssignment :: Eq Assignment where eq = genericEq
instance encodeAssignment :: Encode Assignment where
  encode q = genericEncode defaultOptions q
instance decodeAssignment :: Decode Assignment where
  decode q = genericDecode defaultOptions q
instance prettyPrintAssignment :: PrettyPrint Assignment where
  prettyPrint' t (Remove {roleExpression}) = "Remove " <> prettyPrint' t roleExpression
  prettyPrint' t (CreateRole {roleIdentifier, contextExpression}) = "CreateRole " <> roleIdentifier <> " " <> prettyPrint' t contextExpression
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
instance encodeAssignmentOperator :: Encode AssignmentOperator where
  encode = genericEncode defaultOptions
instance decodeAssignmentOperator :: Decode AssignmentOperator where
  decode = genericDecode defaultOptions
instance prettyPrintAssignmentOperator :: PrettyPrint AssignmentOperator where
  prettyPrint' _ (Set _) = "Set"
  prettyPrint' _ (AddTo _) = "AddTo"
  prettyPrint' _ (DeleteFrom _) = "DeleteFrom"

-- newtype ComputationStep = ComputationStep {functionName :: String, arguments :: List Step, computedType :: String}

derive instance genericComputationStep :: Generic ComputationStep _
instance showComputationStep :: Show ComputationStep where show s = genericShow s
instance eqComputationStep :: Eq ComputationStep where eq c1 c2 = genericEq c1 c2
instance encodeComputationStep :: Encode ComputationStep where
  encode q = genericEncode defaultOptions q
instance decodeComputationStep :: Decode ComputationStep where
  decode q = genericDecode defaultOptions q
instance prettyPrintComputationStep :: PrettyPrint ComputationStep where
  prettyPrint' t (ComputationStep{functionName, arguments, computedType}) = "Computation\n" <> intercalate ("\n" <> t) (prettyPrint' (t <> " ") <$> arguments) <> "\n" <> t <> computedType
