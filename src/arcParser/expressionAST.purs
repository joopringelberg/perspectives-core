module Perspectives.Parsing.Arc.Expression.AST where

-- | An Abstract Syntax Tree data model for Perspectives expressions. The expression grammar is below.
-- |
-- | step = simpleStep | unaryStep | compoundStep
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
-- | unaryStep = 'not' step | 'create' ArcIdentifier | 'exists' ArcIdentifier
-- |
-- | compoundStep = (filter step 'with' step) | (step operator step) | filter step 'with' step | step operator step
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

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.EnumeratedProperty (Range)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

-- | Step represents an Expression conforming to the grammar given above.
data Step = Simple SimpleStep | Binary BinaryStep | Unary UnaryStep

data SimpleStep =
  ArcIdentifier ArcPosition String
  | Value ArcPosition Range String
  | Binding ArcPosition
  | Binder ArcPosition String
  | Context ArcPosition
  | Extern ArcPosition

data UnaryStep =
  LogicalNot ArcPosition Step
  | Create ArcPosition String
  | Exists ArcPosition String

newtype BinaryStep = BinaryStep {start :: ArcPosition, end :: ArcPosition, operator :: Operator, left :: Step, right :: Step}

data Operator =
  Compose ArcPosition
  | Equals ArcPosition
  | NotEquals ArcPosition
  | LessThen ArcPosition
  | LessThenEqual ArcPosition
  | GreaterThen ArcPosition
  | GreaterThenEqual ArcPosition
  | LogicalAnd ArcPosition
  | LogicalOr ArcPosition
  | Add ArcPosition
  | Subtract ArcPosition
  | Divide ArcPosition
  | Multiply ArcPosition
  | Filter ArcPosition

newtype Assignment = Assignment {start :: ArcPosition, end :: ArcPosition, lhs :: String, operator :: AssignmentOperator, value :: Maybe Step}

data AssignmentOperator =
  Set ArcPosition
  | AddTo ArcPosition
  | DeleteFrom ArcPosition
  | Delete ArcPosition

derive instance genericStep :: Generic Step _
instance showStep :: Show Step where show s = genericShow s
instance eqStep :: Eq Step where eq = genericEq
instance writeForeignStep :: WriteForeign Step where
  writeImpl q = unsafeToForeign (writeJSON q)
instance readForeignStep :: ReadForeign Step where
  readImpl q = readJSON' (unsafeFromForeign q)

derive instance genericSimpleStep :: Generic SimpleStep _
instance showSimpleStep :: Show SimpleStep where show = genericShow
instance eqSimpleStep :: Eq SimpleStep where eq = genericEq

derive instance genericBinaryStep :: Generic BinaryStep _
instance showBinaryStep :: Show BinaryStep where show = genericShow
instance eqBinaryStep :: Eq BinaryStep where eq s1 s2 = genericEq s1 s2

derive instance genericUnaryStep :: Generic UnaryStep _
instance showUnaryStep :: Show UnaryStep where show = genericShow
instance eqUnaryStep :: Eq UnaryStep where eq u1 u2 = genericEq u1 u2

derive instance genericOperator :: Generic Operator _
instance showOperator :: Show Operator where show = genericShow
instance eqOperator :: Eq Operator where eq = genericEq

derive instance genericAssignment :: Generic Assignment _
instance showAssignment :: Show Assignment where show = genericShow

derive instance genericAssignmentOperator :: Generic AssignmentOperator _
instance showAssignmentOperator :: Show AssignmentOperator where show = genericShow
