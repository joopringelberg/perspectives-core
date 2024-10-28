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

module Perspectives.Parsing.Arc.Expression.AST where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (intercalate) 
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Representation.QueryFunction (FunctionName) as QF 
import Perspectives.Representation.Range (Range)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')

-- | Step represents an Expression conforming to the grammar given above.
data Step = Simple SimpleStep | Binary BinaryStep | Unary UnaryStep | PureLet PureLetStep | Computation ComputationStep

data SimpleStep =
  ArcIdentifier ArcPosition String
  | ContextTypeIndividual ArcPosition String
  | RoleTypeIndividual ArcPosition String
  | Value ArcPosition Range String
  -- Filler has an optional embedding context.
  | Filler ArcPosition (Maybe String)
  -- Filled has an optional embedding context.
  | Filled ArcPosition String (Maybe String)
  | Context ArcPosition
  | Extern ArcPosition
  | IndexedName ArcPosition
  | SequenceFunction ArcPosition QF.FunctionName
  | Identity ArcPosition
  | Modelname ArcPosition
  | Variable ArcPosition String

  | TypeOfContext ArcPosition
  | RoleTypes ArcPosition
  | SpecialisesRoleType ArcPosition String
  | IsInState ArcPosition String
  | RegEx ArcPosition RegExP

  | PublicRole ArcPosition String
  | PublicContext ArcPosition String

  -- These step types are used in Perspectives.Parsing.Arc.PhaseThree for the standard variables.
  | TypeTimeOnlyContext ArcPosition String
  -- ArcPosition, ContextType, EnumeratedRoleType
  | TypeTimeOnlyEnumeratedRole ArcPosition String String
  | TypeTimeOnlyCalculatedRole ArcPosition String

data UnaryStep =
  LogicalNot ArcPosition Step
  | Exists ArcPosition Step
  | FilledBy ArcPosition Step
  | Fills ArcPosition Step
  | Available ArcPosition Step
  | DurationOperator ArcPosition Operator Step
  | ContextIndividual ArcPosition String Step
  | RoleIndividual ArcPosition String Step


newtype BinaryStep = BinaryStep {start :: ArcPosition, end :: ArcPosition, operator :: Operator, left :: Step, right :: Step, parenthesised :: Boolean}

newtype PureLetStep = PureLetStep {start :: ArcPosition, end :: ArcPosition, bindings:: Array VarBinding, body :: Step}

newtype ComputationStep = ComputationStep {functionName :: String, arguments :: Array Step, computedType :: ComputedType, start :: ArcPosition, end :: ArcPosition}

data ComputedType = ComputedRange Range | OtherType String
instance Show ComputedType where
  show (ComputedRange r) = show r
  show (OtherType s) = s

instance Eq ComputedType where
  eq (ComputedRange r1) (ComputedRange r2) = eq r1 r2
  eq (OtherType s1) (OtherType s2) = eq s1 s2
  eq _ _ = false
instance Ord ComputedType where
  compare (ComputedRange r1) (ComputedRange r2) = compare r1 r2
  compare (OtherType s1) (OtherType s2) = compare s1 s2
  compare _ _ = EQ

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
  | Union ArcPosition
  | Intersection ArcPosition
  | OrElse ArcPosition
  | BindsOp ArcPosition
  | Matches ArcPosition
  | Year ArcPosition
  | Month ArcPosition
  | Week ArcPosition
  | Day ArcPosition
  | Hour ArcPosition
  | Minute ArcPosition
  | Second ArcPosition
  | Millisecond ArcPosition

derive instance genericStep :: Generic Step _
instance showStep :: Show Step where show s = genericShow s
instance eqStep :: Eq Step where eq = genericEq
derive instance ordStep :: Ord Step

instance prettyPrintStep :: PrettyPrint Step where
  prettyPrint' t (Simple s) = prettyPrint' t s
  prettyPrint' t (Binary s) = prettyPrint' t s
  prettyPrint' t (Unary s) = prettyPrint' t s
  prettyPrint' t (PureLet s) = prettyPrint' t s
  prettyPrint' t (Computation s) = prettyPrint' t s

derive instance genericSimpleStep :: Generic SimpleStep _
instance showSimpleStep :: Show SimpleStep where show = genericShow
instance eqSimpleStep :: Eq SimpleStep where eq = genericEq
derive instance ordSimpleStep :: Ord SimpleStep
instance prettyPrintSimpleStep :: PrettyPrint SimpleStep where
  prettyPrint' t (ArcIdentifier _ s) = "ArcIdentifier " <> s
  
  prettyPrint' t (ContextTypeIndividual _ s) = "ArcIdentifier " <> s
  prettyPrint' t (RoleTypeIndividual _ s) = "ArcIdentifier " <> s
  prettyPrint' t (Filler _ embeddingContext) = "Filler " <> (case embeddingContext of
    Nothing -> ""
    Just ec -> ec)
  prettyPrint' t (Filled _ s embeddingContext) = "Filled " <> s <> (case embeddingContext of
    Nothing -> ""
    Just ec -> " " <> ec)
  prettyPrint' t (Context _) = "Context"
  prettyPrint' t (Extern _) = "Extern"
  prettyPrint' t (IndexedName _) = "IndexedName"
  prettyPrint' t (SequenceFunction _ s) = "SequenceFunction " <> show s
  prettyPrint' t (Identity _ ) = "Identity"
  prettyPrint' t (Modelname _) = "Modelname"
  prettyPrint' t (Variable _ s) = "Variable " <> s
  prettyPrint' t (Value _ range s) = "Value " <> show range <> " " <> s
  prettyPrint' t (RoleTypes _) = "RoleTypes"
  prettyPrint' t (TypeOfContext _) = "TypeOfContext"
  prettyPrint' t (SpecialisesRoleType _ s) = "SpecialisesRoleType " <> s
  prettyPrint' t (IsInState _ s) = "IsInState " <> s
  prettyPrint' t (TypeTimeOnlyContext _ s) = "TypeTimeOnlyContext " <> s
  prettyPrint' t (TypeTimeOnlyEnumeratedRole _ r c) = "TypeTimeOnlyEnumeratedRole " <> r <> " " <> c
  prettyPrint' t (TypeTimeOnlyCalculatedRole _ s) = "TypeTimeOnlyCalculatedRole " <> s
  prettyPrint' t (RegEx _ r) = show r
  prettyPrint' t (PublicRole _ u) = "PublicRole " <> u
  prettyPrint' t (PublicContext _ u) = "PublicContext " <> u

derive instance genericBinaryStep :: Generic BinaryStep _
instance showBinaryStep :: Show BinaryStep where show = genericShow
instance eqBinaryStep :: Eq BinaryStep where eq s1 s2 = genericEq s1 s2
derive instance ordBinaryStap :: Ord BinaryStep
instance prettyPrintBinaryStep :: PrettyPrint BinaryStep where
  prettyPrint' t (BinaryStep {operator, left, right}) = prettyPrint' t operator <> "\n" <> t <> (prettyPrint' (t <> "  ") left) <> "\n" <> t <> (prettyPrint' (t <> "  ") right)

derive instance genericUnaryStep :: Generic UnaryStep _
instance showUnaryStep :: Show UnaryStep where show = genericShow
instance eqUnaryStep :: Eq UnaryStep where eq u1 u2 = genericEq u1 u2
derive instance ordUnaryStap :: Ord UnaryStep
instance prettyPrintUnaryStep :: PrettyPrint UnaryStep where
  prettyPrint' t (LogicalNot _ s) = "LogicalNot " <> prettyPrint' t s
  prettyPrint' t (Exists _ s) = "Exists " <> prettyPrint' t s
  prettyPrint' t (FilledBy _ s) = "FilledBy " <> prettyPrint' t s
  prettyPrint' t (Fills _ s) = "Fills " <> prettyPrint' t s
  prettyPrint' t (Available _ s) = "Available " <> prettyPrint' t s
  prettyPrint' t (DurationOperator _ op s) = show op <> " " <> prettyPrint' t s
  prettyPrint' t (ContextIndividual _ tp s) = "contextindividual (" <> tp <> ") " <> prettyPrint' t s
  prettyPrint' t (RoleIndividual _ tp s) = "roleindividual (" <> tp <> ") " <> prettyPrint' t s

derive instance genericPureLetStep :: Generic PureLetStep _
instance showPureLetStep :: Show PureLetStep where show = genericShow
instance eqPureLetStep :: Eq PureLetStep where eq u1 u2 = genericEq u1 u2
derive instance ordPureLetStep :: Ord PureLetStep
instance prettyPrintPureLetStep :: PrettyPrint PureLetStep where
  prettyPrint' t (PureLetStep {bindings, body}) = "LetStep\n" <> intercalate (t <> "\n") (prettyPrint' (t <> "  ") <$> bindings) <> "\n" <> t <> "in\n" <> (prettyPrint' (t <> "  ") body)

derive instance genericVarBinding :: Generic VarBinding _
instance showVarBinding :: Show VarBinding where show = genericShow
instance eqVarBinding :: Eq VarBinding where eq = genericEq
derive instance ordVarBinding :: Ord VarBinding
instance prettyPrintVarBinding :: PrettyPrint VarBinding where
  prettyPrint' t (VarBinding name s) = name <> " = " <> prettyPrint' t s

derive instance genericOperator :: Generic Operator _
instance showOperator :: Show Operator where show = genericShow
instance eqOperator :: Eq Operator where eq = genericEq
derive instance ordOperator :: Ord Operator
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
  prettyPrint' t (Union _) = "Union"
  prettyPrint' t (Intersection _) = "Intersection"
  prettyPrint' t (OrElse _) = "OrElse"
  prettyPrint' t (BindsOp _) = "FilledBy"
  prettyPrint' t (Matches _) = "Matches"
  prettyPrint' t (Year _) = "Year"
  prettyPrint' t (Month _) = "Month"
  prettyPrint' t (Week _) = "Week"
  prettyPrint' t (Day _) = "Day"
  prettyPrint' t (Hour _) = "Hour"
  prettyPrint' t (Minute _) = "Minute"
  prettyPrint' t (Second _) = "Second"
  prettyPrint' t (Millisecond _) = "Millisecond"
  

derive instance genericComputationStep :: Generic ComputationStep _
instance showComputationStep :: Show ComputationStep where show s = genericShow s
instance eqComputationStep :: Eq ComputationStep where eq c1 c2 = genericEq c1 c2
derive instance ordComputationStep :: Ord ComputationStep
instance prettyPrintComputationStep :: PrettyPrint ComputationStep where
  prettyPrint' t (ComputationStep{functionName, arguments, computedType}) = "Computation\n" <> intercalate ("\n" <> t) (prettyPrint' (t <> " ") <$> arguments) <> "\n" <> t <> show computedType
