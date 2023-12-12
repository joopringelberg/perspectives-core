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

module Perspectives.Representation.Assignment where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Perspectives.Instances.Environment (Environment)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Eq, class Show)

type FunctionName = String

data AssignmentStatement
  = SetRol EnumeratedRoleType QueryFunctionDescription
  | AddToRol EnumeratedRoleType QueryFunctionDescription
  | RemoveFromRol EnumeratedRoleType QueryFunctionDescription
  | SetProperty EnumeratedPropertyType QueryFunctionDescription
  | AddToProperty EnumeratedPropertyType QueryFunctionDescription
  | RemoveFromProperty EnumeratedPropertyType QueryFunctionDescription
  | EffectFullFunction FunctionName (Array String)
  | DeleteRol EnumeratedRoleType
  | DeleteProperty EnumeratedPropertyType
  -- TODO: full delete.

newtype LetWithAssignment = LetWithAssignment {variableBindings :: Environment QueryFunctionDescription, assignments:: Array AssignmentStatement}

derive instance genericRepAssignmentStatement :: Generic AssignmentStatement _

instance showAssignmentStatement :: Show AssignmentStatement where
  show x = genericShow x

instance eqAssignmentStatement :: Eq AssignmentStatement where
  eq x = genericEq x

type ConstructorRep = {tag :: String, dat :: Array String}

derive instance genericRepLetWithAssignment :: Generic LetWithAssignment _
instance showLetWithAssignment :: Show LetWithAssignment where
  show x = genericShow x
instance eqLetWithAssignment :: Eq LetWithAssignment where
  eq x = genericEq x
