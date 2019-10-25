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

module Perspectives.Representation.SideEffect where

-- | The SideEffect data structure represents two phases in the transformation of the surface syntax for
-- | [Assignments](Perspectives.Parsing.Arc.Expression.AST.html#t:Assignment):
-- |   * the parse tree, in terms of the Assignment data structure
-- |   * the AssignmentStatement (Perspectives.Representation.Assignment.html#t:AssignmentStatement), the format that can be compiled to an executable function that actually changes Perspectives state.
-- | Representing the two phases cannot be avoided because we can only compile the parse tree to an AssignmentStatement
-- | after the model file has been otherwise completely transformed. This is because we check, while compiling the Assignment
-- | AST, whether the range of a step complies with the domain of the following step. Moreover, only then can we qualify names used in the expressions.

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Kishimen (genericSumToVariant)
import Perspectives.Parsing.Arc.Expression.AST (Assignment)
import Perspectives.Representation.Assignment (AssignmentStatement)
import Prelude (class Eq, class Show, (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data SideEffect = A (Array Assignment) | AS (Array AssignmentStatement)

derive instance genericRepSideEffect :: Generic SideEffect _
instance writeForeignSideEffect :: WriteForeign SideEffect where
  writeImpl = writeImpl <<< genericSumToVariant
instance readForeignSideEffect :: ReadForeign SideEffect where
  readImpl f = readImpl f
instance showSideEffect :: Show SideEffect where
  show = genericShow
instance eqSideEffect :: Eq SideEffect where
  eq = genericEq
