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

module Perspectives.Representation.QueryFunction where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Representation.EnumeratedProperty (Range)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType)
import Prelude (class Eq, class Show)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

type FunctionName = String
type VariableName = String

data QueryFunction
  = DataTypeGetter FunctionName
  | DataTypeGetterWithParameter FunctionName String
  | PropertyGetter PropertyType
  | RolGetter RoleType
  -- 'Computed' is not 'calculated': call a Purescript function here.
  | ComputedRoleGetter FunctionName
  | ComputedPropertyGetter FunctionName
  | VariableLookup VariableName
  | BindVariable VariableName

  | UnaryCombinator FunctionName
  -- | NaryCombinator FunctionName (Array QueryFunction)
  | BinaryCombinator FunctionName
  | Constant Range String

derive instance genericRepQueryFunction :: Generic QueryFunction _

instance showQueryFunction :: Show QueryFunction where
  show x = genericShow x

instance eqQueryFunction :: Eq QueryFunction where
  eq x = genericEq x

type ConstructorRep = {tag :: String, dat :: Array String}

instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  writeImpl q = unsafeToForeign (writeJSON q)

instance readForeignQueryFunction :: ReadForeign QueryFunction where
  readImpl q = readJSON' (unsafeFromForeign q)
