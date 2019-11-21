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
import Data.Maybe (Maybe)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Foreign.Class (class Encode)
import Foreign.Generic (defaultOptions, genericEncode)
import Kishimen (genericSumToVariant)
import Perspectives.Representation.EnumeratedProperty (Range)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType)
import Prelude (class Eq, class Show)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

type VariableName = String

data FunctionName =
  ContextF
  | BindingF
  | ExternalRoleF
  | IdentityF
  | SequenceF
  | NotF
  | ExistsF
  | FilterF
  | ComposeF
  | DisjunctionF
  | ConjunctionF
  | CreateContextF
  | CreateRoleF
  | GetRoleBindersF
  | EqualsF
  | NotEqualsF
  | LessThanF
  | LessThanEqualF
  | GreaterThanF
  | GreaterThanEqualF
  | AddF
  | SubtractF
  | DivideF
  | MultiplyF
  | AndF
  | OrF
  | CountF
  | MinimumF
  | MaximumF

isFunctionalFunction :: FunctionName -> ThreeValuedLogic
isFunctionalFunction fn = case fn of
  ContextF -> True
  BindingF -> True
  ExternalRoleF -> True
  IdentityF -> Unknown
  SequenceF -> Unknown
  NotF -> True
  ExistsF -> True
  FilterF -> Unknown
  ComposeF -> Unknown
  DisjunctionF -> Unknown
  ConjunctionF -> False
  CreateContextF -> True
  CreateRoleF -> True
  GetRoleBindersF -> False
  EqualsF -> True
  NotEqualsF -> True
  LessThanF -> True
  LessThanEqualF -> True
  GreaterThanF -> True
  GreaterThanEqualF -> True
  AddF -> True
  SubtractF -> True
  DivideF -> True
  MultiplyF -> True
  AndF -> True
  OrF -> True
  CountF -> True
  MinimumF -> True
  MaximumF -> True


data QueryFunction
  = DataTypeGetter FunctionName
  | DataTypeGetterWithParameter FunctionName String
  | PropertyGetter PropertyType
  | RolGetter RoleType
  -- 'Computed' is not 'calculated': call a Purescript function here.
  | ComputedRoleGetter String
  | ComputedPropertyGetter String
  | VariableLookup VariableName
  | BindVariable VariableName
  | AssignmentOperator FunctionName
  | Identity

  | UnaryCombinator FunctionName
  -- | NaryCombinator FunctionName (Array QueryFunction)
  | BinaryCombinator FunctionName
  | Constant Range String

  | CreateRole EnumeratedRoleType
	| CreateContext' ContextType
	| Bind EnumeratedRoleType
  | Bind_
	| Unbind (Maybe EnumeratedRoleType)
  | Unbind_
	| DeleteRole
	| DeleteProperty EnumeratedPropertyType
  | Move
  | Remove
  | AddPropertyValue
  | RemovePropertyValue
  | SetPropertyValue
  -- | EffectFullFunction

derive instance genericRepQueryFunction :: Generic QueryFunction _

instance showQueryFunction :: Show QueryFunction where
  show x = genericShow x

instance eqQueryFunction :: Eq QueryFunction where
  eq x = genericEq x

-- instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  -- writeImpl q = unsafeToForeign (writeJSON ((genericSumToVariant q)))
  -- writeImpl q = genericEncode defaultOptions q

instance readForeignQueryFunction :: ReadForeign QueryFunction where
  readImpl q = readJSON' (unsafeFromForeign q)

derive instance genericFunctionName :: Generic FunctionName _
instance writeForeignFunctionName :: WriteForeign FunctionName where
  writeImpl q = unsafeToForeign (writeJSON (genericSumToVariant q))
instance readForeignFunctionName :: ReadForeign FunctionName where
  readImpl q = readJSON' (unsafeFromForeign q)
instance encodeFunctionName :: Encode FunctionName where
  encode = genericEncode defaultOptions

-- | The show function produces the very same string that the parser parses.
instance showFunctionName :: Show FunctionName where
    show ContextF = "context"
    show BindingF = "binding"
    show ExternalRoleF = "external" -- TODO klopt dit met de parser?
    show IdentityF = "identity"
    show SequenceF = "sequence"
    show NotF = "not"
    show ExistsF = "exists"
    show FilterF = "filter"
    show ComposeF = "compose"
    show DisjunctionF = "disjunction"
    show ConjunctionF = "conjunction"
    show CreateContextF = "createContext"
    show CreateRoleF = "createRole"
    show GetRoleBindersF = "binder" -- TODO en dit?
    show EqualsF = "="
    show NotEqualsF = "/="
    show LessThanF = "<"
    show LessThanEqualF = "<="
    show GreaterThanF = ">"
    show GreaterThanEqualF = ">="
    show AddF = "+"
    show SubtractF = "-"
    show DivideF = "/"
    show MultiplyF = "*"
    show AndF = "and"
    show OrF = "or"
    show CountF = "count"
    show MinimumF = "minimum"
    show MaximumF = "maximum"

instance eqFunctionName :: Eq FunctionName where eq = genericEq
