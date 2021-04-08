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

module Perspectives.Representation.QueryFunction where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType)
import Prelude (class Eq, class Ord, class Show, map, (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type VariableName = String

---------------------------------------------------------------------------------------
---- FUNCTIONNAME
---------------------------------------------------------------------------------------

data FunctionName =
  ContextF
  | BindingF
  | ExternalRoleF
  | IdentityF           -- TODO IN QUERYCOMPILER
  | ModelNameF
  | SequenceF
  | NotF
  | ExistsF
  | BindsF
  | BoundByF
  | FilterF
  | ComposeF
  | UnionF
  | IntersectionF
  | CreateContextF      -- TODO
  | CreateRoleF         -- TODO
  | GetRoleBindersF
  | GetRoleInstancesForContextFromDatabaseF
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
  | CountF              -- TODO
  | MinimumF            -- TODO
  | MaximumF            -- TODO
  | AvailableF           -- TODO

  | TypeOfContextF
  | RoleTypesF
  | SpecialisesRoleTypeF


derive instance genericFunctionName :: Generic FunctionName _
instance encodeFunctionName :: Encode FunctionName where
  encode = genericEncode defaultOptions
instance decodeFunctionName :: Decode FunctionName where
  decode = genericDecode defaultOptions

instance eqFunctionName :: Eq FunctionName where eq = genericEq
derive instance ordFunctionName :: Ord FunctionName

-- | The show function produces the very same string that the parser parses.
instance showFunctionName :: Show FunctionName where
    show ContextF = "context"
    show BindingF = "binding"
    show ExternalRoleF = "external" -- TODO klopt dit met de parser?
    show IdentityF = "identity"
    show ModelNameF = "Namespace"
    show SequenceF = "sequence"
    show NotF = "not"
    show ExistsF = "exists"
    show BindsF = "binds"
    show BoundByF = "boundBy"
    show FilterF = "filter"
    show ComposeF = "compose"
    show UnionF = "either"
    show IntersectionF = "both"
    show CreateContextF = "createContext"
    show CreateRoleF = "createRole"
    show GetRoleBindersF = "binder" -- TODO en dit?
    show GetRoleInstancesForContextFromDatabaseF = "GetRoleInstancesForContextFromDatabaseF"
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
    show AvailableF = "available"
    show TypeOfContextF = "TypeOfContextF"
    show RoleTypesF = "RoleTypesF"
    show SpecialisesRoleTypeF = "SpecialisesRoleTypeF"

instance writeForeignFunctionName :: WriteForeign FunctionName where
  writeImpl = writeImpl <<< genericSumToVariant

instance readForeignFunctionName :: ReadForeign FunctionName where
  readImpl = map variantToGenericSum <<< readImpl

isFunctionalFunction :: FunctionName -> ThreeValuedLogic
isFunctionalFunction fn = case fn of
  ContextF -> True
  BindingF -> True
  ExternalRoleF -> True
  IdentityF -> Unknown
  ModelNameF -> True
  SequenceF -> Unknown
  NotF -> True
  ExistsF -> True
  BindsF -> True
  BoundByF -> True
  FilterF -> Unknown
  ComposeF -> Unknown
  UnionF -> False
  IntersectionF -> False
  CreateContextF -> True
  CreateRoleF -> True
  GetRoleBindersF -> False
  GetRoleInstancesForContextFromDatabaseF -> False
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
  AvailableF -> True
  TypeOfContextF -> True
  RoleTypesF -> False
  SpecialisesRoleTypeF -> True

---------------------------------------------------------------------------------------
---- QUERYFUNCTION
---------------------------------------------------------------------------------------
data QueryFunction
  = DataTypeGetter FunctionName
  | DataTypeGetterWithParameter FunctionName String
  | PropertyGetter PropertyType
  | Value2Role PropertyType
  | RolGetter RoleType
  -- 'External': call a Purescript function here.
  | ExternalCoreRoleGetter String
  | ExternalCorePropertyGetter String
  | ExternalCoreContextGetter String
  -- 'Foreign': call a Javascript function here.
  | ForeignRoleGetter String
  | ForeignPropertyGetter String
  | VariableLookup VariableName
  | BindVariable VariableName
  | AssignmentOperator FunctionName
  | WithFrame

  | TypeGetter FunctionName

  | UnaryCombinator FunctionName
  -- | NaryCombinator FunctionName (Array QueryFunction)
  | BinaryCombinator FunctionName
  | Constant Range String
  | RoleIndividual RoleInstance
  | ContextIndividual ContextInstance

  | CreateContext ContextType EnumeratedRoleType
  | CreateContext_ ContextType
  | CreateRole EnumeratedRoleType
	| Bind EnumeratedRoleType
  | Bind_
	| Unbind (Maybe EnumeratedRoleType)
  | Unbind_
	| DeleteRole EnumeratedRoleType
	| DeleteProperty EnumeratedPropertyType
  | Move
  | Remove
  | AddPropertyValue EnumeratedPropertyType
  | RemovePropertyValue EnumeratedPropertyType
  | SetPropertyValue EnumeratedPropertyType
  | ExternalEffectFullFunction String
  | ForeignEffectFullFunction String

derive instance genericRepQueryFunction :: Generic QueryFunction _

instance showQueryFunction :: Show QueryFunction where
  show x = genericShow x

instance eqQueryFunction :: Eq QueryFunction where
  eq x = genericEq x

instance encodeQueryFunction :: Encode QueryFunction where
  encode q = genericEncode defaultOptions q
  -- encode = writeImpl

instance decodeQueryFunction :: Decode QueryFunction where
  decode = genericDecode defaultOptions
  -- decode = readImpl

instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  writeImpl = writeImpl <<< genericSumToVariant

instance readForeightQueryFunction :: ReadForeign QueryFunction where
  readImpl = map variantToGenericSum <<< readImpl
