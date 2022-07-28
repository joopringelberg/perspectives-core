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

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Perspectives.Identifiers (LocalName)
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType)
import Prelude (class Eq, class Ord, class Show, map)
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
  | FilledByF
  | FillsF
  | FilterF
  | ComposeF
  | ComposeSequenceF
  | UnionF
  | IntersectionF
  | CreateContextF      -- TODO
  | CreateRoleF         -- TODO
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
    show FilledByF = "filledBy"
    show FillsF = "fills"
    show FilterF = "filter"
    show ComposeF = "compose"
    show ComposeSequenceF = "composeSequence"
    show UnionF = "either"
    show IntersectionF = "both"
    show CreateContextF = "createContext"
    show CreateRoleF = "createRole"
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
  writeImpl a = writeImpl( genericSumToVariant a )

instance readForeignFunctionName :: ReadForeign FunctionName where
  readImpl f = map variantToGenericSum (readImpl f)

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
  FilledByF -> True
  FillsF -> True
  FilterF -> Unknown
  ComposeF -> Unknown
  ComposeSequenceF -> True
  UnionF -> False
  IntersectionF -> False
  CreateContextF -> True
  CreateRoleF -> True
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

-- | False if the function can return an empty result.
isMandatoryFunction :: FunctionName -> ThreeValuedLogic
isMandatoryFunction fn = case fn of
  ContextF -> True
  BindingF -> False
  ExternalRoleF -> True
  IdentityF -> True
  ModelNameF -> True
  SequenceF -> True
  NotF -> True
  ExistsF -> True
  FilledByF -> True
  FillsF -> True
  FilterF -> False
  ComposeF -> Unknown
  ComposeSequenceF -> Unknown
  UnionF -> False
  IntersectionF -> False
  CreateContextF -> True
  CreateRoleF -> True
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
  RoleTypesF -> True
  SpecialisesRoleTypeF -> True

---------------------------------------------------------------------------------------
---- QUERYFUNCTION
---------------------------------------------------------------------------------------
data QueryFunction
  = DataTypeGetter FunctionName
  | DataTypeGetterWithParameter FunctionName String
  | RegExMatch RegExP
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
  | BindResultFromCreatingAssignment VariableName
  | AssignmentOperator FunctionName
  | WithFrame

  | TypeGetter FunctionName

  | UnaryCombinator FunctionName
  -- | NaryCombinator FunctionName (Array QueryFunction)
  | BinaryCombinator FunctionName
  | Constant Range String
  | RoleIndividual RoleInstance
  | ContextIndividual ContextInstance

  | CreateContext ContextType (Maybe LocalName) RoleType
  | CreateContext_ ContextType (Maybe LocalName)
  | CreateRole EnumeratedRoleType
  | Bind EnumeratedRoleType
  | Bind_
  | Unbind (Maybe EnumeratedRoleType)
  | Unbind_
  | DeleteRole EnumeratedRoleType
  | DeleteContext RoleType
  | DeleteProperty EnumeratedPropertyType
  | Move
  | RemoveRole
  | RemoveContext
  | AddPropertyValue EnumeratedPropertyType
  | RemovePropertyValue EnumeratedPropertyType
  | SetPropertyValue EnumeratedPropertyType
  | ExternalEffectFullFunction String
  | ExternalDestructiveFunction String
  | ForeignEffectFullFunction String

  | TypeTimeOnlyContextF String
  | TypeTimeOnlyEnumeratedRoleF String
  | TypeTimeOnlyCalculatedRoleF String

  | GetRoleBindersF EnumeratedRoleType ContextType


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
  writeImpl f = writeImpl( genericSumToVariant f)
  -- writeImpl a = unsafeToForeign (writeJSON (genericSumToVariant a))

instance readForeignQueryFunction :: ReadForeign QueryFunction where
  readImpl f = map variantToGenericSum (readImpl f)

instance ordQueryFunction :: Ord QueryFunction where compare = genericCompare
