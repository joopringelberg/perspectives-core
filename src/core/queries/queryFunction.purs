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

import Control.Monad.Error.Class (throwError)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Foreign (ForeignError(..))


import Perspectives.Parsing.Arc.Expression.RegExP (RegExP)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType, RoleType)
import Prelude (class Eq, class Ord, class Show, Ordering(..), bind, compare, eq, flip, pure, show, ($), (&&), (<$>), (<*>), (<>), (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, read', readJSON', writeImpl, writeJSON)

type VariableName = String
---------------------------------------------------------------------------------------
---- FUNCTIONNAME
---------------------------------------------------------------------------------------

data FunctionName =
  ContextF
  | FillerF
  | DirectFillerF
  | ExternalRoleF
  | IndexedContextName
  | IndexedRoleName
  | IdentityF           -- TODO IN QUERYCOMPILER
  | ModelNameF
  | SequenceF
  | NotF
  | ExistsF
  | FilledByF
  | FillsF
  | ComposeF
  | ComposeSequenceF
  | UnionF
  | IntersectionF
  | OrElseF
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
  | CountF
  | MinimumF
  | MaximumF
  | AvailableF
  | ContextIndividualF
  | RoleIndividualF

  | TypeOfContextF
  | RoleTypesF
  | SpecialisesRoleTypeF
  | IsInStateF

  | FirstF

instance eqFunctionName :: Eq FunctionName where 
  eq ContextF ContextF = true
  eq FillerF FillerF = true
  eq DirectFillerF DirectFillerF = true
  eq ExternalRoleF ExternalRoleF = true
  eq IndexedContextName IndexedContextName = true
  eq IndexedRoleName IndexedRoleName = true
  eq IdentityF IdentityF = true           -- TODO IN QUERYCOMPILER
  eq ModelNameF ModelNameF = true
  eq SequenceF SequenceF = true
  eq NotF NotF = true
  eq ExistsF ExistsF = true
  eq FilledByF FilledByF = true
  eq FillsF FillsF = true
  eq ComposeF ComposeF = true
  eq ComposeSequenceF ComposeSequenceF = true
  eq UnionF UnionF = true
  eq IntersectionF IntersectionF = true
  eq OrElseF OrElseF = true
  eq GetRoleInstancesForContextFromDatabaseF GetRoleInstancesForContextFromDatabaseF = true
  eq EqualsF EqualsF = true
  eq NotEqualsF NotEqualsF = true
  eq LessThanF LessThanF = true
  eq LessThanEqualF LessThanEqualF = true
  eq GreaterThanF GreaterThanF = true
  eq GreaterThanEqualF GreaterThanEqualF = true
  eq AddF AddF = true
  eq SubtractF SubtractF = true
  eq DivideF DivideF = true
  eq MultiplyF MultiplyF = true
  eq AndF AndF = true
  eq OrF OrF = true
  eq CountF CountF = true
  eq MinimumF MinimumF = true
  eq MaximumF MaximumF = true
  eq AvailableF AvailableF = true
  eq ContextIndividualF ContextIndividualF = true
  eq RoleIndividualF RoleIndividualF = true

  eq TypeOfContextF TypeOfContextF = true
  eq RoleTypesF RoleTypesF = true
  eq SpecialisesRoleTypeF SpecialisesRoleTypeF = true
  eq IsInStateF IsInStateF = true

  eq FirstF FirstF = true
  eq _ _ = false

instance Ord FunctionName where
  compare c1 c2 = compare (show c1) (show c2)

-- | The show function produces the very same string that the parser parses.
instance showFunctionName :: Show FunctionName where
    show ContextF = "context"
    show FillerF = "binding"
    show DirectFillerF = "directFiller"
    show ExternalRoleF = "external" -- TODO klopt dit met de parser?
    show IndexedContextName = "indexedContext"
    show IndexedRoleName = "indexedRole"
    show IdentityF = "identity"
    show ModelNameF = "Namespace"
    show SequenceF = "sequence"
    show NotF = "not"
    show ExistsF = "exists"
    show FilledByF = "filledBy"
    show FillsF = "fills"
    show ComposeF = "compose"
    show ComposeSequenceF = "composeSequence"
    show UnionF = "union"
    show IntersectionF = "intersection"
    show OrElseF = "otherwise"
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
    show RoleIndividualF = "roleindividualf"
    show ContextIndividualF = "contextindividualf"
    show TypeOfContextF = "TypeOfContextF"
    show RoleTypesF = "RoleTypesF"
    show SpecialisesRoleTypeF = "SpecialisesRoleTypeF"
    show IsInStateF = "IsInStateF"
    show FirstF = "first"

instance writeForeignFunctionName :: WriteForeign FunctionName where
  writeImpl a = writeImpl (show a)

instance readForeignFunctionName :: ReadForeign FunctionName where
  readImpl f = do 
    x <- read' f
    case x of 
      "context" -> pure ContextF
      "binding" -> pure FillerF
      "directFiller" -> pure DirectFillerF
      "external" -> pure ExternalRoleF -- TODO klopt dit met de parser?
      "indexedContext" -> pure IndexedContextName
      "indexedRole" -> pure IndexedRoleName
      "identity" -> pure IdentityF
      "Namespace" -> pure ModelNameF
      "sequence" -> pure SequenceF
      "not" -> pure NotF
      "exists" -> pure ExistsF
      "filledBy" -> pure FilledByF
      "fills" -> pure FillsF
      "compose" -> pure ComposeF
      "composeSequence" -> pure ComposeSequenceF
      "union" -> pure UnionF
      "intersection" -> pure IntersectionF
      "otherwise" -> pure OrElseF
      "GetRoleInstancesForContextFromDatabaseF" -> pure GetRoleInstancesForContextFromDatabaseF
      "=" -> pure EqualsF
      "/=" -> pure NotEqualsF
      "<" -> pure LessThanF
      "<=" -> pure LessThanEqualF
      ">" -> pure GreaterThanF
      ">=" -> pure GreaterThanEqualF
      "+" -> pure AddF
      "-" -> pure SubtractF
      "/" -> pure DivideF
      "*" -> pure MultiplyF
      "and" -> pure AndF
      "or" -> pure OrF
      "count" -> pure CountF
      "minimum" -> pure MinimumF
      "maximum" -> pure MaximumF
      "available" -> pure AvailableF
      "roleindividualf" -> pure RoleIndividualF
      "contextindividualf" -> pure ContextIndividualF
      "TypeOfContextF" -> pure TypeOfContextF
      "RoleTypesF" -> pure RoleTypesF
      "SpecialisesRoleTypeF" -> pure SpecialisesRoleTypeF
      "IsInStateF" -> pure IsInStateF
      "first" -> pure FirstF
      f' -> throwError $ singleton (ForeignError $ "readForeign FunctionName: Unknown FunctionName: " <> f')

isFunctionalFunction :: FunctionName -> ThreeValuedLogic
isFunctionalFunction fn = case fn of
  ContextF -> True
  FillerF -> True
  DirectFillerF -> True
  ExternalRoleF -> True
  IndexedRoleName -> True
  IndexedContextName -> True
  IdentityF -> Unknown
  ModelNameF -> True
  SequenceF -> Unknown
  NotF -> True
  ExistsF -> True
  FilledByF -> True
  FillsF -> True
  ComposeF -> Unknown
  ComposeSequenceF -> True
  UnionF -> False
  IntersectionF -> False
  OrElseF -> False
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
  RoleIndividualF -> True
  ContextIndividualF -> True
  TypeOfContextF -> True
  RoleTypesF -> False
  SpecialisesRoleTypeF -> True
  IsInStateF -> True
  FirstF -> True

-- | False if the function can return an empty result.
isMandatoryFunction :: FunctionName -> ThreeValuedLogic
isMandatoryFunction fn = case fn of
  ContextF -> True
  FillerF -> False
  DirectFillerF -> False
  ExternalRoleF -> True
  IndexedRoleName -> False
  IndexedContextName -> False
  IdentityF -> True
  ModelNameF -> True
  SequenceF -> True
  NotF -> True
  ExistsF -> True
  FilledByF -> True
  FillsF -> True
  ComposeF -> Unknown
  ComposeSequenceF -> Unknown
  UnionF -> False
  IntersectionF -> False
  OrElseF -> False
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
  ContextIndividualF -> False
  RoleIndividualF -> False
  TypeOfContextF -> True
  RoleTypesF -> True
  SpecialisesRoleTypeF -> True
  IsInStateF -> False
  FirstF -> False

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

  | FilterF

  | TypeGetter FunctionName
  | RoleTypeConstant RoleType
  | ContextTypeConstant ContextType

  | UnaryCombinator FunctionName
  -- | NaryCombinator FunctionName (Array QueryFunction)
  | BinaryCombinator FunctionName
  | Constant Range String
  | RoleIndividual RoleInstance
  | ContextIndividual ContextInstance

  | PublicContext ContextInstance
  | PublicRole RoleInstance

  | CreateContext ContextType RoleType
  | CreateRootContext ContextType
  | CreateContext_ ContextType
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
  -- CreateFileF mimmeType property
  | CreateFileF String EnumeratedPropertyType
  | ExternalEffectFullFunction String
  | ExternalDestructiveFunction String
  | ForeignEffectFullFunction String

  | TypeTimeOnlyContextF String
  | TypeTimeOnlyEnumeratedRoleF String
  | TypeTimeOnlyCalculatedRoleF String

  | FilledF EnumeratedRoleType ContextType

instance showQueryFunction :: Show QueryFunction where
  show (DataTypeGetter functionName) = "DataTypeGetter " <> show functionName
  show (DataTypeGetterWithParameter functionName string) = "DataTypeGetterWithParameter " <> show functionName <> " " <> show string
  show (RegExMatch regExP) = "RegExMatch " <> show regExP
  show (PropertyGetter propertyType) = "PropertyGetter " <> show propertyType
  show (Value2Role propertyType) = "Value2Role " <> show propertyType
  show (RolGetter roleType) = "RolGetter " <> show roleType
  -- 'External': call a Purescript function here.
  show (ExternalCoreRoleGetter string) = "ExternalCoreRoleGetter " <> show string
  show (ExternalCorePropertyGetter string) = "ExternalCorePropertyGetter " <> show string
  show (ExternalCoreContextGetter string) = "ExternalCoreContextGetter " <> show string
  -- 'Foreign': call a Javascript function here.
  show (ForeignRoleGetter string) = "ForeignRoleGetter " <> show string
  show (ForeignPropertyGetter string) = "ForeignPropertyGetter " <> show string
  show (VariableLookup variableName) = "VariableLookup " <> show variableName
  show (BindVariable variableName) = "BindVariable " <> show variableName
  show (BindResultFromCreatingAssignment variableName) = "BindResultFromCreatingAssignment " <> show variableName
  show (AssignmentOperator functionName) = "AssignmentOperator " <> show functionName
  show WithFrame = "WithFrame"

  show FilterF = "FilterF"

  show (TypeGetter functionName) = "TypeGetter " <> show functionName
  show (RoleTypeConstant roleType) = "RoleTypeConstant " <> show roleType
  show (ContextTypeConstant contextType) = "ContextTypeConstant " <> show contextType

  show (UnaryCombinator functionName) = "UnaryCombinator " <> show functionName
  -- | NaryCombinator functionName (Array QueryFunction)
  show (BinaryCombinator functionName) = "BinaryCombinator " <> show functionName
  show (Constant range string) = "Constant " <> show range <> " " <> show string
  show (RoleIndividual roleInstance) = "RoleIndividual " <> show roleInstance
  show (ContextIndividual contextInstance) = "ContextIndividual " <> show contextInstance

  show (PublicContext contextInstance) = "PublicContext " <> show contextInstance
  show (PublicRole roleInstance) = "PublicRole " <> show roleInstance

  show (CreateContext contextType roleType) = "CreateContext " <> show contextType <> " " <> show roleType
  show (CreateRootContext contextType) = "CreateRootContext " <> show contextType
  show (CreateContext_ contextType) = "CreateContext_ " <> show contextType
  show (CreateRole enumeratedRoleType) = "CreateRole " <> show enumeratedRoleType
  show (Bind enumeratedRoleType) = "Bind " <> show enumeratedRoleType
  show Bind_ = "Bind_"
  show (Unbind menumeratedRoleType) = "Unbind " <> " " <> show menumeratedRoleType
  show Unbind_ = "Unbind_"
  show (DeleteRole enumeratedRoleType) = "DeleteRole " <> show enumeratedRoleType
  show (DeleteContext roleType) = "DeleteContext " <> show roleType
  show (DeleteProperty enumeratedPropertyType) = "DeleteProperty " <> show enumeratedPropertyType
  show Move = "Move"
  show RemoveRole = "RemoveRole"
  show RemoveContext = "RemoveContext"
  show (AddPropertyValue enumeratedPropertyType) = "AddPropertyValue " <> show enumeratedPropertyType
  show (RemovePropertyValue enumeratedPropertyType) = "RemovePropertyValue " <> show enumeratedPropertyType
  show (SetPropertyValue enumeratedPropertyType) = "SetPropertyValue " <> show enumeratedPropertyType
  -- CreateFileF mimmeType property
  show (CreateFileF string enumeratedPropertyType) = "CreateFileF " <> show string <> " " <> show enumeratedPropertyType
  show (ExternalEffectFullFunction string) = "ExternalEffectFullFunction " <> show string
  show (ExternalDestructiveFunction string) = "ExternalDestructiveFunction " <> show string
  show (ForeignEffectFullFunction string) = "ForeignEffectFullFunction " <> show string

  show (TypeTimeOnlyContextF string) = "TypeTimeOnlyContextF " <> show string
  show (TypeTimeOnlyEnumeratedRoleF string) = "TypeTimeOnlyEnumeratedRoleF " <> show string
  show (TypeTimeOnlyCalculatedRoleF string) = "TypeTimeOnlyCalculatedRoleF " <> show string

  show (FilledF enumeratedRoleType contextType) = "FilledF " <> show enumeratedRoleType <> " " <> show contextType

instance eqQueryFunction :: Eq QueryFunction where
  -- eq x = genericEq x
  eq (DataTypeGetter a) (DataTypeGetter b) = eq a b
  eq (DataTypeGetterWithParameter a p) (DataTypeGetterWithParameter b q) = eq a b && eq p q
  eq (RegExMatch a) (RegExMatch b) = eq a b
  eq (PropertyGetter a) (PropertyGetter b) = eq a b
  eq (Value2Role a) (Value2Role b) = eq a b
  eq (RolGetter a) (RolGetter b) = eq a b
  -- 'External': call a Purescript function here.
  eq (ExternalCoreRoleGetter a) (ExternalCoreRoleGetter b) = eq a b
  eq (ExternalCorePropertyGetter a) (ExternalCorePropertyGetter b) = eq a b
  eq (ExternalCoreContextGetter a) (ExternalCoreContextGetter b) = eq a b
  -- 'Foreign': call a Javascript function here.
  eq (ForeignRoleGetter a) (ForeignRoleGetter b) = eq a b
  eq (ForeignPropertyGetter a) (ForeignPropertyGetter b) = eq a b
  eq (VariableLookup a) (VariableLookup b) = eq a b
  eq (BindVariable a) (BindVariable b) = eq a b
  eq (BindResultFromCreatingAssignment a) (BindResultFromCreatingAssignment b) = eq a b
  eq (AssignmentOperator a) (AssignmentOperator b) = eq a b
  eq WithFrame WithFrame = true

  eq FilterF FilterF = true

  eq (TypeGetter a) (TypeGetter b) = eq a b
  eq (RoleTypeConstant a) (RoleTypeConstant b) = eq a b
  eq (ContextTypeConstant a) (ContextTypeConstant b) = eq a b

  eq (UnaryCombinator a) (UnaryCombinator b) = eq a b
  -- | NaryCombinator FunctionName (Array QueryFunction)
  eq (BinaryCombinator a) (BinaryCombinator b) = eq a b
  eq (Constant a p) (Constant b q) = eq a b && eq p q
  eq (RoleIndividual a) (RoleIndividual b) = eq a b
  eq (ContextIndividual a) (ContextIndividual b) = eq a b

  eq (PublicContext a) (PublicContext b) = eq a b
  eq (PublicRole a) (PublicRole b) = eq a b

  eq (CreateContext a p) (CreateContext b q) = eq a b && eq p q
  eq (CreateRootContext a) (CreateRootContext b) = eq a b
  eq (CreateContext_ a) (CreateContext_ b) = eq a b
  eq (CreateRole a) (CreateRole b) = eq a b
  eq (Bind a) (Bind b) = eq a b
  eq Bind_ Bind_ = true
  eq (Unbind x) (Unbind y) = eq x y 
  eq Unbind_ Unbind_ = true
  eq (DeleteRole a) (DeleteRole b) = eq a b
  eq (DeleteContext a) (DeleteContext b) = eq a b
  eq (DeleteProperty a) (DeleteProperty b) = eq a b
  eq Move Move = true
  eq RemoveRole RemoveRole = true
  eq RemoveContext RemoveContext = true
  eq (AddPropertyValue a) (AddPropertyValue b) = eq a b
  eq (RemovePropertyValue a) (RemovePropertyValue b) = eq a b
  eq (SetPropertyValue a) (SetPropertyValue b) = eq a b
  -- CreateFileF mimmeType property
  eq (CreateFileF a p) (CreateFileF b q) = eq a b && eq p q
  eq (ExternalEffectFullFunction a) (ExternalEffectFullFunction b) = eq a b
  eq (ExternalDestructiveFunction a) (ExternalDestructiveFunction b) = eq a b
  eq (ForeignEffectFullFunction a) (ForeignEffectFullFunction b) = eq a b

  eq (TypeTimeOnlyContextF a) (TypeTimeOnlyContextF b) = eq a b
  eq (TypeTimeOnlyEnumeratedRoleF a) (TypeTimeOnlyEnumeratedRoleF b) = eq a b
  eq (TypeTimeOnlyCalculatedRoleF a) (TypeTimeOnlyCalculatedRoleF b) = eq a b

  eq (FilledF a p) (FilledF b q) = eq a b && eq p q
  eq _ _ = false

-- We don't need encode and decode instances of QueryFunction.

instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  -- writeImpl f = writeImpl( genericSumToVariant f)
  -- writeImpl a = unsafeToForeign (writeJSON (genericSumToVariant a))
  writeImpl (DataTypeGetter functionName) = writeImpl {constructor: "DataTypeGetter", arg1: writeJSON functionName, arg2: ""}
  writeImpl (DataTypeGetterWithParameter functionName string) = writeImpl {constructor: "DataTypeGetterWithParameter", arg1: writeJSON functionName, arg2: string}
  writeImpl (RegExMatch regExP) = writeImpl {constructor: "RegExMatch", arg1: writeJSON regExP, arg2: ""}
  writeImpl (PropertyGetter propertyType) = writeImpl {constructor: "PropertyGetter", arg1: writeJSON propertyType, arg2: ""}
  writeImpl (Value2Role propertyType) = writeImpl {constructor: "Value2Role", arg1: writeJSON propertyType, arg2: ""}
  writeImpl (RolGetter roleType) = writeImpl {constructor: "RolGetter", arg1: writeJSON roleType, arg2: ""}
  -- 'External': call a Purescript function here.
  writeImpl (ExternalCoreRoleGetter string) = writeImpl {constructor: "ExternalCoreRoleGetter", arg1: string, arg2: ""}
  writeImpl (ExternalCorePropertyGetter string) = writeImpl {constructor: "ExternalCorePropertyGetter", arg1: string, arg2: ""}
  writeImpl (ExternalCoreContextGetter string) = writeImpl {constructor: "ExternalCoreContextGetter", arg1: string, arg2: ""}
  -- 'Foreign': call a Javascript function here.
  writeImpl (ForeignRoleGetter string) = writeImpl {constructor: "ForeignRoleGetter", arg1: string, arg2: ""}
  writeImpl (ForeignPropertyGetter string) = writeImpl {constructor: "ForeignPropertyGetter", arg1: string, arg2: ""}
  writeImpl (VariableLookup variableName) = writeImpl {constructor: "VariableLookup", arg1: variableName, arg2: ""}
  writeImpl (BindVariable variableName) = writeImpl {constructor: "BindVariable", arg1: variableName, arg2: ""}
  writeImpl (BindResultFromCreatingAssignment variableName) = writeImpl {constructor: "BindResultFromCreatingAssignment", arg1: variableName, arg2: ""}
  writeImpl (AssignmentOperator functionName) = writeImpl {constructor: "AssignmentOperator", arg1: writeJSON functionName, arg2: ""}
  writeImpl WithFrame = writeImpl {constructor: "WithFrame", arg1: "", arg2: ""}

  writeImpl FilterF = writeImpl {constructor: "FilterF", arg1: "", arg2: ""}

  writeImpl (TypeGetter functionName) = writeImpl {constructor: "TypeGetter", arg1: writeJSON functionName, arg2: ""}
  writeImpl (RoleTypeConstant roleType) = writeImpl {constructor: "RoleTypeConstant", arg1: writeJSON roleType, arg2: ""}
  writeImpl (ContextTypeConstant contextType) = writeImpl {constructor: "ContextTypeConstant", arg1: writeJSON contextType, arg2: ""}

  writeImpl (UnaryCombinator functionName) = writeImpl {constructor: "UnaryCombinator", arg1: writeJSON functionName, arg2: ""}
  -- writeImpl (NaryCombinator functionName) = writeImpl {constructor: "NaryCombinator", arg1: writeJSON functionName, arg2: ""} (Array QueryFunction)
  writeImpl (BinaryCombinator functionName) = writeImpl {constructor: "BinaryCombinator", arg1: writeJSON functionName, arg2: ""}
  writeImpl (Constant range string) = writeImpl {constructor: "Constant", arg1: writeJSON range, arg2: string}
  writeImpl (RoleIndividual roleInstance) = writeImpl {constructor: "RoleIndividual", arg1: writeJSON roleInstance, arg2: ""}
  writeImpl (ContextIndividual contextInstance) = writeImpl {constructor: "ContextIndividual", arg1: writeJSON contextInstance, arg2: ""}

  writeImpl (PublicContext contextInstance) = writeImpl {constructor: "PublicContext", arg1: writeJSON contextInstance, arg2: ""}
  writeImpl (PublicRole roleInstance) = writeImpl {constructor: "PublicRole", arg1: writeJSON roleInstance, arg2: ""}

  writeImpl (CreateContext contextType roleType) = writeImpl {constructor: "CreateContext", arg1: writeJSON contextType, arg2: writeJSON roleType}
  writeImpl (CreateRootContext contextType) = writeImpl {constructor: "CreateRootContext", arg1: writeJSON contextType, arg2: ""}
  writeImpl (CreateContext_ contextType) = writeImpl {constructor: "CreateContext_", arg1: writeJSON contextType, arg2: ""}
  writeImpl (CreateRole enumeratedRoleType) = writeImpl {constructor: "CreateRole", arg1: writeJSON enumeratedRoleType, arg2: ""}
  writeImpl (Bind enumeratedRoleType) = writeImpl {constructor: "Bind", arg1: writeJSON enumeratedRoleType, arg2: ""}
  writeImpl Bind_ = writeImpl {constructor: "Bind_", arg1: "", arg2: ""}
  writeImpl (Unbind mEnumeratedRoleType) = writeImpl {constructor: "Unbind", arg1: writeJSON mEnumeratedRoleType, arg2: ""}
  writeImpl Unbind_ = writeImpl {constructor: "Unbind_", arg1: "", arg2: ""}
  writeImpl (DeleteRole enumeratedRoleType) = writeImpl {constructor: "DeleteRole", arg1: writeJSON enumeratedRoleType, arg2: ""}
  writeImpl (DeleteContext roleType) = writeImpl {constructor: "DeleteContext", arg1: writeJSON roleType, arg2: ""}
  writeImpl (DeleteProperty enumeratedPropertyType) = writeImpl {constructor: "DeleteProperty", arg1: writeJSON enumeratedPropertyType, arg2: ""}
  writeImpl Move = writeImpl {constructor: "Move", arg1: "", arg2: ""}
  writeImpl RemoveRole = writeImpl {constructor: "RemoveRole", arg1: "", arg2: ""}
  writeImpl RemoveContext = writeImpl {constructor: "RemoveContext", arg1: "", arg2: ""}
  writeImpl (AddPropertyValue enumeratedPropertyType) = writeImpl {constructor: "AddPropertyValue", arg1: writeJSON enumeratedPropertyType, arg2: ""}
  writeImpl (RemovePropertyValue enumeratedPropertyType) = writeImpl {constructor: "RemovePropertyValue", arg1: writeJSON enumeratedPropertyType, arg2: ""}
  writeImpl (SetPropertyValue enumeratedPropertyType) = writeImpl {constructor: "SetPropertyValue", arg1: writeJSON enumeratedPropertyType, arg2: ""}
  -- CreateFileF mimmeType property
  writeImpl (CreateFileF string enumeratedPropertyType) = writeImpl {constructor: "CreateFileF", arg1: string, arg2: writeJSON enumeratedPropertyType}
  writeImpl (ExternalEffectFullFunction string) = writeImpl {constructor: "ExternalEffectFullFunction", arg1: string, arg2: ""}
  writeImpl (ExternalDestructiveFunction string) = writeImpl {constructor: "ExternalDestructiveFunction", arg1: string, arg2: ""}
  writeImpl (ForeignEffectFullFunction string) = writeImpl {constructor: "ForeignEffectFullFunction", arg1: string, arg2: ""}

  writeImpl (TypeTimeOnlyContextF string) = writeImpl {constructor: "TypeTimeOnlyContextF", arg1: string, arg2: ""}
  writeImpl (TypeTimeOnlyEnumeratedRoleF string) = writeImpl {constructor: "TypeTimeOnlyEnumeratedRoleF", arg1: string, arg2: ""}
  writeImpl (TypeTimeOnlyCalculatedRoleF string) = writeImpl {constructor: "TypeTimeOnlyCalculatedRoleF", arg1: string, arg2: ""}

  writeImpl (FilledF enumeratedRoleType contextType) = writeImpl {constructor: "FilledF", arg1: writeJSON enumeratedRoleType, arg2: writeJSON contextType}

type QueryFunctionSerialised = 
  { constructor :: String
  , arg1 :: String
  , arg2 :: String
  }

instance readForeignQueryFunction :: ReadForeign QueryFunction where
  -- readImpl f = map variantToGenericSum (readImpl f)
  readImpl f = do 
    x ::QueryFunctionSerialised <- read' f
    case x.constructor, x.arg1, x.arg2 of
      "DataTypeGetter", fname, _ -> DataTypeGetter <$> readJSON' fname
      "DataTypeGetterWithParameter", fname, s -> flip DataTypeGetterWithParameter s <$> readJSON' fname
      "RegExMatch", regExP, _-> RegExMatch <$> readJSON' regExP
      "PropertyGetter", propertyType, _-> PropertyGetter <$> readJSON' propertyType
      "Value2Role", propertyType, _-> Value2Role <$> readJSON' propertyType
      "RolGetter", roleType, _-> RolGetter <$> readJSON' roleType
      "ExternalCoreRoleGetter", string, _-> pure $ ExternalCoreRoleGetter string
      "ExternalCorePropertyGetter", string, _-> pure $ ExternalCorePropertyGetter string
      "ExternalCoreContextGetter", string, _-> pure $ ExternalCoreContextGetter string
      -- 'Foreign': call a Javascript function here.
      "ForeignRoleGetter", string, _-> pure $ ForeignRoleGetter string
      "ForeignPropertyGetter", string, _-> pure $ ForeignPropertyGetter string
      "VariableLookup", variableName, _-> pure $ VariableLookup variableName
      "BindVariable", variableName, _-> pure $ BindVariable variableName
      "BindResultFromCreatingAssignment", variableName, _-> pure $ BindResultFromCreatingAssignment variableName
      "AssignmentOperator", functionName, _-> AssignmentOperator <$> readJSON' functionName
      "WithFrame", _, _-> pure $ WithFrame
      "FilterF", _, _-> pure $ FilterF
      "TypeGetter", functionName, _-> TypeGetter <$> readJSON' functionName
      "RoleTypeConstant", roleType, _-> RoleTypeConstant <$> readJSON' roleType
      "ContextTypeConstant", contextType, _-> ContextTypeConstant <$> readJSON' contextType
      "UnaryCombinator", functionName, _-> UnaryCombinator <$> readJSON' functionName
      "BinaryCombinator", functionName, _-> BinaryCombinator <$> readJSON' functionName
      "Constant", range, string -> flip Constant string <$> readJSON' range
      "RoleIndividual", roleInstance, _-> RoleIndividual <<< RoleInstance <$> readJSON' roleInstance
      "ContextIndividual", contextInstance, _-> ContextIndividual <<< ContextInstance <$> readJSON' contextInstance
      "PublicContext", contextInstance, _-> PublicContext <<< ContextInstance <$> readJSON' contextInstance
      "PublicRole", roleInstance, _-> PublicRole <<< RoleInstance <$> readJSON' roleInstance
      "CreateContext", contextType, roleType -> CreateContext <$> readJSON' contextType <*> readJSON' roleType
      "CreateRootContext", contextType, _-> CreateRootContext <$> (readJSON' contextType)
      "CreateContext_", contextType, _-> CreateContext_ <$> (readJSON' contextType)
      "CreateRole", enumeratedRoleType, _-> CreateRole <$> readJSON' enumeratedRoleType
      "Bind", enumeratedRoleType, _-> Bind <$> readJSON' enumeratedRoleType
      "Bind_", _, _-> pure $ Bind_
      "Unbind", mEnumeratedRoleType, _ -> Unbind <$> readJSON' mEnumeratedRoleType
      "Unbind_", _, _-> pure $ Unbind_
      "DeleteRole", enumeratedRoleType, _-> DeleteRole <$> readJSON' enumeratedRoleType
      "DeleteContext", roleType, _-> DeleteContext <$> readJSON' roleType
      "DeleteProperty", enumeratedPropertyType, _-> DeleteProperty <$> readJSON' enumeratedPropertyType
      "Move", _, _-> pure $ Move
      "RemoveRole", _, _-> pure $ RemoveRole
      "RemoveContext", _, _-> pure $ RemoveContext
      "AddPropertyValue", enumeratedPropertyType, _-> AddPropertyValue <$> readJSON' enumeratedPropertyType
      "RemovePropertyValue", enumeratedPropertyType, _-> RemovePropertyValue <$> readJSON' enumeratedPropertyType
      "SetPropertyValue", enumeratedPropertyType, _-> SetPropertyValue <$> readJSON' enumeratedPropertyType
      -- CreateFileF mimmeType property
      "CreateFileF", string, enumeratedPropertyType -> CreateFileF string <$> readJSON' enumeratedPropertyType
      "ExternalEffectFullFunction", string, _-> pure $ ExternalEffectFullFunction string
      "ExternalDestructiveFunction", string, _-> pure $ ExternalDestructiveFunction string
      "ForeignEffectFullFunction", string, _-> pure $ ForeignEffectFullFunction string
      "TypeTimeOnlyContextF", string, _-> pure $ TypeTimeOnlyContextF string
      "TypeTimeOnlyEnumeratedRoleF", string, _-> pure $ TypeTimeOnlyEnumeratedRoleF string
      "TypeTimeOnlyCalculatedRoleF", string, _-> pure $ TypeTimeOnlyCalculatedRoleF string
      "FilledF", enumeratedRoleType, contextType -> FilledF <$> readJSON' enumeratedRoleType <*> readJSON' contextType
      c, a1, a2 -> throwError (singleton $ ForeignError ("Unknown case in ReadForeign QueryFunction " <> c <> " " <> a1 <> " " <> a2))

instance ordQueryFunction :: Ord QueryFunction where 
  compare (DataTypeGetter functionName) (DataTypeGetter functionName') = compare functionName functionName'
  compare (DataTypeGetterWithParameter functionName string) (DataTypeGetterWithParameter functionName' string') = compare functionName functionName'
  compare (RegExMatch regExP) (RegExMatch regExP') = compare regExP regExP'
  compare (PropertyGetter propertyType) (PropertyGetter propertyType') = compare propertyType propertyType'
  compare (Value2Role propertyType) (Value2Role propertyType') = compare propertyType propertyType'
  compare (RolGetter roleType) (RolGetter roleType') = compare roleType roleType'
  -- 'External': call a Purescript function here.
  compare (ExternalCoreRoleGetter string) (ExternalCoreRoleGetter string') = compare string string'
  compare (ExternalCorePropertyGetter string) (ExternalCorePropertyGetter string') = compare string string'
  compare (ExternalCoreContextGetter string) (ExternalCoreContextGetter string') = compare string string'
  -- 'Foreign': call a Javascript function here.
  compare (ForeignRoleGetter string) (ForeignRoleGetter string') = compare string string'
  compare (ForeignPropertyGetter string) (ForeignPropertyGetter string') = compare string string'
  compare (VariableLookup variableName) (VariableLookup variableName') = compare variableName variableName'
  compare (BindVariable variableName) (BindVariable variableName') = compare variableName variableName'
  compare (BindResultFromCreatingAssignment variableName) (BindResultFromCreatingAssignment variableName') = compare variableName variableName'
  compare (AssignmentOperator functionName) (AssignmentOperator functionName') = compare functionName functionName'
  compare WithFrame WithFrame = EQ

  compare FilterF FilterF = EQ

  compare (TypeGetter functionName) (TypeGetter functionName') = compare functionName functionName'
  compare (RoleTypeConstant roleType) (RoleTypeConstant roleType') = compare roleType roleType'
  compare (ContextTypeConstant contextType) (ContextTypeConstant contextType') = compare contextType contextType'

  compare (UnaryCombinator functionName) (UnaryCombinator functionName') = compare functionName functionName'
  compare (BinaryCombinator functionName) (BinaryCombinator functionName') = compare functionName functionName'
  compare (Constant range string) (Constant range' string') = compare range range'
  compare (RoleIndividual roleInstance) (RoleIndividual roleInstance') = compare roleInstance roleInstance'
  compare (ContextIndividual contextInstance) (ContextIndividual contextInstance') = compare contextInstance contextInstance'

  compare (PublicContext contextInstance) (PublicContext contextInstance') = compare contextInstance contextInstance'
  compare (PublicRole roleInstance) (PublicRole roleInstance') = compare roleInstance roleInstance'

  compare (CreateContext contextType roleType) (CreateContext contextType' roleType') = compare contextType contextType'
  compare (CreateRootContext contextType) (CreateRootContext contextType') = compare contextType contextType'
  compare (CreateContext_ contextType) (CreateContext_ contextType') = compare contextType contextType'
  compare (CreateRole enumeratedRoleType) (CreateRole enumeratedRoleType') = compare enumeratedRoleType enumeratedRoleType'
  compare (Bind enumeratedRoleType) (Bind enumeratedRoleType') = compare enumeratedRoleType enumeratedRoleType'
  compare Bind_ Bind_ = EQ
  compare (Unbind m1) (Unbind m2) = compare m1 m2
  compare Unbind_ Unbind_ = EQ
  compare (DeleteRole enumeratedRoleType) (DeleteRole enumeratedRoleType') = compare enumeratedRoleType enumeratedRoleType'
  compare (DeleteContext roleType) (DeleteContext roleType') = compare roleType roleType'
  compare (DeleteProperty enumeratedPropertyType) (DeleteProperty enumeratedPropertyType') = compare enumeratedPropertyType enumeratedPropertyType'
  compare Move Move = EQ
  compare RemoveRole RemoveRole = EQ
  compare RemoveContext RemoveContext = EQ
  compare (AddPropertyValue enumeratedPropertyType) (AddPropertyValue enumeratedPropertyType') = compare enumeratedPropertyType enumeratedPropertyType'
  compare (RemovePropertyValue enumeratedPropertyType) (RemovePropertyValue enumeratedPropertyType') = compare enumeratedPropertyType enumeratedPropertyType'
  compare (SetPropertyValue enumeratedPropertyType) (SetPropertyValue enumeratedPropertyType') = compare enumeratedPropertyType enumeratedPropertyType'
  -- CreateFileF mimmeType property
  compare (CreateFileF string enumeratedPropertyType) (CreateFileF string' enumeratedPropertyType') = compare string string'
  compare (ExternalEffectFullFunction string) (ExternalEffectFullFunction string') = compare string string'
  compare (ExternalDestructiveFunction string) (ExternalDestructiveFunction string') = compare string string'
  compare (ForeignEffectFullFunction string) (ForeignEffectFullFunction string') = compare string string'

  compare (TypeTimeOnlyContextF string) (TypeTimeOnlyContextF string') = compare string string'
  compare (TypeTimeOnlyEnumeratedRoleF string) (TypeTimeOnlyEnumeratedRoleF string') = compare string string'
  compare (TypeTimeOnlyCalculatedRoleF string) (TypeTimeOnlyCalculatedRoleF string') = compare string string'

  compare (FilledF enumeratedRoleType contextType) (FilledF enumeratedRoleType' contextType') = compare enumeratedRoleType enumeratedRoleType'
  
  compare _ _ = EQ
