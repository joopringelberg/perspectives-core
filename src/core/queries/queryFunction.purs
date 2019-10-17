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

data QueryFunction
  = DataTypeGetter FunctionName
  | DataTypeGetterWithParameter FunctionName String
  | PropertyGetter PropertyType
  | RolGetter RoleType
  -- 'Computed' is not 'calculated': call a Purescript function here.
  | ComputedRoleGetter FunctionName
  | ComputedPropertyGetter FunctionName

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
