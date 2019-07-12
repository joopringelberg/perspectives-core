module Perspectives.Representation.QueryFunction where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Variant (Variant)
import Foreign (unsafeToForeign)
import Kishimen (genericSumToVariant)
import Prelude (class Eq, class Show, pure, ($))
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)

type FunctionName = String

data QueryFunction
  = DataTypeGetter FunctionName
  -- | PropertyGetter FunctionName PropertyType
  -- | RolGetter FunctionName RoleType
  | ComputedRoleGetter FunctionName
  | ComputedPropertyGetter FunctionName
  -- | UnaryCombinator FunctionName QueryFunction
  -- | NaryCombinator FunctionName (Array QueryFunction)
  -- | Filter QueryFunction QueryFunction

derive instance genericRepQueryFunction :: Generic QueryFunction _

instance showQueryFunction :: Show QueryFunction where
  show = genericShow

instance eqQueryFunction :: Eq QueryFunction where
  eq = genericEq

instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  writeImpl q = unsafeToForeign $ writeJSON (genericSumToVariant q :: Variant ("DataTypeGetter" :: FunctionName, "ComputedRoleGetter" :: FunctionName, "ComputedPropertyGetter" :: FunctionName))

instance readForeignQueryFunction :: ReadForeign QueryFunction where
  readImpl q = pure $ DataTypeGetter "aap"

-- instance revisionQueryFunction :: Revision QueryFunction where
--   rev = _._rev <<< unwrap
--   changeRevision s = over QueryFunction (\vr -> vr {_rev = s})
