module Perspectives.Representation.QueryFunction where

import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (ForeignError(..), fail, unsafeFromForeign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType)
import Prelude (class Eq, class Show, pure, ($), (<>), show)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON, readJSON)

type FunctionName = String

data QueryFunction
  = DataTypeGetter FunctionName
  | PropertyGetter PropertyType
  | RolGetter RoleType
  -- 'Computed' is not 'calculated': call a Purescript function here.
  | ComputedRoleGetter FunctionName
  | ComputedPropertyGetter FunctionName

  | UnaryCombinator FunctionName
  -- | NaryCombinator FunctionName (Array QueryFunction)
  | BinaryCombinator FunctionName
  -- | Filter QueryFunction QueryFunction

derive instance genericRepQueryFunction :: Generic QueryFunction _

instance showQueryFunction :: Show QueryFunction where
  show x = genericShow x

instance eqQueryFunction :: Eq QueryFunction where
  eq x = genericEq x

type ConstructorRep = {tag :: String, dat :: Array String}

instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  writeImpl (DataTypeGetter f) = unsafeToForeign $ writeJSON {tag: "DataTypeGetter", dat: f}
  writeImpl (PropertyGetter p) = unsafeToForeign $ writeJSON {tag: "PropertyGetter", dat: writeJSON p}
  writeImpl (RolGetter p) = unsafeToForeign $ writeJSON {tag: "RolGetter", dat: writeJSON p}
  writeImpl (ComputedRoleGetter f) = unsafeToForeign $ writeJSON {tag: "ComputedRoleGetter", dat: f}
  writeImpl (ComputedPropertyGetter f) = unsafeToForeign $ writeJSON {tag: "ComputedPropertyGetter", dat: f}
  writeImpl (UnaryCombinator f) = unsafeToForeign $ writeJSON {tag: "UnaryCombinator", dat: [f]}
  -- writeImpl (NaryCombinator f q) = unsafeToForeign $ writeJSON {tag: "NaryCombinator", dat: [f, writeJSON q]}
  writeImpl (BinaryCombinator f) = unsafeToForeign $ writeJSON {tag: "BinaryCombinator", dat: [f]}
  -- writeImpl (Filter c q) = unsafeToForeign $ writeJSON {tag: "Filter", dat: [writeJSON c, writeJSON q]}


instance readForeignQueryFunction :: ReadForeign QueryFunction where
  readImpl q = case readJSON (unsafeFromForeign q) of
    (Left e) -> fail (ForeignError "Could not parse QueryFunction data")
    (Right ({tag, dat} :: ConstructorRep)) -> case tag of
      "DataTypeGetter" -> pure $ unsafePartial $ DataTypeGetter $ head dat
      "PropertyGetter" -> case (readJSON (unsafePartial $ head dat)) of
        (Left e) -> fail $ ForeignError (show e)
        (Right r) -> pure $ unsafePartial $ PropertyGetter r
      "RolGetter" -> case (readJSON (unsafePartial $ head dat)) of
        (Left e) -> fail $ ForeignError (show e)
        (Right r) -> pure $ unsafePartial $ RolGetter r
      "ComputedRoleGetter" -> pure $ unsafePartial $ ComputedRoleGetter $ head dat
      "ComputedPropertyGetter" -> pure $ unsafePartial $ ComputedPropertyGetter $ head dat
      "UnaryCombinator" -> pure $ unsafePartial $ UnaryCombinator (head dat)
      "BinaryCombinator" -> pure $ unsafePartial $ BinaryCombinator (head dat)
      x -> fail $ ForeignError ("Unknown case in ReadForeign QueryFunction: " <> show x)
