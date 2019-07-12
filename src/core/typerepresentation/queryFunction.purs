module Perspectives.Representation.QueryFunction where

import Data.Array.Partial (head, tail)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (unwrap)
import Foreign (ForeignError(..), fail, unsafeFromForeign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType(..))
import Prelude (class Eq, class Show, pure, ($), (<>), show)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON, readJSON)

type FunctionName = String

data QueryFunction
  = DataTypeGetter FunctionName
  | PropertyGetter FunctionName PropertyType
  | RolGetter FunctionName EnumeratedRoleType
  | ComputedRoleGetter FunctionName
  | ComputedPropertyGetter FunctionName
  | UnaryCombinator FunctionName QueryFunction
  | NaryCombinator FunctionName (Array QueryFunction)
  | Filter QueryFunction QueryFunction

derive instance genericRepQueryFunction :: Generic QueryFunction _

instance showQueryFunction :: Show QueryFunction where
  show x = genericShow x

instance eqQueryFunction :: Eq QueryFunction where
  eq x = genericEq x

type ConstructorRep = {tag :: String, dat :: Array String}

instance writeForeignQueryFunction :: WriteForeign QueryFunction where
  writeImpl (DataTypeGetter f) = unsafeToForeign $ writeJSON {tag: "DataTypeGetter", dat: f}
  writeImpl (PropertyGetter f p) = unsafeToForeign $ writeJSON {tag: "PropertyGetter", dat: [f, unwrap p]}
  writeImpl (RolGetter f p) = unsafeToForeign $ writeJSON {tag: "RolGetter", dat: [f, unwrap p]}
  writeImpl (ComputedRoleGetter f) = unsafeToForeign $ writeJSON {tag: "ComputedRoleGetter", dat: f}
  writeImpl (ComputedPropertyGetter f) = unsafeToForeign $ writeJSON {tag: "ComputedPropertyGetter", dat: f}
  writeImpl (UnaryCombinator f q) = unsafeToForeign $ writeJSON {tag: "UnaryCombinator", dat: [f, writeJSON q]}
  writeImpl (NaryCombinator f q) = unsafeToForeign $ writeJSON {tag: "NaryCombinator", dat: [f, writeJSON q]}
  writeImpl (Filter c q) = unsafeToForeign $ writeJSON {tag: "Filter", dat: [writeJSON c, writeJSON q]}


instance readForeignQueryFunction :: ReadForeign QueryFunction where
  readImpl q = case readJSON (unsafeFromForeign q) of
    (Left e) -> fail (ForeignError "Could not parse QueryFunction data")
    (Right ({tag, dat} :: ConstructorRep)) -> case tag of
      "DataTypeGetter" -> pure $ unsafePartial $ DataTypeGetter $ head dat
      "PropertyGetter" -> pure $ unsafePartial $ PropertyGetter (head dat) (PropertyType (head $ tail dat))
      "RolGetter" -> pure $ unsafePartial $ RolGetter (head dat) (EnumeratedRoleType (head $ tail dat))
      "ComputedRoleGetter" -> pure $ unsafePartial $ ComputedRoleGetter $ head dat
      "ComputedPropertyGetter" -> pure $ unsafePartial $ ComputedPropertyGetter $ head dat
      "UnaryCombinator" -> case (readJSON (unsafePartial (head $ tail dat))) of
        (Left e) -> fail $ ForeignError $ show e
        (Right sq) -> pure $ unsafePartial $ UnaryCombinator (head dat) sq
      "NaryCombinator" -> case (readJSON (unsafePartial (head $ tail dat))) of
        (Left e) -> fail $ ForeignError $ show e
        (Right sq) -> pure $ unsafePartial $ NaryCombinator (head dat) sq
      "Filter" -> case (readJSON (unsafePartial (head dat))) of
        (Left e) -> fail $ ForeignError $ show e
        (Right c) -> case (readJSON (unsafePartial (head $ tail dat))) of
          (Left e) -> fail $ ForeignError $ show e
          (Right sq) -> pure $ unsafePartial $ Filter c sq
      x -> fail $ ForeignError ("Unknown case in ReadForeign QueryFunction: " <> show x)
