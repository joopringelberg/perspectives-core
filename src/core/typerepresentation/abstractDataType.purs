module Perspectives.Representation.ADT where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Prelude (class Eq, class Show)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

data ADT a = ST a | SUM (Array (ADT a)) | PROD (Array (ADT a))

derive instance genericRepBinding :: Generic (ADT a) _

instance showBinding :: (Show a) => Show (ADT a) where
  show b = genericShow b

instance eqBinding :: (Eq a) => Eq (ADT a) where
  eq b1 b2 = genericEq b1 b2

instance writeForeignBinding :: (WriteForeign a) => WriteForeign (ADT a) where
  writeImpl b = unsafeToForeign (writeJSON b)

instance readForeignBinding :: (ReadForeign a) => ReadForeign (ADT a) where
  readImpl b = readJSON' (unsafeFromForeign b)
