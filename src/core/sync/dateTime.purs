module Perspectives.Sync.DateTime where

import Data.DateTime (DateTime)
import Foreign (unsafeToForeign)
import Foreign.Class (class Encode)
import Prelude (class Show, show, ($))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- DATETIME
-- We need a newtype for DateTime in order to be able to serialize and show it.
-----------------------------------------------------------
newtype SerializableDateTime = SerializableDateTime DateTime

instance encodeSerializableDateTime :: Encode SerializableDateTime where
  encode d = unsafeToForeign $ show d

instance showSerializableDateTime :: Show SerializableDateTime where
  show (SerializableDateTime d) = "todo"

instance writeForeignSerializableDateTime :: WriteForeign SerializableDateTime where
  writeImpl (SerializableDateTime dt) = unsafeToForeign $ show dt
