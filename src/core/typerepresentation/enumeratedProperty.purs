module Perspectives.Representation.EnumeratedProperty where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Kishimen (genericSumToVariant)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

-----------------------------------------------------------
-- ENUMERATEDPROPERTY
-----------------------------------------------------------
newtype EnumeratedProperty = EnumeratedProperty EnumeratedPropertyRecord

type EnumeratedPropertyRecord =
  { _id :: EnumeratedPropertyType
  , _rev :: Revision_
  , displayName :: String

  , role :: EnumeratedRoleType
  , range :: Range
  , functional :: Boolean
  , mandatory :: Boolean
  }

derive instance genericRepEnumeratedProperty :: Generic EnumeratedProperty _

instance showEnumeratedProperty :: Show EnumeratedProperty where
  show = genericShow

instance eqEnumeratedProperty :: Eq EnumeratedProperty where
  eq (EnumeratedProperty {_id : id1}) (EnumeratedProperty {_id : id2}) = id1 == id2

derive instance newtypeEnumeratedProperty :: Newtype EnumeratedProperty _

derive newtype instance writeForeignEnumeratedProperty :: WriteForeign EnumeratedProperty

derive newtype instance readForeignEnumeratedProperty :: ReadForeign EnumeratedProperty

instance revisionEnumeratedProperty :: Revision EnumeratedProperty where
  rev = _._rev <<< unwrap
  changeRevision s = over EnumeratedProperty (\vr -> vr {_rev = s})

instance identifiableEnumeratedProperty :: Identifiable EnumeratedProperty EnumeratedPropertyType where
  identifier (EnumeratedProperty{_id}) = _id

-----------------------------------------------------------
-- RANGE
-----------------------------------------------------------
data Range = PString | PBool | PNumber | PDate

derive instance genericRange :: Generic Range _

instance writeForeignRange :: WriteForeign Range where
  writeImpl = writeImpl <<< genericSumToVariant

instance readForeignRange :: ReadForeign Range where
  readImpl = enumReadForeign

instance rangeShow :: Show Range where
  show = genericShow
