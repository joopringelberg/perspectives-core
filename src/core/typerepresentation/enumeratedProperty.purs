module Perspectives.Representation.EnumeratedProperty where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Kishimen (genericSumToVariant)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

-----------------------------------------------------------
-- ENUMERATED PROPERTY TYPE CLASS
-----------------------------------------------------------
class EnumeratedPropertyClass r where
  role :: r -> EnumeratedRoleType
  range :: r -> Range
  functional :: r -> Boolean
  mandatory :: r -> Boolean

instance enumeratedPropertyEnumeratedPropertyClass :: EnumeratedPropertyClass EnumeratedProperty where
  role r = (unwrap r).role
  range r = (unwrap r).range
  functional r = (unwrap r).functional
  mandatory r = (unwrap r).mandatory

-----------------------------------------------------------
-- ENUMERATEDPROPERTY
-----------------------------------------------------------
newtype EnumeratedProperty = EnumeratedProperty EnumeratedPropertyRecord

type EnumeratedPropertyRecord =
  { _id :: EnumeratedPropertyType
  , _rev :: Revision
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
