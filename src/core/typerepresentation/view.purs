module Perspectives.Representation.View where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, PropertyType, ViewType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- VIEW TYPE CLASS
-----------------------------------------------------------
class ViewClass r where
  role :: r -> EnumeratedRoleType
  propertyReferences :: r -> Array PropertyType

instance calculatedPropertyViewClass :: ViewClass View where
  role r = (unwrap r).role
  propertyReferences r = (unwrap r).propertyReferences

-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------
newtype View = View ViewRecord

type ViewRecord =
  { _id :: ViewType
  , _rev :: Revision
  , displayName :: String

  , propertyReferences :: Array PropertyType
  , role :: EnumeratedRoleType
  }

derive instance genericRepView :: Generic View _

instance showView :: Show View where
  show = genericShow

instance eqView :: Eq View where
  eq (View {_id : id1}) (View {_id : id2}) = id1 == id2

derive instance newtypeView :: Newtype View _

derive newtype instance writeForeignView :: WriteForeign View

derive newtype instance readForeignView :: ReadForeign View

instance revisionView :: Revision View where
  rev = _._rev <<< unwrap
  changeRevision s = over View (\vr -> vr {_rev = s})
