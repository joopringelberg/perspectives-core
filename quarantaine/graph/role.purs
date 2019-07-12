module Perspectives.Graph.Role where

import Data.Array (singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (lookup)
import Foreign.Object as F
import Perspectives.Graph.Base (Revision, PropertyValueWithComments, Comments, RoleId, ContextId)
import Perspectives.Graph.Class.Vertex (class Vertex, addEdgeDefault, addPropertyDefault, changePrototypeDefault, changeRevisionDefault, propertyDefault, removeEdgeDefault, removePropertyDefault, setPropertyDefault, setEdgeDefault)
import Prelude (class Show, identity, ($), (<<<), (==))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- ROLE
-----------------------------------------------------------
newtype Role = Role RoleRecord

type RoleRecord =
  { _id :: RoleId
  , pspType :: ContextId
  , prototype :: ContextId
  , context :: ContextId
  -- While the fields above occur in every role, those below do not.
  , _rev :: Revision
  , binding :: Binding
  -- The four fields below could also be modeled as Maybe values.
  , properties :: F.Object PropertyValueWithComments
  , gevuldeRollen :: F.Object (Array ContextId)
  , occurrence :: Int
  , comments :: Comments
  }

derive instance genericRepRole :: Generic Role _

instance showRole :: Show Role where
  show = genericShow

instance encodeRole :: Encode Role where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeRole :: Decode Role where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

derive instance newtypeRole :: Newtype Role _

derive newtype instance writeForeignRole :: WriteForeign Role

type Binding = Maybe ContextId

-----------------------------------------------------------
-- ROLE AS INSTANCE OF VERTEX
-----------------------------------------------------------
instance vertexRole :: Vertex Role where
  identifier = _._id <<< unwrap
  rev = _._rev <<< unwrap
  changeRevision s = over Role (changeRevisionDefault s)
  displayName r = "No name for Role"
  -- changeDisplayName n c = Role $ changeDisplayNameDefault n (unwrap c)
  changeDisplayName n r = r
  pspType = _.pspType <<< unwrap
  prototype = _.prototype <<< unwrap
  changePrototype p = over Role (changePrototypeDefault p)
  edgeLabelsOut _ = ["binding"]
  edgeLabelsIn (Role {gevuldeRollen}) = F.keys gevuldeRollen

  edgeOut (Role{binding}) edgeId = if edgeId == "binding" then maybe [] singleton binding else []
  addEdgeOut r _ _ = r
  removeEdgeOut r _ _ = r
  setEdgeOut r _ _  = r

  edgeIn (Role{gevuldeRollen}) rn = maybe [] identity (lookup rn gevuldeRollen)
  addEdgeIn (Role r@{gevuldeRollen}) e v= Role r {gevuldeRollen = addEdgeDefault gevuldeRollen e v}
  removeEdgeIn (Role r@{gevuldeRollen}) e v= Role r {gevuldeRollen = removeEdgeDefault gevuldeRollen e v}
  setEdgeIn (Role r@{gevuldeRollen}) e va = Role r {gevuldeRollen = setEdgeDefault gevuldeRollen e va}

  properties = _.properties <<< unwrap

  property c = propertyDefault $ unwrap c
  addProperty (Role r) pn v = Role $ addPropertyDefault r pn v
  removeProperty (Role r) pn v = Role $ removePropertyDefault r pn v
  setProperty (Role r) pn pa = Role $ setPropertyDefault r pn pa

  comments = _.comments <<< unwrap
