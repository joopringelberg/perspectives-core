module Perspectives.Graph.Context where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object as F
import Perspectives.Graph.Base (Revision, PropertyValueWithComments, Comments, ContextId)
import Perspectives.Graph.Class.Vertex (class Vertex, addEdgeDefault, addPropertyDefault, changeDisplayNameDefault, changePrototypeDefault, changeRevisionDefault, edgeInDefault, edgeOutDefault, propertyDefault, removeEdgeDefault, removePropertyDefault, setPropertyDefault, setEdgeDefault)
import Prelude (class Show, ($), class Eq, (==), (<<<))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- CONTEXT
-----------------------------------------------------------
newtype Context = Context ContextRecord

type ContextRecord =
  { _id :: ContextId
  , _rev :: Revision
  , displayName :: String
  , pspType :: ContextId
  , prototype :: ContextId
  , edgesOut :: F.Object (Array ContextId)
  , edgesIn :: F.Object (Array ContextId)
  , properties :: F.Object PropertyValueWithComments
  , comments :: Comments
  }

derive instance genericRepContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

instance encodeContext :: Encode Context where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeContext :: Decode Context where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance eqContext :: Eq Context where
  eq (Context {_id : id1}) (Context {_id : id2}) = id1 == id2

derive instance newtypeContext :: Newtype Context _

derive newtype instance writeForeignContext :: WriteForeign Context

-----------------------------------------------------------
-- CONTEXT AS INSTANCE OF VERTEX
-----------------------------------------------------------
instance vertexContext :: Vertex Context where
  identifier = _._id <<< unwrap
  rev = _._rev <<< unwrap
  changeRevision s = over Context (changeRevisionDefault s)
  displayName = _.displayName <<< unwrap
  -- changeDisplayName n c = Context $ changeDisplayNameDefault n (unwrap c)
  changeDisplayName n = over Context (changeDisplayNameDefault n)
  pspType = _.pspType <<< unwrap
  prototype = _.prototype <<< unwrap
  changePrototype p = over Context (changePrototypeDefault p)
  edgeLabelsOut (Context {edgesOut}) = F.keys edgesOut
  edgeLabelsIn (Context {edgesIn}) = F.keys edgesIn

  edgeOut c = edgeOutDefault (unwrap c)
  addEdgeOut (Context r@{edgesOut}) e v= Context r {edgesOut = addEdgeDefault edgesOut e v}
  removeEdgeOut (Context r@{edgesOut}) e v= Context r {edgesOut = removeEdgeDefault edgesOut e v}
  setEdgeOut (Context r@{edgesOut}) e va = Context r {edgesOut = setEdgeDefault edgesOut e va}

  edgeIn c = edgeInDefault (unwrap c)
  addEdgeIn (Context r@{edgesIn}) e v= Context r {edgesIn = addEdgeDefault edgesIn e v}
  removeEdgeIn (Context r@{edgesIn}) e v= Context r {edgesIn = removeEdgeDefault edgesIn e v}
  setEdgeIn (Context r@{edgesIn}) e va = Context r {edgesIn = setEdgeDefault edgesIn e va}

  properties = _.properties <<< unwrap

  property c = propertyDefault $ unwrap c
  addProperty (Context r) pn v = Context $ addPropertyDefault r pn v
  removeProperty (Context r) pn v = Context $ removePropertyDefault r pn v
  setProperty (Context r) pn pa = Context $ setPropertyDefault r pn pa

  comments = _.comments <<< unwrap
