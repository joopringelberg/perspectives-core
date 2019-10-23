module Perspectives.Graph.Class.Vertex where

import Data.Array (cons, delete, elemIndex, snoc) as Arr
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object, lookup, insert)
import Perspectives.Graph.Base (Comments, EdgeId, PropertyName, PropertyValueWithComments(..), Revision, Value, VertexId, propertyValue)
import Prelude (identity)

class Vertex v where
  identifier :: v -> VertexId
  -- getVertex :: VertexId -> v

  rev :: v -> Revision
  changeRevision :: String -> v -> v

  displayName :: v -> String
  changeDisplayName :: String -> v -> v

  pspType :: v -> VertexId
  -- pspTypeVertex :: v -> v

  prototype :: v -> VertexId
  changePrototype :: VertexId -> v -> v

  edgeLabelsOut :: v -> Array VertexId
  edgeLabelsIn :: v -> Array VertexId

  edgeOut :: v -> EdgeId -> Array VertexId
  -- edgeOutVertices :: v -> EdgeId -> Array v
  addEdgeOut :: v -> EdgeId -> VertexId -> v
  removeEdgeOut :: v -> EdgeId -> VertexId -> v
  setEdgeOut :: v -> EdgeId -> Array VertexId -> v

  edgeIn :: v -> EdgeId -> Array VertexId
  -- edgeInVertices :: v -> EdgeId -> Array v
  addEdgeIn :: v -> EdgeId -> VertexId -> v
  removeEdgeIn :: v -> EdgeId -> VertexId -> v
  setEdgeIn :: v -> EdgeId -> Array VertexId -> v

  properties :: v -> Object PropertyValueWithComments

  property :: v -> PropertyName -> Array Value
  addProperty :: v -> PropertyName -> Value -> v
  removeProperty :: v -> PropertyName -> Value -> v
  setProperty :: v -> PropertyName -> Array Value -> v

  comments :: v -> Comments

changeRevisionDefault :: forall f. Maybe String -> {_rev :: Revision | f} -> {_rev :: Revision | f}
changeRevisionDefault rev cr = cr {_rev = rev}

changeDisplayNameDefault :: forall f. String -> {displayName :: String | f} -> {displayName :: String | f}
changeDisplayNameDefault n cr = cr {displayName = n}

changePrototypeDefault :: forall f. String -> {prototype :: VertexId | f} -> {prototype :: VertexId | f}
changePrototypeDefault p cr = cr {prototype = p}

edgeOutDefault :: forall f. {edgesOut :: Object (Array VertexId) | f} -> EdgeId -> Array VertexId
edgeOutDefault {edgesOut} rn = maybe [] identity (lookup rn edgesOut)

edgeInDefault :: forall f. {edgesIn :: Object (Array VertexId) | f} -> EdgeId -> Array VertexId
edgeInDefault {edgesIn} rn = maybe [] identity (lookup rn edgesIn)

type Edges = Object (Array VertexId)

addEdgeDefault :: Edges -> EdgeId -> VertexId -> Edges
addEdgeDefault edges edgeId vertexId =
  case lookup edgeId edges of
    Nothing -> insert edgeId [vertexId] edges
    (Just roles) -> do
      case Arr.elemIndex vertexId roles of
        Nothing -> insert edgeId (Arr.snoc roles vertexId) edges
        otherwise -> edges

removeEdgeDefault :: Edges -> EdgeId -> VertexId -> Edges
removeEdgeDefault edges edgeId vertexId =
  case lookup edgeId edges of
    Nothing -> edges
    (Just (roles :: Array EdgeId)) -> do
      case Arr.elemIndex vertexId roles of
        Nothing -> edges
        otherwise -> insert edgeId (Arr.delete vertexId roles) edges

setEdgeDefault :: Edges -> EdgeId -> Array VertexId -> Edges
setEdgeDefault edges edgeId vertices = insert edgeId vertices edges


type RecordWithProperties f = {properties :: Object PropertyValueWithComments | f}

propertyDefault :: forall f. RecordWithProperties f -> PropertyName -> Array Value
propertyDefault {properties} pn = maybe [] propertyValue (lookup pn properties)

addPropertyDefault :: forall f. RecordWithProperties f -> PropertyName -> Value -> RecordWithProperties f
addPropertyDefault rp@{properties} propertyName value =
  case lookup propertyName properties of
    Nothing -> rp {properties = insert
      propertyName
      (PropertyValueWithComments {value: [value], commentBefore: [], commentAfter: [] })
      properties}
    (Just (PropertyValueWithComments pvc@{value: values})) -> do
      case Arr.elemIndex value values of
        Nothing -> rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = (Arr.cons value values)})
          properties}
        otherwise -> rp

removePropertyDefault :: forall f. RecordWithProperties f -> PropertyName -> Value -> RecordWithProperties f
removePropertyDefault rp@{properties} propertyName value =
  case lookup propertyName properties of
    Nothing -> rp
    (Just (PropertyValueWithComments pvc@{value: values})) -> do
      case Arr.elemIndex value values of
        Nothing -> rp
        otherwise -> rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = (Arr.delete value values)})
          properties}

setPropertyDefault :: forall f. RecordWithProperties f -> PropertyName -> Array Value -> RecordWithProperties f
setPropertyDefault rp@{properties} propertyName value =
  case lookup propertyName properties of
    Nothing -> rp {properties = insert
      propertyName
      (PropertyValueWithComments {value: value, commentBefore: [], commentAfter: [] })
      properties}
    (Just (PropertyValueWithComments pvc)) -> do
      rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = value})
          properties}
