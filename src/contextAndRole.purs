module Perspectives.ContextAndRole where

import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap, empty)
import Perspectives.Syntax (Comments(..), ContextRecord, ID, PerspectContext(..), PerspectRol(..), PropertyValueWithComments, Revision, RolRecord, noRevision)

-- CONTEXT

context_id :: PerspectContext -> ID
context_id (PerspectContext{_id})= _id

context_rev :: PerspectContext -> Maybe String
context_rev (PerspectContext{_rev}) = unNullOrUndefined _rev

context_rev' :: PerspectContext -> Revision
context_rev' (PerspectContext{_rev}) = _rev

context_displayName :: PerspectContext -> String
context_displayName (PerspectContext{displayName})= displayName

context_pspType :: PerspectContext -> ID
context_pspType (PerspectContext{pspType})= pspType

context_binnenRol :: PerspectContext -> PerspectRol
context_binnenRol (PerspectContext{binnenRol})= binnenRol

context_buitenRol :: PerspectContext -> ID
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_rolInContext :: PerspectContext -> StrMap (Array ID)
context_rolInContext (PerspectContext{rolInContext})= rolInContext

context_comments :: PerspectContext -> Comments
context_comments (PerspectContext{comments})= comments

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ""
  , _rev: noRevision
  , displayName: ""
  , pspType: ""
  , binnenRol: PerspectRol defaultRolRecord
  , buitenRol: ""
  , rolInContext: empty
  , comments: defaultComments
  }

defaultComments :: Comments
defaultComments = Comments { commentBefore: [], commentAfter: []}

defaultRolRecord :: RolRecord
defaultRolRecord =
  { _id: ""
  , pspType: ""
  , context: ""
  , _rev: noRevision
  , binding: NullOrUndefined Nothing
  , properties: empty
  , gevuldeRollen: empty
  , occurrence: 0
  , comments: defaultComments
  }

-- ROL

rol_id :: PerspectRol -> ID
rol_id (PerspectRol{_id}) = _id

rol_rev :: PerspectRol -> Maybe String
rol_rev (PerspectRol{_rev}) = unNullOrUndefined _rev

rol_rev' :: PerspectRol -> Revision
rol_rev' (PerspectRol{_rev}) = _rev

rol_occurrence :: PerspectRol -> Int
rol_occurrence (PerspectRol{occurrence}) = occurrence

rol_pspType :: PerspectRol -> ID
rol_pspType (PerspectRol{pspType}) = pspType

rol_binding :: PerspectRol -> Maybe ID
rol_binding (PerspectRol{binding}) = unNullOrUndefined binding

rol_context :: PerspectRol -> ID
rol_context (PerspectRol{context}) = context

rol_properties :: PerspectRol -> StrMap PropertyValueWithComments
rol_properties (PerspectRol{properties}) = properties

rol_gevuldeRollen :: PerspectRol -> StrMap (Array ID)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

rol_comments :: PerspectRol -> Comments
rol_comments (PerspectRol{comments}) = comments

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)
