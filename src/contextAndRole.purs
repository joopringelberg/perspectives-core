module Perspectives.ContextAndRole where

import Data.Array (cons, elemIndex)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap, empty, lookup, insert)
import Perspectives.Syntax (Comments(..), ContextRecord, ID, PerspectContext(..), PerspectRol(..), PropertyValueWithComments, Revision, RolRecord, noRevision, toRevision)
import Prelude (($))

-- CONTEXT

context_id :: PerspectContext -> ID
context_id (PerspectContext{_id})= _id

changeContext_id :: ID -> PerspectContext -> PerspectContext
changeContext_id id (PerspectContext cr) = PerspectContext $ cr {_id = id}

context_rev :: PerspectContext -> Maybe String
context_rev (PerspectContext{_rev}) = unNullOrUndefined _rev

changeContext_rev :: String -> PerspectContext -> PerspectContext
changeContext_rev rev (PerspectContext cr) = PerspectContext $ cr {_rev = toRevision $ Just rev}

context_rev' :: PerspectContext -> Revision
context_rev' (PerspectContext{_rev}) = _rev

context_displayName :: PerspectContext -> String
context_displayName (PerspectContext{displayName})= displayName

changeContext_displayName :: String -> PerspectContext -> PerspectContext
changeContext_displayName dn (PerspectContext cr) = PerspectContext $ cr {displayName = dn}

context_pspType :: PerspectContext -> ID
context_pspType (PerspectContext{pspType})= pspType

changeContext_type :: ID -> PerspectContext -> PerspectContext
changeContext_type tp (PerspectContext cr) = PerspectContext $ cr {pspType = tp}

context_binnenRol :: PerspectContext -> PerspectRol
context_binnenRol (PerspectContext{binnenRol})= binnenRol

context_buitenRol :: PerspectContext -> ID
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_rolInContext :: PerspectContext -> StrMap (Array ID)
context_rolInContext (PerspectContext{rolInContext})= rolInContext

addContext_rolInContext :: PerspectContext -> ID -> ID -> PerspectContext
addContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectContext cr {rolInContext = insert rolName (cons rolID roles) rolInContext}
        otherwise -> ct

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

changeRol_type :: ID -> PerspectRol -> PerspectRol
changeRol_type tp (PerspectRol cr) = PerspectRol $ cr {pspType = tp}

rol_binding :: PerspectRol -> Maybe ID
rol_binding (PerspectRol{binding}) = unNullOrUndefined binding

changeRol_binding :: ID -> PerspectRol -> PerspectRol
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = NullOrUndefined (Just b)}

rol_context :: PerspectRol -> ID
rol_context (PerspectRol{context}) = context

changeRol_context :: ID -> PerspectRol -> PerspectRol
changeRol_context cid (PerspectRol rp) = PerspectRol rp {context = cid}

rol_properties :: PerspectRol -> StrMap PropertyValueWithComments
rol_properties (PerspectRol{properties}) = properties

rol_gevuldeRollen :: PerspectRol -> StrMap (Array ID)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

rol_comments :: PerspectRol -> Comments
rol_comments (PerspectRol{comments}) = comments

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)
