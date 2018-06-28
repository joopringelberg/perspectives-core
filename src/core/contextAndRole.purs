module Perspectives.ContextAndRole where

import Perspectives.EntiteitAndRDFAliases
import Data.Array (cons, delete, elemIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap, empty, lookup, insert)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (Namespace, deconstructNamespace)
import Perspectives.Syntax (Comments(..), ContextRecord, PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), Revision, RolRecord)
import Prelude (($))

-- CONTEXT

context_id :: PerspectContext -> ContextID
context_id (PerspectContext{_id})= _id

context_Namespace :: PerspectContext -> Namespace
context_Namespace (PerspectContext{_id}) = unsafePartial $ fromJust $ deconstructNamespace _id

changeContext_id :: ContextID -> PerspectContext -> PerspectContext
changeContext_id id (PerspectContext cr) = PerspectContext $ cr {_id = id}

context_rev :: PerspectContext -> Maybe String
context_rev (PerspectContext{_rev}) = _rev

changeContext_rev :: String -> PerspectContext -> PerspectContext
changeContext_rev rev (PerspectContext cr) = PerspectContext $ cr {_rev = Just rev}

context_rev' :: PerspectContext -> Revision
context_rev' (PerspectContext{_rev}) = _rev

changeContext_rev' :: Revision -> PerspectContext -> PerspectContext
changeContext_rev' rev (PerspectContext cr) = PerspectContext $ cr {_rev = rev}

context_displayName :: PerspectContext -> String
context_displayName (PerspectContext{displayName})= displayName

changeContext_displayName :: String -> PerspectContext -> PerspectContext
changeContext_displayName dn (PerspectContext cr) = PerspectContext $ cr {displayName = dn}

context_pspType :: PerspectContext -> ContextID
context_pspType (PerspectContext{pspType})= pspType

changeContext_type :: ContextID -> PerspectContext -> PerspectContext
changeContext_type tp (PerspectContext cr) = PerspectContext $ cr {pspType = tp}

context_binnenRol :: PerspectContext -> PerspectRol
context_binnenRol (PerspectContext{binnenRol})= binnenRol

context_buitenRol :: PerspectContext -> RolID
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_rolInContext :: PerspectContext -> StrMap (Array RolID)
context_rolInContext (PerspectContext{rolInContext})= rolInContext

addContext_rolInContext :: PerspectContext -> RolName -> RolID -> PerspectContext
addContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectContext cr {rolInContext = insert rolName (cons rolID roles) rolInContext}
        otherwise -> ct

removeContext_rolInContext :: PerspectContext -> RolName -> RolID -> PerspectContext
removeContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> ct
    (Just (roles :: Array RolID)) -> do
      case elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectContext cr {rolInContext = insert rolName (delete rolID roles) rolInContext}

setContext_rolInContext :: PerspectContext -> RolName -> RolID -> PerspectContext
setContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}

context_comments :: PerspectContext -> Comments
context_comments (PerspectContext{comments})= comments

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ""
  , _rev: Nothing
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
  , _rev: Nothing
  , binding: Nothing
  , properties: empty
  , gevuldeRollen: empty
  , occurrence: 0
  , comments: defaultComments
  }

-- ROL

rol_id :: PerspectRol -> RolID
rol_id (PerspectRol{_id}) = _id

rol_rev :: PerspectRol -> Maybe String
rol_rev (PerspectRol{_rev}) = _rev

changeRol_rev :: String -> PerspectRol -> PerspectRol
changeRol_rev rev (PerspectRol cr) = PerspectRol $ cr {_rev = Just rev}

rol_rev' :: PerspectRol -> Revision
rol_rev' (PerspectRol{_rev}) = _rev

changeRol_rev' :: Revision -> PerspectRol -> PerspectRol
changeRol_rev' rev (PerspectRol cr) = PerspectRol $ cr {_rev = rev}

rol_occurrence :: PerspectRol -> Int
rol_occurrence (PerspectRol{occurrence}) = occurrence

rol_pspType :: PerspectRol -> ContextID
rol_pspType (PerspectRol{pspType}) = pspType

changeRol_type :: ContextID -> PerspectRol -> PerspectRol
changeRol_type tp (PerspectRol cr) = PerspectRol $ cr {pspType = tp}

rol_binding :: PerspectRol -> Maybe RolID
rol_binding (PerspectRol{binding}) = binding

changeRol_binding :: RolID -> PerspectRol -> PerspectRol
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = (Just b)}

rol_context :: PerspectRol -> ContextID
rol_context (PerspectRol{context}) = context

changeRol_context :: ContextID -> PerspectRol -> PerspectRol
changeRol_context cid (PerspectRol rp) = PerspectRol rp {context = cid}

rol_properties :: PerspectRol -> StrMap PropertyValueWithComments
rol_properties (PerspectRol{properties}) = properties

addRol_property :: PerspectRol -> PropertyName -> Value -> PerspectRol
addRol_property rl@(PerspectRol rp@{properties}) propertyName value =
  case lookup propertyName properties of
    Nothing -> PerspectRol rp {properties = insert
      propertyName
      (PropertyValueWithComments {value: [value], commentBefore: [], commentAfter: [] })
      properties}
    (Just (PropertyValueWithComments pvc@{value: values})) -> do
      case elemIndex value values of
        Nothing -> PerspectRol rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = (cons value values)})
          properties}
        otherwise -> rl

removeRol_property :: PerspectRol -> PropertyName -> Value -> PerspectRol
removeRol_property rl@(PerspectRol rp@{properties}) propertyName value =
  case lookup propertyName properties of
    Nothing -> rl
    (Just (PropertyValueWithComments pvc@{value: values})) -> do
      case elemIndex value values of
        Nothing -> rl
        otherwise -> PerspectRol rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = (delete value values)})
          properties}

setRol_property :: PerspectRol -> PropertyName -> Value -> PerspectRol
setRol_property rl@(PerspectRol rp@{properties}) propertyName value =
  case lookup propertyName properties of
    Nothing -> PerspectRol rp {properties = insert
      propertyName
      (PropertyValueWithComments {value: [value], commentBefore: [], commentAfter: [] })
      properties}
    (Just (PropertyValueWithComments pvc)) -> do
      PerspectRol rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = [value]})
          properties}

rol_gevuldeRollen :: PerspectRol -> StrMap (Array RolID)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

addRol_gevuldeRollen :: PerspectRol -> RolName -> RolID -> PerspectRol
addRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> PerspectRol cr {gevuldeRollen = insert rolName [rolID] gevuldeRollen}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectRol cr {gevuldeRollen = insert rolName (cons rolID roles) gevuldeRollen}
        otherwise -> ct

removeRol_gevuldeRollen :: PerspectRol -> RolName -> RolID -> PerspectRol
removeRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> ct
    (Just (roles :: Array RolID)) -> do
      case elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectRol cr {gevuldeRollen = insert rolName (delete rolID roles) gevuldeRollen}

rol_comments :: PerspectRol -> Comments
rol_comments (PerspectRol{comments}) = comments

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)
