module Perspectives.ContextAndRole where

import Data.Array (cons, delete, elemIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap, empty, lookup, insert)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (deconstructNamespace)
import Perspectives.PerspectivesTypesInPurescript (class Binding, class Rol, BinnenRol(..), BuitenRol(..), Context(..), ContextDef(..), PropertyDef(..), RolDef(..), RolInContext, Val)
import Perspectives.Syntax (Comments(..), ContextRecord, PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), Revision, RolRecord)
import Prelude (($))

-- CONTEXT

context_id :: PerspectContext -> Context
context_id (PerspectContext{_id})= _id

context_Namespace :: PerspectContext -> ContextDef
context_Namespace (PerspectContext{_id:(Context id)}) = ContextDef $ unsafePartial $ fromJust $ deconstructNamespace id

changeContext_id :: Context -> PerspectContext -> PerspectContext
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

context_pspType :: PerspectContext -> ContextDef
context_pspType (PerspectContext{pspType})= pspType

changeContext_type :: ContextDef -> PerspectContext -> PerspectContext
changeContext_type tp (PerspectContext cr) = PerspectContext $ cr {pspType = tp}

context_binnenRol :: PerspectContext -> PerspectRol BinnenRol BuitenRol
context_binnenRol (PerspectContext{binnenRol})= binnenRol

context_buitenRol :: PerspectContext -> BuitenRol
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_rolInContext :: forall b. Binding b => PerspectContext -> StrMap (Array RolInContext)
context_rolInContext (PerspectContext{rolInContext})= rolInContext

addContext_rolInContext :: PerspectContext -> RolDef -> RolInContext -> PerspectContext
addContext_rolInContext ct@(PerspectContext cr@{rolInContext}) (RolDef rolName) rolID =
  case lookup rolName rolInContext of
    Nothing -> PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectContext cr {rolInContext = insert rolName (cons rolID roles) rolInContext}
        otherwise -> ct

removeContext_rolInContext :: forall r b. Rol r => Binding b => PerspectContext -> RolDef -> RolInContext -> PerspectContext
removeContext_rolInContext ct@(PerspectContext cr@{rolInContext}) (RolDef rolName) rolID =
  case lookup rolName rolInContext of
    Nothing -> ct
    (Just (roles :: Array RolInContext)) -> do
      case elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectContext cr {rolInContext = insert rolName (delete rolID roles) rolInContext}

setContext_rolInContext :: PerspectContext -> RolDef -> RolInContext -> PerspectContext
setContext_rolInContext ct@(PerspectContext cr@{rolInContext}) (RolDef rolName) rolID =
  PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}

context_comments :: PerspectContext -> Comments
context_comments (PerspectContext{comments})= comments

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: Context ""
  , _rev: Nothing
  , displayName: ""
  , pspType: ContextDef ""
  , binnenRol: PerspectRol defaultBinnenRolRecord
  , buitenRol: BuitenRol ""
  , rolInContext: empty
  , comments: defaultComments
  }

defaultComments :: Comments
defaultComments = Comments { commentBefore: [], commentAfter: []}

defaultBinnenRolRecord :: RolRecord BinnenRol BuitenRol
defaultBinnenRolRecord =
  { _id: BinnenRol ""
  , pspType: RolDef ""
  , context: Context ""
  , _rev: Nothing
  , binding: Nothing
  , properties: empty
  , gevuldeRollen: empty
  , occurrence: 0
  , comments: defaultComments
  }

-- ROL

rol_id :: forall r b. Rol r => Binding b => PerspectRol r b -> r
rol_id (PerspectRol{_id}) = _id

rol_rev :: forall r b. Rol r => Binding b => PerspectRol r b -> Maybe String
rol_rev (PerspectRol{_rev}) = _rev

changeRol_rev :: forall r b. Rol r => Binding b => String -> PerspectRol r b -> PerspectRol r b
changeRol_rev rev (PerspectRol cr) = PerspectRol $ cr {_rev = Just rev}

rol_rev' :: forall r b. Rol r => Binding b => PerspectRol r b -> Revision
rol_rev' (PerspectRol{_rev}) = _rev

changeRol_rev' :: forall r b. Rol r => Binding b => Revision -> PerspectRol r b -> PerspectRol r b
changeRol_rev' rev (PerspectRol cr) = PerspectRol $ cr {_rev = rev}

rol_occurrence :: forall r b. Rol r => Binding b => PerspectRol r b -> Int
rol_occurrence (PerspectRol{occurrence}) = occurrence

rol_pspType :: forall r b. Rol r => Binding b => PerspectRol r b -> RolDef
rol_pspType (PerspectRol{pspType}) = pspType

changeRol_type :: forall r b. Rol r => Binding b => RolDef -> PerspectRol r b -> PerspectRol r b
changeRol_type tp (PerspectRol cr) = PerspectRol $ cr {pspType = tp}

rol_binding :: forall r b. Rol r => Binding b => PerspectRol r b -> Maybe b
rol_binding (PerspectRol{binding}) = binding

changeRol_binding :: forall r b. Rol r => Binding b => b -> PerspectRol r b -> PerspectRol r b
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = (Just b)}

rol_context :: forall r b. Rol r => Binding b => PerspectRol r b -> Context
rol_context (PerspectRol{context}) = context

changeRol_context :: forall r b. Rol r => Binding b => Context -> PerspectRol r b -> PerspectRol r b
changeRol_context cid (PerspectRol rp) = PerspectRol rp {context = cid}

rol_properties :: forall r b. Rol r => Binding b => PerspectRol r b -> StrMap PropertyValueWithComments
rol_properties (PerspectRol{properties}) = properties

addRol_property :: forall r b. Rol r => Binding b => PerspectRol r b -> PropertyDef -> Val -> PerspectRol r b
addRol_property rl@(PerspectRol rp@{properties}) (PropertyDef propertyName) value =
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

removeRol_property :: forall r b. Rol r => Binding b => PerspectRol r b -> PropertyDef -> Val -> PerspectRol r b
removeRol_property rl@(PerspectRol rp@{properties}) (PropertyDef propertyName) value =
  case lookup propertyName properties of
    Nothing -> rl
    (Just (PropertyValueWithComments pvc@{value: values})) -> do
      case elemIndex value values of
        Nothing -> rl
        otherwise -> PerspectRol rp {properties = insert
          propertyName
          (PropertyValueWithComments pvc {value = (delete value values)})
          properties}

setRol_property :: forall r b. Rol r => Binding b => PerspectRol r b -> PropertyDef -> Val -> PerspectRol r b
setRol_property rl@(PerspectRol rp@{properties}) (PropertyDef propertyName) value =
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

rol_gevuldeRollen :: forall r b. Rol r => Binding b => PerspectRol r b -> StrMap (Array r)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

addRol_gevuldeRollen :: forall r b. Rol r => Binding b => PerspectRol r b -> RolDef -> r -> PerspectRol r b
addRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) (RolDef rolName) rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> PerspectRol cr {gevuldeRollen = insert rolName [rolID] gevuldeRollen}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectRol cr {gevuldeRollen = insert rolName (cons rolID roles) gevuldeRollen}
        otherwise -> ct

removeRol_gevuldeRollen :: forall r b. Rol r => Binding b => PerspectRol r b -> RolDef -> r -> PerspectRol r b
removeRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) (RolDef rolName) rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> ct
    (Just (roles :: Array r)) -> do
      case elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectRol cr {gevuldeRollen = insert rolName (delete rolID roles) gevuldeRollen}

rol_comments :: forall r b. Rol r => Binding b => PerspectRol r b -> Comments
rol_comments (PerspectRol{comments}) = comments

compareOccurrences :: forall r b. Rol r => Binding b => PerspectRol r b -> PerspectRol r b-> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)
