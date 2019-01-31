module Perspectives.ContextAndRole where

import Data.Array (cons, delete, elemIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap, empty, lookup, insert)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (deconstructNamespace)
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolType, BinnenRol(..), BuitenRol(..), Context(..), ContextDef(..), PropertyDef(..), RolDef(..), RolInContext, Val)
import Perspectives.Syntax (Comments(..), ContextRecord, PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), Revision, RolRecord)
import Prelude (($))

-- CONTEXT

context_id :: forall c. ContextType c => PerspectContext c -> c
context_id (PerspectContext{_id})= _id

context_Namespace :: forall c. ContextType c => PerspectContext c -> ContextDef
context_Namespace (PerspectContext{_id}) = ContextDef $ unsafePartial $ fromJust $ deconstructNamespace (unwrap _id)

changeContext_id :: forall c1 c2. ContextType c1 => ContextType c2 => c1 -> PerspectContext c2 -> PerspectContext c1
changeContext_id id (PerspectContext cr) = PerspectContext $ cr {_id = id}

context_rev :: forall c. ContextType c => PerspectContext c -> Maybe String
context_rev (PerspectContext{_rev}) = _rev

changeContext_rev :: forall c. ContextType c => String -> PerspectContext c -> PerspectContext c
changeContext_rev rev (PerspectContext cr) = PerspectContext $ cr {_rev = Just rev}

context_rev' :: forall c. ContextType c => PerspectContext c -> Revision
context_rev' (PerspectContext{_rev}) = _rev

changeContext_rev' :: forall c. ContextType c => Revision -> PerspectContext c -> PerspectContext c
changeContext_rev' rev (PerspectContext cr) = PerspectContext $ cr {_rev = rev}

context_displayName :: forall c. ContextType c => PerspectContext c -> String
context_displayName (PerspectContext{displayName})= displayName

changeContext_displayName :: forall c. ContextType c => String -> PerspectContext c -> PerspectContext c
changeContext_displayName dn (PerspectContext cr) = PerspectContext $ cr {displayName = dn}

context_pspType :: forall c. ContextType c => PerspectContext c -> ContextDef
context_pspType (PerspectContext{pspType})= pspType

changeContext_type :: forall c. ContextType c => ContextDef -> PerspectContext c -> PerspectContext c
changeContext_type tp (PerspectContext cr) = PerspectContext $ cr {pspType = tp}

context_binnenRol :: forall c. ContextType c => PerspectContext c -> PerspectRol BinnenRol BuitenRol
context_binnenRol (PerspectContext{binnenRol})= binnenRol

context_buitenRol :: forall c. ContextType c => PerspectContext c -> BuitenRol
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_rolInContext :: forall c b. ContextType b => Binding b => PerspectContext c -> StrMap (Array RolInContext)
context_rolInContext (PerspectContext{rolInContext})= rolInContext

addContext_rolInContext :: forall c. ContextType c => PerspectContext c -> RolDef -> RolInContext -> PerspectContext c
addContext_rolInContext ct@(PerspectContext cr@{rolInContext}) (RolDef rolName) rolID =
  case lookup rolName rolInContext of
    Nothing -> PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectContext cr {rolInContext = insert rolName (cons rolID roles) rolInContext}
        otherwise -> ct

removeContext_rolInContext :: forall c r b. ContextType c => RolType r => Binding b => PerspectContext c -> RolDef -> RolInContext -> PerspectContext c
removeContext_rolInContext ct@(PerspectContext cr@{rolInContext}) (RolDef rolName) rolID =
  case lookup rolName rolInContext of
    Nothing -> ct
    (Just (roles :: Array RolInContext)) -> do
      case elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectContext cr {rolInContext = insert rolName (delete rolID roles) rolInContext}

setContext_rolInContext :: forall c. ContextType c => PerspectContext c -> RolDef -> RolInContext -> PerspectContext c
setContext_rolInContext ct@(PerspectContext cr@{rolInContext}) (RolDef rolName) rolID =
  PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}

context_comments :: forall c. ContextType c => PerspectContext c -> Comments
context_comments (PerspectContext{comments})= comments

defaultContextRecord :: forall c. ContextType c => c -> ContextRecord c
defaultContextRecord id =
  { _id: id
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

rol_id :: forall r b. RolType r => Binding b => PerspectRol r b -> r
rol_id (PerspectRol{_id}) = _id

rol_rev :: forall r b. RolType r => Binding b => PerspectRol r b -> Maybe String
rol_rev (PerspectRol{_rev}) = _rev

changeRol_rev :: forall r b. RolType r => Binding b => String -> PerspectRol r b -> PerspectRol r b
changeRol_rev rev (PerspectRol cr) = PerspectRol $ cr {_rev = Just rev}

rol_rev' :: forall r b. RolType r => Binding b => PerspectRol r b -> Revision
rol_rev' (PerspectRol{_rev}) = _rev

changeRol_rev' :: forall r b. RolType r => Binding b => Revision -> PerspectRol r b -> PerspectRol r b
changeRol_rev' rev (PerspectRol cr) = PerspectRol $ cr {_rev = rev}

rol_occurrence :: forall r b. RolType r => Binding b => PerspectRol r b -> Int
rol_occurrence (PerspectRol{occurrence}) = occurrence

rol_pspType :: forall r b. RolType r => Binding b => PerspectRol r b -> RolDef
rol_pspType (PerspectRol{pspType}) = pspType

changeRol_type :: forall r b. RolType r => Binding b => RolDef -> PerspectRol r b -> PerspectRol r b
changeRol_type tp (PerspectRol cr) = PerspectRol $ cr {pspType = tp}

rol_binding :: forall r b. RolType r => Binding b => PerspectRol r b -> Maybe b
rol_binding (PerspectRol{binding}) = binding

changeRol_binding :: forall r b. RolType r => Binding b => b -> PerspectRol r b -> PerspectRol r b
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = (Just b)}

rol_context :: forall r b. RolType r => Binding b => PerspectRol r b -> Context
rol_context (PerspectRol{context}) = context

changeRol_context :: forall r b. RolType r => Binding b => Context -> PerspectRol r b -> PerspectRol r b
changeRol_context cid (PerspectRol rp) = PerspectRol rp {context = cid}

rol_properties :: forall r b. RolType r => Binding b => PerspectRol r b -> StrMap PropertyValueWithComments
rol_properties (PerspectRol{properties}) = properties

addRol_property :: forall r b. RolType r => Binding b => PerspectRol r b -> PropertyDef -> Val -> PerspectRol r b
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

removeRol_property :: forall r b. RolType r => Binding b => PerspectRol r b -> PropertyDef -> Val -> PerspectRol r b
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

setRol_property :: forall r b. RolType r => Binding b => PerspectRol r b -> PropertyDef -> Val -> PerspectRol r b
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

rol_gevuldeRollen :: forall r b. RolType r => Binding b => PerspectRol r b -> StrMap (Array r)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

addRol_gevuldeRollen :: forall r b. RolType r => Binding b => PerspectRol r b -> RolDef -> r -> PerspectRol r b
addRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) (RolDef rolName) rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> PerspectRol cr {gevuldeRollen = insert rolName [rolID] gevuldeRollen}
    (Just roles) -> do
      case elemIndex rolID roles of
        Nothing -> PerspectRol cr {gevuldeRollen = insert rolName (cons rolID roles) gevuldeRollen}
        otherwise -> ct

removeRol_gevuldeRollen :: forall r b. RolType r => Binding b => PerspectRol r b -> RolDef -> r -> PerspectRol r b
removeRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) (RolDef rolName) rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> ct
    (Just (roles :: Array r)) -> do
      case elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectRol cr {gevuldeRollen = insert rolName (delete rolID roles) gevuldeRollen}

rol_comments :: forall r b. RolType r => Binding b => PerspectRol r b -> Comments
rol_comments (PerspectRol{comments}) = comments

compareOccurrences :: forall r b. RolType r => Binding b => PerspectRol r b -> PerspectRol r b-> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)
