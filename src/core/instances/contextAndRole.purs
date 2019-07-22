module Perspectives.ContextAndRole where

import Data.Array (cons, delete, elemIndex, snoc) as Arr
import Data.Array (last)
import Data.Int (floor, fromString, toNumber)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Ord (Ordering, compare)
import Data.String (Pattern(..), lastIndexOf, splitAt)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, insert, lookup, pop)
import Math (ln10, log)
import Partial.Unsafe (unsafePartial)
import Perspectives.EntiteitAndRDFAliases (PropertyName, RolName)
import Perspectives.Identifiers (Namespace, deconstructNamespace)
import Perspectives.InstanceRepresentation (ContextRecord, PerspectContext(..), PerspectRol(..), RolRecord)
import Perspectives.Representation.Class.Revision (Revision_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Prelude (identity, ($), (/), (<>), show, (+))

-- CONTEXT

context_id :: PerspectContext -> ContextInstance
context_id (PerspectContext{_id})= _id

context_Namespace :: PerspectContext -> Namespace
context_Namespace (PerspectContext{_id}) = unsafePartial $ fromJust $ deconstructNamespace (unwrap _id)

changeContext_id :: ContextInstance -> PerspectContext -> PerspectContext
changeContext_id id (PerspectContext cr) = PerspectContext $ cr {_id = id}

context_rev :: PerspectContext -> Maybe String
context_rev (PerspectContext{_rev}) = _rev

changeContext_rev :: String -> PerspectContext -> PerspectContext
changeContext_rev rev (PerspectContext cr) = PerspectContext $ cr {_rev = Just rev}

context_rev' :: PerspectContext -> Revision_
context_rev' (PerspectContext{_rev}) = _rev

changeContext_rev' :: Revision_ -> PerspectContext -> PerspectContext
changeContext_rev' rev (PerspectContext cr) = PerspectContext $ cr {_rev = rev}

context_displayName :: PerspectContext -> String
context_displayName (PerspectContext{displayName})= displayName

changeContext_displayName :: String -> PerspectContext -> PerspectContext
changeContext_displayName dn (PerspectContext cr) = PerspectContext $ cr {displayName = dn}

context_pspType :: PerspectContext -> ContextType
context_pspType (PerspectContext{pspType})= pspType

changeContext_type :: String -> PerspectContext -> PerspectContext
changeContext_type tp (PerspectContext cr) = PerspectContext $ cr {pspType = ContextType tp}

context_buitenRol :: PerspectContext -> RoleInstance
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_iedereRolInContext :: PerspectContext -> Object (Array RoleInstance)
context_iedereRolInContext (PerspectContext{rolInContext})= rolInContext

context_rolInContext :: PerspectContext -> EnumeratedRoleType -> Array RoleInstance
context_rolInContext (PerspectContext{rolInContext}) rn = maybe [] identity (lookup (unwrap rn) rolInContext)

addContext_rolInContext :: PerspectContext -> RolName -> RoleInstance -> PerspectContext
addContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}
    (Just roles) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> PerspectContext cr {rolInContext = insert rolName (Arr.snoc roles rolID) rolInContext}
        otherwise -> ct

removeContext_rolInContext :: PerspectContext -> RolName -> RoleInstance -> PerspectContext
removeContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> ct
    (Just (roles :: Array RoleInstance)) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectContext cr {rolInContext = insert rolName (Arr.delete rolID roles) rolInContext}

setContext_rolInContext :: PerspectContext -> RolName -> RoleInstance -> PerspectContext
setContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}

context_changeRolIdentifier :: PerspectContext -> RolName -> RolName -> PerspectContext
context_changeRolIdentifier ct@(PerspectContext cr@{rolInContext}) oldName newName =
  case pop oldName rolInContext of
    Nothing -> ct
    (Just (Tuple vs cr')) -> PerspectContext cr {rolInContext = insert newName vs cr'}

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ContextInstance ""
  , _rev: Nothing
  , displayName: ""
  , pspType: ContextType ""
  , buitenRol: RoleInstance ""
  , rolInContext: empty
  }

defaultRolRecord :: RolRecord
defaultRolRecord =
  { _id: RoleInstance ""
  , pspType: EnumeratedRoleType ""
  , context: ContextInstance ""
  , _rev: Nothing
  , binding: Nothing
  , properties: empty
  , gevuldeRollen: empty
  , occurrence: 0
  }

-- ROL

rol_id :: PerspectRol -> RoleInstance
rol_id (PerspectRol{_id}) = _id

rol_rev :: PerspectRol -> Maybe String
rol_rev (PerspectRol{_rev}) = _rev

changeRol_rev :: String -> PerspectRol -> PerspectRol
changeRol_rev rev (PerspectRol cr) = PerspectRol $ cr {_rev = Just rev}

rol_rev' :: PerspectRol -> Revision_
rol_rev' (PerspectRol{_rev}) = _rev

changeRol_rev' :: Revision_ -> PerspectRol -> PerspectRol
changeRol_rev' rev (PerspectRol cr) = PerspectRol $ cr {_rev = rev}

rol_occurrence :: PerspectRol -> Int
rol_occurrence (PerspectRol{occurrence}) = occurrence

rol_pspType :: PerspectRol -> EnumeratedRoleType
rol_pspType (PerspectRol{pspType}) = pspType

changeRol_type :: String -> PerspectRol -> PerspectRol
changeRol_type tp (PerspectRol cr) = PerspectRol $ cr {pspType = EnumeratedRoleType tp}

rol_binding :: PerspectRol -> Maybe RoleInstance
rol_binding (PerspectRol{binding}) = binding

changeRol_binding :: RoleInstance -> PerspectRol -> PerspectRol
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = (Just b)}

removeRol_binding :: PerspectRol -> PerspectRol
removeRol_binding (PerspectRol cr) = PerspectRol $ cr {binding = Nothing}

rol_context :: PerspectRol -> ContextInstance
rol_context (PerspectRol{context}) = context

changeRol_context :: ContextInstance -> PerspectRol -> PerspectRol
changeRol_context cid (PerspectRol rp) = PerspectRol rp {context = cid}

rol_properties :: PerspectRol -> Object (Array Value)
rol_properties (PerspectRol{properties}) = properties

rol_property :: PerspectRol -> PropertyName -> Array Value
rol_property (PerspectRol{properties}) pn = maybe [] identity (lookup pn properties)

addRol_property :: PerspectRol -> PropertyName -> Value -> PerspectRol
addRol_property rl@(PerspectRol rp@{properties}) propertyName value =
  case lookup propertyName properties of
    Nothing -> PerspectRol rp {properties = insert
      propertyName
      [value]
      properties}
    (Just values) -> do
      case Arr.elemIndex value values of
        Nothing -> PerspectRol rp {properties = insert
          propertyName
          (Arr.cons value values)
          properties}
        otherwise -> rl

removeRol_property :: PerspectRol -> PropertyName -> Value -> PerspectRol
removeRol_property rl@(PerspectRol rp@{properties}) propertyName value =
  case lookup propertyName properties of
    Nothing -> rl
    (Just values) -> do
      case Arr.elemIndex value values of
        Nothing -> rl
        otherwise -> PerspectRol rp {properties = insert
          propertyName
          (Arr.delete value values)
          properties}

setRol_property :: PerspectRol -> PropertyName -> Value -> PerspectRol
setRol_property rl@(PerspectRol rp@{properties}) propertyName value =
  case lookup propertyName properties of
    Nothing -> PerspectRol rp {properties = insert
      propertyName
      [value]
      properties}
    (Just pvc) -> do
      PerspectRol rp {properties = insert
          propertyName
          [value]
          properties}

rol_gevuldeRollen :: PerspectRol -> Object (Array RoleInstance)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

rol_gevuldeRol :: PerspectRol -> RolName -> Array RoleInstance
rol_gevuldeRol  (PerspectRol{gevuldeRollen}) rn = maybe [] identity (lookup rn gevuldeRollen)

addRol_gevuldeRollen :: PerspectRol -> RolName -> RoleInstance -> PerspectRol
addRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> PerspectRol cr {gevuldeRollen = insert rolName [rolID] gevuldeRollen}
    (Just roles) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> PerspectRol cr {gevuldeRollen = insert rolName (Arr.snoc roles rolID) gevuldeRollen}
        otherwise -> ct

removeRol_gevuldeRollen :: PerspectRol -> RolName -> RoleInstance -> PerspectRol
removeRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> ct
    (Just (roles :: Array RoleInstance)) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectRol cr {gevuldeRollen = insert rolName (Arr.delete rolID roles) gevuldeRollen}

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)

-- Returns a string representation of the integer, left padded with zero's.
rol_padOccurrence :: Int -> String
rol_padOccurrence n =  case floor( log( toNumber n) / ln10 ) of
  0 -> "000" <> show n -- 1 position
  1 -> "00" <> show n
  2 -> "0" <> show n
  _ -> show n

-- Assume the Array is sorted alphabetically. Role index numbers follow the (last) underscore ("_") and are left padded with zeroes.
getNextRolIndex :: Array RoleInstance -> Int
getNextRolIndex rolIds = case last rolIds of
  Nothing -> 0
  (Just id) -> case lastIndexOf (Pattern "_") (unwrap id) of
    Nothing -> 0
    (Just n) -> let {after} = splitAt (n + 1) (unwrap id) in
      case fromString after of
        Nothing -> 0
        (Just x) -> x + 1
