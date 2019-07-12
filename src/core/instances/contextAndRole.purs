module Perspectives.ContextAndRole where

import Data.Array (cons, delete, elemIndex, snoc) as Arr
import Data.Array (last)
import Data.Int (floor, fromString, toNumber)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Ord (Ordering, compare)
import Data.String (Pattern(..), lastIndexOf, splitAt)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, insert, lookup, pop)
import Math (ln10, log)
import Partial.Unsafe (unsafePartial)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName, RolID, RolName, Value)
import Perspectives.Identifiers (Namespace, deconstructNamespace)
import Perspectives.InstanceRepresentation (ContextRecord, PerspectContext(..), PerspectRol(..), Revision, RolRecord)
import Prelude (identity, ($), (/), (<>), show, (+))

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

context_buitenRol :: PerspectContext -> RolID
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_iedereRolInContext :: PerspectContext -> Object (Array RolID)
context_iedereRolInContext (PerspectContext{rolInContext})= rolInContext

context_rolInContext :: PerspectContext -> RolName -> Array Value
context_rolInContext (PerspectContext{rolInContext}) rn = maybe [] identity (lookup rn rolInContext)

addContext_rolInContext :: PerspectContext -> RolName -> RolID -> PerspectContext
addContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}
    (Just roles) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> PerspectContext cr {rolInContext = insert rolName (Arr.snoc roles rolID) rolInContext}
        otherwise -> ct

removeContext_rolInContext :: PerspectContext -> RolName -> RolID -> PerspectContext
removeContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  case lookup rolName rolInContext of
    Nothing -> ct
    (Just (roles :: Array RolID)) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectContext cr {rolInContext = insert rolName (Arr.delete rolID roles) rolInContext}

setContext_rolInContext :: PerspectContext -> RolName -> RolID -> PerspectContext
setContext_rolInContext ct@(PerspectContext cr@{rolInContext}) rolName rolID =
  PerspectContext cr {rolInContext = insert rolName [rolID] rolInContext}

context_changeRolIdentifier :: PerspectContext -> RolName -> RolName -> PerspectContext
context_changeRolIdentifier ct@(PerspectContext cr@{rolInContext}) oldName newName =
  case pop oldName rolInContext of
    Nothing -> ct
    (Just (Tuple vs cr')) -> PerspectContext cr {rolInContext = insert newName vs cr'}

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ""
  , _rev: Nothing
  , displayName: ""
  , pspType: ""
  , buitenRol: ""
  , rolInContext: empty
  }

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

removeRol_binding :: PerspectRol -> PerspectRol
removeRol_binding (PerspectRol cr) = PerspectRol $ cr {binding = Nothing}

rol_context :: PerspectRol -> ContextID
rol_context (PerspectRol{context}) = context

changeRol_context :: ContextID -> PerspectRol -> PerspectRol
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

rol_gevuldeRollen :: PerspectRol -> Object (Array RolID)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

rol_gevuldeRol :: PerspectRol -> RolName -> Array RolID
rol_gevuldeRol  (PerspectRol{gevuldeRollen}) rn = maybe [] identity (lookup rn gevuldeRollen)

addRol_gevuldeRollen :: PerspectRol -> RolName -> RolID -> PerspectRol
addRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> PerspectRol cr {gevuldeRollen = insert rolName [rolID] gevuldeRollen}
    (Just roles) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> PerspectRol cr {gevuldeRollen = insert rolName (Arr.snoc roles rolID) gevuldeRollen}
        otherwise -> ct

removeRol_gevuldeRollen :: PerspectRol -> RolName -> RolID -> PerspectRol
removeRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup rolName gevuldeRollen of
    Nothing -> ct
    (Just (roles :: Array RolID)) -> do
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
getNextRolIndex :: Array RolID -> Int
getNextRolIndex rolIds = case last rolIds of
  Nothing -> 0
  (Just id) -> case lastIndexOf (Pattern "_") id of
    Nothing -> 0
    (Just n) -> let {after} = splitAt (n + 1) id in
      case fromString after of
        Nothing -> 0
        (Just x) -> x + 1
