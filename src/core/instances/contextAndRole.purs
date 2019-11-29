-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.ContextAndRole where

import Data.Array (cons)
import Data.Array (delete, difference, elemIndex, last, snoc, union) as Arr
import Data.Int (floor, fromString, toNumber)
import Data.Lens (Lens', Traversal', _Just, over, set, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Ord (Ordering, compare)
import Data.String (Pattern(..), lastIndexOf, splitAt)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, insert, lookup, pop, delete)
import Math (ln10, log)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (Namespace, deconstructNamespace)
import Perspectives.InstanceRepresentation (ContextRecord, PerspectContext(..), PerspectRol(..), RolRecord)
import Perspectives.Representation.Class.Revision (Revision_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), EnumeratedPropertyType(..))
import Prelude (flip, identity, show, ($), (+), (/), (<<<), (<>), (<$>))

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

_roleInstances :: EnumeratedRoleType -> Traversal' PerspectContext (Array RoleInstance)
_roleInstances (EnumeratedRoleType t) = _Newtype <<< _rolInContext <<< at t <<< _Just
  where
    _rolInContext :: forall a r. Lens' { rolInContext :: a | r } a
    _rolInContext = prop (SProxy :: SProxy "rolInContext")

_roleInstances' :: EnumeratedRoleType -> Traversal' PerspectContext (Maybe (Array RoleInstance))
_roleInstances' (EnumeratedRoleType t) = _Newtype <<< _rolInContext <<< at t
  where
    _rolInContext :: forall a r. Lens' { rolInContext :: a | r } a
    _rolInContext = prop (SProxy :: SProxy "rolInContext")

addContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> RoleInstance -> PerspectContext
addContext_rolInContext ct rolName rolId = case view (_roleInstances' rolName) ct of
  Nothing -> set (_roleInstances' rolName) (Just [rolId]) ct
  Just roles -> set (_roleInstances' rolName) (Just (cons rolId roles)) ct

removeContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> RoleInstance -> PerspectContext
removeContext_rolInContext ct rolName rolId = over (_roleInstances rolName) (Arr.delete rolId) ct
-- removeContext_rolInContext ct rolName rolId = case view (_roleInstances' rolName) ct of
--   Nothing -> ct
--   Just roles -> set (_roleInstances' rolName) (Just (Arr.delete rolId roles)) ct

deleteContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> PerspectContext
deleteContext_rolInContext (PerspectContext ct@{rolInContext}) rolName = PerspectContext (ct {rolInContext = delete (unwrap rolName) rolInContext})

setContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> Array RoleInstance -> PerspectContext
setContext_rolInContext ct rolName rolIDs = set (_roleInstances' rolName) (Just rolIDs) ct

type Modifier = Array RoleInstance -> Array RoleInstance

modifyContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> Modifier -> PerspectContext
modifyContext_rolInContext ct rolName f = case view (_roleInstances' rolName) ct of
  Nothing -> set (_roleInstances' rolName) (Just $ f []) ct
  Just roles -> set (_roleInstances' rolName) (Just (f roles)) ct
  -- over (_roleInstances rolName) f ct

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ContextInstance ""
  , _rev: Nothing
  , displayName: ""
  , pspType: ContextType ""
  , buitenRol: RoleInstance ""
  , rolInContext: empty
  , me: RoleInstance ""
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
  , isMe: false
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

_propertyValues :: EnumeratedPropertyType -> Traversal' PerspectRol (Array Value)
_propertyValues (EnumeratedPropertyType t) = _Newtype <<< _properties <<< at t <<< _Just
  where
    _properties :: forall a r. Lens' { properties :: a | r } a
    _properties = prop (SProxy :: SProxy "properties")

_propertyValues' :: EnumeratedPropertyType -> Traversal' PerspectRol (Maybe (Array Value))
_propertyValues' (EnumeratedPropertyType t) = _Newtype <<< _properties <<< at t
  where
    _properties :: forall a r. Lens' { properties :: a | r } a
    _properties = prop (SProxy :: SProxy "properties")

rol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value
rol_property (PerspectRol{properties}) pn = maybe [] identity (lookup (unwrap pn) properties)

addRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> PerspectRol
-- addRol_property rl propertyName values = over (_propertyValues propertyName) (flip Arr.union values) rl
addRol_property rl propertyName values = case view (_propertyValues' propertyName) rl of
  Nothing -> set (_propertyValues' propertyName) (Just values) rl
  Just pvals -> set (_propertyValues' propertyName) (Just (Arr.union pvals values)) rl

removeRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> PerspectRol
removeRol_property rl propertyName values = over (_propertyValues propertyName) (flip Arr.difference values) rl

deleteRol_property :: PerspectRol -> EnumeratedPropertyType -> PerspectRol
deleteRol_property (PerspectRol rl@{properties}) propertyName = PerspectRol (rl {properties = delete (unwrap propertyName) properties})

setRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> PerspectRol
setRol_property rl propertyName values = set (_propertyValues' propertyName) (Just values) rl

rol_gevuldeRollen :: PerspectRol -> Object (Array RoleInstance)
rol_gevuldeRollen (PerspectRol{gevuldeRollen}) = gevuldeRollen

rol_gevuldeRol :: PerspectRol -> EnumeratedRoleType -> Array RoleInstance
rol_gevuldeRol  (PerspectRol{gevuldeRollen}) rn = maybe [] identity (lookup (unwrap rn) gevuldeRollen)

addRol_gevuldeRollen :: PerspectRol -> EnumeratedRoleType -> RoleInstance -> PerspectRol
addRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup (unwrap rolName) gevuldeRollen of
    Nothing -> PerspectRol cr {gevuldeRollen = insert (unwrap rolName) [rolID] gevuldeRollen}
    (Just roles) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> PerspectRol cr {gevuldeRollen = insert (unwrap rolName) (Arr.snoc roles rolID) gevuldeRollen}
        otherwise -> ct

removeRol_gevuldeRollen :: PerspectRol -> EnumeratedRoleType -> RoleInstance -> PerspectRol
removeRol_gevuldeRollen ct@(PerspectRol cr@{gevuldeRollen}) rolName rolID =
  case lookup (unwrap rolName) gevuldeRollen of
    Nothing -> ct
    (Just (roles :: Array RoleInstance)) -> do
      case Arr.elemIndex rolID roles of
        Nothing -> ct
        otherwise -> PerspectRol cr {gevuldeRollen = insert (unwrap rolName) (Arr.delete rolID roles) gevuldeRollen}

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
getNextRolIndex rolIds = case Arr.last rolIds of
  Nothing -> 0
  (Just id) -> case lastIndexOf (Pattern "_") (unwrap id) of
    Nothing -> 0
    (Just n) -> let {after} = splitAt (n + 1) (unwrap id) in
      case fromString after of
        Nothing -> 0
        (Just x) -> x + 1
