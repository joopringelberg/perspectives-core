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

import Data.Array (cons, foldl)
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
import Foreign.Object (Object, delete, empty, filter, insert, lookup)
import Math (ln10, log)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (###=))
import Perspectives.Couchdb.Revision (Revision_)
import Perspectives.Identifiers (Namespace, deconstructNamespace)
import Perspectives.InstanceRepresentation (ContextRecord, PerspectContext(..), PerspectRol(..), RolRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), EnumeratedPropertyType(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Types.ObjectGetters (aspectsClosure)
import Prelude (flip, identity, pure, show, ($), (+), (/), (<<<), (<>), bind, not, eq)

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
context_rolInContext (PerspectContext{aliases, rolInContext}) (EnumeratedRoleType rn) = case lookup rn aliases of
  Nothing -> []
  Just s -> case lookup s rolInContext of
    Nothing -> []
    Just rs -> rs

context_me :: PerspectContext -> Maybe RoleInstance
context_me (PerspectContext{me}) = me

changeContext_me :: PerspectContext -> Maybe RoleInstance -> PerspectContext
changeContext_me (PerspectContext cr) me = PerspectContext cr {me = me}

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

_aliases' :: Traversal' PerspectContext (Object String)
_aliases' = _Newtype <<< _aliases
  where
    _aliases :: forall a r. Lens' { aliases :: a | r } a
    _aliases = prop (SProxy :: SProxy "aliases")

addContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> RoleInstance -> MonadPerspectives PerspectContext
addContext_rolInContext ct@(PerspectContext cr@{aliases,rolInContext}) r@(EnumeratedRoleType rolName) rolId = case lookup rolName rolInContext of
  Nothing -> do
    aspects <- r ###= aspectsClosure
    pure $ PerspectContext cr
      { rolInContext = insert rolName [rolId] rolInContext
      , aliases = foldl (\als aspect -> insert (unwrap aspect) rolName als) aliases aspects
      }
  Just roles -> pure $ PerspectContext cr {rolInContext = insert rolName (cons rolId roles) rolInContext}

removeContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> RoleInstance -> PerspectContext
removeContext_rolInContext c@(PerspectContext cr@{aliases, rolInContext}) (EnumeratedRoleType rolName) rolId = case lookup rolName rolInContext of
  Nothing -> c
  Just roles -> PerspectContext cr {rolInContext = insert rolName (Arr.delete rolId roles) rolInContext}

deleteContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> PerspectContext
deleteContext_rolInContext (PerspectContext ct@{rolInContext, aliases}) (EnumeratedRoleType rolName) = PerspectContext (ct
  { rolInContext = delete rolName rolInContext
  , aliases = filter (not <<< eq rolName) aliases
  })

type Modifier = Array RoleInstance -> Array RoleInstance

modifyContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> Modifier -> MonadPerspectives PerspectContext
modifyContext_rolInContext ct@(PerspectContext cr@{rolInContext, aliases}) r@(EnumeratedRoleType rolName) f = case view (_roleInstances' r) ct of
  Nothing -> do
    aspects <- r ###= aspectsClosure
    pure $ PerspectContext cr
      { rolInContext = insert rolName (f []) rolInContext
      , aliases = foldl (\als aspect -> insert (unwrap aspect) rolName als) aliases aspects
      }
  Just roles -> pure $ set (_roleInstances' r) (Just (f roles)) ct

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ContextInstance ""
  , _rev: Nothing
  , displayName: ""
  , pspType: ContextType ""
  , buitenRol: RoleInstance ""
  , rolInContext: empty
  , aliases: empty
  , me: Nothing
  , actionConditionState: empty
  , universeContextDelta: SignedDelta{author: "", encryptedDelta: "UniverseContextDelta from defaultContextRecord"}
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
  , universeRoleDelta: SignedDelta {author: "", encryptedDelta: "UniverseRoleDelta from defaultRolRecord"}
  , contextDelta: SignedDelta {author: "", encryptedDelta: "ContextDelta from defaultRolRecord"}
  , bindingDelta: Nothing
  , propertyDeltas: empty
  }

isDefaultContextDelta :: SignedDelta -> Boolean
isDefaultContextDelta (SignedDelta {encryptedDelta}) = encryptedDelta `eq` "ContextDelta from defaultRolRecord"

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

-- | The first argument is the new binding;
-- | the second argument is the role instance that receives the binding.
changeRol_binding :: RoleInstance -> PerspectRol -> PerspectRol
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = (Just b)}

removeRol_binding :: PerspectRol -> PerspectRol
removeRol_binding (PerspectRol cr) = PerspectRol $ cr {binding = Nothing}

rol_context :: PerspectRol -> ContextInstance
rol_context (PerspectRol{context}) = context

changeRol_context :: ContextInstance -> PerspectRol -> PerspectRol
changeRol_context cid (PerspectRol rp) = PerspectRol rp {context = cid}

rol_isMe :: PerspectRol -> Boolean
rol_isMe (PerspectRol {isMe}) = isMe

changeRol_isMe :: PerspectRol -> Boolean -> PerspectRol
changeRol_isMe (PerspectRol cr) isMe = PerspectRol $ cr {isMe = isMe}

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

setRol_gevuldeRollen :: PerspectRol -> Object (Array RoleInstance) -> PerspectRol
setRol_gevuldeRollen (PerspectRol r) grollen = PerspectRol (r {gevuldeRollen = grollen})

rol_gevuldeRol :: PerspectRol -> EnumeratedRoleType -> Array RoleInstance
rol_gevuldeRol  (PerspectRol{gevuldeRollen}) rn = maybe [] identity (lookup (unwrap rn) gevuldeRollen)

-- | The first argument is the role instance that receives a new inverse link to a role that binds it.
-- | The third argument represents the role instance that binds the role of the first argument.
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
