-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.ContextAndRole where

import Data.Array (cons, foldl, null)
import Data.Array (delete, difference, elemIndex, snoc, union) as Arr
import Data.Foldable (maximum)
import Data.Int (floor, fromString, toNumber)
import Data.Lens (Lens', Traversal', _Just, over, set, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap, over) as NT
import Data.Number (ln10, log)
import Data.Ord (Ordering, compare)
import Data.String (Pattern(..), lastIndexOf, splitAt)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, delete, empty, fromFoldable, insert, lookup, singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP, (###=))
import Perspectives.Identifiers (Namespace, typeUri2typeNameSpace)
import Perspectives.InstanceRepresentation (ContextRecord, PerspectContext(..), PerspectRol(..), RolRecord)
import Perspectives.InstanceRepresentation.PublicUrl (PublicUrl)
import Perspectives.Representation.Class.PersistentType (getContext)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Types.ObjectGetters (contextAspectsClosure, roleAspectsClosure)
import Prelude (bind, eq, flip, identity, pure, show, ($), (+), (/), (<#>), (<$>), (<<<), (<>))
import Type.Proxy (Proxy(..))

-- CONTEXT

context_id :: PerspectContext -> ContextInstance
context_id (PerspectContext{id})= id

context_Namespace :: PerspectContext -> Namespace
context_Namespace (PerspectContext{id}) = unsafePartial $ fromJust $ typeUri2typeNameSpace (NT.unwrap id)

changeContext_id :: ContextInstance -> PerspectContext -> PerspectContext
changeContext_id id (PerspectContext cr) = PerspectContext $ cr {id = id}

context_displayName :: PerspectContext -> String
context_displayName (PerspectContext{displayName})= displayName

changeContext_displayName :: String -> PerspectContext -> PerspectContext
changeContext_displayName dn (PerspectContext cr) = PerspectContext $ cr {displayName = dn}

context_pspType :: PerspectContext -> ContextType
context_pspType (PerspectContext{pspType})= pspType

changeContext_type :: String -> PerspectContext -> PerspectContext
changeContext_type tp (PerspectContext cr) = PerspectContext $ cr {pspType = ContextType tp}

context_allTypes :: PerspectContext -> Array ContextType
context_allTypes (PerspectContext{allTypes}) = allTypes

context_buitenRol :: PerspectContext -> RoleInstance
context_buitenRol (PerspectContext{buitenRol})= buitenRol

context_universeContextDelta :: PerspectContext -> SignedDelta
context_universeContextDelta (PerspectContext{universeContextDelta}) = universeContextDelta

context_iedereRolInContext :: PerspectContext -> Object (Array RoleInstance)
context_iedereRolInContext (PerspectContext{rolInContext})= rolInContext

context_rolInContext :: PerspectContext -> EnumeratedRoleType -> MP (Tuple String (Array RoleInstance))
context_rolInContext (PerspectContext{pspType, rolInContext}) (EnumeratedRoleType rn) = case lookup rn rolInContext of
  Nothing -> do
    Context{roleAliases} <- getContext pspType
    case lookup rn roleAliases of
      Nothing -> pure $ Tuple rn []
      Just (EnumeratedRoleType s) -> case lookup s rolInContext of
        Nothing -> pure $ Tuple s []
        Just rs -> pure $ Tuple s rs
  Just rs -> pure $ Tuple rn rs

-- This version does no model reflection. 
context_rolInContext_ :: PerspectContext -> EnumeratedRoleType -> MP (Array RoleInstance)
context_rolInContext_ (PerspectContext{pspType, rolInContext}) (EnumeratedRoleType rn) = case lookup rn rolInContext of
  Nothing -> pure []
  Just rs -> pure rs

context_me :: PerspectContext -> Maybe RoleInstance
context_me (PerspectContext{me}) = me

changeContext_me :: PerspectContext -> Maybe RoleInstance -> PerspectContext
changeContext_me (PerspectContext cr) me = PerspectContext cr {me = me}

context_preferredUserRoleType :: PerspectContext -> Maybe RoleType
context_preferredUserRoleType (PerspectContext {preferredUserRoleType}) = preferredUserRoleType

changeContext_preferredUserRoleType :: PerspectContext -> Maybe RoleType -> PerspectContext
changeContext_preferredUserRoleType (PerspectContext cr) mytype = PerspectContext cr {preferredUserRoleType = mytype}

_roleInstances :: EnumeratedRoleType -> Traversal' PerspectContext (Array RoleInstance)
_roleInstances (EnumeratedRoleType t) = _Newtype <<< _rolInContext <<< at t <<< _Just
  where
    _rolInContext :: forall a r. Lens' { rolInContext :: a | r } a
    _rolInContext = prop (Proxy :: Proxy "rolInContext")

_roleInstances' :: EnumeratedRoleType -> Traversal' PerspectContext (Maybe (Array RoleInstance))
_roleInstances' (EnumeratedRoleType t) = _Newtype <<< _rolInContext <<< at t
  where
    _rolInContext :: forall a r. Lens' { rolInContext :: a | r } a
    _rolInContext = prop (Proxy :: Proxy "rolInContext")

addContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> RoleInstance -> PerspectContext
addContext_rolInContext (PerspectContext cr@{rolInContext}) r@(EnumeratedRoleType rolName) rolId = case lookup rolName rolInContext of
  Nothing -> PerspectContext cr { rolInContext = insert rolName [rolId] rolInContext }
  Just roles -> PerspectContext cr {rolInContext = insert rolName (cons rolId roles) rolInContext}

removeContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> RoleInstance -> PerspectContext
removeContext_rolInContext c@(PerspectContext cr@{rolInContext}) (EnumeratedRoleType rolName) rolId = case lookup rolName rolInContext of
  Nothing -> c
  Just roles -> PerspectContext cr {rolInContext = insert rolName (Arr.delete rolId roles) rolInContext}

deleteContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> PerspectContext
deleteContext_rolInContext (PerspectContext ct@{rolInContext}) (EnumeratedRoleType rolName) = PerspectContext (ct
  { rolInContext = delete rolName rolInContext
  })

type Modifier = Array RoleInstance -> Array RoleInstance

-- TODO. This function need not be in MonadPerspectives.
modifyContext_rolInContext :: PerspectContext -> EnumeratedRoleType -> Modifier -> MonadPerspectives PerspectContext
modifyContext_rolInContext ct@(PerspectContext cr@{rolInContext}) r@(EnumeratedRoleType rolName) f = case view (_roleInstances' r) ct of
  Nothing -> do
    pure $ PerspectContext cr
      { rolInContext = insert rolName (f []) rolInContext}
  Just roles -> pure $ set (_roleInstances' r) (Just (f roles)) ct

context_states :: PerspectContext -> Array StateIdentifier
context_states (PerspectContext{states}) = states

-- | Add the state identifier as the last one in the Array (the Array represents the path from the rootstate).
pushContext_state :: PerspectContext -> StateIdentifier -> PerspectContext
pushContext_state (PerspectContext cr@{states}) stateId = PerspectContext cr {states =
  case Arr.elemIndex stateId states of
    Nothing -> Arr.snoc cr.states stateId
    Just _ -> states
  }

-- | Remove a state.
popContext_state :: PerspectContext -> StateIdentifier -> PerspectContext
popContext_state (PerspectContext cr) stateId = PerspectContext cr { states = Arr.delete stateId cr.states}

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ""
  , id: ContextInstance ""
  , _rev: Nothing
  , displayName: ""
  , pspType: ContextType ""
  , allTypes: []
  , buitenRol: RoleInstance ""
  , rolInContext: empty
  , me: Nothing
  , preferredUserRoleType: Nothing
  , universeContextDelta: SignedDelta{author: PerspectivesUser "", encryptedDelta: "UniverseContextDelta from defaultContextRecord", signature: Nothing}
  , states: []
  , publicUrl: Nothing
  }

defaultRolRecord :: RolRecord
defaultRolRecord =
  { _id: ""
  , id: RoleInstance ""
  , pspType: EnumeratedRoleType ""
  , allTypes: []
  , context: ContextInstance ""
  , _rev: Nothing
  , binding: Nothing
  , properties: empty
  , filledRoles: empty
  , occurrence: 0
  , isMe: false
  , universeRoleDelta: SignedDelta {author: PerspectivesUser "", encryptedDelta: "UniverseRoleDelta from defaultRolRecord", signature: Nothing}
  , contextDelta: SignedDelta {author: PerspectivesUser "", encryptedDelta: "ContextDelta from defaultRolRecord", signature: Nothing}
  , bindingDelta: Nothing
  , propertyDeltas: empty
  , states: []
  , roleAliases: empty
  , contextAliases: empty
  , _attachments: Nothing
  }

isDefaultContextDelta :: SignedDelta -> Boolean
isDefaultContextDelta (SignedDelta {encryptedDelta}) = encryptedDelta `eq` "ContextDelta from defaultRolRecord"

context_publicUrl :: PerspectContext -> Maybe PublicUrl
context_publicUrl (PerspectContext{publicUrl}) = publicUrl

change_context_publicUrl :: PerspectContext -> Maybe PublicUrl -> PerspectContext
change_context_publicUrl (PerspectContext cr) murl = PerspectContext cr {publicUrl = murl}

-- ROL

rol_id :: PerspectRol -> RoleInstance
rol_id (PerspectRol{id}) = id

rol_occurrence :: PerspectRol -> Int
rol_occurrence (PerspectRol{occurrence}) = occurrence

rol_pspType :: PerspectRol -> EnumeratedRoleType
rol_pspType (PerspectRol{pspType}) = pspType

rol_allTypes :: PerspectRol -> Array EnumeratedRoleType
rol_allTypes (PerspectRol{allTypes}) = allTypes

changeRol_type :: String -> PerspectRol -> PerspectRol
changeRol_type tp (PerspectRol cr) = PerspectRol $ cr {pspType = EnumeratedRoleType tp}

rol_binding :: PerspectRol -> Maybe RoleInstance
rol_binding (PerspectRol{binding}) = binding 

-- | The first argument is the new binding;
-- | the second argument is the role instance that receives the binding.
changeRol_binding :: RoleInstance -> PerspectRol -> PerspectRol
changeRol_binding b (PerspectRol cr) = PerspectRol $ cr {binding = (Just b)}

removeRol_binding :: PerspectRol -> PerspectRol
removeRol_binding (PerspectRol cr) = PerspectRol $ cr {binding = Nothing, bindingDelta = Nothing}

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
    _properties = prop (Proxy :: Proxy "properties")

_propertyValues' :: EnumeratedPropertyType -> Traversal' PerspectRol (Maybe (Array Value))
_propertyValues' (EnumeratedPropertyType t) = _Newtype <<< _properties <<< at t
  where
    _properties :: forall a r. Lens' { properties :: a | r } a
    _properties = prop (Proxy :: Proxy "properties")

rol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value
rol_property (PerspectRol{properties}) pn = maybe [] identity (lookup (NT.unwrap pn) properties)

rol_propertyDelta :: PerspectRol -> EnumeratedPropertyType -> Value -> Maybe SignedDelta
rol_propertyDelta (PerspectRol{propertyDeltas}) (EnumeratedPropertyType pn) (Value v) = 
  case lookup pn propertyDeltas of
    Nothing -> Nothing
    Just x -> lookup v x

rol_universeRoleDelta :: PerspectRol -> SignedDelta
rol_universeRoleDelta (PerspectRol{universeRoleDelta}) = universeRoleDelta

rol_contextDelta :: PerspectRol -> SignedDelta
rol_contextDelta (PerspectRol{contextDelta}) = contextDelta

addRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> PerspectRol
-- addRol_property rl propertyName values = over (_propertyValues propertyName) (flip Arr.union values) rl
addRol_property rl propertyName values = case view (_propertyValues' propertyName) rl of
  Nothing -> set (_propertyValues' propertyName) (Just values) rl
  Just pvals -> set (_propertyValues' propertyName) (Just (Arr.union pvals values)) rl

removeRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> PerspectRol
removeRol_property rl propertyName values = over (_propertyValues propertyName) (flip Arr.difference values) rl

deleteRol_property :: PerspectRol -> EnumeratedPropertyType -> PerspectRol
deleteRol_property (PerspectRol rl@{properties}) propertyName = PerspectRol (rl {properties = delete (NT.unwrap propertyName) properties})

-- setRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> PerspectRol
-- setRol_property rl propertyName values = set (_propertyValues' propertyName) (Just values) rl

-- | Notice that the delta is associated with each individual value.
setRol_property :: PerspectRol -> EnumeratedPropertyType -> Array Value -> SignedDelta -> PerspectRol
setRol_property (PerspectRol rl@{propertyDeltas, properties}) propertyName values signedDelta = PerspectRol rl
    { propertyDeltas = insert (NT.unwrap propertyName) (fromFoldable (flip Tuple signedDelta <<< NT.unwrap <$> values)) propertyDeltas
    , properties = insert (NT.unwrap propertyName) values properties
    }

rol_gevuldeRollen :: PerspectRol -> Object (Object (Array RoleInstance))
rol_gevuldeRollen (PerspectRol{filledRoles}) = filledRoles

setRol_gevuldeRollen :: PerspectRol -> Object (Object (Array RoleInstance)) -> PerspectRol
setRol_gevuldeRollen (PerspectRol r) grollen = PerspectRol (r {filledRoles = grollen})

rol_gevuldeRol :: PerspectRol -> ContextType -> EnumeratedRoleType -> {context :: ContextType, role :: EnumeratedRoleType, instances :: Array RoleInstance}
rol_gevuldeRol  (PerspectRol{filledRoles, roleAliases, contextAliases}) cType rn = case lookup (NT.unwrap cType) contextAliases of
  Nothing -> {context: cType, role: rn, instances: []}
  Just contextAlias -> case lookup contextAlias filledRoles of
    Nothing -> {context: ContextType contextAlias, role: rn, instances: []}
    Just roleMap -> case lookup (NT.unwrap rn) roleAliases of
      Nothing -> {context: ContextType contextAlias, role: rn, instances: []}
      Just alias -> maybe 
        {context: ContextType contextAlias, role: (EnumeratedRoleType alias), instances: []} 
        (\instances -> {context: ContextType contextAlias, role: (EnumeratedRoleType alias), instances}) 
        (lookup alias roleMap)

-- | The first argument is the role Filler instance that receives a new inverse link to a role that binds it.
-- | The fourth argument represents the Filled role instance that binds the role of the first argument.
-- | the second and third argument form the double index under which we store the new filled role.
-- | They are respectively its context type and its role type.
-- | This operation is idempotent.
addRol_gevuldeRollen :: PerspectRol -> ContextType -> EnumeratedRoleType -> RoleInstance -> MonadPerspectives PerspectRol
addRol_gevuldeRollen filler cType rolName filled = do
  cIndex <- pure $ NT.unwrap cType
  rIndex <- pure $ NT.unwrap rolName
  roleAspects <- rolName ###= roleAspectsClosure
  contextAspects <- cType ###= contextAspectsClosure
  f <- pure $ NT.over PerspectRol (\cr -> let
    roleAliases' = foldl (\als alias -> insert alias rIndex als) cr.roleAliases (NT.unwrap <$> roleAspects)
    contextAliases' = foldl (\als alias -> insert alias cIndex als) cr.contextAliases (NT.unwrap <$> contextAspects)
    in 
    case lookup cIndex cr.filledRoles of
      Nothing -> cr { filledRoles = insert cIndex (singleton rIndex [filled]) cr.filledRoles
                    , roleAliases = roleAliases'
                    , contextAliases = contextAliases'}
      Just (roleMap :: Object (Array RoleInstance)) -> case lookup rIndex roleMap of
        Nothing -> cr { filledRoles = insert cIndex (insert rIndex [filled] roleMap) cr.filledRoles
                      , roleAliases = roleAliases'
                      , contextAliases = contextAliases'}
        (Just roles) -> do
          case Arr.elemIndex filled roles of
            Nothing -> cr { filledRoles = insert cIndex (insert rIndex (Arr.union [filled] roles) roleMap) cr.filledRoles
                          , roleAliases = roleAliases'
                          , contextAliases = contextAliases'}
            _ -> cr
      )
  pure $ f filler

removeRol_gevuldeRollen :: PerspectRol -> ContextType -> EnumeratedRoleType -> RoleInstance -> PerspectRol
removeRol_gevuldeRollen ct cType rolName rolID = let
  cIndex = NT.unwrap cType
  rIndex = NT.unwrap rolName
  f = NT.over PerspectRol \cr ->
    case lookup cIndex cr.filledRoles of
      Nothing -> cr
      Just roleMap -> case lookup rIndex roleMap of
        Nothing -> cr
        (Just (roles :: Array RoleInstance)) -> do
          case Arr.elemIndex rolID roles of
            Nothing -> cr
            _ -> let
              roles' = Arr.delete rolID roles in
                if null roles'
                  then cr {filledRoles = insert cIndex (delete rIndex roleMap) cr.filledRoles}
                  else cr {filledRoles = insert cIndex (insert rIndex roles' roleMap) cr.filledRoles}
  in f ct

rol_states :: PerspectRol -> Array StateIdentifier
rol_states (PerspectRol{states}) = states

-- | Add the state identifier as the last one in the Array (the Array represents the path from the rootstate).
pushRol_state :: PerspectRol -> StateIdentifier -> PerspectRol
pushRol_state (PerspectRol cr@{states}) stateId = PerspectRol cr {states =
  case Arr.elemIndex stateId states of
    Nothing -> Arr.snoc cr.states stateId
    Just _ -> states
  }

-- | Remove a state.
popRol_state :: PerspectRol -> StateIdentifier -> PerspectRol
popRol_state (PerspectRol cr) stateId = PerspectRol cr { states = Arr.delete stateId cr.states}

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)

-- Returns a string representation of the integer, left padded with zero's.
rol_padOccurrence :: Int -> String
rol_padOccurrence n =  case floor( log( toNumber n) / ln10 ) of
  0 -> "000" <> show n -- 1 position
  1 -> "00" <> show n
  2 -> "0" <> show n
  _ -> show n

getNextRolIndex :: Array RoleInstance -> Int
getNextRolIndex rolIds = case (maximum $ rolIds <#> \(RoleInstance id) -> case lastIndexOf (Pattern "_") id of
  Nothing -> 0
  Just n -> let {after} = splitAt (n+1) id in
    case fromString after of
      Nothing -> 0
      Just x -> x) of
  Nothing -> 0
  Just n -> n + 1
