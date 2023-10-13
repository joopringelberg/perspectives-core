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

module Perspectives.Representation.EnumeratedRole where

import Data.Array (cons, delete, elemIndex)
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object, empty) as OBJ
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Data.EncodableMap (EncodableMap, empty, lookup, insert)
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Query.QueryTypes (Calculation, RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.Perspective (Perspective, StateSpec)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType, RoleKind, ViewType)
import Prelude (class Eq, class Ord, class Show, (<<<), (==), ($))

-----------------------------------------------------------
-- ENUMERATEDROLE
-----------------------------------------------------------
newtype EnumeratedRole = EnumeratedRole EnumeratedRoleRecord

type EnumeratedRoleRecord =
  { _id :: EnumeratedRoleType
  , _rev :: Revision_
  , displayName :: String
  , kindOfRole :: RoleKind
  -- An EnumeratedRole instance is public if its context is public,
  -- unless it has been explicitly declared private.
  -- TODO. This may be obsolete by now (March 10 2023)
  , declaredAsPrivate :: Boolean
  , context :: ContextType
  , views :: Array ViewType
  , pos :: ArcPosition

  , perspectives :: Array Perspective
  , actions :: EncodableMap StateSpec (OBJ.Object Action)

  , roleAspects :: Array RoleInContext
  , properties :: Array PropertyType
  , propertyAliases :: OBJ.Object (EnumeratedPropertyType)

  , binding :: ADT RoleInContext

  , functional :: Boolean
  , mandatory :: Boolean

  -- The keys in these objects are the String representations of context types.
  , fillerInvertedQueries :: InvertedQueryMap
  , filledInvertedQueries :: InvertedQueryMap

  , contextInvertedQueries :: OBJ.Object (Array InvertedQuery)

  , indexedRole :: Maybe RoleInstance

  , unlinked :: Boolean

  -- This should be the URL of a public store of instances.
  , publicUrl :: Maybe Calculation

  }

defaultEnumeratedRole :: String -> String -> RoleKind -> String -> Boolean -> ArcPosition -> EnumeratedRole
defaultEnumeratedRole qname dname kindOfRole context declaredAsPrivate pos = EnumeratedRole
  { _id: EnumeratedRoleType qname
  , _rev: Nothing
  , displayName: dname
  , kindOfRole: kindOfRole
  , declaredAsPrivate

  , roleAspects: []
  , properties: []
  , propertyAliases: OBJ.empty

  , context: ContextType context
  , binding: EMPTY

  , views: []

  , perspectives: []

  , actions: empty

  , functional: true
  , mandatory: false

  , pos: pos

  , fillerInvertedQueries: empty
  , filledInvertedQueries: empty
  , contextInvertedQueries: OBJ.empty

  , indexedRole: Nothing
  , unlinked: false
  , publicUrl: Nothing
  }

derive instance genericRepEnumeratedRole :: Generic EnumeratedRole _

instance showEnumeratedRole :: Show EnumeratedRole where
  show = genericShow

instance eqEnumeratedRole :: Eq EnumeratedRole where
  eq (EnumeratedRole {_id : id1}) (EnumeratedRole {_id : id2}) = id1 == id2

derive instance newtypeEnumeratedRole :: Newtype EnumeratedRole _

instance encodeEnumeratedRole :: Encode EnumeratedRole where
  encode = genericEncode defaultOptions

instance decodeEnumeratedRole :: Decode EnumeratedRole where
  decode = genericDecode defaultOptions

instance revisionEnumeratedRole :: Revision EnumeratedRole where
  rev = _._rev <<< unwrap
  changeRevision s = over EnumeratedRole (\vr -> vr {_rev = s})

instance identifiableEnumeratedRole :: Identifiable EnumeratedRole EnumeratedRoleType where
  identifier (EnumeratedRole{_id}) = _id
  displayName (EnumeratedRole{displayName:d}) = d

-----------------------------------------------------------
-- INVERTEDQUERYKEY
-----------------------------------------------------------
data InvertedQueryKey = InvertedQueryKey ContextType ContextType EnumeratedRoleType

startContext :: InvertedQueryKey -> ContextType
startContext (InvertedQueryKey ct _ _) = ct

fillerContext :: InvertedQueryKey -> ContextType
fillerContext (InvertedQueryKey _ ct _) = ct

fillerRole :: InvertedQueryKey -> EnumeratedRoleType
fillerRole (InvertedQueryKey _ _ rt) = rt

derive instance genericInvertedQueryKey :: Generic InvertedQueryKey _
instance eqInvertedQueryKey :: Eq InvertedQueryKey where eq = genericEq
instance ordInvertedQueryKey :: Ord InvertedQueryKey where compare = genericCompare
instance showInvertedQueryKey :: Show InvertedQueryKey where show = genericShow
instance encodeInvertedQueryKey :: Encode InvertedQueryKey where encode = genericEncode defaultOptions
instance decodeInvertedQueryKey :: Decode InvertedQueryKey where decode = genericDecode defaultOptions

type InvertedQueryMap = EncodableMap InvertedQueryKey (Array InvertedQuery)

-- | Add an InvertedQuery to a PropertyType, indexed with an EnumeratedRoleType.
-- | Computes whether the InvertedQuery can be modified.
addInvertedQueryIndexedByTripleKeys ::
  InvertedQuery ->
  Array InvertedQueryKey ->
  InvertedQueryMap ->
  Array RoleInContext ->
  EnumeratedRoleType ->
  InvertedQueryMap
addInvertedQueryIndexedByTripleKeys q@(InvertedQuery qr) keys iqs modifiesRoleBindingOf role = foldl
  (\qs key -> let
    q' = if isJust $ elemIndex (RoleInContext {context: startContext key, role}) modifiesRoleBindingOf
      then InvertedQuery (qr {modifies=true})
      else q
    in case lookup key qs of
      Nothing -> insert key [q] qs
      Just x -> insert key (cons q x) qs)
  iqs
  keys

deleteInvertedQueryIndexedByTripleKeys :: InvertedQuery -> Array InvertedQueryKey -> InvertedQueryMap -> InvertedQueryMap
deleteInvertedQueryIndexedByTripleKeys q keys iqs = foldl
  (\qs key -> case lookup key qs of
    Nothing -> qs
    Just x -> insert key (delete q x) qs)
  iqs
  keys
