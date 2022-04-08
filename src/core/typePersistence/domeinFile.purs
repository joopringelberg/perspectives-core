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

module Perspectives.DomeinFile where

import Control.Monad.State (State, execState, modify)
import Data.Array (cons)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object, empty, insert, lookup)
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, getRev)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Data.EncodableMap (empty) as EM
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.ScreenDefinition (ScreenDefinition, ScreenKey)
import Perspectives.Representation.State (State) as PEState
import Perspectives.Representation.TypeIdentifiers (ContextType, DomeinFileId(..), EnumeratedRoleType)
import Perspectives.Representation.UserGraph (UserGraph(..))
import Perspectives.Representation.View (View)
import Prelude (class Eq, class Show, Unit, bind, pure, unit, void, ($), (<<<))

newtype DomeinFile = DomeinFile DomeinFileRecord

type DomeinFileRecord =
  { _rev :: Revision_
  , _id :: String
  , contexts :: Object Context
  , enumeratedRoles :: Object EnumeratedRole
  , calculatedRoles :: Object CalculatedRole
  , enumeratedProperties :: Object EnumeratedProperty
  , calculatedProperties :: Object CalculatedProperty
  , views :: Object View
  , states :: Object PEState.State
  , crl :: String
  , arc :: String
  -- These are instances of types in this model that have been declared 'indexed'.
  , indexedRoles :: Array RoleInstance
  , indexedContexts :: Array ContextInstance
  , modelDescription :: Maybe PerspectRol
  , referredModels :: Array DomeinFileId
  , invertedQueriesInOtherDomains :: Object (Array SeparateInvertedQuery)
  , userGraph :: UserGraph
  , screens :: EncodableMap ScreenKey ScreenDefinition
  }

derive instance genericDomeinFile :: Generic DomeinFile _

derive instance newtypeDomeinFile :: Newtype DomeinFile _

instance encodeDomeinFile :: Encode DomeinFile where
  encode = genericEncode $ defaultOptions --{unwrapSingleConstructors = true}

instance decodeDomeinFile :: Decode DomeinFile where
  decode (json :: Foreign) = do
    rev <- getRev json
    a <- genericDecode defaultOptions json
    pure (changeRevision rev a)

instance showDomeinFile :: Show DomeinFile where
  show = genericShow

instance eqDomeinFile :: Eq DomeinFile where
  eq = genericEq

instance identifiableDomeinFile :: Identifiable DomeinFile DomeinFileId where
  identifier (DomeinFile{_id}) = DomeinFileId _id
  displayName (DomeinFile{_id}) = _id

instance revisionDomeinFile :: Revision DomeinFile where
  rev = _._rev <<< unwrap
  changeRevision s = over DomeinFile (\vr -> vr {_rev = s})

-------------------------------------------------------------------------------
---- INVERTEDQUERYCOLLECTION
-------------------------------------------------------------------------------
data SeparateInvertedQuery =
  -- Type of the role instance; Type of the context instance to store on; InvertedQuery
  RoleInvertedQuery EnumeratedRoleType TypeName InvertedQuery |                  -- `role` step
  -- Type of the context of the role instance; Type of the role instance to store on; InvertedQuery
  ContextInvertedQuery ContextType TypeName InvertedQuery |                      -- `context` step
  -- Triple keys; Type of the role instance to store on; InvertedQuery
  FilledByInvertedQuery (Array InvertedQueryKey) TypeName InvertedQuery |        -- `filledBy` step
  FillsInvertedQuery (Array InvertedQueryKey) TypeName InvertedQuery |           -- `fills` step
  -- EnumeratedRoleTypes to index with; EnumeratedProperty to store on; InvertedQuery
  OnPropertyDelta (Array EnumeratedRoleType) TypeName InvertedQuery              -- `Value2Role` step.

type TypeName = String

derive instance genericSeparateInvertedQuery :: Generic SeparateInvertedQuery _

instance showSeparateInvertedQuery :: Show SeparateInvertedQuery where
  show = genericShow

derive instance eqSeparateInvertedQuery :: Eq SeparateInvertedQuery

instance encodeSeparateInvertedQuery :: Encode SeparateInvertedQuery where
  encode = genericEncode defaultOptions

instance decodeSeparateInvertedQuery :: Decode SeparateInvertedQuery where
  decode = genericDecode defaultOptions

addInvertedQueryForDomain :: TypeName -> InvertedQuery -> (TypeName -> InvertedQuery -> SeparateInvertedQuery) -> DomeinFileRecord -> DomeinFileRecord
addInvertedQueryForDomain typeName iq collectionConstructor dfr@{invertedQueriesInOtherDomains} = case deconstructModelName typeName of
  Nothing -> dfr
  Just modelName -> let
    invertedQueriesInOtherDomains' = case lookup modelName invertedQueriesInOtherDomains of
      Nothing -> insert modelName [collectionConstructor typeName iq] invertedQueriesInOtherDomains
      Just separateQueries -> insert modelName (cons (collectionConstructor typeName iq) separateQueries) invertedQueriesInOtherDomains
    in
    dfr {invertedQueriesInOtherDomains = invertedQueriesInOtherDomains'}
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

defaultDomeinFileRecord :: DomeinFileRecord
defaultDomeinFileRecord =
  { _rev: Nothing
  , _id: ""
  , contexts: empty
  , enumeratedRoles: empty
  , calculatedRoles: empty
  , enumeratedProperties: empty
  , calculatedProperties: empty
  , views: empty
  , states: empty
  , crl: ""
  , arc: ""
  , indexedRoles: []
  , indexedContexts: []
  , modelDescription: Nothing
  , referredModels: []
  , invertedQueriesInOtherDomains: empty
  , userGraph: UserGraph $ EM.empty
  , screens: EM.empty
}

defaultDomeinFile :: DomeinFile
defaultDomeinFile = DomeinFile defaultDomeinFileRecord

type DomeinFileEnumeratedRoles = Object EnumeratedRole

setRevision :: String -> DomeinFile -> DomeinFile
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}

-- | Returns a table with indexed names as key and ContextType as value.
indexedContexts :: DomeinFile -> Object ContextType
indexedContexts (DomeinFile{contexts}) = execState indexedContexts_ empty
  where
    indexedContexts_ :: State (Object ContextType) Unit
    indexedContexts_ = for_ contexts \(Context{_id, indexedContext}) -> case indexedContext of
      Nothing -> pure unit
      Just i -> void $ modify \table -> insert (unwrap i) _id table

-- | Returns a table with indexed names as key and ContextType as value.
indexedRoles :: DomeinFile -> Object EnumeratedRoleType
indexedRoles (DomeinFile{enumeratedRoles}) = execState indexedRoles_ empty
  where
    indexedRoles_ :: State (Object EnumeratedRoleType) Unit
    indexedRoles_ = for_ enumeratedRoles \(EnumeratedRole{_id, indexedRole}) -> case indexedRole of
      Nothing -> pure unit
      Just i -> void $ modify \table -> insert (unwrap i) _id table
