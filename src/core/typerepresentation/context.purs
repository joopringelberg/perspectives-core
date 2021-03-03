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

module Perspectives.Representation.Context where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, ContextType(..), RoleType)
import Prelude (class Eq, class Show, map, (<<<), (==))

-----------------------------------------------------------
-- CONTEXT
-----------------------------------------------------------
newtype Context = Context ContextRecord

type ContextRecord =
  { _id :: ContextType
  , _rev :: Revision_
  , displayName :: String
  , kindOfContext :: ContextKind

  , contextAspects :: Array ContextType
  , defaultPrototype :: Maybe ContextInstance

  , rolInContext :: Array RoleType
  , contextRol :: Array RoleType
  , gebruikerRol :: Array RoleType

  , nestedContexts :: Array ContextType
  , actions :: Array ActionType
  , context :: Maybe ContextType

  , indexedContext :: Maybe ContextInstance

  , pos :: ArcPosition
  }

data ContextKind = Domain | Case | Party | Activity | State

-- | We assume the id is a qualified name.
defaultContext :: String -> String -> ContextKind -> Maybe String -> ArcPosition -> Context
defaultContext id dname kind context pos = Context { _id: (ContextType id)
  , _rev: Nothing
  , displayName: dname
  , kindOfContext: kind
  , contextAspects: []
  , defaultPrototype: Nothing

  , rolInContext: []
  , contextRol: []
  , gebruikerRol: []

  , nestedContexts: []
  , actions: []
  , context: map ContextType context
  , pos: pos

  , indexedContext: Nothing
  }

derive instance genericRepContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

instance eqContext :: Eq Context where
  eq (Context {_id : id1}) (Context {_id : id2}) = id1 == id2

derive instance newtypeContext :: Newtype Context _

instance endodeContext :: Encode Context where
  encode = genericEncode defaultOptions

instance decodeContext :: Decode Context where
  decode = genericDecode defaultOptions

instance revisionContext :: Revision Context where
  rev = _._rev <<< unwrap
  changeRevision s = over Context (\vr -> vr {_rev = s})

instance identifiableContext :: Identifiable Context ContextType where
  identifier (Context{_id}) = _id

derive instance genericContextKind :: Generic ContextKind _
instance showContextKind :: Show ContextKind where show = genericShow
derive instance eqContextKind :: Eq ContextKind
instance encodeContextKind :: Encode ContextKind where
  encode = genericEncode defaultOptions
instance decodeContextKind :: Decode ContextKind where
  decode = genericDecode defaultOptions
