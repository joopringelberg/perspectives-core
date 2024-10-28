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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Foreign (unsafeToForeign)
import Foreign.Object (Object, empty)
import Perspectives.Couchdb.Revision (class Revision, Revision_)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Persistent.PublicStore (PublicStore)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType, RoleType)
import Prelude (class Eq, class Show, map, show, (<<<), (==), (<$>))
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

----------------------------------------------------------- 
-- CONTEXT
-----------------------------------------------------------
newtype Context = Context ContextRecord

type ContextRecord =
  { id :: ContextType
  , _rev :: Revision_
  , displayName :: String
  , kindOfContext :: ContextKind
  , public :: Maybe PublicStore

  , contextAspects :: Array ContextType
  , defaultPrototype :: Maybe ContextInstance

  , rolInContext :: Array RoleType
  , contextRol :: Array RoleType
  , gebruikerRol :: Array RoleType

  , nestedContexts :: Array ContextType
  , context :: Maybe ContextType

  , indexedContext :: Maybe ContextInstance

  , roleInvertedQueries :: Object (Array InvertedQuery)

  , roleAliases :: Object (EnumeratedRoleType)

  , pos :: ArcPosition
  }

-- TODO: verwijder State. Het is geen ContextKind.
data ContextKind = Domain | Case | Party | Activity | State

instance WriteForeign ContextKind where
  writeImpl = unsafeToForeign <<< show

instance ReadForeign ContextKind where  
  readImpl = enumReadForeign

-- | We assume the id is a qualified name.
defaultContext :: String -> String -> ContextKind -> Maybe String -> ArcPosition -> Maybe PublicStore -> Context
defaultContext id dname kind context pos public = Context { id: (ContextType id)
  , _rev: Nothing
  , displayName: dname
  , kindOfContext: kind
  , contextAspects: []
  , defaultPrototype: Nothing
  , public

  , rolInContext: []
  , contextRol: []
  , gebruikerRol: []

  , nestedContexts: []
  , context: map ContextType context
  , pos: pos

  , indexedContext: Nothing

  , roleInvertedQueries: empty

  , roleAliases: empty
  }

derive instance genericRepContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

instance eqContext :: Eq Context where
  eq (Context {id : id1}) (Context {id : id2}) = id1 == id2

derive instance newtypeContext :: Newtype Context _

instance WriteForeign Context where
  writeImpl (Context cr) = writeImpl cr

instance ReadForeign Context where
  readImpl f = Context <$> read' f

instance revisionContext :: Revision Context where
  rev = _._rev <<< unwrap
  changeRevision s = over Context (\vr -> vr {_rev = s})

instance identifiableContext :: Identifiable Context ContextType where
  identifier (Context{id}) = id
  displayName (Context{displayName:d}) = d

derive instance genericContextKind :: Generic ContextKind _
instance showContextKind :: Show ContextKind where show = genericShow
derive instance eqContextKind :: Eq ContextKind
