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

module Perspectives.InstanceRepresentation where 

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, over)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object) as F
import Persistence.Attachment (class Attachment, Attachments)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.InstanceRepresentation.PublicUrl (PublicUrl)
import Perspectives.Persistence.Types (PouchbdDocumentFields)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, RoleType, StateIdentifier)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Prelude (class Eq, class Show, eq, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- PERSPECTCONTEXT TYPE CLASS
-----------------------------------------------------------
class PerspectContextClass c where
  pspType :: c -> ContextType
  externalRole :: c -> RoleInstance
  roles :: c -> F.Object (Array RoleInstance)
  states :: c -> Array StateIdentifier

instance perspectContextPerspectContextClass :: PerspectContextClass PerspectContext where
  pspType c = (unwrap c).pspType
  externalRole c = (unwrap c).buitenRol
  roles c = (unwrap c).rolInContext
  states c = (unwrap c).states

-----------------------------------------------------------
-- PERSPECTCONTEXT
-----------------------------------------------------------
newtype PerspectContext = PerspectContext ContextRecord

type ContextRecord = PouchbdDocumentFields
  ( id :: ContextInstance
  , displayName :: String
  , pspType :: ContextType
  , allTypes :: Array ContextType
  , buitenRol :: RoleInstance
  , rolInContext :: F.Object (Array RoleInstance)
  , me :: Maybe RoleInstance
  , preferredUserRoleType :: Maybe RoleType
  , universeContextDelta :: SignedDelta
  , states :: Array StateIdentifier
  , publicUrl :: Maybe PublicUrl
  )

derive instance genericRepPerspectContext :: Generic PerspectContext _

instance showPerspectContext :: Show PerspectContext where
  show = genericShow

derive newtype instance WriteForeign PerspectContext
derive newtype instance ReadForeign PerspectContext
 
instance eqPerspectContext :: Eq PerspectContext where
  eq (PerspectContext {id : id1}) (PerspectContext {id : id2}) = id1 == id2

derive instance newtypePerspectContext :: Newtype PerspectContext _

instance identifiablePerspectContext :: Identifiable PerspectContext ContextInstance where
  identifier (PerspectContext{id}) = id
  displayName (PerspectContext{displayName:d}) = d

instance revisionPerspectContext :: Revision PerspectContext where
  rev = _._rev <<< unwrap
  changeRevision s = over PerspectContext (\vr -> vr {_rev = s})

instance Attachment PerspectContext where
  setAttachment c _ = c
  getAttachments _ = Nothing

-----------------------------------------------------------
-- PERSPECTROL
-----------------------------------------------------------
newtype PerspectRol = PerspectRol RolRecord

-- TODO. #17 Represent the closure of aspects directly on the type.
type RolRecord = PouchbdDocumentFields
  ( id :: RoleInstance
  , pspType :: EnumeratedRoleType
  , allTypes :: Array EnumeratedRoleType
  , context :: ContextInstance
  -- While the fields above occur in every role, those below do not.
  , binding :: Filler
  -- The four fields below could also be modeled as Maybe values.
  -- The first index is the propertytype; the second is the Array of values.
  , properties :: F.Object (Array Value)
  -- The first index is the String representation of the type of the context of the filled role, the second is the string representation of the type of the flled role.
  , filledRoles :: F.Object (F.Object (Array RoleInstance))
  , occurrence :: Int
  , isMe :: Boolean
  , universeRoleDelta :: SignedDelta
  , contextDelta :: SignedDelta
  , bindingDelta :: Maybe SignedDelta
  -- The first index is the propertytype; the second is the value.
  , propertyDeltas :: F.Object (F.Object SignedDelta)
  , states :: Array StateIdentifier
  , roleAliases :: F.Object String
  , contextAliases :: F.Object String
  , _attachments :: Maybe Attachments
  )

derive instance genericRepPerspectRol :: Generic PerspectRol _

instance showPerspectRol :: Show PerspectRol where
  show = genericShow

instance eqPerspectRol :: Eq PerspectRol where
  eq (PerspectRol{id: id1}) (PerspectRol{id: id2}) = eq id1 id2

derive instance newtypePerspectRol :: Newtype PerspectRol _

derive newtype instance WriteForeign PerspectRol
derive newtype instance ReadForeign PerspectRol

instance revisionPerspectRol :: Revision PerspectRol where
  rev = _._rev <<< unwrap
  changeRevision s = over PerspectRol (\vr -> vr {_rev = s})

instance identifiablePerspectRol :: Identifiable PerspectRol RoleInstance where
  identifier (PerspectRol{id}) = id
  displayName (PerspectRol{id}) = unwrap id

instance Attachment PerspectRol where
  setAttachment (PerspectRol r) ma = PerspectRol (r {_attachments = ma})
  getAttachments (PerspectRol {_attachments}) = _attachments

-----------------------------------------------------------
-- BINDING
-----------------------------------------------------------
type Filler = Maybe RoleInstance

binding :: RoleInstance -> Filler
binding id = case id of
  RoleInstance "" -> Nothing
  otherwise -> (Just id)

