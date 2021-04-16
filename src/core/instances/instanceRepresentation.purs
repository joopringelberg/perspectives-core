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
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, over)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object) as F
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, getRev)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, StateIdentifier)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Prelude (class Show, class Eq, (==), (<<<), eq, bind, pure)

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

type ContextRecord =
  { _id :: ContextInstance
  , _rev :: Revision_
  , displayName :: String
  , pspType :: ContextType
  , buitenRol :: RoleInstance
  , rolInContext :: F.Object (Array RoleInstance)
  , aliases :: F.Object String
  , me :: Maybe RoleInstance
  , universeContextDelta :: SignedDelta
  , states :: Array StateIdentifier
  }

derive instance genericRepPerspectContext :: Generic PerspectContext _

instance showPerspectContext :: Show PerspectContext where
  show = genericShow

instance encodePerspectContext :: Encode PerspectContext where
  encode = genericEncode defaultOptions

instance decodePerspectContext :: Decode PerspectContext where
  decode (json :: Foreign) = do
    rev <- getRev json
    a <- genericDecode defaultOptions json
    pure (changeRevision rev a)

instance eqPerspectContext :: Eq PerspectContext where
  eq (PerspectContext {_id : id1}) (PerspectContext {_id : id2}) = id1 == id2

derive instance newtypePerspectContext :: Newtype PerspectContext _

instance identifiablePerspectContext :: Identifiable PerspectContext ContextInstance where
  identifier (PerspectContext{_id}) = _id
  displayName (PerspectContext{displayName:d}) = d

instance revisionPerspectContext :: Revision PerspectContext where
  rev = _._rev <<< unwrap
  changeRevision s = over PerspectContext (\vr -> vr {_rev = s})

-----------------------------------------------------------
-- PERSPECTROL
-----------------------------------------------------------
newtype PerspectRol = PerspectRol RolRecord

type RolRecord =
  { _id :: RoleInstance
  , pspType :: EnumeratedRoleType
  , context :: ContextInstance
  -- While the fields above occur in every role, those below do not.
  , _rev :: Revision_
  , binding :: Binding
  -- The four fields below could also be modeled as Maybe values.
  , properties :: F.Object (Array Value)
  , gevuldeRollen :: F.Object (Array RoleInstance)
  , occurrence :: Int
  , isMe :: Boolean
  , universeRoleDelta :: SignedDelta
  , contextDelta :: SignedDelta
  , bindingDelta :: Maybe SignedDelta
  -- The first index is the propertytype; the second is the value.
  , propertyDeltas :: F.Object (F.Object SignedDelta)
  , states :: Array StateIdentifier
  }

derive instance genericRepPerspectRol :: Generic PerspectRol _

instance showPerspectRol :: Show PerspectRol where
  show = genericShow

instance eqPerspectRol :: Eq PerspectRol where
  eq (PerspectRol{_id: id1}) (PerspectRol{_id: id2}) = eq id1 id2

derive instance newtypePerspectRol :: Newtype PerspectRol _

instance encodePerspectRol :: Encode PerspectRol where
  encode = genericEncode defaultOptions

instance decodePerspectRol :: Decode PerspectRol where
  -- decode = genericDecode defaultOptions
  decode (json :: Foreign) = do
    rev <- getRev json
    a <- genericDecode defaultOptions json
    pure (changeRevision rev a)

instance revisionPerspectRol :: Revision PerspectRol where
  rev = _._rev <<< unwrap
  changeRevision s = over PerspectRol (\vr -> vr {_rev = s})

instance identifiablePerspectRol :: Identifiable PerspectRol RoleInstance where
  identifier (PerspectRol{_id}) = _id
  displayName (PerspectRol{_id}) = unwrap _id

-----------------------------------------------------------
-- REVISION, BINDING
-----------------------------------------------------------
type Binding = Maybe RoleInstance

binding :: RoleInstance -> Binding
binding id = case id of
  RoleInstance "" -> Nothing
  otherwise -> (Just id)
