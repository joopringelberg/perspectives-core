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

module Perspectives.Sync.Class.DeltaClass where

import Data.Array (cons, findIndex, union, unsafeIndex, updateAt)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (over2)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseRoleDelta(..))
import Prelude ((+), class Eq, (==), ($))

class Eq d <= DeltaClass d where
  addBase :: Int -> d -> d
  setIndex :: Int -> d -> d
  getSequenceNumber :: d -> Int
  -- | Add the users of the second delta to those of the first delta, returning that new delta as the function result.
  unionOfUsers :: d -> d -> d

instance contextDeltaDeltaUsers :: DeltaClass ContextDelta where
  addBase i (ContextDelta r@{sequenceNumber}) = ContextDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (ContextDelta r) = ContextDelta r {sequenceNumber = i}
  getSequenceNumber (ContextDelta{sequenceNumber}) = sequenceNumber
  unionOfUsers = over2 ContextDelta unionOfUsers_

instance roleBindingDeltaDeltaUsers :: DeltaClass RoleBindingDelta where
  addBase i (RoleBindingDelta r@{sequenceNumber}) = RoleBindingDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (RoleBindingDelta r) = RoleBindingDelta r {sequenceNumber = i}
  getSequenceNumber (RoleBindingDelta{sequenceNumber}) = sequenceNumber
  unionOfUsers = over2 RoleBindingDelta unionOfUsers_

instance rolePropertyDeltaDeltaUsers :: DeltaClass RolePropertyDelta where
  addBase i (RolePropertyDelta r@{sequenceNumber}) = RolePropertyDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (RolePropertyDelta r) = RolePropertyDelta r {sequenceNumber = i}
  getSequenceNumber (RolePropertyDelta{sequenceNumber}) = sequenceNumber
  unionOfUsers = over2 RolePropertyDelta unionOfUsers_

instance deltaUsersUniverseContextDelta :: DeltaClass UniverseContextDelta where
  addBase i (UniverseContextDelta r@{sequenceNumber}) = UniverseContextDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (UniverseContextDelta r) = UniverseContextDelta r {sequenceNumber = i}
  getSequenceNumber (UniverseContextDelta{sequenceNumber}) = sequenceNumber
  unionOfUsers = over2 UniverseContextDelta unionOfUsers_

instance deltaUsersUniverseRoleDelta :: DeltaClass UniverseRoleDelta where
  addBase i (UniverseRoleDelta r@{sequenceNumber}) = UniverseRoleDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (UniverseRoleDelta r) = UniverseRoleDelta r {sequenceNumber = i}
  getSequenceNumber (UniverseRoleDelta{sequenceNumber}) = sequenceNumber
  unionOfUsers = over2 UniverseRoleDelta unionOfUsers_

unionOfUsers_ :: forall f. {users :: Array RoleInstance | f} -> {users :: Array RoleInstance | f} -> {users :: Array RoleInstance | f}
unionOfUsers_ r1@{users: u1} {users: u2} = r1 {users = u1 `union` u2}

-- | Add a Delta to the Array of the same Delta type. If that contains a Delta that is equal except for
-- | the users and the index, modify that found Delta by adding the users of the new Delta to it.
addToSet :: forall d. DeltaClass d => d -> Array d -> Int -> Array d
addToSet d ds i = let
  old = findIndex ((==) d) ds
  in
    case old of
      Nothing -> cons (setIndex i d) ds
      Just ind -> let
        x = unsafePartial $ unsafeIndex ds ind
        in unsafePartial $ fromJust $ updateAt ind (unionOfUsers x d) ds
