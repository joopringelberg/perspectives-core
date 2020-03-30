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

module Perspectives.Assignment.SerialiseAsJson where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Array.NonEmpty (fromArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (Object, singleton, empty) as OBJ
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.CoreTypes (MonadPerspectives, (###>>))
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value, externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Prelude (Unit, bind, map, pure, unit, when, ($), (<$>))

serialisedAsJsonFor :: ContextInstance -> RoleInstance -> MonadPerspectives ContextSerialization
serialisedAsJsonFor cid userId = do
  userType <- roleType_ userId
  serialisedAsJsonFor_ cid userId userType

serialisedAsJsonFor_:: ContextInstance -> RoleInstance -> EnumeratedRoleType -> MonadPerspectives ContextSerialization
serialisedAsJsonFor_ cid userId userType = do
  (PerspectContext{pspType, rolInContext}) <- getPerspectContext cid
  (rollen :: OBJ.Object (SerializableNonEmptyArray RolSerialization)) <- execWriterT $ forWithIndex_ rolInContext serialiseRoleInstances
  PerspectRol{properties} <- getPerspectRol (externalRole cid)
  (externeProperties :: OBJ.Object (Array String)) <- execWriterT $ forWithIndex_ properties serialisePropertiesFor
  pure $ ContextSerialization
    { id: (unwrap cid)
    , prototype: Nothing
    , ctype: (unwrap pspType)
    , rollen
    -- , properties: PropertySerialization externeProperties
    , externeProperties: PropertySerialization OBJ.empty
    }

  where
    serialiseRoleInstances :: String -> Array RoleInstance -> WriterT (OBJ.Object (SerializableNonEmptyArray RolSerialization)) MonadPerspectives Unit
    serialiseRoleInstances roleTypeId roleInstances = do
      -- Now for each role, decide if the user may see it.
      -- If so, add a UniverseRoleDelta and a ContextDelta.
      allowed <- lift (userType ###>> roleIsInPerspectiveOf (ENR $ EnumeratedRoleType roleTypeId))
      when allowed
        case fromArray roleInstances of
          Nothing -> pure unit
          Just roleInstances' -> do
            x <- lift $ traverse (serialiseRoleInstance cid (EnumeratedRoleType roleTypeId)) roleInstances'
            tell $ OBJ.singleton roleTypeId (SerializableNonEmptyArray x)

    serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectives RolSerialization
    serialiseRoleInstance cid' roleTypeId roleInstance = do
      PerspectRol{binding, properties} <- getPerspectRol roleInstance
      (properties' :: (OBJ.Object (Array String))) <- execWriterT $ forWithIndex_ properties serialisePropertiesFor
      -- pure $ RolSerialization { properties: PropertySerialization OBJ.empty, binding: map unwrap binding }
      pure $ RolSerialization { properties: (PropertySerialization properties'), binding: map unwrap binding }

    serialisePropertiesFor :: String -> Array Value -> WriterT (OBJ.Object (Array String)) MonadPerspectives Unit
    serialisePropertiesFor propertyTypeId values = do
      -- For each set of Property Values, add a RolePropertyDelta if the user may see it.
      propAllowed <- lift (userType ###>> propertyIsInPerspectiveOf (ENP (EnumeratedPropertyType propertyTypeId)))
      when propAllowed $ tell (OBJ.singleton propertyTypeId (unwrap <$> values))
