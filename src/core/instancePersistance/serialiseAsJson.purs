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

module Perspectives.Instances.SerialiseAsJson where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Array (singleton) as ARR
import Data.Array.NonEmpty (fromArray)
import Data.Array.Partial (head)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Class (encode)
import Foreign.Object (Object, singleton, empty) as OBJ
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.CollectAffectedContexts (userHasNoPerspectiveOnRoleInstance)
import Perspectives.CoreTypes (MonadPerspectives, MPQ, (###>>), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (bottom, roleType, roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance, Value(..), externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), singleton)
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Prelude (Unit, bind, discard, map, pure, unit, when, ($), (<$>), (<<<), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | A function for the External Core Module `model:Serialise`. The first argument should be a singleton holding
-- | the string value of a User role type; the second should be a singleton holding the string value of the
-- | context instance to be serialised.
-- | Notice that, in the use case we want to serialise an Invitation context for a user we do not yet have contact
-- | with, we can only have his type.
serialiseFor :: Array String -> Array String -> MPQ Value
serialiseFor userTypeIds externalRoleIds = ArrayT $ lift $ (execWriterT $ serialiseAsJsonFor_ (ContextInstance $ deconstructBuitenRol $ unsafePartial $ head externalRoleIds) Nothing (EnumeratedRoleType $ unsafePartial $ head userTypeIds) )>>= pure <<< ARR.singleton <<< Value <<< unsafeStringify <<< encode

serialiseAsJsonFor :: RoleInstance -> ContextInstance -> MonadPerspectives (Array ContextSerialization)
serialiseAsJsonFor userId cid = do
  userType <- roleType_ userId
  systemUser <- userId ##>> bottom
  execWriterT $ serialiseAsJsonFor_ cid (Just systemUser) userType

type WriteContexts m = WriterT (Array ContextSerialization) m

serialiseAsJsonFor_:: ContextInstance -> Maybe RoleInstance -> EnumeratedRoleType -> WriteContexts MonadPerspectives Unit
serialiseAsJsonFor_ cid userId userType = do
  (PerspectContext{pspType, rolInContext}) <- lift $ getPerspectContext cid
  (rollen :: OBJ.Object (SerializableNonEmptyArray RolSerialization)) <- execWriterT $ forWithIndex_ rolInContext serialiseRoleInstances
  PerspectRol{properties} <- lift $ getPerspectRol (externalRole cid)
  (externeProperties :: OBJ.Object (Array String)) <- lift $ execWriterT $ forWithIndex_ properties serialisePropertiesFor
  tell $ [ContextSerialization
    { id: (unwrap cid)
    , prototype: Nothing
    , ctype: (unwrap pspType)
    , rollen
    , externeProperties: PropertySerialization OBJ.empty
    }]

  where
    serialiseRoleInstances :: String -> Array RoleInstance -> WriterT (OBJ.Object (SerializableNonEmptyArray RolSerialization)) (WriteContexts MonadPerspectives) Unit
    serialiseRoleInstances roleTypeId roleInstances = do
      -- Now for each role, decide if the user may see it.
      allowed <- lift $ lift (userType ###>> roleIsInPerspectiveOf (ENR $ EnumeratedRoleType roleTypeId))
      when allowed
        case fromArray roleInstances of
          Nothing -> pure unit
          Just roleInstances' -> do
            rolesAsJson <- lift $ traverse (serialiseRoleInstance cid (EnumeratedRoleType roleTypeId)) roleInstances'
            tell $ OBJ.singleton roleTypeId (SerializableNonEmptyArray rolesAsJson)

    serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> WriteContexts MonadPerspectives RolSerialization
    serialiseRoleInstance cid' roleTypeId roleInstance = do
      PerspectRol{binding, properties} <- lift $ getPerspectRol roleInstance
      (properties' :: (OBJ.Object (Array String))) <- lift $ execWriterT $ forWithIndex_ properties serialisePropertiesFor
      case binding of
        Nothing -> pure unit
        Just b -> case userId of
          Nothing -> (lift $ execWriterT $ serialiseBinding b) >>= tell
          Just userId' -> do
            typeOfBinding <- lift (b ##>> roleType)
            shouldBeSent <- lift $ userHasNoPerspectiveOnRoleInstance typeOfBinding b userId'
            when shouldBeSent do
              (ctxts :: Array ContextSerialization) <- lift $ execWriterT $ serialiseBinding b
              tell ctxts
      pure $ RolSerialization {id: Just (unwrap roleInstance), properties: (PropertySerialization properties'), binding: map unwrap binding }

    serialisePropertiesFor :: String -> Array Value -> WriterT (OBJ.Object (Array String)) MonadPerspectives Unit
    serialisePropertiesFor propertyTypeId values = do
      -- For each set of Property Values, add a RolePropertyDelta if the user may see it.
      propAllowed <- lift (userType ###>> propertyIsInPerspectiveOf (ENP (EnumeratedPropertyType propertyTypeId)))
      when propAllowed $ tell (OBJ.singleton propertyTypeId (unwrap <$> values))

    serialiseBinding :: RoleInstance -> WriteContexts MonadPerspectives Unit
    serialiseBinding roleInstance = do
      PerspectRol{context, pspType: roleType} <- lift $ getPerspectRol roleInstance
      -- Serialise the roleInstance
      (r :: RolSerialization) <- serialiseRoleInstance context roleType roleInstance
      -- Serialise the contextInstance with just the roleInstance
      (PerspectContext{pspType}) <- lift $ getPerspectContext context
      tell $ [ContextSerialization
        { id: (unwrap context)
        , prototype: Nothing
        , ctype: (unwrap pspType)
        , rollen: OBJ.singleton (unwrap roleType) (singleton r)
        , externeProperties: PropertySerialization OBJ.empty
        }]

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Serialise$SerialiseFor" {func: unsafeCoerce serialiseFor, nArgs: 1}
  ]
