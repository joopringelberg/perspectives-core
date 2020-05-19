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

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Control.Plus (empty, void)
import Data.Array (catMaybes, find, snoc)
import Data.Array (singleton, head) as ARR
import Data.Array.NonEmpty (fromArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Class (encode)
import Foreign.Object (Object, singleton, empty, toUnfoldable, fromFoldable) as OBJ
import Global.Unsafe (unsafeStringify)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MPQ, MPT, MonadPerspectives, (###>>), (##>>), (##>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (bottom, context, roleType, roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..), externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.Sync.Channel (addPartnerToChannel, createChannel)
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Perspectives.User (getHost, getPort)
import Prelude (class Monad, Unit, bind, discard, eq, map, pure, when, ($), (<$>), (>>=), (&&), not, (>>>), (<>), show)
import Unsafe.Coerce (unsafeCoerce)

-- | A function for the External Core Module `model:Serialise`. The first argument should be a singleton holding
-- | the RoleType (representing a User); the second should be the external role of the
-- | context instance to be serialised.
serialiseFor :: Array RoleType -> RoleInstance -> MPQ Value
serialiseFor userTypes externalRoleId = ArrayT $ case ARR.head userTypes of
  Nothing -> empty
  Just u -> do
    (sers :: Array ContextSerialization) <- lift $ execStateT (serialiseAsJsonFor_ u (ContextInstance $ deconstructBuitenRol $ unwrap externalRoleId)) []
    pure $ ARR.singleton $ Value $ unsafeStringify $ encode sers

serialiseAsJsonFor :: RoleInstance -> ContextInstance -> MonadPerspectives (Array ContextSerialization)
serialiseAsJsonFor userId cid = do
  userType <- roleType_ userId
  execStateT (serialiseAsJsonFor_ (ENR userType) cid) []

type ContextDone m = WriterT (Array ContextSerialization) m

-----------------------------------------------------------------------------------------
---- A STATE PATTERN TO COLLECT SERIALIZATIONS
-----------------------------------------------------------------------------------------
-- | Collect ContextSerializations in state. This is work done.
type ContextsDone m = StateT (Array ContextSerialization) m

-- | Have we serialised this context instance already?
hasContextBeenDone :: forall m. Monad m => ContextInstance -> (ContextsDone m) Boolean
hasContextBeenDone c = gets \ctxts -> isJust $ find hasId ctxts
  where
    hasId :: ContextSerialization -> Boolean
    hasId (ContextSerialization{id}) = eq id (unwrap c)

-- | Save work done.
contextHasBeenDone :: forall m. Monad m => ContextSerialization -> (ContextsDone m) (Array ContextSerialization)
contextHasBeenDone ser = modify \sers -> snoc sers ser

-----------------------------------------------------------------------------------------
---- SERIALIZING
-----------------------------------------------------------------------------------------
type Collecting =  (ContextsDone MonadPerspectives)

lift2Coll :: forall a. MonadPerspectives a -> Collecting a
lift2Coll = lift

serialiseAsJsonFor_:: RoleType -> ContextInstance -> Collecting Unit
serialiseAsJsonFor_ userType cid = do
  (PerspectContext{pspType, rolInContext}) <- lift2Coll $ getPerspectContext cid
  (rollen :: OBJ.Object (SerializableNonEmptyArray RolSerialization)) <- traverse serialiseRoleInstances (OBJ.toUnfoldable rolInContext) >>= catMaybes >>> OBJ.fromFoldable >>> pure
  PerspectRol{properties} <- lift $ getPerspectRol (externalRole cid)
  (externeProperties :: OBJ.Object (Array String)) <- lift $ execWriterT $ forWithIndex_ properties serialisePropertiesFor
  void $ contextHasBeenDone $ ContextSerialization
    { id: (unwrap cid)
    , prototype: Nothing
    , ctype: (unwrap pspType)
    , rollen
    , externeProperties: PropertySerialization externeProperties
    }

  where
    serialiseRoleInstances :: Tuple String (Array RoleInstance) -> Collecting (Maybe (Tuple String (SerializableNonEmptyArray RolSerialization)))
    serialiseRoleInstances (Tuple roleTypeId roleInstances) = do
      -- Now for each role, decide if the user may see it.
      allowed <- lift $ (userType ###>> roleIsInPerspectiveOf (ENR $ EnumeratedRoleType roleTypeId))
      if allowed
        then case fromArray roleInstances of
          Nothing -> pure Nothing
          Just roleInstances' -> do
            rolesAsJson <- traverse (serialiseRoleInstance cid (EnumeratedRoleType roleTypeId)) roleInstances'
            pure $ Just $ Tuple roleTypeId (SerializableNonEmptyArray rolesAsJson)
        else pure Nothing

    serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> Collecting RolSerialization
    serialiseRoleInstance cid' roleTypeId roleInstance = do
      PerspectRol{binding, properties} <- lift $ getPerspectRol roleInstance
      (properties' :: (OBJ.Object (Array String))) <- lift $ execWriterT $ forWithIndex_ properties serialisePropertiesFor
      -- If the userType has a perspective on the role instance, add the context to
      -- the todo list.
      case binding of
        Nothing -> pure $ RolSerialization {id: Just (unwrap roleInstance), properties: (PropertySerialization properties'), binding: Nothing}
        Just b -> do
          c <- lift (b ##>> context)
          doneBefore <- hasContextBeenDone c
          typeOfBinding <- lift (b ##>> roleType)
          allowed <- lift (userType ###>> roleIsInPerspectiveOf (ENR typeOfBinding))
          -- TODO. Serialiseer de context niet als de ander er al een rol bij speelt!
          when (allowed && not doneBefore) (serialiseAsJsonFor_ userType c)
          pure $ RolSerialization {id: Just (unwrap roleInstance), properties: (PropertySerialization properties'), binding: if allowed then map unwrap binding else Nothing}

    serialisePropertiesFor :: String -> Array Value -> WriterT (OBJ.Object (Array String)) MonadPerspectives Unit
    serialisePropertiesFor propertyTypeId values = do
      -- For each set of Property Values, add a RolePropertyDelta if the user may see it.
      propAllowed <- lift (userType ###>> propertyIsInPerspectiveOf (ENP (EnumeratedPropertyType propertyTypeId)))
      when propAllowed $ tell (OBJ.singleton propertyTypeId (unwrap <$> values))

-- | This function expects an instance of type sys:Invitation, creates a channel and binds it to the Invitation
-- | in the role PrivateChannel.
addChannel :: ContextInstance -> MPT Unit
addChannel invitation = createChannel >>= \channel -> void $ createAndAddRoleInstance
  (EnumeratedRoleType "model:System$Invitation$PrivateChannel")
  (unwrap invitation)
  (RolSerialization{id: Nothing, properties: PropertySerialization OBJ.empty, binding: Just (buitenRol $ unwrap channel)})

addConnectedPartnerToChannel :: Array String -> Array String -> (ContextInstance -> MPT Unit)
addConnectedPartnerToChannel userArr channelArr cid = do
  log $ "addConnectedPartnerToChannel " <> show userArr <> " en " <> show channelArr
  case ARR.head userArr of
    Nothing -> throwError (error "addConnectedPartnerToChannel did not get a value for the first argument.")
    Just r -> do
      sysUser <- lift2 ((RoleInstance r) ##> bottom)
      case sysUser of
        Nothing -> throwError (error "addConnectedPartnerToChannel: first argument is not a User Role (this error may not occur).")
        Just usr -> case ARR.head channelArr of
          Nothing -> throwError (error "addConnectedPartnerToChannel did not get a value for the second argument.")
          Just channelId -> do
            log $ "addConnectedPartnerToChannel " <> show usr <> " en " <> show channelId
            host <- lift2 getHost
            port <- lift2 getPort
            addPartnerToChannel usr (ContextInstance channelId) host port

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Serialise$SerialiseFor" {func: unsafeCoerce serialiseFor, nArgs: 1}
  , Tuple "model:Serialise$AddChannel" {func: unsafeCoerce addChannel, nArgs: 0}
  , Tuple "model:Serialise$AddConnectedPartnerToChannel" {func: unsafeCoerce addConnectedPartnerToChannel, nArgs: 2}
  ]
