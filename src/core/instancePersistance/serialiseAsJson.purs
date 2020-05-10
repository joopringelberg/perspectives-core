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

import Control.Monad.State (StateT, execStateT, gets, modify)
import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Control.Plus (empty, void)
import Data.Array (cons, find, tail, union)
import Data.Array (singleton, head) as ARR
import Data.Array.NonEmpty (fromArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Class (encode)
import Foreign.Object (Object, singleton, empty) as OBJ
import Global.Unsafe (unsafeStringify)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.CoreTypes (MPQ, MonadPerspectives, MPT, (###>>), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (context, roleType, roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance, Value(..), externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.Sync.Channel (createChannel)
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Prelude (class Monad, Unit, bind, discard, eq, identity, map, pure, unit, when, ($), (<$>), (<<<), (>>=), (<*), (&&), not)
import Unsafe.Coerce (unsafeCoerce)

-- | A function for the External Core Module `model:Serialise`. The first argument should be a singleton holding
-- | the RoleType (representing a User); the second should be the external role of the
-- | context instance to be serialised.
serialiseFor :: Array RoleType -> RoleInstance -> MPQ Value
serialiseFor userTypes externalRoleId = ArrayT $ case ARR.head userTypes of
  Nothing -> empty
  Just u -> do
    (sers :: Array ContextSerialization) <- lift $ getContextSerialisations (serialiseAsJsonFor_ u) (ContextInstance $ deconstructBuitenRol $ unwrap externalRoleId)
    pure $ ARR.singleton $ Value $ unsafeStringify $ encode sers

serialiseAsJsonFor :: RoleInstance -> ContextInstance -> MonadPerspectives (Array ContextSerialization)
serialiseAsJsonFor userId cid = do
  userType <- roleType_ userId
  getContextSerialisations (serialiseAsJsonFor_ (ENR userType)) cid

type ContextDone m = WriterT (Array ContextSerialization) m

-----------------------------------------------------------------------------------------
---- A NESTED STATE PATTERN TO COLLECT SERIALIZATIONS
-----------------------------------------------------------------------------------------
-- | Collect ContextSerializations in state. This is work done.
type ContextsDone m = StateT (Array ContextSerialization) m

-- | Collect ContextInstances in state. These will represent a list of contexts we have
-- | yet to serialize.
type ContextsToDo m = StateT (Array ContextInstance) m

-- | A function that takes a ContextInstance and produces a stateful computation that
-- | holds both work done and a todo list.
type Serializer m = ContextInstance -> (ContextsToDo (ContextsDone m)) Unit

-- | From a Serializer and a starting point, produce an array of serialized contexts.
getContextSerialisations :: forall m. Monad m => Serializer m -> ContextInstance -> m (Array ContextSerialization)
getContextSerialisations serializer cid = execStateT (execStateT (f cid) []) []
  where
    f :: ContextInstance -> ContextsToDo (ContextsDone m) Unit
    f cid' = do
      serializer cid'
      next <- getNextContextToDo
      case next of
        Just c -> f c
        otherwise -> pure unit

-- | Pop an entry from the todo list.
getNextContextToDo :: forall m. Monad m => ContextsToDo m (Maybe ContextInstance)
getNextContextToDo = gets ARR.head <* modify \s -> maybe [] identity (tail s)

-- | Add to the todo list.
doThisContext :: forall m. Monad m => ContextInstance -> ContextsToDo m (Array ContextInstance)
doThisContext c = modify \ctxts -> union ctxts [c]

-- | Have we serialised this context instance already?
hasContextBeenDone :: forall m. Monad m => ContextInstance -> ContextsToDo (ContextsDone m) Boolean
hasContextBeenDone c = lift $ gets \ctxts -> isJust $ find hasId ctxts
  where
    hasId :: ContextSerialization -> Boolean
    hasId (ContextSerialization{id}) = eq id (unwrap c)

-- | Save work done.
contextHasBeenDone :: forall m. Monad m => ContextSerialization -> ContextsToDo (ContextsDone m) (Array ContextSerialization)
contextHasBeenDone ser = lift $ modify \sers -> cons ser sers

-----------------------------------------------------------------------------------------
---- SERIALIZING
-----------------------------------------------------------------------------------------
type Collecting =  ContextsToDo (ContextsDone MonadPerspectives)

lift2Coll :: forall a. MonadPerspectives a -> Collecting a
lift2Coll = lift <<< lift

serialiseAsJsonFor_:: RoleType -> ContextInstance -> Collecting Unit
serialiseAsJsonFor_ userType cid = do
  (PerspectContext{pspType, rolInContext}) <- lift2Coll $ getPerspectContext cid
  (rollen :: OBJ.Object (SerializableNonEmptyArray RolSerialization)) <- execWriterT $ forWithIndex_ rolInContext serialiseRoleInstances
  PerspectRol{properties} <- lift2Coll $ getPerspectRol (externalRole cid)
  (externeProperties :: OBJ.Object (Array String)) <- lift2Coll $ execWriterT $ forWithIndex_ properties serialisePropertiesFor
  void $ contextHasBeenDone $ ContextSerialization
    { id: (unwrap cid)
    , prototype: Nothing
    , ctype: (unwrap pspType)
    , rollen
    , externeProperties: PropertySerialization externeProperties
    }

  where
    serialiseRoleInstances :: String -> Array RoleInstance -> WriterT (OBJ.Object (SerializableNonEmptyArray RolSerialization)) Collecting Unit
    serialiseRoleInstances roleTypeId roleInstances = do
      -- Now for each role, decide if the user may see it.
      allowed <- lift $ lift2Coll (userType ###>> roleIsInPerspectiveOf (ENR $ EnumeratedRoleType roleTypeId))
      when allowed
        case fromArray roleInstances of
          Nothing -> pure unit
          Just roleInstances' -> do
            rolesAsJson <- lift $ traverse (serialiseRoleInstance cid (EnumeratedRoleType roleTypeId)) roleInstances'
            tell $ OBJ.singleton roleTypeId (SerializableNonEmptyArray rolesAsJson)

    serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> Collecting RolSerialization
    serialiseRoleInstance cid' roleTypeId roleInstance = do
      PerspectRol{binding, properties} <- lift2Coll $ getPerspectRol roleInstance
      (properties' :: (OBJ.Object (Array String))) <- lift2Coll $ execWriterT $ forWithIndex_ properties serialisePropertiesFor
      -- If the userType has a perspective on the role instance, add the context to
      -- the todo list.
      case binding of
        Nothing -> pure $ RolSerialization {id: Just (unwrap roleInstance), properties: (PropertySerialization properties'), binding: Nothing}
        Just b -> do
          c <- lift2Coll (b ##>> context)
          doneBefore <- hasContextBeenDone c
          typeOfBinding <- lift2Coll (b ##>> roleType)
          allowed <- lift2Coll (userType ###>> roleIsInPerspectiveOf (ENR typeOfBinding))
          when (allowed && not doneBefore) (void $ doThisContext c)
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

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Serialise$SerialiseFor" {func: unsafeCoerce serialiseFor, nArgs: 1}
  , Tuple "model:Serialise$AddChannel" {func: unsafeCoerce addChannel, nArgs: 0}
  ]
