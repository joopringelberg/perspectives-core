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

module Perspectives.BasicConstructors
  ( constructContext
  , constructAnotherRol
  )

where

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Data.Array (concat, find, length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff (error, throwError)
import Foreign.Object (Object, fromFoldable, toUnfoldable) as FO
import Foreign.Object (values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextsSerialisation(..), ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Checking.PerspectivesTypeChecker.Messages (UserMessage(..))
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_isMe, rol_padOccurrence)
import Perspectives.CoreTypes (MP, MonadPerspectives, (##=))
import Perspectives.Identifiers (buitenRol, deconstructLocalName, expandDefaultNamespaces)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Persistent (getPerspectEntiteit, tryGetPerspectEntiteit, getPerspectRol)
import Perspectives.Representation.Class.Cacheable (cacheInitially, removeInternally)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Prelude (Unit, bind, const, discard, identity, join, map, pure, show, unit, void, ($), (<$>), (<<<), (<>), (>=>), (>>=), (>>>))

-- | Construct contexts and roles from the serialisation.
constructContexts :: ContextsSerialisation -> MonadPerspectives (Array UserMessage)
constructContexts (ContextsSerialisation contexts) = (traverse (constructContext >=> (pure <<< (either identity (const [])))) >=> (pure <<< concat)) contexts

-- | As constructContext but rethrows errors.
constructContext' :: ContextSerialization -> MonadPerspectives ContextInstance
constructContext' c = do
      r <- constructContext c
      case r of
        (Left messages) -> throwError (error (show messages))
        (Right id) -> pure id

-- | Construct a context from the serialization. If a context with the given id exists, returns a UserMessage.
-- | Type checks the context and returns any semantic problems as UserMessages.
-- | If there are no problems, returns the ID.
-- | Caches the result but does not save it to Couchdb, neither adds it to a Transaction.
-- | Instead, use saveContextInstance to do so.
constructContext :: ContextSerialization -> MonadPerspectives (Either (Array UserMessage) ContextInstance)
constructContext c@(ContextSerialization{id, prototype, ctype, rollen, externeProperties}) = do
  contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
  (mc :: Maybe PerspectContext) <- tryGetPerspectEntiteit contextInstanceId
  case mc of
    Nothing -> do
      candidate <- runExceptT $ constructContext_
      case candidate of
        (Left messages) -> do
          -- TODO. Hier kunnen alle gecachede rollen weer verwijderd worden...
          removeFromCache contextInstanceId
          pure $ Left messages
        (Right _) -> do
          -- (m :: Array UserMessage) <- checkAContext $ Context contextInstanceId
          m <- pure []
          case length m of
            0 -> do
              -- setupAndRunBotActions contextInstanceId
              pure $ Right contextInstanceId
            otherwise -> do
              -- TODO. Hier kunnen alle gecachede rollen weer verwijderd worden...
              removeFromCache contextInstanceId
              pure $ Left m
    otherwise -> pure $ Left $ [ContextExists $ unwrap contextInstanceId]

  where
    constructContext_ :: ExceptT (Array UserMessage) (MonadPerspectives) ContextInstance
    constructContext_ = do
      contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
      localName <- maybe (throwError [(NotAValidIdentifier $ unwrap contextInstanceId)]) pure (deconstructLocalName $ unwrap contextInstanceId)
      (roles :: FO.Object (Array PerspectRol)) <-constructRollen

      externalRole <- pure $ RoleInstance $ buitenRol $ unwrap contextInstanceId
      _ <- lift $ cacheInitially contextInstanceId
        (PerspectContext defaultContextRecord
          { _id = contextInstanceId
          , displayName  = localName
          , pspType = ContextType $ expandDefaultNamespaces ctype
          , buitenRol = externalRole
          , rolInContext = (map (identifier :: PerspectRol -> RoleInstance)) <$> roles
          , me = identifier <$> find rol_isMe (join $ values roles)
        })
      (b :: Maybe RoleInstance) <- case prototype of
        Nothing -> pure Nothing
        (Just p) -> pure (Just $ RoleInstance (buitenRol (expandDefaultNamespaces p)))
      _ <- lift $ cacheInitially externalRole
        (PerspectRol defaultRolRecord
          { _id = externalRole
          , pspType = EnumeratedRoleType $ expandDefaultNamespaces ctype <> "$External"
          , context = contextInstanceId
          , binding = b
          , properties = constructProperties externeProperties
          })
      pure contextInstanceId

    removeFromCache :: ContextInstance -> MP Unit
    removeFromCache id' = do
      -- Here we know for sure that id is in the cache, as it has been just created (but did fail the tests).
      (PerspectContext{rolInContext} :: PerspectContext) <- getPerspectEntiteit id'
      (_ :: Maybe (AVar PerspectContext)) <- removeInternally id'
      (_ :: Maybe (AVar PerspectRol)) <- removeInternally $ RoleInstance $ buitenRol $ unwrap id'
      (_ :: FO.Object (Array (Maybe (AVar PerspectRol)))) <- traverse (traverse removeInternally) rolInContext
      pure unit

    constructRollen :: ExceptT (Array UserMessage) (MonadPerspectives) (FO.Object (Array PerspectRol))
    constructRollen = do
      (ts :: Array (Tuple String (Array RolSerialization))) <- pure $ FO.toUnfoldable rollen
      (x :: Array (Tuple String (Array PerspectRol))) <- traverse keyRolInstances ts
      pure $ FO.fromFoldable x

      where
        keyRolInstances :: Tuple String (Array RolSerialization) -> ExceptT (Array UserMessage) (MonadPerspectives) (Tuple String (Array PerspectRol))
        keyRolInstances (Tuple rol rolSerialisations) = do
          expandedRol <- pure $ expandDefaultNamespaces rol
          instances <- constructRolInstances (EnumeratedRoleType expandedRol) rolSerialisations
          pure $ Tuple expandedRol instances

        constructRolInstances :: EnumeratedRoleType -> Array RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives) (Array PerspectRol)
        constructRolInstances rolType rollen' = do
            contextInstanceId <- pure $ expandDefaultNamespaces id
            localName <- maybe (throwError [(NotAValidIdentifier $ unwrap rolType)]) pure (deconstructLocalName $ unwrap rolType)
            -- The id without the numeric index.
            rolId <- pure (contextInstanceId  <> "$" <> localName)
            lift $ traverseWithIndex (constructRol rolType (ContextInstance contextInstanceId) rolId) rollen'

-- | Constructs a rol and caches it.
constructRol :: EnumeratedRoleType -> ContextInstance -> String -> Int -> RolSerialization -> MonadPerspectives PerspectRol
constructRol rolType contextId localName i (RolSerialization {properties, binding: bnd}) = do
  isMe <- case bnd of
    Nothing -> pure false
    Just id -> getPerspectRol (RoleInstance id) >>= pure <<< (\(PerspectRol{isMe}) -> isMe)
  rolInstanceId <- pure $ RoleInstance (localName <> "_" <> (rol_padOccurrence i))
  role <- pure (PerspectRol defaultRolRecord
        { _id = rolInstanceId
        , pspType = rolType
        , context = contextId
        , binding = maybe Nothing (Just <<< RoleInstance <<< expandDefaultNamespaces) bnd
        , properties = constructProperties properties
        , occurrence = i
        , isMe = isMe
        })
  void $ cacheInitially rolInstanceId role
  pure role

-- | Construct a Role instance for an existing Context instance.
-- | Caches the instance but does not save it, nor add it to a Transaction.
-- | User saveRoleInstance for that purpose
constructAnotherRol :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectives PerspectRol
constructAnotherRol rolType id rolSerialisation = do
  contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
  rolInstanceId <- pure $ (unwrap contextInstanceId  <> "$" <> (unsafePartial $ fromJust (deconstructLocalName $ unwrap rolType)))
  rolInstances <- contextInstanceId ##= getRole rolType
  constructRol rolType contextInstanceId rolInstanceId (getNextRolIndex rolInstances) rolSerialisation

constructProperties :: PropertySerialization -> FO.Object (Array Value)
constructProperties (PropertySerialization props) = ((FO.toUnfoldable :: FO.Object (Array String) -> Array (Tuple String (Array String))) >>> map keyValuePair >>> FO.fromFoldable) props
  where
    keyValuePair :: Tuple String (Array String) -> (Tuple String (Array Value))
    keyValuePair (Tuple key values) = Tuple (expandDefaultNamespaces key) (Value <$> values)
