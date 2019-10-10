module Perspectives.BasicConstructors where

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Data.Array (concat, length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff (error, throwError)
import Foreign.Object (Object, fromFoldable, toUnfoldable) as FO
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextsSerialisation(..), ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (addRol, removeRol)
import Perspectives.Checking.PerspectivesTypeChecker.Messages (UserMessage(..))
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MP, MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Identifiers (buitenRol, deconstructLocalName, expandDefaultNamespaces)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances (getPerspectEntiteit, tryGetPerspectEntiteit)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Representation.Class.Persistent (cacheUncachedEntiteit, removeInternally)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Prelude (Unit, bind, const, discard, identity, map, pure, show, unit, void, ($), (<<<), (<>), (>=>), (>>>), (<$>))

-- | Construct contexts and roles from the serialisation.
constructContexts :: ContextsSerialisation -> MonadPerspectives (Array UserMessage)
constructContexts (ContextsSerialisation contexts) = (traverse (constructContext >=> (pure <<< (either identity (const [])))) >=> (pure <<< concat)) contexts

constructContext' :: ContextSerialization -> MonadPerspectives ContextInstance
constructContext' c = do
      r <- constructContext c
      case r of
        (Left messages) -> throwError (error (show messages))
        (Right id) -> pure id

-- | Construct a context from the serialization. If a context with the given id exists, returns a UserMessage.
-- | Type checks the context and returns any semantic problems as UserMessages. If there are no problems, returns the ID.
-- | Caches the result but does not save it to Couchdb.
constructContext :: ContextSerialization -> MonadPerspectives (Either (Array UserMessage) ContextInstance)
constructContext c@(ContextSerialization{id, prototype, ctype, rollen, interneProperties, externeProperties}) = do
  contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
  (mc :: Maybe PerspectContext) <- tryGetPerspectEntiteit contextInstanceId
  case mc of
    Nothing -> do
      candidate <- runExceptT $ constructContext_
      case candidate of
        (Left messages) -> do
          removeFromCache contextInstanceId
          pure $ Left messages
        (Right _) -> do
          -- (m :: Array UserMessage) <- checkAContext $ Context contextInstanceId
          m <- pure []
          case length m of
            0 -> pure $ Right contextInstanceId
            otherwise -> do
              removeFromCache contextInstanceId
              pure $ Left m
          -- pure $ Right contextInstanceId
    otherwise -> pure $ Left $ [ContextExists $ unwrap contextInstanceId]

  where
    constructContext_ :: ExceptT (Array UserMessage) (MonadPerspectives) ContextInstance
    constructContext_ = do
      contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
      localName <- maybe (throwError [(NotAValidIdentifier $ unwrap contextInstanceId)]) pure (deconstructLocalName $ unwrap contextInstanceId)
      -- ik denk dat we moeten mappen. Maar de keys moeten ook veranderen.
      (rolIds :: FO.Object (Array RoleInstance)) <-constructRollen
      externalRole <- pure $ RoleInstance $ buitenRol $ unwrap contextInstanceId
      lift $ cacheUncachedEntiteit contextInstanceId
        (PerspectContext defaultContextRecord
          { _id = contextInstanceId
          , displayName  = localName
          , pspType = ContextType $ expandDefaultNamespaces ctype
          , buitenRol = externalRole
          , rolInContext = rolIds
        })
      (b :: Maybe RoleInstance) <- case prototype of
        Nothing -> pure Nothing
        (Just p) -> pure (Just $ RoleInstance (buitenRol (expandDefaultNamespaces p)))
      lift $ cacheUncachedEntiteit externalRole
        (PerspectRol defaultRolRecord
          { _id = externalRole
          , pspType = EnumeratedRoleType $ expandDefaultNamespaces ctype <> "$buitenRolBeschrijving"
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

    constructRollen :: ExceptT (Array UserMessage) (MonadPerspectives) (FO.Object (Array RoleInstance))
    constructRollen = do
      (ts :: Array (Tuple String (Array RolSerialization))) <- pure $ FO.toUnfoldable rollen
      (x :: Array (Tuple String (Array RoleInstance))) <- traverse keyRolInstances ts
      pure $ FO.fromFoldable x

      where
        keyRolInstances :: Tuple String (Array RolSerialization) -> ExceptT (Array UserMessage) (MonadPerspectives) (Tuple String (Array RoleInstance))
        keyRolInstances (Tuple rol rolSerialisations) = do
          expandedRol <- pure $ expandDefaultNamespaces rol
          instances <- constructRolInstances (EnumeratedRoleType expandedRol) rolSerialisations
          pure $ Tuple expandedRol instances

        constructRolInstances :: EnumeratedRoleType -> Array RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives) (Array RoleInstance)
        constructRolInstances rolType rollen' = do
            contextInstanceId <- pure $ expandDefaultNamespaces id
            localName <- maybe (throwError [(NotAValidIdentifier $ unwrap rolType)]) pure (deconstructLocalName $ unwrap rolType)
            -- The id without the numeric index.
            rolId <- pure (contextInstanceId  <> "$" <> localName)
            rolIds <- traverseWithIndex (constructRol rolType (ContextInstance contextInstanceId) rolId) rollen'
            pure rolIds

-- | Caches the result but does not save it to Couchdb.
constructRol :: EnumeratedRoleType -> ContextInstance -> String -> Int -> RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives) RoleInstance
constructRol rolType contextId localName i (RolSerialization {properties, binding: bnd}) = do
  rolInstanceId <- pure $ RoleInstance (localName <> "_" <> (rol_padOccurrence i))
  lift$ cacheUncachedEntiteit rolInstanceId
    (PerspectRol defaultRolRecord
      { _id = rolInstanceId
      , pspType = rolType
      , context = contextId
      , binding = maybe Nothing (Just <<< RoleInstance <<< expandDefaultNamespaces) bnd
      , properties = constructProperties properties
      , occurrence = i
      })
  pure rolInstanceId

-- | Construct and add a Rol instance to the Context instance, provided the construction process doesn't yield
-- | exceptions and that the resulting context instance is semantically correct.
-- | Saves the new Rol instance.
constructAnotherRol :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction (Either (Array UserMessage) RoleInstance)
constructAnotherRol rolType id rolSerialisation = do
  contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
  (candidate :: Either (Array UserMessage) RoleInstance) <- lift $ lift $ do
    rolInstances <- contextInstanceId ##= getRole rolType
    runExceptT $ constructRol rolType contextInstanceId (unwrap contextInstanceId  <> "$" <> (unsafePartial $ fromJust (deconstructLocalName $ unwrap rolType))) (getNextRolIndex rolInstances) rolSerialisation

  case candidate of
    (Left messages) -> pure $ Left messages
    (Right rolId) -> do
      void $ addRol contextInstanceId rolType [rolId]
      -- (m :: Array UserMessage) <- checkAContext $ Context contextInstanceId
      m <- pure []
      case length m of
        0 -> do
          pure $ Right rolId
        otherwise -> do
          void $ removeRol contextInstanceId rolType [rolId]
          pure $ Left m

constructProperties :: PropertySerialization -> FO.Object (Array Value)
constructProperties (PropertySerialization props) = ((FO.toUnfoldable :: FO.Object (Array String) -> Array (Tuple String (Array String))) >>> map keyValuePair >>> FO.fromFoldable) props
  where
    keyValuePair :: Tuple String (Array String) -> (Tuple String (Array Value))
    keyValuePair (Tuple key values) = Tuple (expandDefaultNamespaces key) (Value <$> values)
