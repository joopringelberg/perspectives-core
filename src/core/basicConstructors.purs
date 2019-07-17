module Perspectives.BasicConstructors where

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Data.Array (concat, length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff (error, throwError)
import Foreign.Object (Object, fromFoldable, toUnfoldable) as FO
import Perspectives.Actions (addRol, removeRol)
import Perspectives.ApiTypes (ContextsSerialisation(..), ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage(..), MP)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName)
import Perspectives.Identifiers (LocalName, binnenRol, buitenRol, deconstructLocalNameFromDomeinURI, expandDefaultNamespaces)
import Perspectives.ObjectGetterConstructors (getRolInContext)
import Perspectives.PerspectEntiteit (cacheUncachedEntiteit, removeInternally)
import Perspectives.Instances (getPerspectEntiteit, tryGetPerspectEntiteit)
import Perspectives.InstanceRepresentation (Comments(..), PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..))
import Prelude (Unit, bind, const, discard, identity, map, pure, show, unit, void, ($), (<<<), (<>), (>=>), (>>>))

-- | Construct contexts and roles from the serialisation.
constructContexts :: ContextsSerialisation -> MonadPerspectives (Array UserMessage)
constructContexts (ContextsSerialisation contexts) = (traverse (constructContext >=> (pure <<< (either identity (const [])))) >=> (pure <<< concat)) contexts

constructContext' :: ContextSerialization -> MonadPerspectives ID
constructContext' c = do
      r <- constructContext c
      case r of
        (Left messages) -> throwError (error (show messages))
        (Right id) -> pure id

-- | Construct a context from the serialization. If a context with the given id exists, returns a UserMessage.
-- | Type checks the context and returns any semantic problems as UserMessages. If there are no problems, returns the ID.
constructContext :: ContextSerialization -> MonadPerspectives (Either (Array UserMessage) ID)
constructContext c@(ContextSerialization{id, prototype, ctype, rollen, interneProperties, externeProperties}) = do
  ident <- pure $ expandDefaultNamespaces id
  (mc :: Maybe PerspectContext) <- tryGetPerspectEntiteit ident
  case mc of
    Nothing -> do
      candidate <- runExceptT $ constructContext_
      case candidate of
        (Left messages) -> do
          removeFromCache ident
          pure $ Left messages
        (Right _) -> do
          -- (m :: Array UserMessage) <- checkAContext $ Context ident
          m <- pure []
          case length m of
            0 -> pure $ Right ident
            otherwise -> do
              removeFromCache ident
              pure $ Left m
          -- pure $ Right ident
    otherwise -> pure $ Left $ [ContextExists ident]

  where
    constructContext_ :: ExceptT (Array UserMessage) (MonadPerspectives) ID
    constructContext_ = do
      ident <- pure $ expandDefaultNamespaces id
      localName <- maybe (throwError [(NotAValidIdentifier ident)]) pure (deconstructLocalNameFromDomeinURI ident)
      -- ik denk dat we moeten mappen. Maar de keys moeten ook veranderen.
      (rolIds :: FO.Object (Array RolID)) <-constructRollen
      lift $ cacheUncachedEntiteit ident
        (PerspectContext defaultContextRecord
          { _id = ident
          , displayName  = localName
          , pspType = expandDefaultNamespaces ctype
          , binnenRol = binnenRol ident
          , buitenRol = buitenRol ident
          , rolInContext = rolIds
          , comments = Comments { commentBefore: [], commentAfter: []}
        })
      lift $ cacheUncachedEntiteit (binnenRol ident)
        (PerspectRol defaultRolRecord
          { _id = binnenRol ident
          , pspType = expandDefaultNamespaces ctype <> "$binnenRolBeschrijving"
          , binding = Just $ buitenRol ident
          , context = ident
          , properties = constructProperties interneProperties
          })
      (b :: Maybe String) <- case prototype of
        Nothing -> pure Nothing
        (Just p) -> pure (Just (buitenRol (expandDefaultNamespaces p)))
      lift $ cacheUncachedEntiteit (buitenRol ident)
        (PerspectRol defaultRolRecord
          { _id = buitenRol ident
          , pspType = expandDefaultNamespaces ctype <> "$buitenRolBeschrijving"
          , context = ident
          , binding = b
          , properties = constructProperties externeProperties
          })
      pure ident

    removeFromCache :: ContextID -> MP Unit
    removeFromCache id' = do
      -- Here we know for sure that id is in the cache, as it has been just created (but did fail the tests).
      (PerspectContext{rolInContext} :: PerspectContext) <- getPerspectEntiteit id'
      (_ :: Maybe (AVar PerspectContext)) <- removeInternally id'
      (_ :: Maybe (AVar PerspectRol)) <- removeInternally $ buitenRol id'
      (_ :: FO.Object (Array (Maybe (AVar PerspectRol)))) <- traverse (traverse removeInternally) rolInContext
      pure unit

    constructRollen :: ExceptT (Array UserMessage) (MonadPerspectives) (FO.Object (Array ID))
    constructRollen = do
      (ts :: Array (Tuple String (Array RolSerialization))) <- pure $ FO.toUnfoldable rollen
      (x :: Array (Tuple String (Array ID))) <- traverse keyRolInstances ts
      pure $ FO.fromFoldable x

      where
        keyRolInstances :: Tuple String (Array RolSerialization) -> ExceptT (Array UserMessage) (MonadPerspectives) (Tuple String (Array ID))
        keyRolInstances (Tuple rol rolSerialisations) = do
          expandedRol <- pure $ expandDefaultNamespaces rol
          instances <- constructRolInstances expandedRol rolSerialisations
          pure $ Tuple expandedRol instances

        constructRolInstances :: String -> Array RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives) (Array RolID)
        constructRolInstances rolType rollen' = do
            contextId <- pure $ expandDefaultNamespaces id
            localName <- maybe (throwError [(NotAValidIdentifier rolType)]) pure (deconstructLocalNameFromDomeinURI rolType)
            -- The id without the numeric index.
            rolId <- pure (contextId  <> "$" <> localName <> "_")
            rolIds <- traverseWithIndex (constructRol rolType contextId rolId) rollen'
            pure rolIds

constructRol :: RolName -> ContextID -> LocalName -> Int -> RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives) RolID
constructRol rolType contextId localName i (RolSerialization {properties, binding: bnd}) = do
  rolInstanceId <- pure (expandDefaultNamespaces localName <> "_" <> (rol_padOccurrence i))
  lift$ cacheUncachedEntiteit rolInstanceId
    (PerspectRol defaultRolRecord
      { _id = rolInstanceId
      , pspType = rolType
      , context = contextId
      , binding = maybe Nothing (Just <<< expandDefaultNamespaces) bnd
      , properties = constructProperties properties
      , occurrence = i
      })
  pure rolInstanceId

-- | Construct and add a Rol instance to the Context instance, provided the construction process doesn't yield
-- | exceptions and that the resulting context instance is semantically correct.
-- | Saves the new Rol instance.
constructAnotherRol :: RolName -> ContextID -> RolSerialization -> MonadPerspectives (Either (Array UserMessage) ID)
constructAnotherRol rolType id rolSerialisation = do
  ident <- pure $ expandDefaultNamespaces id
  rolInstances <- getRolInContext (RolDef rolType) ident
  candidate <- runExceptT $ constructRol rolType ident (ident  <> "$" <> (ident <> maybe "" (\x -> x) (deconstructLocalNameFromDomeinURI rolType))) (typeWithPerspectivesTypes getNextRolIndex rolInstances) rolSerialisation
  case candidate of
    (Left messages) -> pure $ Left messages
    (Right rolId) -> do
      void $ addRol rolType rolId ident
      -- (m :: Array UserMessage) <- checkAContext $ Context ident
      m <- pure []
      case length m of
        0 -> do
          pure $ Right rolId
        otherwise -> do
          void $ removeRol rolType rolId ident
          pure $ Left m

constructProperties :: PropertySerialization -> FO.Object PropertyValueWithComments
constructProperties (PropertySerialization props) = ((FO.toUnfoldable :: FO.Object (Array String) -> Array (Tuple String (Array String))) >>> map keyValuePair >>> FO.fromFoldable >>> map f) props
  where
    keyValuePair :: Tuple String (Array String) -> (Tuple String (Array String))
    keyValuePair (Tuple key values) = Tuple (expandDefaultNamespaces key) values

    f :: Array String -> PropertyValueWithComments
    f v = PropertyValueWithComments {commentBefore: [], commentAfter: [], value: v}
