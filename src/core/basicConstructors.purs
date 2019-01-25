module Perspectives.BasicConstructors where

import Control.Monad.Eff.AVar (AVar)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Array (length, concat)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Perspectives.ApiTypes (ContextsSerialisation(..), ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextAndRole (addContext_rolInContext, defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage(..), MP)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName)
import Perspectives.Identifiers (binnenRol, buitenRol, deconstructLocalNameFromDomeinURI)
import Perspectives.ObjectGetterConstructors (getRol)
import Perspectives.PerspectEntiteit (cacheUncachedEntiteit, removeInternally)
import Perspectives.Resource (getPerspectEntiteit, tryGetPerspectEntiteit)
import Perspectives.Syntax (Comments(..), PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), binding)
import Perspectives.TypeDefChecker (checkContext)
import Prelude (Unit, bind, discard, map, pure, show, ($), (<>), unit, (<<<), id, const, (>=>))

-- | Construct contexts and roles from the serialisation.
constructContexts :: forall e. ContextsSerialisation -> MonadPerspectives (AjaxAvarCache e) (Array UserMessage)
constructContexts (ContextsSerialisation contexts) = (traverse (constructContext >=> (pure <<< (either id (const [])))) >=> (pure <<< concat)) contexts

-- | Construct a context from the serialization. If a context with the given id exists, returns a UserMessage.
-- | Type checks the context and returns any semantic problems as UserMessages. If there are no problems, returns the ID.
constructContext :: forall e. ContextSerialization -> MonadPerspectives (AjaxAvarCache e) (Either (Array UserMessage) ID)
constructContext c@(ContextSerialization{id}) = do
  (mc :: Maybe PerspectContext) <- tryGetPerspectEntiteit id
  case mc of
    Nothing -> do
      candidate <- runExceptT $ constructContext_ c
      case candidate of
        (Left messages) -> do
          removeFromCache id
          pure $ Left messages
        (Right _) -> do
          (m :: Array UserMessage) <- checkContext id
          case length m of
            0 -> pure $ Right id
            otherwise -> do
              removeFromCache id
              pure $ Left m
    otherwise -> pure $ Left $ [ContextExists id]

  where
    constructContext_ :: ContextSerialization -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) ID
    constructContext_ (ContextSerialization{id, ctype, rollen, interneProperties, externeProperties}) = do
      rolIds <- traverseWithIndex constructRolInstances rollen
      lift $ cacheUncachedEntiteit id
        (PerspectContext defaultContextRecord
          { _id = id
          , displayName  = id
          , pspType = ctype
          , binnenRol =
            PerspectRol defaultRolRecord
              { _id = binnenRol id
              , pspType = ctype <> "$binnenRolBeschrijving"
              , binding = binding $ buitenRol id
              , properties = constructProperties interneProperties
              }
          , buitenRol = buitenRol id
          , rolInContext = rolIds
          , comments = Comments { commentBefore: [], commentAfter: []}
        })
      lift$ cacheUncachedEntiteit (buitenRol id)
        (PerspectRol defaultRolRecord
          { _id = buitenRol id
          , pspType = ctype <> "$buitenRolBeschrijving"
          , context = id
          , binding = Nothing
          , properties = constructProperties externeProperties
          })
      pure id

    constructRolInstances :: RolID -> Array RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) (Array RolID)
    constructRolInstances rolType rollen = do
        localName <- maybe (throwError [(NotAValidIdentifier rolType)]) pure (deconstructLocalNameFromDomeinURI rolType)
        -- The id without the numeric index.
        rolId <- pure (id  <> "$" <> localName <> "_")
        rolIds <- traverseWithIndex (constructRol id rolType rolId) rollen
        pure rolIds

    removeFromCache :: ContextID -> MP e Unit
    removeFromCache id = do
      -- Here we know for sure that id is in the cache, as it has been just created (but did fail the tests).
      (PerspectContext{rolInContext} :: PerspectContext) <- getPerspectEntiteit id
      (_ :: Maybe (AVar PerspectContext)) <- removeInternally id
      (_ :: Maybe (AVar PerspectRol)) <- removeInternally $ buitenRol id
      (_ :: StrMap (Array (Maybe (AVar PerspectRol)))) <- traverse (traverse removeInternally) rolInContext
      pure unit

constructRol :: forall e. RolName -> ContextID -> RolID -> Int -> RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) RolID
constructRol rolType id rolId i (RolSerialization {properties, binding: bnd}) = do
  rolInstanceId <- pure (rolId <> (show i))
  lift$ cacheUncachedEntiteit rolInstanceId
    (PerspectRol defaultRolRecord
      { _id = rolInstanceId
      , pspType = rolType <> "$buitenRolBeschrijving"
      , context = id
      , binding = Just bnd
      , properties = constructProperties properties
      })
  pure rolInstanceId

-- | Construct and add a Rol instance to the Context instance, provided the construction process doesn't yield
-- | exceptions and that the resulting context instance is semantically correct.
constructAnotherRol :: forall e. RolName -> ContextID -> RolSerialization -> MonadPerspectives (AjaxAvarCache e) (Either (Array UserMessage) ID)
constructAnotherRol rolType id rolSerialisation = do
  rolInstances <- getRol rolType id
  candidate <- runExceptT $ constructRol rolType id rolType (length rolInstances) rolSerialisation
  case candidate of
    (Left messages) -> pure $ Left messages
    (Right rolId) -> do
      (m :: Array UserMessage) <- checkContext id
      case length m of
        0 -> do
          (ctxt :: PerspectContext) <- getPerspectEntiteit id
          _ <- pure $ addContext_rolInContext ctxt rolType rolId
          pure $ Right id
        otherwise -> pure $ Left m


constructProperties :: PropertySerialization -> StrMap PropertyValueWithComments
constructProperties (PropertySerialization props) = map (\(v :: Array String) -> PropertyValueWithComments {commentBefore: [], commentAfter: [], value: v}) props
