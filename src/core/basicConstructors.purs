module Perspectives.BasicConstructors where

import Control.Monad.Eff.AVar (AVar)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Data.Array (concat, length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, fromFoldable, toUnfoldable)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Perspectives.Actions (addRol, removeRol)
import Perspectives.ApiTypes (ContextsSerialisation(..), ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage(..), MP)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName)
import Perspectives.Identifiers (binnenRol, buitenRol, deconstructLocalNameFromDomeinURI, expandDefaultNamespaces)
import Perspectives.ObjectGetterConstructors (getRolInContext)
import Perspectives.PerspectEntiteit (cacheUncachedEntiteit, removeInternally)
import Perspectives.PerspectivesTypes (RolDef(..))
import Perspectives.Resource (getPerspectEntiteit, tryGetPerspectEntiteit)
import Perspectives.Syntax (Comments(..), PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..))
import Perspectives.TypeDefChecker (checkContext)
import Prelude (Unit, bind, const, discard, id, map, pure, show, unit, void, ($), (<<<), (<>), (>=>), (>>>))

-- | Construct contexts and roles from the serialisation.
constructContexts :: forall e. ContextsSerialisation -> MonadPerspectives (AjaxAvarCache e) (Array UserMessage)
constructContexts (ContextsSerialisation contexts) = (traverse (constructContext >=> (pure <<< (either id (const [])))) >=> (pure <<< concat)) contexts

-- | Construct a context from the serialization. If a context with the given id exists, returns a UserMessage.
-- | Type checks the context and returns any semantic problems as UserMessages. If there are no problems, returns the ID.
constructContext :: forall e. ContextSerialization -> MonadPerspectives (AjaxAvarCache e) (Either (Array UserMessage) ID)
constructContext c@(ContextSerialization{id}) = do
  ident <- pure $ expandDefaultNamespaces id
  (mc :: Maybe PerspectContext) <- tryGetPerspectEntiteit ident
  case mc of
    Nothing -> do
      candidate <- runExceptT $ constructContext_ c
      case candidate of
        (Left messages) -> do
          removeFromCache ident
          pure $ Left messages
        (Right _) -> do
          (m :: Array UserMessage) <- checkContext ident
          case length m of
            0 -> pure $ Right ident
            otherwise -> do
              removeFromCache ident
              pure $ Left m
          -- pure $ Right ident
    otherwise -> pure $ Left $ [ContextExists ident]

  where
    constructContext_ :: ContextSerialization -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) ID
    constructContext_ (ContextSerialization{id, ctype, rollen, interneProperties, externeProperties}) = do
      ident <- pure $ expandDefaultNamespaces id
      localName <- maybe (throwError [(NotAValidIdentifier ident)]) pure (deconstructLocalNameFromDomeinURI ident)
      -- ik denk dat we moeten mappen. Maar de keys moeten ook veranderen.
      (rolIds :: StrMap (Array RolID)) <-constructRollen rollen
      lift $ cacheUncachedEntiteit ident
        (PerspectContext defaultContextRecord
          { _id = ident
          , displayName  = localName
          , pspType = expandDefaultNamespaces ctype
          , binnenRol =
            PerspectRol defaultRolRecord
              { _id = binnenRol ident
              , pspType = expandDefaultNamespaces ctype <> "$binnenRolBeschrijving"
              , binding = Just $ buitenRol ident
              , context = ident
              , properties = constructProperties interneProperties
              }
          , buitenRol = buitenRol ident
          , rolInContext = rolIds
          , comments = Comments { commentBefore: [], commentAfter: []}
        })
      lift$ cacheUncachedEntiteit (buitenRol ident)
        (PerspectRol defaultRolRecord
          { _id = buitenRol ident
          , pspType = expandDefaultNamespaces ctype <> "$buitenRolBeschrijving"
          , context = ident
          , binding = Nothing
          , properties = constructProperties externeProperties
          })
      pure ident

    removeFromCache :: ContextID -> MP e Unit
    removeFromCache id = do
      -- Here we know for sure that id is in the cache, as it has been just created (but did fail the tests).
      (PerspectContext{rolInContext} :: PerspectContext) <- getPerspectEntiteit id
      (_ :: Maybe (AVar PerspectContext)) <- removeInternally id
      (_ :: Maybe (AVar PerspectRol)) <- removeInternally $ buitenRol id
      (_ :: StrMap (Array (Maybe (AVar PerspectRol)))) <- traverse (traverse removeInternally) rolInContext
      pure unit

    constructRollen :: StrMap (Array RolSerialization) -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) (StrMap (Array ID))
    constructRollen rollen = do
      (ts :: Array (Tuple String (Array RolSerialization))) <- pure $ toUnfoldable rollen
      (x :: Array (Tuple String (Array ID))) <- traverse keyRolInstances ts
      pure $ fromFoldable x

      where
        keyRolInstances :: Tuple String (Array RolSerialization) -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) (Tuple String (Array ID))
        keyRolInstances (Tuple rol rolSerialisations) = do
          expandedRol <- pure $ expandDefaultNamespaces rol
          instances <- constructRolInstances expandedRol rolSerialisations
          pure $ Tuple expandedRol instances

        constructRolInstances :: String -> Array RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) (Array RolID)
        constructRolInstances rolType rollen = do
            contextId <- pure $ expandDefaultNamespaces id
            localName <- maybe (throwError [(NotAValidIdentifier rolType)]) pure (deconstructLocalNameFromDomeinURI rolType)
            -- The id without the numeric index.
            rolId <- pure (contextId  <> "$" <> localName <> "_")
            rolIds <- traverseWithIndex (constructRol rolType contextId rolId) rollen
            pure rolIds

constructRol :: forall e. RolName -> ContextID -> RolID -> Int -> RolSerialization -> ExceptT (Array UserMessage) (MonadPerspectives (AjaxAvarCache e)) RolID
constructRol rolType contextId rolId i (RolSerialization {properties, binding: bnd}) = do
  rolInstanceId <- pure (expandDefaultNamespaces rolId <> (show i))
  lift$ cacheUncachedEntiteit rolInstanceId
    (PerspectRol defaultRolRecord
      { _id = rolInstanceId
      , pspType = rolType -- TODO: dit is verkeerd: hier wordt de context id gebruikt
      , context = contextId
      , binding = Just $ expandDefaultNamespaces bnd
      , properties = constructProperties properties
      })
  pure rolInstanceId

-- | Construct and add a Rol instance to the Context instance, provided the construction process doesn't yield
-- | exceptions and that the resulting context instance is semantically correct.
-- | Saves the new Rol instance.
constructAnotherRol :: forall e. RolName -> ContextID -> RolSerialization -> MonadPerspectives (AjaxAvarCache e) (Either (Array UserMessage) ID)
constructAnotherRol rolType id rolSerialisation = do
  ident <- pure $ expandDefaultNamespaces id
  rolInstances <- getRolInContext (RolDef rolType) ident
  candidate <- runExceptT $ constructRol rolType ident (ident <> maybe "" (\x -> x) (deconstructLocalNameFromDomeinURI rolType)) (length rolInstances) rolSerialisation
  case candidate of
    (Left messages) -> pure $ Left messages
    (Right rolId) -> do
      void $ addRol rolType rolId ident
      (m :: Array UserMessage) <- checkContext ident
      case length m of
        0 -> do
          pure $ Right rolId
        otherwise -> do
          void $ removeRol rolType rolId ident
          pure $ Left m

constructProperties :: PropertySerialization -> StrMap PropertyValueWithComments
constructProperties (PropertySerialization props) = ((toUnfoldable :: StrMap (Array String) -> Array (Tuple String (Array String))) >>> map keyValuePair >>> fromFoldable >>> map f) props
  where
    keyValuePair :: Tuple String (Array String) -> (Tuple String (Array String))
    keyValuePair (Tuple key values) = Tuple (expandDefaultNamespaces key) values

    f :: Array String -> PropertyValueWithComments
    f v = PropertyValueWithComments {commentBefore: [], commentAfter: [], value: v}
