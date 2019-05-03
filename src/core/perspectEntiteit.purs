module Perspectives.PerspectEntiteit where

import Control.Monad.Except (throwError, runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff.AVar (AVar, isEmpty, empty, put, read, status, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign (MultipleErrors)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Perspectives.ContextAndRole (changeContext_rev, changeContext_rev', changeContext_type, changeRol_rev, changeRol_rev', changeRol_type, context_id, context_pspType, context_rev', rol_id, rol_pspType, rol_rev')
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveContextFromDomein, retrieveRolFromDomein)
import Perspectives.DomeinFile (addContextToDomeinFile, addRolToDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (Namespace)
import Perspectives.PerspectivesState (contextDefinitionsInsert, contextDefinitionsLookup, contextDefinitionsRemove, rolDefinitionsInsert, rolDefinitionsLookup, rolDefinitionsRemove)
import Perspectives.Syntax (PerspectContext, PerspectRol, Revision)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (*>), (<<<), (<>), (>>=), (>=>))

class (Encode a, Decode a) <=  PerspectEntiteit a where
  getRevision' :: a -> Revision
  setRevision' :: Revision -> a -> a
  setRevision :: String -> a -> a
  getType :: a -> ID
  setType :: ID -> a -> a
  getId :: a -> ID
  -- | Create an empty AVar that will be filled by the PerspectEntiteit.
  representInternally :: ID -> MonadPerspectives (AVar a)
  retrieveInternally :: ID -> MonadPerspectives (Maybe (AVar a))
  removeInternally :: ID -> MonadPerspectives (Maybe (AVar a))
  -- | A default implementation for encode is encodeJSON.
  encode :: a -> String
  -- | A default implementation for decode is decodeJSON.
  decode :: String -> Either MultipleErrors a
  retrieveFromDomein :: ID -> Namespace -> MonadPerspectives a
  cacheInDomeinFile :: ID -> a -> MonadPerspectives Unit

instance perspectEntiteitContext :: PerspectEntiteit PerspectContext where
  getRevision' = context_rev'
  setRevision' = changeContext_rev'
  setRevision = changeContext_rev
  getType = context_pspType
  setType = changeContext_type
  getId = context_id
  representInternally c = (liftAff empty) >>= contextDefinitionsInsert c
  retrieveInternally = contextDefinitionsLookup
  removeInternally = contextDefinitionsRemove
  encode = encodeJSON
  decode = runExcept <<< decodeJSON
  retrieveFromDomein = retrieveContextFromDomein
  cacheInDomeinFile ns c = modifyDomeinFileInCache ns (addContextToDomeinFile c)

instance perspectEntiteitRol :: PerspectEntiteit PerspectRol where
  getRevision' = rol_rev'
  setRevision' = changeRol_rev'
  setRevision = changeRol_rev
  getType = rol_pspType
  setType = changeRol_type
  getId = rol_id
  representInternally c = (liftAff empty) >>= rolDefinitionsInsert c
  retrieveInternally = rolDefinitionsLookup
  removeInternally = rolDefinitionsRemove
  encode = encodeJSON
  decode = runExcept <<< decodeJSON
  retrieveFromDomein = retrieveRolFromDomein
  cacheInDomeinFile ns c = modifyDomeinFileInCache ns (addRolToDomeinFile c)

ensureInternalRepresentation :: forall a. PerspectEntiteit a => ID -> MonadPerspectives (AVar a)
ensureInternalRepresentation c = do
    mav <- retrieveInternally c
    case mav of
      Nothing -> representInternally c
      (Just av) -> pure av

-- | Caches the entiteit. If it was cached before, ensures that the newly cached
-- | entiteit has the same revision value as the old one.
cacheEntiteitPreservingVersion :: forall a. PerspectEntiteit a => ID -> a -> MonadPerspectives Unit
cacheEntiteitPreservingVersion id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    (Just avar) -> do
      ent <- liftAff $ take avar
      e' <- pure $ setRevision' (getRevision' ent) e
      liftAff $ put e' avar

-- | If the entiteit is represented in an AVar, overwrites the stored value.
-- | Otherwise adds an AVar and stores the entiteit in it.
cacheEntiteit :: forall a. PerspectEntiteit a => ID -> a -> MonadPerspectives Unit
cacheEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    otherwise -> void $ cacheCachedEntiteit id e

-- | Store an internally created PerspectEntiteit for the first time in the local store.
cacheUncachedEntiteit :: forall a. PerspectEntiteit a => ID -> a -> MonadPerspectives Unit
cacheUncachedEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure unit
    otherwise -> throwError $ error $ "cacheUncachedEntiteit: the cache should not hold an AVar for " <> id

-- | Modify a PerspectEntiteit in the cache.
cacheCachedEntiteit :: forall a. PerspectEntiteit a => ID -> a -> MonadPerspectives a
cacheCachedEntiteit id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error $ "cacheCachedEntiteit: cannot change an entiteit that is not cached: " <> id
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then liftAff $ put e avar *> pure e
        else do
          _ <- liftAff $ take avar
          liftAff $ put e avar *> pure e

-- | Returns an entity. Throws an error if the resource is not represented in cache or not
-- | immediately available in cache.
readEntiteitFromCache :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
readEntiteitFromCache id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("readEntiteitFromCache needs a locally stored resource for " <> id)
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then throwError $ error ("readEntiteitFromCache found an empty AVar for " <> id)
        else liftAff $ read avar
