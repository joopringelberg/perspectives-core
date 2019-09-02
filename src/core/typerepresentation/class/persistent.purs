module Perspectives.Representation.Class.Persistent
  ( module Perspectives.Representation.Class.Persistent
  , module Perspectives.Representation.Class.Revision
  , module Perspectives.Representation.TypeIdentifiers) where

-- | Members of Persistent trade identifiers for a representation.

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Except (throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.AVar (AVar, isEmpty, empty, put, read, status, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.PerspectivesState (insert, lookup, remove)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_, changeRevision, rev)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..))

-- | Members of Persistent trade identifiers for a representation.

type Identifier = String
type Namespace = String

class (Identifiable v i, Revision v, Newtype i String) <= Persistent v i where
  cache :: i -> MonadPerspectives (GLStrMap (AVar v))
  -- | Create an empty AVar that will be filled by the PerspectEntiteit.
  representInternally :: i -> MonadPerspectives (AVar v)
  retrieveInternally :: i -> MonadPerspectives (Maybe (AVar v))
  removeInternally :: i -> MonadPerspectives (Maybe (AVar v))

changeRevisionDefault :: forall f. String -> {_rev :: Revision_ | f} -> {_rev :: Revision_ | f}
changeRevisionDefault rev cr = cr {_rev = Just rev}

ensureInternalRepresentation :: forall i a. Persistent a i => i -> MonadPerspectives (AVar a)
ensureInternalRepresentation c = do
    mav <- retrieveInternally c
    case mav of
      Nothing -> representInternally c
      (Just av) -> pure av

-- | Caches the entiteit. If it was cached before, ensures that the newly cached
-- | entiteit has the same revision value as the old one.
cacheEntiteitPreservingVersion :: forall a i. Persistent a i => i -> a -> MonadPerspectives Unit
cacheEntiteitPreservingVersion id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    (Just avar) -> do
      ent <- liftAff $ take avar
      e' <- pure $ changeRevision (rev ent) e
      liftAff $ put e' avar

-- | If the entiteit is represented in an AVar, overwrites the stored value.
-- | Otherwise adds an AVar and stores the entiteit in it.
cacheEntiteit :: forall a i. Persistent a i => i -> a -> MonadPerspectives Unit
cacheEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    otherwise -> void $ cacheCachedEntiteit id e

-- | Store an internally created PerspectEntiteit for the first time in the local store.
cacheUncachedEntiteit :: forall a i. Persistent a i => i -> a -> MonadPerspectives Unit
cacheUncachedEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure unit
    otherwise -> throwError $ error $ "cacheUncachedEntiteit: the cache should not hold an AVar for " <> unwrap id

-- | Modify a PerspectEntiteit in the cache.
cacheCachedEntiteit :: forall a i. Persistent a i => i -> a -> MonadPerspectives a
cacheCachedEntiteit id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error $ "cacheCachedEntiteit: cannot change an entiteit that is not cached: " <> unwrap id
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then liftAff $ put e avar *> pure e
        else do
          _ <- liftAff $ take avar
          liftAff $ put e avar *> pure e

-- | Returns an entity. Throws an error if the resource is not represented in cache or not
-- | immediately available in cache.
readEntiteitFromCache :: forall a i. Persistent a i => i -> MonadPerspectives a
readEntiteitFromCache id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("readEntiteitFromCache needs a locally stored resource for " <>  unwrap id)
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then throwError $ error ("readEntiteitFromCache found an empty AVar for " <> unwrap id)
        else liftAff $ read avar

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------
instance persistentPerspectContext :: Persistent PerspectContext ContextInstance where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.contextInstances
  representInternally c = do
    av <- liftAff empty
    insert (cache (ContextInstance "")) (unwrap c) av
  retrieveInternally i = lookup (cache (ContextInstance "")) (unwrap i)
  removeInternally i = remove (cache (ContextInstance "")) (unwrap i)

instance persistentPerspectRol :: Persistent PerspectRol RoleInstance where
  cache _ = gets _.rolInstances
  representInternally c = do
    av <- liftAff empty
    insert (cache (RoleInstance "")) (unwrap c) av
  retrieveInternally i = lookup (cache (RoleInstance "")) (unwrap i)
  removeInternally i = remove (cache (RoleInstance "")) (unwrap i)
