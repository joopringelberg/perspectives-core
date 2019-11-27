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

module Perspectives.Representation.Class.Cacheable
  ( module Perspectives.Representation.Class.Cacheable
  , module Perspectives.Representation.Class.Revision
  , module Perspectives.Representation.TypeIdentifiers) where

-- | Members of Cacheable trade identifiers for a representation.

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Except (throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.AVar (AVar, isEmpty, empty, put, read, status, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile, DomeinFileId(..))
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.PerspectivesState (insert, lookup, remove)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_, changeRevision, rev)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..))

-- | Members of Cacheable trade identifiers for a representation.

type Identifier = String
type Namespace = String

class (Identifiable v i, Revision v, Newtype i String) <= Cacheable v i | v -> i, i -> v where
  cache :: i -> MonadPerspectives (GLStrMap (AVar v))
  -- | Create an empty AVar that will be filled by the PerspectEntiteit.
  representInternally :: i -> MonadPerspectives (AVar v)
  retrieveInternally :: i -> MonadPerspectives (Maybe (AVar v))
  removeInternally :: i -> MonadPerspectives (Maybe (AVar v))

changeRevisionDefault :: forall f. String -> {_rev :: Revision_ | f} -> {_rev :: Revision_ | f}
changeRevisionDefault rev cr = cr {_rev = Just rev}

ensureInternalRepresentation :: forall i a. Cacheable a i => i -> MonadPerspectives (AVar a)
ensureInternalRepresentation c = do
    mav <- retrieveInternally c
    case mav of
      Nothing -> representInternally c
      (Just av) -> pure av

-- | Caches the entiteit. If it was cached before, ensures that the newly cached
-- | entiteit has the same revision value as the old one.
cacheEntiteitPreservingVersion :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheEntiteitPreservingVersion id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    (Just avar) -> do
      ent <- liftAff $ take avar
      e' <- pure $ changeRevision (rev ent) e
      liftAff $ put e' avar
      pure avar

-- | If the entiteit is represented in an AVar, overwrites the stored value.
-- | Otherwise adds an AVar and stores the entiteit in it.
cacheEntiteit :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    otherwise -> cacheCachedEntiteit id e

-- | Store an internally created PerspectEntiteit for the first time in the local store.
cacheUncachedEntiteit :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheUncachedEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure av
    otherwise -> throwError $ error $ "cacheUncachedEntiteit: the cache should not hold an AVar for " <> unwrap id

-- | Modify a PerspectEntiteit in the cache.
cacheCachedEntiteit :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheCachedEntiteit id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error $ "cacheCachedEntiteit: cannot change an entiteit that is not cached: " <> unwrap id
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then liftAff $ put e avar *> pure avar
        else do
          _ <- liftAff $ take avar
          liftAff $ put e avar *> pure avar

-- | Returns an entity. Throws an error if the resource is not represented in cache or not
-- | immediately available in cache.
readEntiteitFromCache :: forall a i. Cacheable a i => i -> MonadPerspectives a
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
instance cacheablePerspectContext :: Cacheable PerspectContext ContextInstance where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.contextInstances
  representInternally c = do
    av <- liftAff empty
    insert (cache (ContextInstance "")) (unwrap c) av
  retrieveInternally i = lookup (cache (ContextInstance "")) (unwrap i)
  removeInternally i = remove (cache (ContextInstance "")) (unwrap i)

instance cacheablePerspectRol :: Cacheable PerspectRol RoleInstance where
  cache _ = gets _.rolInstances
  representInternally c = do
    av <- liftAff empty
    insert (cache (RoleInstance "")) (unwrap c) av
  retrieveInternally i = lookup (cache (RoleInstance "")) (unwrap i)
  removeInternally i = remove (cache (RoleInstance "")) (unwrap i)

instance cacheableDomeinFile :: Cacheable DomeinFile DomeinFileId where
  cache _ = gets _.domeinCache
  representInternally c = do
    av <- liftAff empty
    insert (cache (DomeinFileId "")) (unwrap c) av
  retrieveInternally i = lookup (cache (DomeinFileId "")) (unwrap i)
  removeInternally i = remove (cache (DomeinFileId "")) (unwrap i)
