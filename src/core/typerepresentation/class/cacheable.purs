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
  , module Perspectives.Couchdb.Revision
  , module Perspectives.Representation.TypeIdentifiers
  , cachePreservingRevision
  , cacheEntity
  ) where

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
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, rev)
import Perspectives.CouchdbState (CouchdbUser, UserName)
import Perspectives.DomeinFile (DomeinFile, DomeinFileId)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.PerspectivesState (insert, lookup, remove)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..))

-- | Members of class Cacheable provide functionality to cache and retrieve their representation.
-- | Members are by definition also members of class Revision. We now have two dimensions:
-- |  * is an entitity in cache?
-- |  * do want to preserve the revision on overwriting the cache with a new value for the entity?
-- | This gives rise to four possibilities, one of which is nonsense (not cached before,
-- | but preserve the revision). These are the three remaining functions:
-- |
-- | * cacheEntity;
-- | * cachePreservingRevision: before overwriting the cached value, take its revision and
-- | store it in the new value to cache;
-- | * cacheEntity: just overwrite the cached value.
-- |

type Identifier = String
type Namespace = String

class (Identifiable v i, Revision v, Newtype i String) <= Cacheable v i | v -> i, i -> v where
  theCache :: MonadPerspectives (GLStrMap (AVar v))
  -- | Create an empty AVar that will be filled by the PerspectEntiteit.
  representInternally :: i -> MonadPerspectives (AVar v)
  retrieveInternally :: i -> MonadPerspectives (Maybe (AVar v))
  removeInternally :: i -> MonadPerspectives (Maybe (AVar v))

-- | Caches the entiteit. This operation is neutral w.r.t. the revision value
-- | (ff it was cached before, ensures that the newly cached
-- | entiteit has the same revision value as the old one).
-- overwriteWithOldRevision
cachePreservingRevision :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cachePreservingRevision id e = do
  (mcachedE :: Maybe a) <- tryReadEntiteitFromCache id
  case mcachedE of
    Nothing -> cacheEntity id e
    Just cachedE -> cacheEntity id (changeRevision (rev cachedE) e)

-- TODO. Vervang cacheEntity en cacheEntity door deze functie.
cacheEntity :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheEntity id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure av
    Just avar -> do
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

tryReadEntiteitFromCache :: forall a i. Cacheable a i => i -> MonadPerspectives (Maybe a)
tryReadEntiteitFromCache id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> pure Nothing
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then pure Nothing
        else Just <$> (liftAff $ read avar)

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------
instance cacheablePerspectContext :: Cacheable PerspectContext ContextInstance where
  -- identifier = _._id <<< unwrap
  theCache = gets _.contextInstances
  representInternally c = do
    av <- liftAff empty
    insert theCache (unwrap c) av
  retrieveInternally i = lookup theCache (unwrap i)
  removeInternally i = remove theCache (unwrap i)

instance cacheablePerspectRol :: Cacheable PerspectRol RoleInstance where
  theCache = gets _.rolInstances
  representInternally c = do
    av <- liftAff empty
    insert theCache (unwrap c) av
  retrieveInternally i = lookup theCache (unwrap i)
  removeInternally i = remove theCache (unwrap i)

instance cacheableDomeinFile :: Cacheable DomeinFile DomeinFileId where
  theCache = gets _.domeinCache
  representInternally c = do
    av <- liftAff empty
    insert theCache (unwrap c) av
  retrieveInternally i = lookup theCache (unwrap i)
  removeInternally i = remove theCache (unwrap i)

instance cacheableCouchdbUser :: Cacheable CouchdbUser UserName where
  theCache = pure $ new unit
  representInternally c = do
    av <- liftAff empty
    insert theCache (unwrap c) av
  retrieveInternally i = lookup theCache (unwrap i)
  removeInternally i = remove theCache (unwrap i)
