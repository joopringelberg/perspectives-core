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
  , module Perspectives.Representation.TypeIdentifiers
  , cachePreservingRevision
  , cacheInitially
  , cacheOverwritingRevision
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
import Perspectives.CouchdbState (CouchdbUser, UserName)
import Perspectives.DomeinFile (DomeinFile, DomeinFileId)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.PerspectivesState (insert, lookup, remove)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_, changeRevision, rev)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..))

-- | Members of class Cacheable provide functionality to cache and retrieve their representation.
-- | Members are by definition also members of class Revision. We now have two dimensions:
-- |  * is an entitity in cache?
-- |  * do want to preserve the revision on overwriting the cache with a new value for the entity?
-- | This gives rise to four possibilities, one of which is nonsense (not cached before,
-- | but preserve the revision). These are the three remaining functions:
-- |
-- | * cacheInitially;
-- | * cachePreservingRevision: before overwriting the cached value, take its revision and
-- | store it in the new value to cache;
-- | * cacheOverwritingRevision: just overwrite the cached value.
-- |

type Identifier = String
type Namespace = String

class (Identifiable v i, Revision v, Newtype i String) <= Cacheable v i | v -> i, i -> v where
  cache :: MonadPerspectives (GLStrMap (AVar v))
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
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheInitially id e
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      -- This is a precaution to prevent us from waiting on an operation that does not return.
      if empty
        then liftAff $ put e avar *> pure avar
        else do
          ent <- liftAff $ take avar
          e' <- pure $ changeRevision (rev ent) e
          liftAff $ put e' avar
          pure avar

-- | Store an internally created PerspectEntiteit for the first time in the local store.
-- cacheInitially
cacheInitially :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheInitially id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure av
    otherwise -> throwError $ error $ "cacheInitially: the cache should not hold an AVar for " <> unwrap id

-- | Modify a PerspectEntiteit in the cache. Overwrites the revision value.
cacheOverwritingRevision :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
cacheOverwritingRevision id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> cacheInitially id e
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
  cache = gets _.contextInstances
  representInternally c = do
    av <- liftAff empty
    insert cache (unwrap c) av
  retrieveInternally i = lookup cache (unwrap i)
  removeInternally i = remove cache (unwrap i)

instance cacheablePerspectRol :: Cacheable PerspectRol RoleInstance where
  cache = gets _.rolInstances
  representInternally c = do
    av <- liftAff empty
    insert cache (unwrap c) av
  retrieveInternally i = lookup cache (unwrap i)
  removeInternally i = remove cache (unwrap i)

instance cacheableDomeinFile :: Cacheable DomeinFile DomeinFileId where
  cache = gets _.domeinCache
  representInternally c = do
    av <- liftAff empty
    insert cache (unwrap c) av
  retrieveInternally i = lookup cache (unwrap i)
  removeInternally i = remove cache (unwrap i)

instance cacheableCouchdbUser :: Cacheable CouchdbUser UserName where
  cache = pure $ new unit
  representInternally c = do
    av <- liftAff empty
    insert cache (unwrap c) av
  retrieveInternally i = lookup cache (unwrap i)
  removeInternally i = remove cache (unwrap i)
