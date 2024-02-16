-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Representation.Class.Cacheable
  ( module Perspectives.Representation.Class.Cacheable
  , module Perspectives.Couchdb.Revision
  , module Perspectives.Representation.TypeIdentifiers
  , cacheEntity
  , overwriteEntity
  ) where

-- | Members of Cacheable trade identifiers for a representation.

import Prelude

import Control.Monad.Except (throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, isEmpty, put, read, status, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (class Cacheable, MonadPerspectives, representInternally, retrieveInternally)
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, rev)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..), DomeinFileId)

-- | Members of class Cacheable provide functionality to cache and retrieve their representation.
-- | Members are by definition also members of class Revision.

type Identifier = String
type Namespace = String

-- | Handles all cases:
-- | * when an entity had not been cached before;
-- | * when an entity has no revision (i.e. when it had not been saved to Couchdb before)
-- | * when it had been saved to Couchdb before.
-- | Finally, it also handles the dynamics of an AVar that can be empty and filled.
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
          oldE <- liftAff $ take avar
          -- If the cache holds a version with a revision that is higher than 
          -- that of the entity, replace the entities revision.
          if (rev oldE) > (rev e)
          then liftAff $ put (changeRevision (rev oldE) e) avar *> pure avar
          else liftAff $ put e avar *> pure avar

-- | Put the entity in the existing AVar (overwriting it completely) or create a new AVar.
overwriteEntity :: forall a i. Cacheable a i => i -> a -> MonadPerspectives (AVar a)
overwriteEntity id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure av
    Just avar -> do 
      -- Make the AVar empty.
      _ <- liftAff $ take avar
      -- Put the new value in place.
      liftAff $ put e avar *> pure avar

-- | Returns an entity. Throws an error if the resource is not represented in cache or not
-- | immediately available in cache.
takeEntiteitFromCache :: forall a i. Cacheable a i => i -> MonadPerspectives a
takeEntiteitFromCache = retrieveFromCache take

readEntiteitFromCache :: forall a i. Cacheable a i => i -> MonadPerspectives a
readEntiteitFromCache = retrieveFromCache read

retrieveFromCache :: forall a i. Cacheable a i => (AVar a -> Aff a) -> i -> MonadPerspectives a
retrieveFromCache retrieve id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("retrieveFromCache needs a locally stored resource for " <>  unwrap id)
    (Just avar) -> liftAff $ retrieve avar

tryTakeEntiteitFromCache :: forall a i. Cacheable a i => i -> MonadPerspectives (Maybe a)
tryTakeEntiteitFromCache = tryRetrieveFromCache take

tryReadEntiteitFromCache :: forall a i. Cacheable a i => i -> MonadPerspectives (Maybe a)
tryReadEntiteitFromCache = tryRetrieveFromCache read

tryRetrieveFromCache :: forall a i. Cacheable a i => (AVar a -> Aff a) -> i -> MonadPerspectives (Maybe a)
tryRetrieveFromCache retrieve id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> pure Nothing
    (Just avar) -> liftAff $ Just <$> retrieve avar


-- | Blocks is AVar is empty; does nothing when id is not represented internally.
setRevision :: forall a i. Cacheable a i => i -> Revision_ -> MonadPerspectives Unit
setRevision id r = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> pure unit
    Just avar -> do
      entity <- liftAff $ take avar
      if r > rev entity 
      then liftAff $ put (changeRevision r entity) avar
      else liftAff $ put entity avar

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------

