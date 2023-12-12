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

module Perspectives.Representation.Class.PersistentType
  ( module Perspectives.Representation.Class.PersistentType
  , module Perspectives.Couchdb.Revision
  , module Perspectives.Representation.TypeIdentifiers) where

import Perspectives.Couchdb.Revision

import Control.Monad.Except (catchError, throwError)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Exception (error)
import Foreign.Object (insert, lookup) as FO
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.State (State)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, CalculatedRoleType, ContextType, DomeinFileId(..), EnumeratedPropertyType, EnumeratedRoleType, PerspectiveType(..), StateIdentifier(..), ViewType)
import Perspectives.Representation.View (View)
import Prelude (class Eq, class Show, Unit, bind, const, pure, show, unit, ($), (<<<), (<>), (>>=), (<$>))

type Namespace = String

class (Show i, Identifiable v i, Revision v, Newtype i String, Eq v) <= PersistentType v i | v -> i, i -> v where
  retrieveFromDomein :: i -> DomeinFileId -> MonadPerspectives v
  cacheInDomeinFile :: i -> v -> MonadPerspectives Unit

-- | Get any type representation for Perspectives, either from cache or from a model file in
-- | couchdb.
getPerspectType :: forall v i. PersistentType v i => i -> MonadPerspectives v
getPerspectType id = do
  mns <- pure (typeUri2ModelUri (unwrap id))
  case mns of
    Nothing -> throwError (error $ "getPerspectType cannot retrieve type with incorrectly formed id: '" <> show id <> "'.")
    (Just ns) -> retrieveFromDomein id (DomeinFileId ns)

tryGetPerspectType :: forall v i. PersistentType v i => i -> MonadPerspectives (Maybe v)
tryGetPerspectType id = catchError ((getPerspectType id) >>= (pure <<< Just))
  \_ -> pure Nothing

getEnumeratedRole :: EnumeratedRoleType -> MP EnumeratedRole
getEnumeratedRole = getPerspectType

getCalculatedRole :: CalculatedRoleType -> MP CalculatedRole
getCalculatedRole = getPerspectType

getEnumeratedProperty :: EnumeratedPropertyType -> MP EnumeratedProperty
getEnumeratedProperty = getPerspectType

getCalculatedProperty :: CalculatedPropertyType -> MP CalculatedProperty
getCalculatedProperty = getPerspectType

getContext :: ContextType -> MP Context
getContext = getPerspectType

getView :: ViewType -> MP View
getView = getPerspectType

getState :: StateIdentifier -> MP State
getState = getPerspectType

tryGetState :: StateIdentifier -> MP (Maybe State)
tryGetState = tryGetPerspectType

typeExists :: forall v i. PersistentType v i => i -> MP Boolean
typeExists id = catchError (((getPerspectType id) :: MP v) >>= pure <<< const true) \e -> pure false

-----------------------------------------------------------
-- ADD TO A DOMEINFILE
-----------------------------------------------------------
addContextToDomeinFile :: Context -> DomeinFile -> DomeinFile
addContextToDomeinFile c (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = FO.insert (unwrap $ (identifier c :: ContextType)) c contexts}

addEnumeratedRoleToDomeinFile :: EnumeratedRole -> DomeinFile -> DomeinFile
addEnumeratedRoleToDomeinFile c (DomeinFile dff@{enumeratedRoles}) = DomeinFile dff {enumeratedRoles = FO.insert (unwrap $ (identifier c :: EnumeratedRoleType)) c enumeratedRoles}

-- addQueryFunctionToDomeinFile :: String -> QueryFunction -> DomeinFile -> DomeinFile
-- addQueryFunctionToDomeinFile id c (DomeinFile dff@{queries}) = DomeinFile dff {queries = FO.insert (unwrap $ (identifier c :: QueryFunctionType)) c queries}

-----------------------------------------------------------
-----------------------------------------------------------
ifNamespace :: forall i. Newtype i String => i -> (DomeinFile -> DomeinFile) -> MP Unit
ifNamespace i modifier = maybe (pure unit) (modifyDomeinFileInCache modifier) (DomeinFileId <$> typeUri2ModelUri (unwrap i))

-- Put error boundaries around calls to this function.
retrieveFromDomein_ :: forall v i. PersistentType v i =>
  i
  -> (DomeinFile -> Maybe v)
  -> DomeinFileId
  -> (MonadPerspectives v)
retrieveFromDomein_ id lookupFunction ns = do
  df <- retrieveDomeinFile ns
  case lookupFunction df of
    Nothing -> throwError $ error ("retrieveFromDomein_: cannot find definition of " <> (show id) <> " for " <> show ns)
    (Just v) -> pure v

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------
instance persistentContext :: PersistentType Context ContextType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{contexts}) -> DomeinFile dff {contexts = FO.insert (unwrap i) v contexts})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{contexts}) -> FO.lookup (unwrap i) contexts)

instance persistentEnumeratedRole :: PersistentType EnumeratedRole EnumeratedRoleType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{enumeratedRoles}) -> DomeinFile dff {enumeratedRoles = FO.insert (unwrap i) v enumeratedRoles})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{enumeratedRoles}) -> FO.lookup (unwrap i) enumeratedRoles)

instance persistentCalculatedRole :: PersistentType CalculatedRole CalculatedRoleType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{calculatedRoles}) -> DomeinFile dff {calculatedRoles = FO.insert (unwrap i) v calculatedRoles})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{calculatedRoles}) -> FO.lookup (unwrap i) calculatedRoles)

instance persistentEnumeratedProperty :: PersistentType EnumeratedProperty EnumeratedPropertyType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{enumeratedProperties}) -> DomeinFile dff {enumeratedProperties = FO.insert (unwrap i) v enumeratedProperties})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{enumeratedProperties}) -> FO.lookup (unwrap i) enumeratedProperties)

instance persistentCalculatedProperty :: PersistentType CalculatedProperty CalculatedPropertyType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{calculatedProperties}) -> DomeinFile dff {calculatedProperties = FO.insert (unwrap i) v calculatedProperties})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{calculatedProperties}) -> FO.lookup (unwrap i) calculatedProperties) 

instance persistentView :: PersistentType View ViewType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{views}) -> DomeinFile dff {views = FO.insert (unwrap i) v views})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{views}) -> FO.lookup (unwrap i) views)

instance persistentState :: PersistentType State StateIdentifier where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{states}) -> DomeinFile dff {states = FO.insert (unwrap i) v states})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{states}) -> FO.lookup (unwrap i) states)
