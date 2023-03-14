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

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Instances.Indexed where

import Control.Monad.AvarMonadAsk (get) as AMA
import Control.Monad.Error.Class (throwError, try)
import Data.Array (foldM, foldl, head, cons)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (Object, fromFoldable, keys)
import Foreign.Object.Unsafe (unsafeIndex)
import Perspectives.ContextAndRole (rol_binding, rol_property)
import Perspectives.CoreTypes (MonadPerspectives, (##>))
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Instances.ObjectGetters (binding, context)
import Perspectives.ModelDependencies (indexedContextName, indexedRoleName)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.Class.Identifiable (identifier, identifier_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Prelude (bind, pure, ($), (<>), (>=>), (>>=), (<<<), flip)

-- | Replace any occurrence of any indexed name in the string.
replaceIndexedNames :: String -> MonadPerspectives String
replaceIndexedNames s = do
  {indexedRoles:roleReplacements, indexedContexts:contextReplacements} <- AMA.get
  s' <- pure $ foldl (\(crl_ :: String) iname -> replaceAll (Pattern iname) (Replacement $ unwrap $ unsafeIndex roleReplacements iname) crl_) s (keys roleReplacements)
  pure $ foldl (\(crl_ :: String) iname -> replaceAll (Pattern iname) (Replacement $ unwrap $ unsafeIndex contextReplacements iname) crl_) s' (keys contextReplacements)

indexedRoles_ :: Array RoleInstance -> MonadPerspectives (Object RoleInstance)
indexedRoles_ roleIds = do
  rows <- foldM
    (\rows roleId -> (try $ getPerspectRol roleId) >>=
      handlePerspectRolError' "indexedRoles_" rows (f >=> (pure <<< flip cons rows)))
    []
    roleIds
  pure $ fromFoldable rows
  where
    f :: PerspectRol -> MonadPerspectives (Tuple String RoleInstance)
    f r = case rol_binding r of
      Nothing -> throwError (error ("An instance of sys:PerspectivesSystem$IndexedRoles has no binding: " <> identifier_ r))
      Just b -> case head $ rol_property r (EnumeratedPropertyType indexedRoleName) of
        Nothing -> throwError (error ("An instance of sys:PerspectivesSystem$IndexedRoles$Name has no value: " <> identifier_ r))
        Just (Value iname) -> pure (Tuple iname b)

indexedContexts_ :: Array RoleInstance -> MonadPerspectives (Object ContextInstance)
indexedContexts_ contextRoleIds = do
  rows <- foldM
    (\rows roleId -> (try $ getPerspectRol roleId) >>=
      handlePerspectRolError' "indexedContexts_" rows (f >=> (pure <<< flip cons rows)))
    []
    contextRoleIds
  pure $ fromFoldable rows
  where
    f :: PerspectRol -> MonadPerspectives (Tuple String ContextInstance)
    f r = case head $ rol_property r (EnumeratedPropertyType indexedContextName) of
        Nothing -> throwError (error ("An instance of sys:PerspectivesSystem$IndexedContexts$Name has no value: " <> identifier_ r))
        Just (Value iname) -> do
          mcontextId <- (identifier r) ##> binding >=> context
          case mcontextId of
            Nothing -> throwError (error ("An instance of sys:PerspectivesSystem$IndexedContexts has no context bound to it: " <> identifier_ r))
            Just c -> pure (Tuple iname c)
