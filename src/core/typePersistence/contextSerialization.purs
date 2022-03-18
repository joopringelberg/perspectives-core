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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.
-- END LICENSE

-- | This module contains functions to create a JSON structure from a Perspective,
-- | that will be used by the client to build a screen automatically.

module Perspectives.TypePersistence.ContextSerialisation where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (type (~~>), MonadPerspectivesQuery, AssumptionTracking)
import Perspectives.Data.EncodableMap (lookup)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (deconstructNamespace_)
import Perspectives.Query.Interpreter (lift2MPQ)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.ScreenDefinition (ColumnDef(..), FormDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), TabDef(..), TableDef(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleType)
import Perspectives.TypePersistence.PerspectiveSerialisation (perspectiveForContextAndUserFromId)
import Simple.JSON (writeJSON)

newtype SerialisedScreen = SerialisedScreen String
derive instance newTypeSerialisedScreen :: Newtype SerialisedScreen _

screenForContextAndUser :: RoleType -> ContextType -> (ContextInstance ~~> SerialisedScreen)
screenForContextAndUser userRoleType contextType contextInstance = do
  DomeinFile df <- lift2MPQ $ retrieveDomeinFile (deconstructNamespace_ $ unwrap contextType)
  screenDefinition <- case lookup (ScreenKey contextType userRoleType) df.screens of
    Just s -> pure s
    Nothing -> constructDefaultScreen userRoleType contextType
  -- Now populate the screen definition with instance data.
  (screenInstance :: ScreenDefinition) <- lift $ addPerspectives screenDefinition
  pure $ SerialisedScreen $ writeJSON screenInstance

  where
    addPerspectives :: ScreenDefinition -> AssumptionTracking ScreenDefinition
    addPerspectives sd = pure sd

constructDefaultScreen :: RoleType -> ContextType -> MonadPerspectivesQuery ScreenDefinition
constructDefaultScreen userRoleType contextType = pure $ ScreenDefinition
  { title: ""
  , tabs: Nothing
  , rows: Nothing
  , columns: Nothing
  }

-----------------------------------------------------------
-- CLASS ADDPERSPECTIVES
-----------------------------------------------------------
class AddPerspectives a where
  addPerspectives :: RoleInstance -> ContextInstance -> a -> AssumptionTracking a

instance addPerspectivesScreenDefinition :: AddPerspectives ScreenDefinition where
  addPerspectives user ctxt (ScreenDefinition r) = do
    tabs <- case r.tabs of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (addPerspectives user ctxt) t
    rows <- case r.rows of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (addPerspectives user ctxt) t
    columns <- case r.columns of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (addPerspectives user ctxt) t
    pure $ ScreenDefinition {title: r.title, tabs, rows, columns}

instance addPerspectivesScreenElementDef  :: AddPerspectives ScreenElementDef where
  addPerspectives user ctxt (RowElementD re) = RowElementD <$> addPerspectives user ctxt re
  addPerspectives user ctxt (ColumnElementD re) = ColumnElementD <$> addPerspectives user ctxt re
  addPerspectives user ctxt (TableElementD re) = TableElementD <$> addPerspectives user ctxt re
  addPerspectives user ctxt (FormElementD re) = FormElementD <$> addPerspectives user ctxt re

instance addPerspectivesTabDef  :: AddPerspectives TabDef where
  addPerspectives  user ctxt(TabDef r) = do
    elements <- traverse (addPerspectives user ctxt) r.elements
    pure $ TabDef {title: r.title, elements}

instance addPerspectivesColumnDef  :: AddPerspectives ColumnDef where
  addPerspectives user ctxt (ColumnDef cols) = do
    cols' <- traverse (addPerspectives user ctxt) cols
    pure $ ColumnDef cols'

instance addPerspectivesRowDef  :: AddPerspectives RowDef where
  addPerspectives user ctxt (RowDef cols) = do
    cols' <- traverse (addPerspectives user ctxt) cols
    pure $ RowDef cols'

instance addPerspectivesTableDef  :: AddPerspectives TableDef where
  addPerspectives user ctxt (TableDef widgetCommonFields) = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetCommonFields.userRole
      widgetCommonFields.perspectiveId
      ctxt
    pure $ TableDef widgetCommonFields {perspective = Just perspective}

instance addPerspectivesFormDef  :: AddPerspectives FormDef where
  addPerspectives user ctxt (FormDef widgetCommonFields) = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetCommonFields.userRole
      widgetCommonFields.perspectiveId
      ctxt
    pure $ FormDef widgetCommonFields {perspective = Just perspective}
