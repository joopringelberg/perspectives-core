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

module Decacheable where

import Prelude

import Data.Maybe (Maybe, isJust)
import Persistence.Attachment (class Attachment)
import Perspectives.CoreTypes (class Persistent, MonadPerspectives, removeInternally)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Persistence.API (tryGetDocument)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId)

class Persistent v i <= Decacheable v i | i -> v,  v -> i where
  decache :: i -> MonadPerspectives Unit

instance Decacheable DomeinFile DomeinFileId where
  decache dfid = decache_ dfid

instance Decacheable PerspectContext ContextInstance where
  decache dfid = decache_ dfid

instance Decacheable PerspectRol RoleInstance where
  decache dfid = decache_ dfid

decache_ :: forall v i. Attachment v => Decacheable v i => i -> MonadPerspectives Unit
decache_ id = entityIsInDatabase id >>= if _ 
  then void $ removeInternally id
  else pure unit

entityIsInDatabase :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives Boolean
entityIsInDatabase id = do
  {database, documentName} <- pure {database: "", documentName: ""} -- resourceIdentifier2DocLocator (unwrap id)
  (mdoc :: Maybe a) <- tryGetDocument database documentName
  pure $ isJust mdoc

