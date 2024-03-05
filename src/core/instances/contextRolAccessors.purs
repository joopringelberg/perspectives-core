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

module Perspectives.ContextRolAccessors where

import Perspectives.CoreTypes (class Persistent, MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Prelude (pure, (>=>), (<<<))

-- Can we safely assume that the PerspectContext will exist? If it does not because there is no model holding it,
-- this function will break. Callers must handle errors.
getContextMember :: forall a. Persistent PerspectContext ContextInstance => (PerspectContext -> a) -> (ContextInstance -> MonadPerspectives a)
getContextMember f = getPerspectContext >=> pure <<< f

getRolMember :: forall a. (PerspectRol -> a) -> (RoleInstance -> MonadPerspectives a)
getRolMember f = getPerspectRol >=> pure <<< f
