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

-- | Type representations are persisted to a database in a so-called DomeinFile. See:
-- | * [PersistentType](Perspectives.Representation.Class.PersistentType.html#t:PersistentType)
-- | * [Perspectives.DomeinFile](Perspectives.DomeinFile.html#t:DomeinFile)
-- | * [DomeinCache](Perspectives.DomeinCache.html#t:DomeinCache)

module Perspectives.Docu.TypePersistence where
