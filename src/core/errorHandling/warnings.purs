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

module Perspectives.Warning where

import Prelude

data PerspectivesWarning =
    ModelLacksModelId String
  | ModelLacksUrl String

instance showPerspectivesWarning :: Show PerspectivesWarning where
  show (ModelLacksModelId dfid) = "(ModelLacksModelId) The model '" <> dfid <> "' lacks a value for the property ModelIdentification on its Model instance."
  show (ModelLacksUrl dfid) = "(ModelLacksUrl) The model '" <> dfid <> "' lacks a value for the property Url on its Model instance."
