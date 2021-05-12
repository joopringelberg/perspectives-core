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

module Perspectives.Sync.InvertedQueryResult where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Encode)
import Foreign.Generic (defaultOptions, genericEncode)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Utilities (class PrettyPrint)

-- | Represents consequtively the query inversion results for
-- | 	* the State query for context state. These inversions are exclusively used to evaluate the states of a context instance.
-- | 	* the State query for role state.
data InvertedQueryResult =
	  ContextStateQuery (Array ContextInstance)
	| RoleStateQuery (Array RoleInstance)

derive instance genericInvertedQueryResult :: Generic InvertedQueryResult _

instance showInvertedQueryResult :: Show InvertedQueryResult where
  show = genericShow

instance encodeInvertedQueryResult :: Encode InvertedQueryResult where
	encode = genericEncode defaultOptions

instance eqInvertedQueryResult :: Eq InvertedQueryResult where
	eq = genericEq

instance prettyPrintInvertedQueryResult :: PrettyPrint InvertedQueryResult where
	prettyPrint' t (ContextStateQuery ci) = "ContextStateQuery " <> (show ci)
	prettyPrint' t (RoleStateQuery ri) = "RoleStateQuery " <> (show ri)
