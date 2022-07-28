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

module Perspectives.Persistent.PublicStore where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Perspectives.Identifiers (Namespace, modelName2NamespaceStore)

data PublicStore = NAMESPACESTORE

derive instance  Generic PublicStore _
instance Show PublicStore where show = genericShow
derive instance Eq PublicStore
instance Encode PublicStore where
  encode = genericEncode defaultOptions
instance Decode PublicStore where
  decode = genericDecode defaultOptions

mapPublicStore :: Partial => PublicStore -> Namespace -> String
mapPublicStore pStore modelName = case pStore of 
  NAMESPACESTORE -> modelName2NamespaceStore modelName