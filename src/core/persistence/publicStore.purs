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
import Foreign (unsafeToForeign)
import Perspectives.Identifiers (Namespace, modelUri2InstancesStore)
import Simple.JSON (class ReadForeign, class WriteForeign)

data PublicStore = NAMESPACESTORE

derive instance  Generic PublicStore _
instance Show PublicStore where show = genericShow
derive instance Eq PublicStore

instance WriteForeign PublicStore where
  writeImpl _ = unsafeToForeign "NAMESPACESTORE"

instance ReadForeign PublicStore where
  readImpl _ = pure NAMESPACESTORE

-- | Given a public store symbolic name, maps it to an URL.
-- | The special case NAMESPACESTORE maps a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    https://{authority-with-dots}/cw_{subdomains-with-underscores}_{authority-with-underscores}/
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
mapPublicStore :: Partial => PublicStore -> Namespace -> String
mapPublicStore pStore modelName = case pStore of 
  NAMESPACESTORE -> modelUri2InstancesStore modelName