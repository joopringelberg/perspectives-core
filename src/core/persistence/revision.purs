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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Couchdb.Revision where 

import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, readNullOrUndefined, readString)
import Prelude (bind, pure, (<$>))

class Revision v where
  rev :: v -> Revision_
  changeRevision :: Revision_ -> v -> v

type Revision_ = Maybe String

revision :: String -> Revision_
revision = Just

-- | Gets the OUTER _rev of the raw json representation.
foreign import getRev_ :: Foreign -> Foreign

getRev :: Foreign -> F (Maybe String)
getRev f = do
  (x :: Maybe Foreign) <- readNullOrUndefined (getRev_ f)
  case x of
    Nothing -> pure Nothing
    Just r -> Just <$> readString r
