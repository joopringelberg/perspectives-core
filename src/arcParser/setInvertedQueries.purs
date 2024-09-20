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

module Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries where

import Control.Monad.Except (lift)
import Control.Monad.Reader (ReaderT)
import Data.Array (null)
import Data.Foldable (for_)
import Data.Map (Map) as Map
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InvertedQuery (QueryWithAKink(..))
import Perspectives.Parsing.Arc.PhaseThree.StoreInvertedQueries (storeInvertedQuery)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo')
import Perspectives.Query.Kinked (invert)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType, StateIdentifier)
import Prelude (Unit, bind, ($))

--------------------------------------------------------------------------------------------------------------
---- SET INVERTED QUERIES
--------------------------------------------------------------------------------------------------------------
type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

setInvertedQueries ::
  Array RoleType ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Array StateIdentifier ->
  QueryFunctionDescription ->
  Boolean ->
  Boolean -> 
  WithModificationSummary Unit
setInvertedQueries users statesPerProperty roleStates qfd selfOnly authorOnly = do
  -- log ("setInvertedQueries:" <> "\n users =" <> show users <> "\n states = " <> show roleStates <> "\n statesPerProperty = " <> showTree statesPerProperty <> "\n qfd = " <> show qfd)
  (zqs :: (Array QueryWithAKink)) <- lift $ invert qfd

  if null users
    -- This is a state query. The property access is part of the inversion.
    then for_ zqs \qwk -> storeInvertedQuery qwk users roleStates statesPerProperty selfOnly authorOnly
    -- This is a perspective query. Access to properties is not included in the inverted query; it is just the perspective object.
    else for_ zqs \qwk@(ZQ backward forward) -> storeInvertedQuery qwk users roleStates statesPerProperty selfOnly authorOnly
      
