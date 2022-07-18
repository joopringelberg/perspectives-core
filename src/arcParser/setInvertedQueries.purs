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
import Data.Foldable (for_)
import Data.Map (Map) as Map
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InvertedQuery (QueryWithAKink(..))
import Perspectives.Parsing.Arc.InvertQueriesForBindings (setInvertedQueriesForUserAndRole)
import Perspectives.Parsing.Arc.PhaseThree.StoreInvertedQueries (storeInvertedQuery)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo')
import Perspectives.Query.Kinked (invert)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription, domain)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType, StateIdentifier)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<$>))

--------------------------------------------------------------------------------------------------------------
---- SET INVERTED QUERIES
--------------------------------------------------------------------------------------------------------------
type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

-- | Modifies the DomeinFile in PhaseTwoState.
setInvertedQueries ::
  Array RoleType ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Array StateIdentifier ->
  QueryFunctionDescription ->
  Boolean ->
  WithModificationSummary Unit
setInvertedQueries users statesPerProperty roleStates qfd selfOnly = do
  -- log ("setInvertedQueries:" <> "\n users =" <> show users <> "\n states = " <> show roleStates <> "\n statesPerProperty = " <> showTree statesPerProperty <> "\n qfd = " <> show qfd)
  (zqs :: (Array QueryWithAKink)) <- lift $ invert qfd

  for_ zqs \qwk@(ZQ backward forward) -> do

      -- Store the QueryWithAKink.
      storeInvertedQuery qwk users roleStates statesPerProperty selfOnly

      -- If the original query represents the object of a perspective, we need to do more work
      -- to handle the properties in the perspective.
      -- We do this just for the full inversion of the object query, i.e. when there is no forward part.
      -- To ensure we're not dealing with a property definition, we check that the Domain of the backwards part is
      -- an RDOM (role) domain (the inversion of a property definition query would start with a VDOM (property) domain).
      case forward, backward, domain <$> backward of
        Nothing, Just bw, Just (RDOM role) ->
          void $ unsafePartial $
            setInvertedQueriesForUserAndRole
              bw
              users
              role
              statesPerProperty
              qwk
              selfOnly
        _, _, _ -> pure unit

