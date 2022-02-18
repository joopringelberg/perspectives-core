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

-- | The properties of a Role’s binding are as accessible as if they were the Role’s own
-- | properties. The modeller may add a View to an Action that includes any of these binding
-- | properties. And this applies to the binding of the binding, recursively.
-- |
-- | This module addresses the issue: how do we make sure that changes to such properties are
-- | distributed properly?
-- |
-- | For an explanatory text, see: https://joopringelberg.github.io/perspectives-documentation/Perspectives%20on%20bindings.pdf

module Perspectives.Parsing.Arc.InvertQueriesForBindings where

import Prelude

import Control.Monad.Reader (ReaderT, lift)
import Data.Array (concat, elemIndex, foldMap, fromFoldable, intersect, length, nub)
import Data.Foldable (for_)
import Data.Map (Map, filterKeys, values)
import Data.Map (lookup) as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.InvertedQuery (QueryWithAKink(..))
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (makeComposition, storeInvertedQuery)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo')
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..), domain)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getEnumeratedRole)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, rangeOfPropertyType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, functional, mandatory) as RL
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType)

-- | For a User RoleType, and an ADT EnumeratedRoleType that represents the Object of a Perspective,
-- | construct and distribute InvertedQueries that ensure that this User is notified of changes to the filler
-- | of the role and its properties, recursively.
-- | NOTE: this function does not handle the inversion of the Perspective Object itself!

-- | If the role, or its filler, adds properties that are in the Map PropertyType (Array StateIdentifier)
-- | provided as third argument, store an InvertedQuery for each of them on the
-- | PropertyType.
-- | If the filler adds properties, store an InvertedQuery in fillsInvertedQueries of the filler.
-- | The backwards part of each of these inverted queries is postpended with the backwards part that leads from the
-- | role.

type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

-- | Modifies the DomeinFile in PhaseTwoState.
setInvertedQueriesForUserAndRole :: Partial =>
  QueryFunctionDescription ->
  Array RoleType ->
  ADT RoleInContext ->
  Map PropertyType (Array StateIdentifier) ->
  QueryWithAKink ->
  Boolean ->
  WithModificationSummary Boolean
setInvertedQueriesForUserAndRole backwards users (ST ric@(RoleInContext{context, role})) statesPerProperty qWithAkink selfOnly = do
  -- These are the properties that are on this role;
  (propsOfRole :: Array PropertyType) <- lift3 $ RL.allLocallyRepresentedProperties (ST role)
  -- select from among them the ones we want in this perspective (as represented in statesPerProperty).
  propertiesOnThisLevel <- pure $ intersect (fromFoldable $ keys statesPerProperty) propsOfRole
  -- For all these properties (that are on this level in the fill network):
  -- set an inverted query for all states for that property.
  for_ propertiesOnThisLevel \prop -> case Map.lookup prop statesPerProperty of
    Nothing -> pure unit
    Just s -> do
      backwards' <- lift3 $ prependValue2Role prop backwards
      storeInvertedQuery
        (ZQ (Just backwards') Nothing)
        users
        s
        statesPerProperty
        selfOnly

  (adtOfBinding :: ADT RoleInContext) <- (lift3 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
  -- recursive call, where we just pass the submap with properties that do not reside
  -- on this level, and the states in that map.
  mapBelowThisLevel <- pure (filterKeys (isNothing <<< (flip elemIndex) propertiesOnThisLevel) statesPerProperty)
  queryWithBindersStep@(ZQ backwards' _) <- lift3 $ addBindersStep adtOfBinding qWithAkink
  bindingCarriesProperty <- setInvertedQueriesForUserAndRole (fromJust backwards') users adtOfBinding mapBelowThisLevel queryWithBindersStep selfOnly

  -- After processing the binding telescope:
  if (bindingCarriesProperty || length propertiesOnThisLevel > 0)
    -- Now set an inverted query on this level of the telescope, for all states for
    -- properties on this level and below.
    -- That will be just all states in the map.
    -- Do it when a property resides on the telescope below the current level.
    -- Do it when a property resides on the current level.
    then do
      storeInvertedQuery
        queryWithBindersStep
        users
        (nub $ concat $ fromFoldable $ values statesPerProperty)
        statesPerProperty
        selfOnly
      pure true
    else pure false

  where

    prependValue2Role :: PropertyType -> QueryFunctionDescription -> MP QueryFunctionDescription
    prependValue2Role p qfd = do
      fun <- propertyTypeIsFunctional p
      man <- propertyTypeIsMandatory p
      range <- rangeOfPropertyType p
      pure $ makeComposition (SQD (VDOM range (Just p)) (Value2Role p) (domain qfd) True True) qfd

    -- Replace the backwards part of the QueryWithAKink with a composition of a GetRoleBindersF step and the original
    -- backwards part.
    -- This is a fills step.
    addBindersStep :: ADT RoleInContext -> QueryWithAKink -> MP QueryWithAKink
    addBindersStep adtOfBinding (ZQ bw _) = do
      fun <- getEnumeratedRole role >>= RL.functional
      man <- getEnumeratedRole role >>= RL.mandatory
      -- We do not know whether the binding (filling role) is an Aspect in its embedding context.
      backwards' <- pure $ makeComposition
        (SQD (RDOM adtOfBinding)
          (GetRoleBindersF role context)
          (RDOM $ ST ric)
          (bool2threeValued fun)
          (bool2threeValued man)) <$> bw
      pure $ ZQ backwards' Nothing

setInvertedQueriesForUserAndRole backwards users (PROD terms) props invertedQ selfOnly = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole backwards users t props invertedQ selfOnly)
    terms
  pure $ ala Conj foldMap x

setInvertedQueriesForUserAndRole backwards users (SUM terms) props invertedQ selfOnly = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole backwards users t props invertedQ selfOnly)
    terms
  pure $ ala Disj foldMap x

-- This handles the EMPTY and UNIVERSAL case.
setInvertedQueriesForUserAndRole backwards users _ props invertedQ selfOnly = pure false

lift3 :: forall a. MonadPerspectives a -> WithModificationSummary a
lift3 = lift <<< lift <<< lift
