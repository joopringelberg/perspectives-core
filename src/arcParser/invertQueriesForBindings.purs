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
import Data.Array (cons, elemIndex, fromFoldable, intersect)
import Data.Foldable (for_)
import Data.Map (Map, filterKeys, isEmpty, singleton)
import Data.Map (lookup) as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.InvertedQuery (QueryWithAKink(..))
import Perspectives.Parsing.Arc.PhaseThree.StoreInvertedQueries (storeInvertedQuery)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo', lift2, throwError)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Kinked (invert)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..), addTermOnRight, domain, makeComposition)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getEnumeratedRole)
import Perspectives.Representation.Class.Property (getCalculation, getProperty, propertyTypeIsFunctional, propertyTypeIsMandatory, rangeOfPropertyType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, functional, mandatory) as RL
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType)

-- | For a User RoleType, and an ADT RoleInContext that represents the Object of a Perspective,
-- | construct and distribute InvertedQueries that ensure that this User is notified of changes to the filler
-- | of the role and its properties, recursively.
-- | NOTE: this function does not handle the inversion of the Perspective Object itself!

-- | If the role, or its filler, adds properties that are in the Map PropertyType (Array StateIdentifier)
-- | provided as third argument, store an InvertedQuery (including the property) for each of them on the
-- | PropertyType.
-- | If the filler adds properties, store an InvertedQuery in filledInvertedQueries of the filler.
-- | The backwards part of each of these inverted queries is postpended with the backwards part that leads from the
-- | role.
-- | This function does not have to handle the RoleVerbs on the root of the filler hiërarchy.

type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

-- | Modifies the DomeinFile in PhaseTwoState.
-- | This function does NOT save the InvertedQuery that was passed in; it does save 
-- | extensions of that query on the filler hierarchy and on properties in the map.
setInvertedQueriesForUserAndRole :: Partial =>
  QueryFunctionDescription ->                       -- backwards of qWithAKink.
  Array RoleType ->
  ADT RoleInContext ->                              -- The domain of backwards.
  Map PropertyType (Array StateIdentifier) ->
  Array StateIdentifier ->
  QueryWithAKink ->                                 -- The query inversion whose backwards leads up to the role of the third parameter.
  Boolean ->
  WithModificationSummary Unit
setInvertedQueriesForUserAndRole backwards users bwDomain@(ST (RoleInContext{context, role})) statesPerProperty roleStates qWithAkink selfOnly = do
  -- These are the properties that are on this role;
  (propsOfRole :: Array PropertyType) <- lift3 $ RL.allLocallyRepresentedProperties (ST role)
  -- select from among them the ones we want in this perspective (as represented in statesPerProperty).
  propertiesOnThisLevel <- pure $ intersect (fromFoldable $ keys statesPerProperty) propsOfRole
  -- For all these properties (that are on this level in the fill network):
  -- set an inverted query for all states for that property.
  for_ propertiesOnThisLevel \prop -> case Map.lookup prop statesPerProperty of
    Nothing -> pure unit
    Just s -> case prop of 
      ENP _ -> do 
        backwards' <- lift3 $ prependValue2Role prop backwards
        storeInvertedQuery
          (ZQ (Just backwards') Nothing)
          users
          roleStates
          statesPerProperty
          selfOnly
      CP _ -> do
        pCalc <- lift ((lift2 $ getProperty prop) >>= lift2 <<< getCalculation)
        (zqs :: (Array QueryWithAKink)) <- lift (invert pCalc)
        for_ (cons (ZQ Nothing (Just pCalc)) zqs) \qwk@(ZQ backward forward) -> do 
          -- We have to preserve right-association in backwards.
          backwards' <- case backward of
            Nothing -> pure backwards
            Just bw -> pure $ addTermOnRight bw backwards
          storeInvertedQuery 
            (ZQ (Just backwards') forward) 
            users  
            roleStates
            (singleton prop s)
            selfOnly
  (adtOfBinding :: ADT RoleInContext) <- (lift3 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
  mapBelowThisLevel <- pure (filterKeys (isNothing <<< (flip elemIndex) propertiesOnThisLevel) statesPerProperty)
  if adtOfBinding == EMPTY
    then if isEmpty mapBelowThisLevel
      then pure unit
      -- We have an error situation: not all properties accounted for, but no filler.
      else lift $ logPerspectivesError (Custom $ "An impossible situation has arisen: propert(y)(ies) cannot be accounted for in setInvertedQueriesForUserAndRole" <> show (fromFoldable $ keys mapBelowThisLevel))
    else if isEmpty mapBelowThisLevel
      then pure unit
      else do
        -- Extend the backwards part with a step back up the filler hierarchy:
        queryWithBindersStep@(ZQ backwards' _) <- lift3 $ addBindersStep adtOfBinding qWithAkink
        -- Save the inverted query on this level of the telescope, for all states for
        -- properties on this level and below.
        -- That will be just all states in the map.
        storeInvertedQuery
          queryWithBindersStep
          users
          roleStates
          statesPerProperty
          selfOnly
        -- Then make the recursive call, where we just pass the submap with properties that do not reside
        -- on this level, and the states in that map.
        setInvertedQueriesForUserAndRole 
          (fromJust backwards')         -- the backwards that now starts with a FilledF step
          users 
          adtOfBinding                  -- the domain of that new backwards
          mapBelowThisLevel 
          roleStates 
          queryWithBindersStep          -- The complete augmented inverted query.
          selfOnly


  where

    prependValue2Role :: PropertyType -> QueryFunctionDescription -> MP QueryFunctionDescription
    prependValue2Role p qfd = do
      fun <- propertyTypeIsFunctional p
      man <- propertyTypeIsMandatory p
      range <- rangeOfPropertyType p
      pure $ makeComposition (SQD (VDOM range (Just p)) (Value2Role p) (domain qfd) True True) qfd

    -- Replace the backwards part of the QueryWithAKink with a composition of a FilledF step and the original
    -- backwards part.
    -- This is a step from filler to filled
    addBindersStep :: ADT RoleInContext -> QueryWithAKink -> MP QueryWithAKink
    addBindersStep adtOfBinding (ZQ bw _) = do
      fun <- getEnumeratedRole role >>= RL.functional
      man <- getEnumeratedRole role >>= RL.mandatory
      -- We do not know whether the binding (filling role) is an Aspect in its embedding context.
      backwards' <- pure $ makeComposition
        (SQD 
          (RDOM adtOfBinding)               -- The domain of the new backwards query, being the filler of the original bwDomain
          (FilledF role context)            -- A `filled role` operation; should equal the range of the query step.
          (RDOM bwDomain)                   -- The range of the new backwards query, being the original bwDomain.
          (bool2threeValued fun)
          (bool2threeValued man)) <$> bw
      pure $ ZQ backwards' Nothing

setInvertedQueriesForUserAndRole backwards users (PROD terms) props roleStates invertedQ selfOnly = do
  void $ traverse
    (\t -> setInvertedQueriesForUserAndRole backwards users t props roleStates invertedQ selfOnly)
    terms

setInvertedQueriesForUserAndRole backwards users (SUM terms) props roleStates invertedQ selfOnly = do
  void $ traverse
    (\t -> setInvertedQueriesForUserAndRole backwards users t props roleStates invertedQ selfOnly)
    terms

-- This handles the EMPTY and UNIVERSAL case.
setInvertedQueriesForUserAndRole backwards users _ props roleStates invertedQ selfOnly = pure unit

lift3 :: forall a. MonadPerspectives a -> WithModificationSummary a
lift3 = lift <<< lift <<< lift
