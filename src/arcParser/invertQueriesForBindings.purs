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

import Data.Array (concat, elemIndex, foldMap, fromFoldable, intersect, length, nub)
import Data.Foldable (for_)
import Data.Map (Map, filterKeys, values)
import Data.Map (lookup) as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse)
import Foreign.Object (lookup, insert) as OBJ
import Perspectives.CoreTypes (MP)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), RelevantProperties(..), addInvertedQuery)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (removeFirstBackwardsStep, makeComposition)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF, lift2)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getEnumeratedRole)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, rangeOfPropertyType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, functional, mandatory) as RL
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType)

-- | For a User RoleType, and an ADT EnumeratedRoleType that represents the Object of a Perspective,
-- | construct and distribute InvertedQueries that ensure that this User is notified of changes to the role,
-- | its binding and its properties, recursively.

-- | If the role, or its binding, adds properties that are in the Map PropertyType (Array StateIdentifier)
-- | provided as third argument, store an InvertedQuery for each of them on the
-- | PropertyType.
-- | If the binding adds properties, store an InvertedQuery in onRoleDelta_binder of the role.

setInvertedQueriesForUserAndRole ::
  Array RoleType ->
  ADT EnumeratedRoleType ->
  Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  QueryWithAKink ->
  Boolean ->
  PhaseThree Boolean
setInvertedQueriesForUserAndRole users (ST role) statesPerProperty perspectiveOnThisRole qWithAkink selfOnly = do
  if perspectiveOnThisRole
    -- Add qWithAkink in onContextDelta_context of role.
    then addToOnContextDelta qWithAkink role (nub $ concat $ fromFoldable $ values statesPerProperty)
    else pure unit
  (propsOfRole :: Array PropertyType) <- lift2 $ RL.allLocallyRepresentedProperties (ST role)
  propertiesOnThisLevel <- pure $ intersect (fromFoldable $ keys statesPerProperty) propsOfRole
  -- For all properties that are on this level in the telescope:
  -- set an inverted query for all states for that property.
  for_ propertiesOnThisLevel \prop -> case Map.lookup prop statesPerProperty of
    Nothing -> pure unit
    Just s -> addToProperties qWithAkink prop s

  (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
  -- recursive call, where we just pass the submap with properties that do not reside
  -- on this level, and the states in that map.
  mapBelowThisLevel <- pure (filterKeys (isNothing <<< (flip elemIndex) propertiesOnThisLevel) statesPerProperty)
  bindingCarriesProperty <- (lift2 $ addBindingStep b qWithAkink) >>= \qwk -> setInvertedQueriesForUserAndRole users b mapBelowThisLevel false qwk selfOnly
  -- After processing the binding telescope:
  if (bindingCarriesProperty || length propertiesOnThisLevel > 0) && not perspectiveOnThisRole
    -- Now set an inverted query on this level of the telescope, for all states for
    -- properties on this level and below.
    -- That will be just all states in the map.
    -- Don't do it on the root of the telescope (not if perspectiveOnThisRole).
    -- Do it when a property resides on the telescope below the current level.
    -- Do it when a property resides on the current level.
    then do
      addToOnRoleDeltaBinder qWithAkink role (nub $ concat $ fromFoldable $ values statesPerProperty)
      pure true
    else pure false

  where
    addToOnContextDelta :: QueryWithAKink -> EnumeratedRoleType -> Array StateIdentifier -> PhaseThree Unit
    addToOnContextDelta qwk@(ZQ backwards _) (EnumeratedRoleType roleId) states = modifyDF \df@{enumeratedRoles:roles} ->
        case OBJ.lookup roleId roles of
          Nothing -> addInvertedQueryForDomain roleId
            (InvertedQuery
              { description: qwk
              , backwardsCompiled: Nothing
              , forwardsCompiled: Nothing
              , users
              , states
              , statesPerProperty: EncodableMap statesPerProperty
              , selfOnly
              })
            OnContextDelta_context
            df
          Just (EnumeratedRole rr@{onContextDelta_context}) -> df {enumeratedRoles = OBJ.insert roleId (EnumeratedRole rr { onContextDelta_context = addInvertedQuery
            (InvertedQuery
              { description: qwk
              , backwardsCompiled: Nothing
              , forwardsCompiled: Nothing
              , users
              , states
              , statesPerProperty: EncodableMap statesPerProperty
              , selfOnly
              })
            onContextDelta_context }) roles}

    addToOnRoleDeltaBinder :: QueryWithAKink -> EnumeratedRoleType -> Array StateIdentifier -> PhaseThree Unit
    addToOnRoleDeltaBinder qwk (EnumeratedRoleType roleId) states = let
      -- We remove the first step of the backwards path, because we apply it (runtime) not to the binding, but to
      -- the binder. We skip the binder step because its cardinality is larger than one, causing a fan-out while
      -- we know (when working from a RoleBindingDelta) what path to follow.
      -- The forward part will never be applied for this InvertedQuery.
      -- This is because the function `runForwardsComputation` is applied to the binding and will collect all properties
      -- dynamically from it (relying on makeChainGetter), if there is no forwards part.
      description = removeFirstBackwardsStep qwk (\_ _ _ -> Nothing)
      in modifyDF
        \df@{enumeratedRoles:roles} ->
          case OBJ.lookup roleId roles of
            Nothing -> addInvertedQueryForDomain roleId
              (InvertedQuery
                { description
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                , states
                , statesPerProperty: EncodableMap statesPerProperty
                , selfOnly
                })
              OnRoleDelta_binder
              df
            Just (EnumeratedRole rr@{onRoleDelta_binder}) -> df {enumeratedRoles = OBJ.insert roleId (EnumeratedRole rr { onRoleDelta_binder = addInvertedQuery (InvertedQuery
              { description
              , backwardsCompiled: Nothing
              , forwardsCompiled: Nothing
              , users
              , states
              , statesPerProperty: EncodableMap statesPerProperty
              , selfOnly}) onRoleDelta_binder}) roles}

    addToProperties :: QueryWithAKink -> PropertyType -> Array StateIdentifier -> PhaseThree Unit
    addToProperties qwk@(ZQ backwards forwards) prop states = case prop of
      ENP pr@(EnumeratedPropertyType p) -> do
        backwards' <- lift2 $ traverse (prependValue2Role (ENP pr)) backwards
        -- the forwards part is never used in runtime!
        forwards' <- lift2 $ traverse (postPendProp (ENP pr)) forwards
        modifyDF \df@{enumeratedProperties} -> do
          case OBJ.lookup p enumeratedProperties of
            Nothing -> addInvertedQueryForDomain p
              (InvertedQuery
                { description: ZQ backwards' forwards'
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                -- NOTE. Mag het voorkomen dat de property niet in de map zit?
                -- En wat betekent een InvertedQuery zonder states?
                , states: maybe [] identity (Map.lookup prop statesPerProperty)
                , statesPerProperty: EncodableMap statesPerProperty
                , selfOnly
                })
              OnPropertyDelta
              df
            Just (EnumeratedProperty epr@{onPropertyDelta}) -> df {enumeratedProperties = OBJ.insert p (EnumeratedProperty epr {onPropertyDelta = addInvertedQuery (InvertedQuery
              {description: ZQ backwards' forwards'
              , backwardsCompiled: Nothing
              , forwardsCompiled: Nothing
              , users
              , states: maybe [] identity (Map.lookup prop statesPerProperty)
              , statesPerProperty: EncodableMap statesPerProperty
              , selfOnly}) onPropertyDelta}) enumeratedProperties}
        pure unit
      _ -> pure unit

    prependValue2Role :: PropertyType -> QueryFunctionDescription -> MP QueryFunctionDescription
    prependValue2Role p qfd = do
      fun <- propertyTypeIsFunctional p
      man <- propertyTypeIsMandatory p
      range <- rangeOfPropertyType p
      pure $ makeComposition (SQD (VDOM range (Just p)) (Value2Role p) (RDOM (ST role)) True True) qfd

    postPendProp :: PropertyType -> QueryFunctionDescription -> MP QueryFunctionDescription
    postPendProp p qfd = do
      fun <- propertyTypeIsFunctional p
      man <- propertyTypeIsMandatory p
      range <- rangeOfPropertyType p
      pure $ makeComposition qfd (SQD (VDOM range (Just p)) (PropertyGetter p) (VDOM range (Just p)) (bool2threeValued fun) (bool2threeValued man))

    addBindingStep :: ADT EnumeratedRoleType -> QueryWithAKink -> MP QueryWithAKink
    addBindingStep b (ZQ backwards _) = do
      fun <- getEnumeratedRole role >>= RL.functional
      man <- getEnumeratedRole role >>= RL.mandatory
      backwards' <- pure $ makeComposition (SQD (RDOM b) (DataTypeGetterWithParameter GetRoleBindersF (unwrap role)) (RDOM (ST role)) (bool2threeValued fun) (bool2threeValued man)) <$> backwards
      pure $ ZQ backwards' Nothing

    -- | Collect the Properties defined on the EnumeratedRoleType and its Aspect Roles (transitively closed).
    -- | Returns true iff the property is one of them.
    isPropertyOfRole :: PropertyType -> MP Boolean
    isPropertyOfRole p = RL.allLocallyRepresentedProperties (ST role) >>= \ps -> pure $ isJust $ elemIndex p ps

setInvertedQueriesForUserAndRole users (PROD terms) props perspectiveOnThisRole invertedQ selfOnly = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole users t props perspectiveOnThisRole invertedQ selfOnly)
    terms
  pure $ ala Conj foldMap x

setInvertedQueriesForUserAndRole users (SUM terms) props perspectiveOnThisRole invertedQ selfOnly = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole users t props perspectiveOnThisRole invertedQ selfOnly)
    terms
  pure $ ala Disj foldMap x

-- This handles the EMPTY and UNIVERSAL case.
setInvertedQueriesForUserAndRole users _ props perspectiveOnThisRole invertedQ selfOnly = pure false

isRelevant :: PropertyType -> RelevantProperties -> Boolean
isRelevant t All = true
isRelevant t (Properties set) = isJust $ elemIndex t set
