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

import Control.Plus (empty)
import Data.Array (elemIndex, filterA, foldMap, null)
import Data.Foldable (for_)
import Data.Map (singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse)
import Foreign.Object (lookup, insert, empty) as OBJ
import Perspectives.CoreTypes (type (~~~>), MP, (###=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), PropsAndVerbs, QueryWithAKink(..), RelevantProperties(..), addInvertedQuery, allProps, restrictToProperty)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (removeFirstBackwardsStep)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF, lift2)
import Perspectives.Query.DescriptionCompiler (makeComposition)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.Class.PersistentType (getAction, getEnumeratedRole, getView)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, rangeOfPropertyType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, functional, mandatory)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Types.ObjectGetters (aspectsClosure)

-- | For a User RoleType, and an ADT EnumeratedRoleType that represents the Object of a Perspective,
-- | construct and distribute InvertedQueries that ensure that this User is notified of changes to the role,
-- | its binding and its properties, recursively.

-- | If the role, or its binding, adds properties that are in the RelevantProperties
-- | provided as third argument, store an InvertedQuery for each of them on the
-- | PropertyType.
-- | If the binding adds properties, store an InvertedQuery in onRoleDelta_binder of the role.
setInvertedQueriesForUserAndRole :: RoleType -> ADT EnumeratedRoleType -> PropsAndVerbs -> Boolean -> QueryWithAKink -> PhaseThree Boolean
setInvertedQueriesForUserAndRole user (ST role) props perspectiveOnThisRole qWithAkink = do
  if perspectiveOnThisRole
    -- Add qWithAkink in onContextDelta_context of role.
    then addToOnContextDelta qWithAkink role
    else pure unit
  roleHasRequestedProperties <- case allProps props of
    All -> do
      -- for each property of role, store (Value2Role >> invertedQ) in onPropertyDelta of that property
      (propsOfRole :: Array PropertyType) <- lift2 $ allLocallyRepresentedProperties (ST role)
      addToProperties qWithAkink propsOfRole
      pure $ not $ null propsOfRole
    Properties relevant | not $ null relevant -> do
      (propsOfRole :: Array PropertyType) <- lift2 $ filterA isPropertyOfRole relevant
      -- for each property in propsOfRole, store (Value2Role >> qWithAkink) in onPropertyDelta of that property
      if not $ null propsOfRole
        then addToProperties qWithAkink propsOfRole
        else pure unit
      pure $ not $ null propsOfRole
    Properties _ -> pure false

  (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
  -- recursive call
  bindingCarriesProperty <- (lift2 $ addBindingStep b qWithAkink) >>= setInvertedQueriesForUserAndRole user b props false
  if (bindingCarriesProperty || roleHasRequestedProperties) && not perspectiveOnThisRole
    then do
      addToOnRoleDelta qWithAkink role
      pure true
    else pure false

  where
    addToOnContextDelta :: QueryWithAKink -> EnumeratedRoleType -> PhaseThree Unit
    addToOnContextDelta qwk@(ZQ backwards _) (EnumeratedRoleType roleId) = modifyDF \df@{enumeratedRoles:roles} ->
        case OBJ.lookup roleId roles of
          Nothing -> addInvertedQueryForDomain roleId
            (InvertedQuery {description: qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user OBJ.empty})
            OnContextDelta_context
            df
          Just (EnumeratedRole rr@{onContextDelta_context}) -> df {enumeratedRoles = OBJ.insert roleId (EnumeratedRole rr { onContextDelta_context = addInvertedQuery
            (InvertedQuery {description: qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user OBJ.empty
              -- add verbs here.
              })
            onContextDelta_context }) roles}

    addToOnRoleDelta :: QueryWithAKink -> EnumeratedRoleType -> PhaseThree Unit
    addToOnRoleDelta qwk (EnumeratedRoleType roleId) = modifyDF
      \df@{enumeratedRoles:roles} ->
        -- We remove the first step of the backwards path, because we apply it (runtime) not to the binder, but to
        -- the binding. We skip the binding because its cardinality is larger than one.
        case OBJ.lookup roleId roles of
          Nothing -> addInvertedQueryForDomain roleId
            (InvertedQuery {description: removeFirstBackwardsStep qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user props})
            OnRoleDelta_binder
            df
          Just (EnumeratedRole rr@{onRoleDelta_binder}) -> df {enumeratedRoles = OBJ.insert roleId (EnumeratedRole rr { onRoleDelta_binder = addInvertedQuery (InvertedQuery {description: removeFirstBackwardsStep qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user props}) onRoleDelta_binder}) roles}

    addToProperties :: QueryWithAKink -> Array PropertyType -> PhaseThree Unit
    addToProperties qwk@(ZQ backwards forwards) roleProps = for_ roleProps \prop -> case prop of
      ENP pr@(EnumeratedPropertyType p) -> do
        backwards' <- lift2 $ traverse (prependValue2Role (ENP pr)) backwards
        forwards' <- lift2 $ traverse (postPendProp (ENP pr)) forwards
        modifyDF \df@{enumeratedProperties} -> do
          case OBJ.lookup p enumeratedProperties of
            Nothing -> addInvertedQueryForDomain p
              (InvertedQuery {description: ZQ backwards' forwards', backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (restrictToProperty prop props)})
              OnPropertyDelta
              df
            Just (EnumeratedProperty epr@{onPropertyDelta}) -> df {enumeratedProperties = OBJ.insert p (EnumeratedProperty epr {onPropertyDelta = addInvertedQuery (InvertedQuery {description: ZQ backwards' forwards', backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (restrictToProperty prop props)}) onPropertyDelta}) enumeratedProperties}
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
      fun <- getEnumeratedRole role >>= functional
      man <- getEnumeratedRole role >>= mandatory
      backwards' <- pure $ makeComposition (SQD (RDOM b) (DataTypeGetterWithParameter GetRoleBindersF (unwrap role)) (RDOM (ST role)) (bool2threeValued fun) (bool2threeValued man)) <$> backwards
      pure $ ZQ backwards' Nothing

    -- | Collect the Properties defined on the EnumeratedRoleType and its Aspect Roles (transitively closed).
    -- | Returns true iff the property is one of them.
    isPropertyOfRole :: PropertyType -> MP Boolean
    isPropertyOfRole p = allLocallyRepresentedProperties (ST role) >>= \ps -> pure $ isJust $ elemIndex p ps

setInvertedQueriesForUserAndRole user (PROD terms) props perspectiveOnThisRole invertedQ = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole user t props perspectiveOnThisRole invertedQ)
    terms
  pure $ ala Conj foldMap x

setInvertedQueriesForUserAndRole user (SUM terms) props perspectiveOnThisRole invertedQ = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole user t props perspectiveOnThisRole invertedQ)
    terms
  pure $ ala Disj foldMap x

-- This handles the EMPTY and UNIVERSAL case.
setInvertedQueriesForUserAndRole user _ props perspectiveOnThisRole invertedQ = pure false

isRelevant :: PropertyType -> RelevantProperties -> Boolean
isRelevant t All = true
isRelevant t (Properties set) = isJust $ elemIndex t set
