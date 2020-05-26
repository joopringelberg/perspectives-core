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
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse)
import Foreign.Object (lookup, insert)
import Perspectives.CoreTypes (type (~~~>), MP, (###=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), RelevantProperties(..), addInvertedQuery)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, modifyDF, lift2)
import Perspectives.Query.DescriptionCompiler (makeComposition)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.Class.PersistentType (getAction, getEnumeratedRole, getView)
import Perspectives.Representation.Class.Property (propertyTypeIsFunctional, propertyTypeIsMandatory, rangeOfPropertyType)
import Perspectives.Representation.Class.Role (functional, mandatory, propertySet)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (isElementOf)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), roletype2string)
import Perspectives.Types.ObjectGetters (aspectsClosure)

-- | For a User RoleType, and an ADT EnumeratedRoleType that represents the Object of a Perspective,
-- | construct and distribute InvertedQueries that ensure that this User is notified of changes to the role,
-- | its binding and its properties, recursively.
setInvertedQueriesForUserAndRole :: RoleType -> ADT EnumeratedRoleType -> RelevantProperties -> QueryWithAKink -> PhaseThree Boolean
setInvertedQueriesForUserAndRole user (ST role) props qWithAkink = case props of
  All -> do
    -- store qWithAkink in onContextDelta_context of role
    addToRole qWithAkink role
  -- for each property of role, store (Value2Role >> invertedQ) in onPropertyDelta of that property
    (lift2 $ getEnumeratedRole role) >>= \(EnumeratedRole{properties}) -> addToProperties qWithAkink properties
    -- get the binding of role
    (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
    -- recursive call
    void $ (lift2 $ addBindingStep b qWithAkink) >>= setInvertedQueriesForUserAndRole user b props
    pure true
  Properties relevant -> do
    (propsOfRole :: Array PropertyType) <- lift2 $ filterA isPropertyOfRole relevant
    if null propsOfRole
      then do
        -- get the binding of role
        (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
        bindingCarriesProperty <- (lift2 $ addBindingStep b qWithAkink) >>= setInvertedQueriesForUserAndRole user b props
        if bindingCarriesProperty
          then do
            -- store qWithAkink in onContextDelta_context of role
            -- TODO: voeg de propsOfRole toe aan de QueryWithAKink. Dat gebruiken we als we Deltas maken.
            addToRole qWithAkink role
            pure true
          else pure false
      else do
      -- for each property in propsOfRole, store (Value2Role >> qWithAkink) in onPropertyDelta of that property
      addToProperties qWithAkink propsOfRole
      -- get the binding of role
      (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
      void $ (lift2 $ addBindingStep b qWithAkink) >>= setInvertedQueriesForUserAndRole user b props
      pure true

  where
    addToRole :: QueryWithAKink -> EnumeratedRoleType -> PhaseThree Unit
    addToRole qwk@(ZQ backwards _) r@(EnumeratedRoleType roleId) =
      case backwards of
        Just (SQD _ (DataTypeGetter ContextF)_ _ _) -> modifyDF \df@{enumeratedRoles:roles} ->
          case lookup roleId roles of
            Nothing -> addInvertedQueryForDomain roleId
              (InvertedQuery {description: qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (Properties [])})
              OnContextDelta_context
              df
            Just (EnumeratedRole rr@{onContextDelta_context}) -> df {enumeratedRoles = insert roleId (EnumeratedRole rr { onContextDelta_context = addInvertedQuery (InvertedQuery {description: qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (Properties [])}) onContextDelta_context }) roles}
        -- We by construction can safely assume the other case is the composition created by addBinding.
        Just _ -> modifyDF \df@{enumeratedRoles:roles} ->
          case lookup roleId roles of
            Nothing -> addInvertedQueryForDomain roleId
              (InvertedQuery {description: qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (Properties [])})
              OnRoleDelta_binder
              df
            Just (EnumeratedRole rr@{onRoleDelta_binder}) -> df {enumeratedRoles = insert roleId (EnumeratedRole rr { onRoleDelta_binder = addInvertedQuery (InvertedQuery {description: qwk, backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (Properties [])}) onRoleDelta_binder }) roles}
        otherwise -> pure unit

    addToProperties :: QueryWithAKink -> Array PropertyType -> PhaseThree Unit
    addToProperties qwk@(ZQ backwards forwards) roleProps = for_ roleProps \prop -> case prop of
      ENP pr@(EnumeratedPropertyType p) -> do
        backwards' <- lift2 $ traverse (prependValue2Role (ENP pr)) backwards
        forwards' <- lift2 $ traverse (postPendProp (ENP pr)) forwards
        modifyDF \df@{enumeratedProperties} -> do
          case lookup p enumeratedProperties of
            Nothing -> addInvertedQueryForDomain p
              (InvertedQuery {description: ZQ forwards' backwards', backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (Properties [])})
              OnPropertyDelta
              df
            Just (EnumeratedProperty epr@{onPropertyDelta}) -> df {enumeratedProperties = insert p (EnumeratedProperty epr {onPropertyDelta = addInvertedQuery (InvertedQuery {description: ZQ forwards' backwards', backwardsCompiled: Nothing, forwardsCompiled: Nothing, userTypes: singleton user (Properties [])}) onPropertyDelta}) enumeratedProperties}
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
      pure $ makeComposition (SQD (VDOM range (Just p)) (PropertyGetter p) (VDOM range (Just p)) (bool2threeValued fun) (bool2threeValued man)) qfd

    addBindingStep :: ADT EnumeratedRoleType -> QueryWithAKink -> MP QueryWithAKink
    addBindingStep b (ZQ backwards forwards) = do
      fun <- getEnumeratedRole role >>= functional
      man <- getEnumeratedRole role >>= mandatory
      backwards' <- pure $ makeComposition (SQD (RDOM b) (DataTypeGetterWithParameter GetRoleBindersF (unwrap role)) (RDOM (ST role)) (bool2threeValued fun) (bool2threeValued man)) <$> backwards
      forwards' <- pure $ makeComposition (SQD (RDOM (ST role)) (DataTypeGetter BindingF) (RDOM b) (bool2threeValued fun) (bool2threeValued man)) <$> forwards
      pure $ ZQ backwards' forwards'

    -- | Collect the Properties defined on the EnumeratedRoleType and its Aspect Roles.
    -- | Returns true iff the property is one of them.
    isPropertyOfRole :: PropertyType -> MP Boolean
    isPropertyOfRole p = propertySet (ST role) >>= \ps -> pure $ isElementOf p ps

setInvertedQueriesForUserAndRole user (PROD terms) props invertedQ = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole user t props invertedQ)
    terms
  pure $ ala Conj foldMap x

setInvertedQueriesForUserAndRole user (SUM terms) props invertedQ = do
  x <- traverse
    (\t -> setInvertedQueriesForUserAndRole user t props invertedQ)
    terms
  pure $ ala Disj foldMap x

-- This handles the EMPTY and UNIVERSAL case.
setInvertedQueriesForUserAndRole user _ props invertedQ = pure false

isRelevant :: PropertyType -> RelevantProperties -> Boolean
isRelevant t All = true
isRelevant t (Properties set) = isJust $ elemIndex t set

-- | For a User Role, find the properties of a Role in the same context that are relevant in the sense that
-- | they occur in the View on the Object of some Action.
-- | This function only returns values for Enumerated User Role types.
-- | <User> `hasAccessToPropertiesOf` <Role> == RelevantProperties
-- | where both arguments are RoleTypes.
hasAccessToPropertiesOf :: RoleType -> RoleType -> MP RelevantProperties
hasAccessToPropertiesOf (ENR user) role = do
  -- Find all Actions on the role as Object (consider Aspects, too!)
  props <- user ###= (aspectsClosure >=> actionOnRole role >=> enumeratedPropertiesForActionObject)
  if null props
    then pure All
    else pure $ Properties props

  where
    actionOnRole :: RoleType -> EnumeratedRoleType ~~~> ActionType
    actionOnRole t r = ArrayT do
      EnumeratedRole{perspectives} <- getEnumeratedRole r
      pure $ maybe [] identity (lookup (roletype2string t) perspectives)

    enumeratedPropertiesForActionObject :: ActionType ~~~> PropertyType
    enumeratedPropertiesForActionObject a = ArrayT do
      Action{requiredObjectProperties} <- getAction a
      case requiredObjectProperties of
        Nothing -> pure []
        Just v -> getView v >>= pure <<< _.propertyReferences <<< unwrap

hasAccessToPropertiesOf _ _ = empty
