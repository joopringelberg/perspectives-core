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
import Data.Array (catMaybes, elemIndex, filterA, foldMap, null)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse)
import Foreign.Object (lookup, insert)
import Perspectives.CoreTypes (type (~~~>), MP, (###=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), addInvertedQuery)
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
import Perspectives.Representation.ThreeValuedLogic (bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), roletype2string)
import Perspectives.Types.ObjectGetters (aspectsClosure)

-- | For a User RoleType, and an ADT EnumeratedRoleType that represents the Object of a Perspective,
-- | construct and distribute InvertedQueries that ensure that this User is notified of changes to the role,
-- | its binding and its properties, recursively.
setInvertedQueriesForUserAndRole :: RoleType -> ADT EnumeratedRoleType -> RelevantProperties -> QueryFunctionDescription -> PhaseThree Boolean
setInvertedQueriesForUserAndRole user (ST role) props invertedQ = case props of
  All -> do
    -- store invertedQ in onContextDelta_context of role
    addToRole invertedQ role
  -- for each property of role, store (Value2Role >> invertedQ) in onPropertyDelta of that property
    (lift2 $ getEnumeratedRole role) >>= \(EnumeratedRole{properties}) -> addToProperties invertedQ properties
    -- get the binding of role
    (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
    -- recursive call
    void $ (lift2 $ addBindingStep b invertedQ) >>= setInvertedQueriesForUserAndRole user b props
    pure true
  Properties relevant -> do
    (propsOfRole :: Array EnumeratedPropertyType) <- lift2 $ filterA isPropertyOfRole relevant
    if null propsOfRole
      then do
        -- get the binding of role
        (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
        bindingCarriesProperty <- (lift2 $ addBindingStep b invertedQ) >>= setInvertedQueriesForUserAndRole user b props
        if bindingCarriesProperty
          then do
            -- store invertedQ in onContextDelta_context of role
            addToRole invertedQ role
            pure true
          else pure false
      else do
      -- for each property in propsOfRole, store (Value2Role >> invertedQ) in onPropertyDelta of that property
      addToProperties invertedQ (ENP <$> propsOfRole)
      -- get the binding of role
      (b :: ADT EnumeratedRoleType) <- (lift2 $ getEnumeratedRole role) >>= pure <<< _.binding <<< unwrap
      void $ (lift2 $ addBindingStep b invertedQ) >>= setInvertedQueriesForUserAndRole user b props
      pure true

  where
    addToRole :: QueryFunctionDescription -> EnumeratedRoleType -> PhaseThree Unit
    addToRole invertedQ' r@(EnumeratedRoleType roleId) = modifyDF \df@{enumeratedRoles:roles} -> do
      case lookup roleId roles of
        -- TODO. We cannot modify this role as it is outside the current model. Somehow save the query with the type
        -- and make sure it is applied if the target model is loaded?
        Nothing -> addInvertedQueryForDomain roleId
          (InvertedQuery {description: invertedQ', compilation: Nothing, userTypes: [user]})
          OnContextDelta_context
          df
        -- TODO. Als er al een InvertedQuery is met dezelfde description, voeg dan het user type daar aan toe.
        Just (EnumeratedRole rr@{onContextDelta_context}) -> df {enumeratedRoles = insert roleId (EnumeratedRole rr { onContextDelta_context = addInvertedQuery (InvertedQuery {description: invertedQ', compilation: Nothing, userTypes: [user]}) onContextDelta_context }) roles}

    addToProperties :: QueryFunctionDescription -> Array PropertyType -> PhaseThree Unit
    addToProperties invertedQ' roleProps = for_ roleProps \prop -> case prop of
      ENP pr@(EnumeratedPropertyType p) -> do
        invertedQ'' <- lift2 $ prependValue2Role (ENP pr) invertedQ'
        modifyDF \df@{enumeratedProperties} -> do
          case lookup p enumeratedProperties of
            Nothing -> addInvertedQueryForDomain p
              (InvertedQuery {description: invertedQ'', compilation: Nothing, userTypes: [user]})
              OnPropertyDelta
              df
            Just (EnumeratedProperty epr@{onPropertyDelta}) -> df {enumeratedProperties = insert p (EnumeratedProperty epr {onPropertyDelta = addInvertedQuery (InvertedQuery {description: invertedQ'', compilation: Nothing, userTypes: [user]}) onPropertyDelta}) enumeratedProperties}
        pure unit
      _ -> pure unit

    prependValue2Role :: PropertyType -> QueryFunctionDescription -> MP QueryFunctionDescription
    prependValue2Role p qfd = do
      fun <- propertyTypeIsFunctional p
      man <- propertyTypeIsMandatory p
      range <- rangeOfPropertyType p
      pure $ makeComposition (SQD (RDOM (ST role)) (Value2Role p) (VDOM range (Just p)) (bool2threeValued fun) (bool2threeValued man)) qfd

    addBindingStep :: ADT EnumeratedRoleType -> QueryFunctionDescription -> MP QueryFunctionDescription
    addBindingStep b qfd = do
      fun <- getEnumeratedRole role >>= functional
      man <- getEnumeratedRole role >>= mandatory
      pure $ makeComposition (SQD (RDOM b) (DataTypeGetterWithParameter GetRoleBindersF (unwrap role)) (RDOM (ST role)) (bool2threeValued fun) (bool2threeValued man)) qfd

    -- | Collect the Properties defined on the EnumeratedRoleType and its Aspect Roles.
    -- | Returns true iff the property is one of them.
    isPropertyOfRole :: EnumeratedPropertyType -> MP Boolean
    isPropertyOfRole p = propertySet (ST role) >>= \ps -> pure $ isElementOf (ENP p) ps

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

data RelevantProperties = All | Properties (Array EnumeratedPropertyType)

isRelevant :: EnumeratedPropertyType -> RelevantProperties -> Boolean
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

    enumeratedPropertiesForActionObject :: ActionType ~~~> EnumeratedPropertyType
    enumeratedPropertiesForActionObject a = ArrayT do
      Action{requiredObjectProperties} <- getAction a
      case requiredObjectProperties of
        Nothing -> pure []
        Just v -> getView v >>= pure <<< catMaybes <<< map enumerated <<< _.propertyReferences <<< unwrap

    enumerated :: PropertyType -> Maybe EnumeratedPropertyType
    enumerated (ENP t) = Just t
    enumerated _ = Nothing

hasAccessToPropertiesOf _ _ = empty
