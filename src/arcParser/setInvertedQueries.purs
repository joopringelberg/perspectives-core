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

import Control.Monad.Except (lift, throwError)
import Control.Monad.Reader (ReaderT, ask)
import Data.Array (elemIndex, foldl, head)
import Data.Map (Map, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), addInvertedQueryIndexedByContext, addInvertedQueryIndexedByRole)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo', modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, domain, domain2roleType, functional, mandatory, range, roleDomain, roleRange)
import Perspectives.Representation.ADT (ADT(..), leavesInADT)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (isElementOf)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and, or)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier)
import Prelude (Unit, flip, identity, pure, unit, ($), (<$>), bind, (==), (>>=), (<<<))

type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

-- The first argument is the backward path of the second argument.
-- | The function is partial, because:
-- |   * we do not handle all cases;
setPathForStep :: Partial =>
  QueryFunctionDescription ->
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  WithModificationSummary Unit
setPathForStep qfd@(SQD dom qf ran fun man) qWithAK users states statesPerProperty selfOnly = do
  {modifiesRoleInstancesOf, modifiesRoleBindingOf, modifiesPropertiesOf} <- ask
  (embeddingContext :: Maybe ContextType) <- getEmbeddingContextFromDomain ran
  case qf of
    QF.Value2Role pt -> if dom == ran
      -- This handles cases like `step >>= sum`, where we construct a Value2Role
      -- but really must ignore it.
      then pure unit
      else case pt of
        ENP p -> modifyDF \dfr@{enumeratedProperties} -> case lookup (unwrap p) enumeratedProperties of
          Nothing -> addInvertedQueryForDomain (unwrap p)
            (InvertedQuery
              { description: qWithAK
              , backwardsCompiled: Nothing
              , forwardsCompiled: Nothing
              , users
              , modifies: isElementOf pt modifiesPropertiesOf
              , statesPerProperty: EncodableMap statesPerProperty
              -- NOTE. Mag het voorkomen dat de property niet in statesPerProperty zit?
              -- en wat is de betekenis van een InvertedQuery zonder states?
              , states: maybe [] identity (Map.lookup (ENP p) statesPerProperty)
              , selfOnly
              })
            (OnPropertyDelta (case ran of RDOM (ST et) _ -> et))
            dfr
          Just ep -> dfr {enumeratedProperties = insert
            (unwrap p)
            -- We add the InvertedQuery to the Property, indexed for all role types in the range.
            (foldl
              (\property erole' -> addPathToProperty ep qWithAK (isElementOf pt modifiesPropertiesOf) erole')
              ep
              (leavesInADT $ domain2roleType ran))
            enumeratedProperties}
        -- CP _ -> throwError $ Custom "Implement the handling of Calculated Properties in setPathForStep."
        CP _ -> pure unit

    -- add to onRoleDelta_binder of the role that we apply `binder enr` to (the domain of the step; the role that is bound).
    QF.DataTypeGetterWithParameter QF.GetRoleBindersF enr -> modifyDF \dfr@{enumeratedRoles} -> let
      -- We remove the first step of the backwards path, because we apply it (runtime) not to the binding, but to
      -- the binder. We skip the binding step because its cardinality is larger than one. It would cause a fan-out
      -- while we know, when applying the inverted query when handling a RoleBindingDelta, the exact path to follow.
      -- The function `usersWithPerspectiveOnRoleBinding` applies the forward part to the binding (not the binder).
      -- Hence we don't have to adapt the forward part.
      -- We add the inverted query to the **BINDING ROLE**, not the bound role. This has two reasons:
      --  * the bound role may well be in another model, leading to an InvertedQuery for another domain;
      --  * if runtime a role is bound that is a specialisation of the required role, it will still trigger the inverted query.
      oneStepLess = removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
      roleNames = unwrap <$> (leavesInADT $ unsafePartial roleRange qfd)
      invertedQuery = case oneStepLess of
        -- Backward consisted of just a single step and that was GetRoleBindersF.
        -- Consequently, the role instance that we are going to apply the backwards part of the inverted query to,
        -- is already the end result we want to obtain. Hence we do with the Identity function, where the domain and
        -- range are the range of the backward step.
        ZQ Nothing fwd -> ZQ (Just (SQD ran (QF.DataTypeGetter IdentityF) ran True True)) fwd
        x -> x
      in
        foldl (\dfr' roleName ->
          case lookup roleName enumeratedRoles of
            Nothing -> addInvertedQueryForDomain roleName
              (InvertedQuery
                { description: invertedQuery
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                , modifies: isJust $ elemIndex (EnumeratedRoleType enr) modifiesRoleBindingOf
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly})
              (OnRoleDelta_binder (unsafePartial $ fromJust embeddingContext))
              dfr'
            Just en -> dfr' {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binder en invertedQuery (isJust $ elemIndex (EnumeratedRoleType enr) modifiesRoleBindingOf) (unsafePartial $ fromJust embeddingContext)) enumeratedRoles })
          dfr
          roleNames

    -- add to onRoleDelta_binding of the role that we apply `binding` to (the domain of the step; the role that binds).
    QF.DataTypeGetter QF.BindingF -> modifyDF \dfr@{enumeratedRoles} -> case dom of
      (RDOM EMPTY _) -> dfr
      _ -> let
        roleNames = unwrap <$> (leavesInADT $ unsafePartial roleRange qfd)
        -- We need the embedding context. It must come from the RDOM that is the range of the first step of backwards (ran).
        in
          foldl (\dfr' roleName ->
            case lookup roleName  enumeratedRoles of
              Nothing -> addInvertedQueryForDomain roleName
                (InvertedQuery
                  { description: qWithAK
                  , backwardsCompiled: Nothing
                  , forwardsCompiled: Nothing
                  , users
                  , modifies: (isJust $ elemIndex (EnumeratedRoleType roleName) modifiesRoleBindingOf)
                  , statesPerProperty: EncodableMap statesPerProperty
                  , states
                  , selfOnly})
                (OnRoleDelta_binding (unsafePartial $ fromJust embeddingContext))
                dfr'
              Just en -> dfr' {enumeratedRoles = insert roleName (addPathToOnRoleDelta_binding en qWithAK (isJust $ elemIndex (EnumeratedRoleType roleName) modifiesRoleBindingOf) (unsafePartial $ fromJust embeddingContext)) enumeratedRoles})
            dfr
            roleNames
    QF.RolGetter roleType -> case roleType of
      ENR (EnumeratedRoleType roleName) -> modifyDF \dfr@{enumeratedRoles} -> let
        -- We remove the first step of the backwards path, because we apply it runtime not to the context, but to
        -- the new role instance. We skip the RolGetter step because its cardinality is larger than one.
        -- Because the forward part will be applied to that same role (instead of the context), we have to compensate
        -- for that by prepending it with the inversal of the first backward step - which is, by construction, a
        -- `context` step.
        oneStepLess = removeFirstBackwardsStep qWithAK
          (\ran' dom' man' -> Just $ SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
        in case oneStepLess of
          ZQ Nothing _ -> dfr
          _ -> case lookup roleName enumeratedRoles of
            Nothing -> addInvertedQueryForDomain roleName
              (InvertedQuery
                { description: oneStepLess
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                , modifies: (isJust $ elemIndex (EnumeratedRoleType roleName) modifiesRoleBindingOf)
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                })
              (OnContextDelta_role (unsafePartial $ fromJust embeddingContext))
              dfr
            Just en -> dfr {enumeratedRoles = insert roleName (addPathToOnContextDelta_role en oneStepLess (isJust $ elemIndex (EnumeratedRoleType roleName) modifiesRoleBindingOf) (unsafePartial $ fromJust embeddingContext)) enumeratedRoles}
      CR _ -> throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

    QF.DataTypeGetter QF.ContextF -> modifyDF \dfr@{enumeratedRoles} -> let
      roleNames = unwrap <$> (leavesInADT $ unsafePartial roleDomain qfd)
      in
        foldl (\dfr' roleName ->
          case lookup roleName  enumeratedRoles of
            Nothing -> addInvertedQueryForDomain roleName
              (InvertedQuery
                { description: qWithAK
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                , modifies: (isJust $ elemIndex (EnumeratedRoleType roleName) modifiesRoleBindingOf)
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                })
              (OnContextDelta_context (unsafePartial $ fromJust embeddingContext))
              dfr'
            Just en -> dfr' {enumeratedRoles = insert roleName (addPathToOnContextDelta_context en qWithAK (isJust $ elemIndex (EnumeratedRoleType roleName) modifiesRoleBindingOf) (unsafePartial $ fromJust embeddingContext)) enumeratedRoles})
        dfr
        roleNames

    -- As there is, by construction, no link from the range (an external role type)
    -- to the domain (a context type), we can not attach an inverted query anywhere
    QF.ExternalCoreContextGetter f -> pure unit

    -- The query would be added to onContextDelta_role of the external role. Such inverse queries are run when a new
    -- instance of the role type is added to the context (or when it is removed). But the external role never changes,
    -- so this is superfluous.
    QF.DataTypeGetter QF.ExternalRoleF -> pure unit

    -- Ignore ExternalRoleF and IdentityF in an inverse path. We do not
    -- establish queries to gather affected contexts on them. For IdentityF this is
    -- because we will establish a query on the next step (or have done so at the
    -- previous step). For ExternalRoleF this is because there cannot be ContextDeltas
    -- for the External role.
    QF.DataTypeGetter QF.IdentityF -> pure unit

    _ -> throwError $ Custom "setPathForStep: there should be no other cases. This is a system programming error."

  where
    -- NOTE that we lose information here in case the domain isn't a single role or context type.
    getEmbeddingContextFromDomain :: Partial => Domain -> WithModificationSummary (Maybe ContextType)
    getEmbeddingContextFromDomain (RDOM adt membeddingContext) = case membeddingContext of
      Nothing -> (lift $ lift $ lift $ getEnumeratedRole (fromJust $ head (leavesInADT adt))) >>= pure <<< Just <<< _.context <<< unwrap
      Just embeddingContext -> pure $ Just embeddingContext
    getEmbeddingContextFromDomain (CDOM adt) = pure $ head (leavesInADT adt)
    getEmbeddingContextFromDomain _ = pure Nothing

    addPathToProperty :: EnumeratedProperty -> QueryWithAKink -> Boolean -> EnumeratedRoleType -> EnumeratedProperty
    addPathToProperty (EnumeratedProperty propRecord@{_id, onPropertyDelta}) inverseQuery modifies eroleType = EnumeratedProperty propRecord {onPropertyDelta = addInvertedQueryIndexedByRole (InvertedQuery
      { description: inverseQuery
      , backwardsCompiled: Nothing
      , forwardsCompiled: Nothing
      , users
      , modifies
      , statesPerProperty: EncodableMap statesPerProperty
      , states: maybe [] identity (Map.lookup (ENP _id) statesPerProperty)
      , selfOnly})
      eroleType
      onPropertyDelta
    }

    addPathToOnRoleDelta_binder :: EnumeratedRole -> QueryWithAKink -> Boolean -> ContextType -> EnumeratedRole
    addPathToOnRoleDelta_binder (EnumeratedRole rolRecord@{onRoleDelta_binder}) inverseQuery modifies embeddingContext = EnumeratedRole rolRecord {onRoleDelta_binder = addInvertedQueryIndexedByContext (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly}) embeddingContext onRoleDelta_binder}

    addPathToOnRoleDelta_binding :: EnumeratedRole -> QueryWithAKink -> Boolean -> ContextType -> EnumeratedRole
    addPathToOnRoleDelta_binding (EnumeratedRole rolRecord@{onRoleDelta_binding}) inverseQuery modifies embeddingContext = EnumeratedRole rolRecord {onRoleDelta_binding = addInvertedQueryIndexedByContext (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly}) embeddingContext onRoleDelta_binding}

    addPathToOnContextDelta_context :: EnumeratedRole -> QueryWithAKink -> Boolean -> ContextType -> EnumeratedRole
    addPathToOnContextDelta_context (EnumeratedRole rolRecord@{onContextDelta_context}) inverseQuery modifies embeddingContext = EnumeratedRole rolRecord {onContextDelta_context = addInvertedQueryIndexedByContext (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly}) embeddingContext onContextDelta_context}

    addPathToOnContextDelta_role :: EnumeratedRole -> QueryWithAKink -> Boolean -> ContextType -> EnumeratedRole
    addPathToOnContextDelta_role (EnumeratedRole rolRecord@{onContextDelta_role}) inverseQuery modifies embeddingContext = EnumeratedRole rolRecord {onContextDelta_role = addInvertedQueryIndexedByContext (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly}) embeddingContext onContextDelta_role}

------------------------------------------------------------------------------------------
---- REMOVE FIRST BACKWARDS STEP
------------------------------------------------------------------------------------------
removeFirstBackwardsStep :: QueryWithAKink ->
  -- A function from domain and range of the step to prepend, and whether it is mandatory.
  (Domain -> Range -> ThreeValuedLogic -> Maybe QueryFunctionDescription) ->
  QueryWithAKink
removeFirstBackwardsStep q@(ZQ backward forward) originalStepF = case backward, forward of
  (Just (BQD _ (BinaryCombinator ComposeF) firstBackwardStep qfd2 _ _ _)), (Just forward') -> ZQ
      (Just qfd2)
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (Just $ maybe forward' (flip makeComposition forward') (originalStepF (range firstBackwardStep) (domain firstBackwardStep) (mandatory firstBackwardStep)))
  (Just (BQD _ (BinaryCombinator ComposeF) firstBackwardStep qfd2 _ _ _)), Nothing -> ZQ
      (Just qfd2)
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (originalStepF (range firstBackwardStep) (domain firstBackwardStep) (mandatory firstBackwardStep))
  (Just i@(SQD dom _ ran fun man)), (Just forward') -> (ZQ
      Nothing
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (Just $ maybe forward' (flip makeComposition forward') (originalStepF ran dom man)))
  (Just i@(SQD dom _ ran fun man)), Nothing -> (ZQ
      Nothing
      -- The domain and range of the new forward step are those of the first backward step switched.
      -- We keep its mandatory status.
      (originalStepF ran dom man))
  _, _ -> q

makeComposition :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
makeComposition left right = BQD
  (domain left)
  (BinaryCombinator ComposeF)
  left
  right
  (range right)
  (and (functional left) (functional right))
  (or (functional left) (functional right))
