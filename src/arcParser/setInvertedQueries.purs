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
import Data.Array (foldl, union, concat, fromFoldable)
import Data.Map (Map, lookup, values) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspective.InvertedQuery.Indices (compiletimeIndexForFilledByQueries, compiletimeIndexForFillsQueries)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), addInvertedQueryIndexedByContext, addInvertedQueryIndexedByRole, addInvertedQueryToPropertyIndexedByRole)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo', modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, RoleInContext(..), domain, domain2roleType, functional, mandatory, range, roleDomain, roleInContext2Role, roleRange)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey, addInvertedQueryIndexedByTripleKeys)
import Perspectives.Representation.ExplicitSet (ExplicitSet)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), and, or)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier)
import Perspectives.Utilities (prettyPrint)
import Prelude (Unit, flip, identity, pure, unit, ($), (<$>), bind, (==), (<>))

type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

-- | Modifies the DomeinFile in PhaseTwoState.
storeInvertedQuery ::
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  WithModificationSummary Unit
storeInvertedQuery qwk@(ZQ backward forward) users roleStates statesPerProperty selfOnly = do
  -- What is confusing about what follows is that it just seems to handle the first step of an inverted query.
  -- What about the steps that follow?
  -- Reflect that we have generated *separate inverted queries* for all these steps, each 'kinking' the original query
  -- at a different position.
  -- So if the original query was:
  --    s1 >> s2 >> s3
  -- we generate:
  --    backwards (forwards)
  --    ^s1 (s2 >> s3)          and for this we store an InvertedQuery with s1
  --    ^s1 << ^s2 (s3)         and for this we store an InvertedQuery with s2
  --    ^s1 << ^s2 << ^s3 ()    and for this we store an InvertedQuery with s3
  -- where x << y equals y >> x.

  -- Handle two cases of the backward query:
  --  * (...) << SQD, i.e. when the backward part is a composition (whose first step is not a composition, that would be an error!). Set the InvertedQuery with respect to the first step of backward.
  --  * SQD, i.e. when the backward part is just a single step. Set the InvertedQuery for that step.
  case backward of
    -- case (...) << SQD
    (Just b@(BQD _ (BinaryCombinator ComposeF) qfd1@(SQD _ _ _ _ _) qfd2 _ _ _)) -> unsafePartial $ setPathForStep qfd1 qwk users (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty)) statesPerProperty selfOnly

    (Just b@(SQD _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty)) statesPerProperty selfOnly

    otherwise -> throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint otherwise)

-- | The function is partial, because we just handle the SQD case.
-- | The first argument is the backward path of the second argument.
-- | This is not a recursive function! It merely adds the QueryWithAKink to a Context, Role or Property type.
-- | Modifies the DomeinFile in PhaseTwoState.
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
              -- Default value. Should be computed per case.
              , modifies: false
              , statesPerProperty: EncodableMap statesPerProperty
              -- NOTE. Mag het voorkomen dat de property niet in statesPerProperty zit?
              -- en wat is de betekenis van een InvertedQuery zonder states?
              , states: maybe [] identity (Map.lookup (ENP p) statesPerProperty)
              , selfOnly
              })
            -- Maybe add modifiesPropertiesOf here; but do we then need to check on
            -- synchronization on importing a model? I doubt it.
            (OnPropertyDelta (allLeavesInADT $ roleInContext2Role <$> domain2roleType ran))
            dfr
          Just ep -> dfr {enumeratedProperties = insert
            (unwrap p)
            -- We add the InvertedQuery to the Property, indexed for all role types in the range.
            (foldl
              (\property (RoleInContext{role}) -> addPathToProperty
                property
                qWithAK
                modifiesPropertiesOf
                role)
              ep
              (allLeavesInADT $ domain2roleType ran))
            enumeratedProperties}
        -- CP _ -> throwError $ Custom "Implement the handling of Calculated Properties in setPathForStep."
        CP _ -> pure unit

    -- add to fillsInvertedQueries of the role that we apply `binder enr` to (the domain of the step; the role that is bound).
    QF.GetRoleBindersF enr ctxt -> do
      -- Compute the keys on the base of the original backwards query.
      modifyDF \dfr -> let
        -- We remove the first step of the backwards path, because we apply it (runtime) not to the filler (binding),
        -- but to the filled (binder). We skip the fills step because its cardinality is larger than one. It would
        -- cause a fan-out while we know, when applying the inverted query when handling a RoleBindingDelta, the exact
        -- path to follow.
        -- The function `usersWithPerspectiveOnRoleBinding` applies the forward part to the filler (binding) (not the
        -- binder (filled)). Hence we don't have to adapt the forward part.
        -- We add the inverted query to the **FILLED** (binder) role, not the filler role (binding).
        -- This has two reasons:
        --  * the filler may well be in another model, leading to an InvertedQuery for another domain;
        --  * if runtime a filler is used that is a specialisation of the required role, it will still trigger
        --    the inverted query.
        oneStepLess = removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
        description = case oneStepLess of
          -- If backwards of oneStepLess is Nothing, the backwards step of qWithAK (== qfd) consisted of just
          -- a single step and that was GetRoleBindersF.
          -- Consequently, the role instance that we are going to apply the backwards part of the inverted query to,
          -- is already the end result we want to obtain. Hence we put the Identity function in the place of backwards,
          -- where the domain and range are the range of the backward step.
          ZQ Nothing fwd -> ZQ (Just (SQD ran (QF.DataTypeGetter IdentityF) ran True True)) fwd
          x -> x
        in
          foldl
            (\dfr'@{enumeratedRoles} (Tuple filledType keys) ->
              case lookup (unwrap filledType) enumeratedRoles of
                Nothing -> addInvertedQueryForDomain (unwrap filledType)
                  (InvertedQuery
                    { description
                    , backwardsCompiled: Nothing
                    , forwardsCompiled: Nothing
                    , users
                    -- Default value. Should be computed per case.
                    , modifies: false
                    , statesPerProperty: EncodableMap statesPerProperty
                    , states
                    , selfOnly})
                  -- Maybe add modifiesRoleBindingOf here; but do we then need to check on
                  -- synchronization on importing a model? I doubt it.
                  (FillsInvertedQuery keys)
                  dfr'
                Just en -> do
                  dfr' {enumeratedRoles = insert
                    (unwrap filledType)
                    (addPathToFillsInvertedQueries
                      en
                      keys
                      description
                      modifiesRoleBindingOf)
                    enumeratedRoles }
                )
            dfr
            (compiletimeIndexForFillsQueries qfd) -- qfd is the ORIGINAL backward query.
    --
    -- add to filledByInvertedQueries of the role that we apply `binding` to (the domain of the step; the role that binds).
    QF.DataTypeGetter QF.BindingF -> do
      keysForRole <- lift $ compiletimeIndexForFilledByQueries qfd
      modifyDF \dfr@{enumeratedRoles} -> case dom of
        (RDOM EMPTY) -> dfr
        _ -> foldl
              (\dfr' (Tuple filledType keys) ->
                case lookup (unwrap filledType) enumeratedRoles of
                  Nothing -> addInvertedQueryForDomain (unwrap filledType)
                    (InvertedQuery
                      { description: qWithAK
                      , backwardsCompiled: Nothing
                      , forwardsCompiled: Nothing
                      , users
                      -- Default value. Should be computed per case.
                      , modifies: false
                      , statesPerProperty: EncodableMap statesPerProperty
                      , states
                      , selfOnly})
                    -- Maybe add modifiesRoleBindingOf here; but do we then need to check on
                    -- synchronization on importing a model? I doubt it.
                    (FilledByInvertedQuery keys)
                    dfr'
                  Just en -> dfr' {enumeratedRoles = insert
                    (unwrap filledType)
                    (addPathToFilledByInvertedQueries
                      en
                      keys
                      qWithAK
                      modifiesRoleBindingOf)
                    enumeratedRoles})
              dfr
              keysForRole

    QF.RolGetter roleType -> case roleType of
      ENR (EnumeratedRoleType roleName) -> modifyDF \dfr@{contexts} -> let
        -- We remove the first step of the backwards path, because we apply it runtime not to the context, but to
        -- the new role instance. We skip the RolGetter step because its cardinality is larger than one.
        -- Because the forward part will be applied to that same role (instead of the context), we have to compensate
        -- for that by prepending it with the inversal of the first backward step - which is, by construction, a
        -- `context` step.
        oneStepLess = removeFirstBackwardsStep
          qWithAK
          (\ran' dom' man' -> Just $ SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
        in case oneStepLess of
          ZQ Nothing _ -> dfr
          otherwise -> foldl
            (\dfr' (RoleInContext{context, role}) -> case lookup (unwrap context) contexts of
              Nothing -> addInvertedQueryForDomain (unwrap context)
                (InvertedQuery
                  { description: oneStepLess
                  , backwardsCompiled: Nothing
                  , forwardsCompiled: Nothing
                  , users
                    -- Default value. But is not used while adding a model to an installation.
                  , modifies: false
                  , statesPerProperty: EncodableMap statesPerProperty
                  , states
                  , selfOnly
                  })
                (RoleInvertedQuery role)
                dfr'
              Just ctxt -> dfr' {contexts = insert
                (unwrap context)
                (addPathToRoleInvertedQueries
                  ctxt
                  oneStepLess
                  modifiesRoleInstancesOf
                  role)
                contexts}
              )
            dfr
            (allLeavesInADT (roleRange qfd))
      CR _ -> throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

    QF.DataTypeGetter QF.ContextF -> modifyDF \dfr@{enumeratedRoles} -> foldl
      (\dfr' ric@(RoleInContext{context, role}) ->
          case lookup (unwrap role) enumeratedRoles of
            Nothing -> addInvertedQueryForDomain (unwrap role)
              (InvertedQuery
                { description: qWithAK
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                -- Default value. But is not used while adding a model to an installation.
                , modifies: false
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                })
              (ContextInvertedQuery context)
              dfr'
            Just en -> dfr' {enumeratedRoles = insert (unwrap role)
              (addPathToContextInvertedQueries
                en
                qWithAK
                modifiesRoleInstancesOf
                context)
              enumeratedRoles})
        dfr
        (allLeavesInADT $ unsafePartial roleDomain qfd) -- qfd is the original backwards path.

    -- As there is, by construction, no link from the range (an external role type)
    -- to the domain (a context type), we can not attach an inverted query anywhere
    QF.ExternalCoreContextGetter f -> pure unit

    -- The query would be added to invertedQueries of the context. Such inverse queries are run when a new
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
    addPathToProperty :: EnumeratedProperty ->
      QueryWithAKink ->
      Map.Map EnumeratedRoleType (ExplicitSet EnumeratedPropertyType) ->
      EnumeratedRoleType ->
      EnumeratedProperty
    addPathToProperty (EnumeratedProperty propRecord@{_id, onPropertyDelta}) inverseQuery modifiesPropertiesOf eroleType = EnumeratedProperty propRecord {onPropertyDelta = addInvertedQueryToPropertyIndexedByRole (InvertedQuery
      { description: inverseQuery
      , backwardsCompiled: Nothing
      , forwardsCompiled: Nothing
      , users
      , modifies:false
      , statesPerProperty: EncodableMap statesPerProperty
      , states: maybe [] identity (Map.lookup (ENP _id) statesPerProperty)
      , selfOnly})
      eroleType
      onPropertyDelta
      modifiesPropertiesOf
      _id
    }

    addPathToFillsInvertedQueries :: EnumeratedRole -> Array InvertedQueryKey -> QueryWithAKink -> Array RoleInContext -> EnumeratedRole
    addPathToFillsInvertedQueries (EnumeratedRole rolRecord@{_id, fillsInvertedQueries}) keys inverseQuery modifiesRoleBindingOf = EnumeratedRole rolRecord {fillsInvertedQueries =
      addInvertedQueryIndexedByTripleKeys
        (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
        keys
        fillsInvertedQueries
        modifiesRoleBindingOf
        _id
        }

    addPathToFilledByInvertedQueries :: EnumeratedRole -> Array InvertedQueryKey -> QueryWithAKink -> Array RoleInContext -> EnumeratedRole
    addPathToFilledByInvertedQueries (EnumeratedRole rolRecord@{_id, filledByInvertedQueries}) keys inverseQuery modifiesRoleBindingOf = EnumeratedRole rolRecord {filledByInvertedQueries =
      addInvertedQueryIndexedByTripleKeys
        (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
        keys
        filledByInvertedQueries
        modifiesRoleBindingOf
        _id}

    -- Add an inverted query to the set of inverted queries (contextInvertedQueries) on an EnumeratedRole type,
    -- for the `context` step and therefore indexed by ContextType.
    addPathToContextInvertedQueries :: EnumeratedRole -> QueryWithAKink -> Array RoleInContext -> ContextType -> EnumeratedRole
    addPathToContextInvertedQueries (EnumeratedRole rolRecord@{_id, contextInvertedQueries}) inverseQuery modifiesRoleInstancesOf embeddingContext = EnumeratedRole rolRecord {contextInvertedQueries = addInvertedQueryIndexedByContext
      (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
      embeddingContext
      contextInvertedQueries
      modifiesRoleInstancesOf
      _id
      }

    -- Add an inverted query to the set of inverted queries on a Context type,
    -- for the `role` step (and therefore indexed by EnumeratedRoleType).
    addPathToRoleInvertedQueries :: Context -> QueryWithAKink -> Array RoleInContext -> EnumeratedRoleType -> Context
    addPathToRoleInvertedQueries (Context contextRecord@{_id, invertedQueries}) inverseQuery modifiesRoleInstancesOf eRoleType = Context contextRecord {invertedQueries = addInvertedQueryIndexedByRole
      (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
      eRoleType
      invertedQueries
      modifiesRoleInstancesOf
      _id
    }

------------------------------------------------------------------------------------------
---- REMOVE FIRST BACKWARDS STEP
------------------------------------------------------------------------------------------
removeFirstBackwardsStep :: QueryWithAKink ->
  -- A function from domain and range of the step to prepend, and whether it is mandatory.
  -- For RoleBindersF (fills step) this function always returns Nothing, and thus does not affect the
  -- forwards part returned from `removeFirstBackwardsStep`.
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
