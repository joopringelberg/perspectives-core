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

module Perspectives.Parsing.Arc.PhaseThree.StoreInvertedQueries where

import Control.Monad.Except (lift)
import Control.Monad.Reader (ReaderT, ask)
import Data.Array (concat, foldl, fromFoldable, head, union)
import Data.Map (Map, values) as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspective.InvertedQuery.Indices (compiletimeIndexForFillerQueries, compiletimeIndexForFilledQueries)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (SeparateInvertedQuery(..), addInvertedQueryForDomain)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), addInvertedQueryIndexedByContext, addInvertedQueryIndexedByRole, addInvertedQueryToPropertyIndexedByRole, backwards, forwards)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo', modifyDF, throwError)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), Range, RoleInContext(..), domain, domain2roleInContext, domain2roleType, makeComposition, mandatory, range, roleDomain, roleInContext2Role, roleRange)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey, addInvertedQueryIndexedByTripleKeys)
import Perspectives.Representation.ExplicitSet (ExplicitSet)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier)
import Perspectives.Utilities (prettyPrint)
import Prelude (Unit, bind, discard, flip, pure, unit, ($), (<$>), (<>), (==))

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

  case backward of
    -- Think of this as: {first source step} << filter criterium << {last criterium step} (NOTICE the inverse composition step <<)
    Just (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _) -> case qfd2 of
      -- qfd1 is the last step of the INVERTED criterium; 'criterium' in FilterF is the ORIGINAL criterium. 
      (BQD _ (BinaryCombinator ComposeF) filter@(UQD _ FilterF criterium _ _ _) source _ _ _) -> do 
        -- Drop the filter. Store  {first source step} << {last criterium step}
        unsafePartial $ setPathForStep 
          qfd1
          (ZQ (Just $ makeComposition qfd1 source) $ forwards qwk)
          users 
          roleStates
          statesPerProperty 
          selfOnly 
          Nothing
        -- prepend the filter to the source. Store {first source step} << filter criterium.
        unsafePartial $ setPathForStep 
          source
          (ZQ (Just source) $ forwards qwk)
          users 
          (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty)) 
          statesPerProperty 
          selfOnly 
          (Just filter)
      _ -> unsafePartial $ setPathForStep qfd1 qwk users roleStates statesPerProperty selfOnly Nothing
    (Just b@(SQD _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users roleStates statesPerProperty selfOnly Nothing
    (Just b@(MQD _ _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users roleStates statesPerProperty selfOnly Nothing
    otherwise -> lift $ throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint otherwise)

-- | The function is partial, because we just handle the SQD and MQD cases.
-- | The first argument is the first step of the backward path of the second argument (but, in the case the backwards part holds a filter, it may have been changed).
-- | This is not a recursive function! It merely adds the QueryWithAKink to a Context, Role or Property type.
-- | Modifies the DomeinFile in PhaseTwoState.
setPathForStep :: Partial =>
  QueryFunctionDescription ->
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  Maybe QueryFunctionDescription ->
  WithModificationSummary Unit
setPathForStep qfd@(SQD dom qf ran fun man) qWithAK users states statesPerProperty selfOnly mfilter = do
  {modifiesRoleInstancesOf, modifiesRoleBindingOf, modifiesPropertiesOf} <- ask
  case qf of
    -- The original property expression can never be the source in `filter source with criterium`, so we ignore mfilter.
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
              -- The inverted query is the inversion of the computation of the Perspective object. This object should be available
              -- to the user in all states that the perspective is valid in. These states are not listed explicitly in the Perspective,
              -- but can be computed from the union of the StateSpecs of the roleVerbs, propertyVerbs and actions.
              , states
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

    -- FILLED STEP
    -- filled step is stored in filledInvertedQueries of the filled role.
    QF.FilledF enr ctxt -> do
      -- Compute the keys on the base of the original backwards query.
      modifyDF \dfr -> let
        -- We remove the first step of the backwards path, because we apply it (runtime) not to the filler
        -- but to the filled. We skip the fills step because its cardinality is larger than one. It would
        -- cause a fan-out while we know, when applying the inverted query when handling a RoleBindingDelta, the exact
        -- path to follow.
        -- The function `usersWithPerspectiveOnRoleBinding` applies the forward part to the filler. Hence we don't 
        -- have to adapt the forward part.
        -- We add the inverted query to the **FILLED** role, not the filler role.
        -- This has two reasons:
        --  * the filler may well be in another model, leading to an InvertedQuery for another domain;
        --  * if runtime a filler is used that is a specialisation of the required role, it will still trigger
        --    the inverted query.
        oneStepLess = removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
        description = case mfilter of 
          Nothing -> case oneStepLess of
            -- If backwards of oneStepLess is Nothing, the backwards step of qWithAK (== qfd) consisted of just
            -- a single step and that was FilledF.
            -- Consequently, the role instance that we are going to apply the backwards part of the inverted query to,
            -- is already the end result we want to obtain. Hence we put the Identity function in the place of backwards,
            -- where the domain and range are the range of the backward step.
            ZQ Nothing fwd -> ZQ (Just (SQD ran (QF.DataTypeGetter IdentityF) ran True True)) fwd
            x -> x
          -- Add the FillerF step to the criterium of the FilterF step;
          -- Prepend the modified filter to the backwards part.
          Just filter -> preprendToCriterium 
            oneStepLess
            (\ran' dom' man' -> SQD ran' (QF.DataTypeGetter FillerF) dom' True man')
            filter
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
                  (FilledInvertedQuery keys)
                  dfr'
                Just en -> do
                  dfr' {enumeratedRoles = insert
                    (unwrap filledType)
                    (addPathToFilledInvertedQueries
                      en
                      keys
                      description
                      modifiesRoleBindingOf)
                    enumeratedRoles }
                )
            dfr
            (compiletimeIndexForFilledQueries qfd) -- qfd is the ORIGINAL backward query.

    -- FILLER STEP
    -- filler step is stored in fillerInvertedQueries of the filled role.
    QF.DataTypeGetter QF.FillerF -> do
      keysForRole <- lift $ compiletimeIndexForFillerQueries qfd
      modifyDF \dfr@{enumeratedRoles} -> let
        oneStepLess = removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
        description = case mfilter of 
          -- We remove the first step of the backwards path, because we apply it (runtime) not to the filled
          -- but to the filler. We skip the fills step because we can: we don't have to compute the filler from the 
          -- filled, because it is already in the Delta.
          -- We add the inverted query to the **FILLED** role, not the filler role.
          Nothing -> case oneStepLess of
            -- If backwards of oneStepLess is Nothing, the backwards step of qWithAK (== qfd) consisted of just
            -- a single step and that was FillerF.
            -- Consequently, the role instance that we are going to apply the backwards part of the inverted query to,
            -- is already the end result we want to obtain. Hence we put the Identity function in the place of backwards,
            -- where the domain and range are the range of the backward step.
            ZQ Nothing fwd -> ZQ (Just (SQD ran (QF.DataTypeGetter IdentityF) ran True True)) fwd
            x -> x
          -- Add the FilledF step to the criterium of the FilterF step;
          -- Prepend the modified filter to the backwards part.
          Just filter -> preprendToCriterium 
            oneStepLess
            -- NOTICE. The domain, being an ADT, may have multiple RoleInContext combinations. But we can only use one of these
            -- as parameters of FilledF. We arbitrarily choose the first.
            (\ran' dom' man' -> 
              let 
                RoleInContext{context, role} = unsafePartial fromJust $ head $ allLeavesInADT (domain2roleInContext dom')
              in
                SQD ran' (FilledF role context) dom' True man')
            filter
        in case dom of
          (RDOM EMPTY) -> dfr
          _ -> foldl
              (\dfr' (Tuple filledType keys) ->
                case lookup (unwrap filledType) enumeratedRoles of
                  Nothing -> addInvertedQueryForDomain (unwrap filledType)
                    (InvertedQuery
                      { description: description
                      , backwardsCompiled: Nothing
                      , forwardsCompiled: Nothing
                      , users
                      -- Default value. Should be computed per case.
                      , modifies: false
                      , statesPerProperty: EncodableMap statesPerProperty
                      -- pas states aan als een stap is weggehaald. D.w.z. Verwijder de states van het oorspronkelijke domein van backwards, voeg als state toe de ground state van het nieuwe domein van backwards.
                      , states
                      , selfOnly})
                    -- Maybe add modifiesRoleBindingOf here; but do we then need to check on
                    -- synchronization on importing a model? I doubt it.
                    (FillerInvertedQuery keys)
                    dfr'
                  Just en -> dfr' {enumeratedRoles = insert
                    (unwrap filledType)
                    (addPathToFillerInvertedQueries
                      en
                      keys
                      description
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
        description = case mfilter of 
          Nothing -> oneStepLess
          -- Add the context step to the criterium of the FilterF step;
          -- Prepend the modified filter to the backwards part.
          Just filter -> preprendToCriterium 
            oneStepLess 
            (\ran' dom' man' -> SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
            filter
        in case description of
          ZQ Nothing _ -> dfr
          otherwise -> foldl
            (\dfr' (RoleInContext{context, role}) -> case lookup (unwrap context) contexts of
              Nothing -> addInvertedQueryForDomain (unwrap context)
                (InvertedQuery
                  { description
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
                  description
                  modifiesRoleInstancesOf
                  role)
                contexts}
              )
            dfr
            (allLeavesInADT (roleRange qfd))
      CR _ -> lift $ throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

    -- We could omit the first backwards step, as in runtime we have the context of a role instance at hand.
    -- However, we handle that situation by `handlebackwardsQuery` and that function expects a RoleInstance.
    QF.DataTypeGetter QF.ContextF -> do
      description <- case mfilter of 
        Nothing -> pure qWithAK
        -- If there is a filter, prepend it to the backwards part.
        Just filter -> pure $ ZQ ((makeComposition filter) <$> backwards qWithAK) (forwards qWithAK)
      modifyDF \dfr@{enumeratedRoles} -> foldl
        (\dfr' ric@(RoleInContext{context, role}) ->
            case lookup (unwrap role) enumeratedRoles of
              Nothing -> addInvertedQueryForDomain (unwrap role)
                (InvertedQuery
                  { description
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
                  description
                  modifiesRoleInstancesOf
                  context)
                enumeratedRoles})
          dfr
          (allLeavesInADT $ unsafePartial roleDomain qfd) -- qfd is the original backwards path.

    -- The query would be added to roleInvertedQueries of the context. Such inverse queries are run when a new
    -- instance of the role type is added to the context (or when it is removed). But the external role never changes,
    -- so this is superfluous.
    QF.DataTypeGetter QF.ExternalRoleF -> pure unit

    -- Ignore ExternalRoleF and IdentityF in an inverse path. We do not
    -- establish queries to gather affected contexts on them. For IdentityF this is
    -- because we will establish a query on the next step (or have done so at the
    -- previous step). For ExternalRoleF this is because there cannot be ContextDeltas
    -- for the External role.
    QF.DataTypeGetter QF.IdentityF -> pure unit

    _ -> lift $ throwError $ Custom "setPathForStep: there should be no other cases. This is a system programming error."

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
      , states
      , selfOnly})
      eroleType
      onPropertyDelta
      modifiesPropertiesOf
      _id
    }

    addPathToFilledInvertedQueries :: EnumeratedRole -> Array InvertedQueryKey -> QueryWithAKink -> Array RoleInContext -> EnumeratedRole
    addPathToFilledInvertedQueries (EnumeratedRole rolRecord@{_id, filledInvertedQueries}) keys inverseQuery modifiesRoleBindingOf = EnumeratedRole rolRecord {filledInvertedQueries =
      addInvertedQueryIndexedByTripleKeys
        (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
        keys
        filledInvertedQueries
        modifiesRoleBindingOf
        _id
        }

    addPathToFillerInvertedQueries :: EnumeratedRole -> Array InvertedQueryKey -> QueryWithAKink -> Array RoleInContext -> EnumeratedRole
    addPathToFillerInvertedQueries (EnumeratedRole rolRecord@{_id, fillerInvertedQueries}) keys inverseQuery modifiesRoleBindingOf = EnumeratedRole rolRecord {fillerInvertedQueries =
      addInvertedQueryIndexedByTripleKeys
        (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
        keys
        fillerInvertedQueries
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
    addPathToRoleInvertedQueries (Context contextRecord@{_id, roleInvertedQueries}) inverseQuery modifiesRoleInstancesOf eRoleType = Context contextRecord {roleInvertedQueries = addInvertedQueryIndexedByRole
      (InvertedQuery {description: inverseQuery, backwardsCompiled: Nothing, forwardsCompiled: Nothing, users, modifies:false, statesPerProperty: EncodableMap statesPerProperty, states, selfOnly})
      eRoleType
      roleInvertedQueries
      modifiesRoleInstancesOf
      _id
    }

setPathForStep (MQD _ qf _ _ _ _) qWithAK users states statesPerProperty selfOnly mfilter = 
  case qf of
    -- ExternalCoreRoleGetter is the inversion of a role individual step, such as sys:Me,
    -- and there are no other query steps that invert to it.
    -- This inversion should be stored with the type of the role individual. 
    -- However, by definition, no instances of such a context individual will be created beyond the original
    -- one that is created at setup of a model. In other words, we have nothing to gain by setting up a new-instance-detection
    -- system at that type!
    -- Therefore we simply ignore this step.
    -- NOTICE: the inversion is useful, as it will be part of backwards queries - we just don't store the inverted query of such
    -- a query that is kinked at this step.
    QF.ExternalCoreRoleGetter f -> pure unit

    -- ExternalCoreContextGetter is the inversion of a context individual step, such as sys:TheWorld,
    -- and there are no other query steps that invert to it.
    -- This inversion should be stored with the type of the context individual. 
    -- However, by definition, no instances of such a context individual will be created beyond the original
    -- one that is created at setup of a model. In other words, we have nothing to gain by setting up a new-instance-detection
    -- system at that type!
    -- Therefore we simply ignore this step.
    -- NOTICE: the inversion is useful, as it will be part of backwards queries - we just don't store the inverted query of such
    -- a query that is kinked at this step.
    QF.ExternalCoreContextGetter f -> pure unit

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

-- Add an extra step to the criterium of the FilterF step;
-- Prepend the modified filter to the backwards part.
preprendToCriterium :: QueryWithAKink ->
  -- A function from domain and range of the step to prepend to the criterium of the filter, and whether it is mandatory.
  (Domain -> Range -> ThreeValuedLogic -> QueryFunctionDescription) ->
  QueryFunctionDescription ->
  QueryWithAKink
preprendToCriterium q@(ZQ backward forward) createExtraStep filter = case filter of 
  (UQD dom FilterF criterium ran fun man) -> case backward of 
    -- backward is just the filter with its criterium augmented by an initial extra step.
    Nothing -> ZQ (Just $ (UQD dom FilterF (makeComposition (createExtraStep dom ran man) criterium) ran fun man)) forward
    -- prepend to backward the filter with its criterium augmented by an initial extra step.
    Just bw -> ZQ (Just $ makeComposition (UQD dom FilterF (makeComposition (createExtraStep dom ran man) criterium) ran fun man) bw) forward
  _ -> q