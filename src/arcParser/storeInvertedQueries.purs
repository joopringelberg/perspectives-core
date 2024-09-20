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
import Data.Array (concat, fromFoldable, head, union)
import Data.Map (Map, values) as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Partial.Unsafe (unsafePartial)
import Perspective.InvertedQuery.Indices (typeLevelKeyForContextQueries, typeLevelKeyForFilledQueries, typeLevelKeyForFillerQueries, typeLevelKeyForPropertyQueries, typeLevelKeyForRoleQueries)
import Perspectives.ArrayUnions (ArrayUnions(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), backwards, forwards)
import Perspectives.InvertedQueryKey (serializeInvertedQueryKey)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo', addStorableInvertedQuery, getsDF, throwError)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Domain, QueryFunctionDescription(..), Range, RoleInContext(..), domain, domain2roleInContext, makeComposition, mandatory, range)
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..), StateIdentifier)
import Perspectives.Utilities (prettyPrint)
import Prelude (Unit, bind, discard, flip, pure, unit, ($), (<$>), (<>), (==))

type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

storeInvertedQuery ::
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  Boolean -> 
  WithModificationSummary Unit
storeInvertedQuery qwk users roleStates statesPerProperty selfOnly authorOnly = storeInvertedQuery' qwk users roleStates statesPerProperty selfOnly authorOnly Nothing


-- | Modifies the DomeinFile in PhaseTwoState.
storeInvertedQuery' ::
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  Boolean -> 
  Maybe QueryFunctionDescription ->
  WithModificationSummary Unit
storeInvertedQuery' qwk@(ZQ backward forward) users roleStates statesPerProperty selfOnly authorOnly mfilter = do
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
          authorOnly
          Nothing
        -- prepend the filter to the source. Store {first source step} << filter criterium.
        storeInvertedQuery' 
          (ZQ (Just source) $ forwards qwk) 
          users 
          (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty)) 
          statesPerProperty
          selfOnly
          authorOnly
          (Just filter)
        -- unsafePartial $ setPathForStep 
        --   source
        --   (ZQ (Just source) $ forwards qwk)
        --   users 
        --   (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty)) 
        --   statesPerProperty 
        --   selfOnly 
        --   (Just filter)
      _ -> unsafePartial $ setPathForStep qfd1 qwk users roleStates statesPerProperty selfOnly authorOnly mfilter
    (Just b@(SQD _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users roleStates statesPerProperty selfOnly authorOnly mfilter
    (Just b@(MQD _ _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users roleStates statesPerProperty selfOnly authorOnly mfilter
    otherwise -> lift $ throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint otherwise)

-- | The function is partial, because we just handle the SQD and MQD cases.
-- | The first argument is the first step of the backward path of the second argument (but, in the case the backwards part holds a filter, it may have been changed).
-- | This is not a recursive function! It merely adds the QueryWithAKink to a Context, Role or Property type.
-- | Modifies the DomeinFile in PhaseTwoState.
setPathForStep :: Partial =>
  QueryFunctionDescription ->                         -- First step of the backward part of the next argument.
  QueryWithAKink ->
  Array RoleType ->
  Array StateIdentifier ->
  Map.Map PropertyType (Array StateIdentifier) ->
  Boolean ->
  Boolean ->
  Maybe QueryFunctionDescription ->
  WithModificationSummary Unit
setPathForStep qfd@(SQD dom qf ran fun man) qWithAK users states statesPerProperty selfOnly authorOnly mfilter = do
  model <- getsDF _.id
  {modifiesRoleInstancesOf, modifiesRoleBindingOf, modifiesPropertiesOf} <- ask
  case qf of
    -- The original property expression can never be the source in `filter source with criterium`, so we ignore mfilter.
    QF.Value2Role pt -> if dom == ran
      -- This handles cases like `step >>= sum`, where we construct a Value2Role
      -- but really must ignore it.
      then pure unit
      else case pt of
        ENP p -> do 
          keys <- lift $ lift $ lift $ typeLevelKeyForPropertyQueries p qfd
          lift $ addStorableInvertedQuery 
            { keys: serializeInvertedQueryKey <$> keys 
            , queryType: "RTPropertyKey"
            , query: (InvertedQuery
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
              , authorOnly
              })
            , model}
        CP _ -> pure unit

    -- FILLED STEP
    QF.FilledF enr ctxt ->
      -- Compute the keys on the base of the original backwards query.
      -- The domain can be a complex ADT RoleInContext. The range is always an ST RoleInContext.
      let 
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
        do 
          (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForFilledQueries qfd -- The first step of the backwards part of the original inverted query, being FilledF.
          lift $ addStorableInvertedQuery
            { keys: serializeInvertedQueryKey <$> keys 
            , queryType: "RTFilledKey"
            , query: (InvertedQuery
                { description
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                -- Default value. Should be computed per case.
                , modifies: false
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                , authorOnly}) 
            , model }

    -- FILLER STEP
    QF.DataTypeGetter QF.FillerF -> do
      (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForFillerQueries qfd
      oneStepLess <- pure $ removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
      description <- pure $ case mfilter of 
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
      case dom of
        _ -> lift $ addStorableInvertedQuery
          { keys: serializeInvertedQueryKey <$> keys 
          , queryType: "RTFillerKey"
          , query: (InvertedQuery
                    { description: description
                    , backwardsCompiled: Nothing
                    , forwardsCompiled: Nothing
                    , users
                    -- Default value. Should be computed per case.
                    , modifies: false
                    , statesPerProperty: EncodableMap statesPerProperty
                    -- pas states aan als een stap is weggehaald. D.w.z. Verwijder de states van het oorspronkelijke domein van backwards, voeg als state toe de ground state van het nieuwe domein van backwards.
                    , states
                    , selfOnly
                    , authorOnly}) 
          , model }
          
    -- Treat the variant with a context restriction in exactly the same way as without that restriction.
    QF.DataTypeGetterWithParameter QF.FillerF _ -> setPathForStep (SQD dom (QF.DataTypeGetter QF.FillerF) ran fun man) qWithAK users states statesPerProperty selfOnly authorOnly mfilter

    QF.RolGetter roleType -> case roleType of
      ENR role -> let
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
          ZQ Nothing _ -> pure unit
          left -> do
            (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForRoleQueries qfd  
            lift $ addStorableInvertedQuery
              { keys: serializeInvertedQueryKey <$> keys 
              , queryType: "RTRoleKey"
              , query: (InvertedQuery
                { description
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                  -- Default value. But is not used while adding a model to an installation.
                , modifies: false
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                , authorOnly
                })
              , model}
      CR _ -> lift $ throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

    -- We could omit the first backwards step, as in runtime we have the context of a role instance at hand.
    -- However, we handle that situation by `handlebackwardsQuery` and that function expects a RoleInstance.
    QF.DataTypeGetter QF.ContextF -> do
      description <- case mfilter of 
        Nothing -> pure qWithAK
        -- If there is a filter, prepend it to the backwards part.
        Just filter -> pure $ ZQ ((makeComposition filter) <$> backwards qWithAK) (forwards qWithAK)
      (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForContextQueries qfd
      lift $ addStorableInvertedQuery
        { keys: serializeInvertedQueryKey <$> keys 
        , queryType: "RTContextKey"
        , query: (InvertedQuery
                  { description
                  , backwardsCompiled: Nothing
                  , forwardsCompiled: Nothing
                  , users
                  -- Default value. But is not used while adding a model to an installation.
                  , modifies: false
                  , statesPerProperty: EncodableMap statesPerProperty
                  , states
                  , selfOnly
                  , authorOnly
                  })
        , model}

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

    -- Exactly the same treatment as case RolGetter for ENR.
    QF.DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF et -> let
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
          ZQ Nothing _ -> pure unit
          left -> do
            (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForRoleQueries qfd  
            lift $ addStorableInvertedQuery 
              { keys: serializeInvertedQueryKey <$> keys 
              , queryType: "RTRoleKey"
              , query: (InvertedQuery
                { description
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                  -- Default value. But is not used while adding a model to an installation.
                , modifies: false
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                , authorOnly
                })
              , model}

    QF.RoleIndividual _ -> pure unit

    QF.ContextIndividual _ -> pure unit

    _ -> lift $ throwError $ Custom "setPathForStep: there should be no other cases. This is a system programming error."

setPathForStep (MQD _ qf _ _ _ _) qWithAK users states statesPerProperty selfOnly authorOnly mfilter = 
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