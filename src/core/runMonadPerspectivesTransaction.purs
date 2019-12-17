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

module Perspectives.RunMonadPerspectivesTransaction where

import Control.Monad.AvarMonadAsk (get, gets, modify) as AA
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Array (catMaybes, foldM, singleton, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Semigroup)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (new)
import Foreign.Object (values)
import Perspectives.Actions (compileBotAction)
import Perspectives.AffectedContextCalculation (AffectedContextCalculation(..))
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Assignment.ActionCache (retrieveAction)
import Perspectives.Assignment.DependencyTracking (actionInstancesForContextInstance, rulesForContextInstance)
import Perspectives.ContextAndRole (context_id, context_me)
import Perspectives.CoreTypes (ActionInstance(..), MonadPerspectives, MonadPerspectivesTransaction, Assumption, (##=), type (~~>))
import Perspectives.Deltas (runTransactie)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DependencyTracking.Dependency (findDependencies, lookupActiveSupportedEffect)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (roleType) as OG
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Query.Compiler (role2context)
import Perspectives.Representation.Class.PersistentType (getAction, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType)
import Perspectives.Sync.Class.Assumption (assumption)
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransactie, isEmptyTransaction)
import Perspectives.TypesForDeltas (ContextDelta(..), PropertyDelta(..), RoleDelta(..))
import Prelude (class Monoid, Unit, bind, discard, join, pure, unit, void, ($), (<$>), (<<<), (<>), (=<<), (>>=), (>>>), (>=>))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- RUN MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall o.
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction a = runMonadPerspectivesTransaction' true a

runMonadPerspectivesTransaction' :: forall o.
  Boolean ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction' share a = (AA.gets _.userInfo.userName) >>= lift <<< createTransactie >>= lift <<< new >>= runReaderT (runArrayT run)
  where
    run :: MonadPerspectivesTransaction o
    run = do
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a
      -- 2. Now run actions, collecting further Deltas in a new Transaction. Locally, side effects are cached and saved to Couchdb already.
      (ft :: Transaction) <- lift AA.get >>= runActions
      -- 3. Send deltas to other participants, save changed domeinfiles.
      if share then runTransactie ft else pure unit
      -- 4. Finally re-run the active queries. Derive changed assumptions from the Transaction and use the dependency
      -- administration to find the queries that should be re-run.
      (corrIds :: Array CorrelationIdentifier) <- lift $ lift $ foldM (\bottom ass -> do
        mcorrIds <- findDependencies ass
        case mcorrIds of
          Nothing -> pure bottom
          (Just ids) -> pure (union bottom ids))
        []
        (assumptionsInTransaction ft)
      lift $ lift $ for_ corrIds \corrId -> do
        me <- pure $ lookupActiveSupportedEffect corrId
        case me of
          Nothing -> pure unit
          (Just {runner}) -> runner unit
      pure r

-- | Derive Assumptions from the Deltas in a Transaction. Each Assumption in the result is unique.
assumptionsInTransaction :: Transaction -> Array Assumption
assumptionsInTransaction (Transaction{contextDeltas, roleDeltas, propertyDeltas}) = union (assumption <$> contextDeltas) (union (assumption <$> roleDeltas) (assumption <$> propertyDeltas))

-- | Execute every ActionInstance that is triggered by Deltas in the Transaction.
-- | Also execute ActionInstances for created contexts.
-- | We need not trigger actions on a context instance that is deleted.
-- | Repeat this recursively, accumulating Deltas in a single Transaction that is the final result of the process.
runActions :: Transaction -> MonadPerspectivesTransaction Transaction
runActions t@(Transaction{createdContexts}) = do
  -- action instances deriving from Deltas.
  -- TODO. Pas hier de AffectedContextQueries toe.
  (AISet as1) <- lift $ lift $ execWriterT $ actionsTriggeredByDeltas t
  -- (as1 :: Array ActionInstance) <- lift $ lift $ actionInstancesDependingOn $ assumptionsInTransaction t
  -- action instances deriving from created contexts.
  as2 <- traverse (lift <<< lift <<< actionInstancesForContextInstance <<< context_id) createdContexts >>= pure <<< join <<< catMaybes
  lift $ void $ AA.modify cloneEmptyTransaction
  for_ (union as1 as2) \(ActionInstance ctxt atype) ->
      case retrieveAction atype of
        (Just (Tuple _ a)) -> a ctxt
        Nothing -> do
          (Tuple _ updater) <- lift $ lift $ compileBotAction atype
          updater ctxt
          pure unit
  nt <- lift AA.get
  if isEmptyTransaction nt
    then pure t
    else pure <<< (<>) t =<< runActions nt

-- | Run the actions that have been set up before, for a context instance.
runRulesForContextInstance :: ContextInstance -> MonadPerspectivesTransaction Unit
runRulesForContextInstance cid = do
  rules <- lift $ lift $ rulesForContextInstance cid
  for_ rules \rule -> rule cid

-- | For each Delta, find the associated AffectedContextQueries. Run them, collecting Context Instances.
-- | For each Context Instance, find the perspective of the me role. For now, take all the Action types.
-- | Filter these, collecting the automatic Actions. Create ActionInstances.
-- | Use WriterT and a Monoid Newtype based on Array with union as append, to collect ActionInstances.
actionsTriggeredByDeltas :: Transaction -> CollectAIs
actionsTriggeredByDeltas (Transaction{contextDeltas, roleDeltas, propertyDeltas}) = do
  for_ contextDeltas aisInContextDelta
  for_ roleDeltas aisInRoleDelta
  for_ propertyDeltas aisInPropertyDelta

-- | From the ContextDelta, collect ActionInstances.
aisInContextDelta :: ContextDelta -> CollectAIs
aisInContextDelta (ContextDelta{id, roleType, roleInstance}) = case roleInstance of
  Just ri -> do
    (EnumeratedRole{onContextDelta_context, onContextDelta_role}) <- lift $ getEnumeratedRole roleType
    for_ onContextDelta_context \(AffectedContextCalculation{description, compilation}) -> do
      -- Get the function that computes the affected contexts from the type of the roleInstance.
      (affectedContextGetter :: RoleInstance ~~> ContextInstance) <- case compilation of
        Just c -> pure $ unsafeCoerce c
        Nothing -> lift $ role2context description
      -- Find all automatic actions from the role instance of the Delta, using the affectedContextGetter.
      affectedContexts <- lift (ri ##= affectedContextGetter)
      for_ affectedContexts \affectedContext -> do
        (automaticActions :: Array ActionType) <- lift (affectedContext ##= filter (getMe >=> OG.roleType >=> allActions) isAutomatic)
        -- Combine these action types with the affected context to form an ActionInstance.
        tell $ AISet ((ActionInstance affectedContext) <$> automaticActions)
  Nothing -> pure unit

  where
    getMe :: ContextInstance ~~> RoleInstance
    getMe ctxt = ArrayT (lift $ getPerspectContext ctxt >>= pure <<< maybe [] singleton <<< context_me)

    allActions :: EnumeratedRoleType ~~> ActionType
    allActions rt = ArrayT (lift $ getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> values >>> join >>> pure)

    isAutomatic :: ActionType ~~> Boolean
    isAutomatic at = ArrayT (lift $ getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)


aisInRoleDelta :: RoleDelta -> CollectAIs
aisInRoleDelta (RoleDelta{id, binding})= pure unit

aisInPropertyDelta :: PropertyDelta -> CollectAIs
aisInPropertyDelta (PropertyDelta{id})= pure unit

type CollectAIs = WriterT AISet MonadPerspectives Unit

newtype AISet = AISet (Array ActionInstance)

instance semigroupAISet :: Semigroup AISet where
  append (AISet a1) (AISet a2) = AISet $ union a1 a2

instance monoidAISet :: Monoid AISet where
  mempty = AISet []
