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
import Data.Array (foldM, singleton, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set (toUnfoldable) as SET
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (new)
import Foreign.Object (values)
import Perspectives.Actions (compileBotAction)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Assignment.ActionCache (retrieveAction)
import Perspectives.ContextAndRole (context_me)
import Perspectives.CoreTypes (type (~~>), ActionInstance(..), Assumption, MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Deltas (runTransactie)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DependencyTracking.Dependency (findDependencies, lookupActiveSupportedEffect)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (roleType) as OG
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Representation.Class.PersistentType (getAction, getEnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType)
import Perspectives.Sync.Class.Assumption (assumption)
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransactie, isEmptyTransaction)
import Prelude (bind, discard, join, pure, unit, void, ($), (<$>), (<<<), (<>), (=<<), (>=>), (>>=), (>>>))

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

runSterileTransaction :: forall o. MonadPerspectivesTransaction o -> (MonadPerspectives (Array o))
runSterileTransaction a = (AA.gets _.userInfo.userName) >>= lift <<< createTransactie >>= lift <<< new >>= runReaderT (runArrayT a)

-- | Derive Assumptions from the Deltas in a Transaction. Each Assumption in the result is unique.
assumptionsInTransaction :: Transaction -> Array Assumption
assumptionsInTransaction (Transaction{contextDeltas, roleDeltas, propertyDeltas}) = union (assumption <$> contextDeltas) (union (assumption <$> roleDeltas) (assumption <$> propertyDeltas))

-- | Execute every ActionInstance that is triggered by Deltas in the Transaction.
-- | Also execute ActionInstances for created contexts.
-- | We need not trigger actions on a context instance that is deleted.
-- | Repeat this recursively, accumulating Deltas in a single Transaction that is the final result of the process.
runActions :: Transaction -> MonadPerspectivesTransaction Transaction
runActions t = do
  -- action instances deriving from Deltas.
  (as :: Array ActionInstance) <- contextsAffectedByTransaction >>= traverse getAllAutomaticActions >>= pure <<< join
  lift $ void $ AA.modify cloneEmptyTransaction
  for_ as \(ActionInstance ctxt atype) ->
      case retrieveAction atype of
        (Just (Tuple _ a)) -> a ctxt
        Nothing -> do
          (Tuple _ updater) <- lift2 $ compileBotAction atype
          updater ctxt
          pure unit
  nt <- lift AA.get
  if isEmptyTransaction nt
    then pure t
    else pure <<< (<>) t =<< runActions nt

-- | Get all ContextInstances collected in the Transaction.
contextsAffectedByTransaction :: MonadPerspectivesTransaction (Array ContextInstance)
contextsAffectedByTransaction = lift AA.get >>= pure <<< SET.toUnfoldable <<< _.affectedContexts <<< unwrap

getMe :: ContextInstance ~~> RoleInstance
getMe ctxt = ArrayT (lift $ getPerspectContext ctxt >>= pure <<< maybe [] singleton <<< context_me)

allActions :: EnumeratedRoleType ~~> ActionType
allActions rt = ArrayT (lift $ getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> values >>> join >>> pure)

isAutomatic :: ActionType ~~> Boolean
isAutomatic at = ArrayT (lift $ getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)

getAllAutomaticActions :: ContextInstance -> MonadPerspectivesTransaction (Array ActionInstance)
getAllAutomaticActions affectedContext = do
  -- find all automatic action types of the me role.
  (automaticActions :: Array ActionType) <- lift2 (affectedContext ##= filter (getMe >=> OG.roleType >=> allActions) isAutomatic)
  pure ((ActionInstance affectedContext) <$> automaticActions)

lift2 :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
lift2 = lift <<< lift
