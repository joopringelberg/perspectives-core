module Perspectives.RunMonadPerspectivesTransaction where

import Control.Monad.AvarMonadAsk (get, gets, modify) as AA
import Control.Monad.Reader (lift, runReaderT)
import Data.Array (foldM, null, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff.AVar (new)
import Effect.Class (liftEffect)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Assignment.ActionCache (retrieveAction)
import Perspectives.Assignment.DependencyTracking (ActionInstance(..), actionInstancesDependingOn)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, Assumption)
import Perspectives.Deltas (runTransactie)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (findDependencies, lookupActiveSupportedEffect)
import Perspectives.Sync.Class.Assumption (assumption)
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransactie)
import Prelude (bind, discard, pure, unit, void, ($), (<$>), (<<<), (>>=), (<>), (=<<))

-----------------------------------------------------------
-- RUN MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall o.
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction a = (AA.gets _.userInfo.userName) >>= lift <<< createTransactie >>= lift <<< new >>= runReaderT (runArrayT run)
  where
    run :: MonadPerspectivesTransaction o
    run = do
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a
      -- 2. Now run actions, collecting further Deltas in a new Transaction.
      (ft :: Transaction) <- lift AA.get >>= runActions
      -- 3. Send deltas to other participants, save changed domeinfiles, etc.
      runTransactie ft
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
        me <- liftEffect $ lookupActiveSupportedEffect corrId
        case me of
          Nothing -> pure unit
          (Just {runner}) -> runner unit
      pure r

-- | Derive Assumptions from the Deltas in a Transaction. Each Assumption in the result is unique.
assumptionsInTransaction :: Transaction -> Array Assumption
assumptionsInTransaction (Transaction{roleDeltas, bindingDeltas, propertyDeltas}) = union (assumption <$> roleDeltas) (union (assumption <$> bindingDeltas) (assumption <$> propertyDeltas))

-- | Execute every ActionInstance that is triggered by changes in the Transaction.
-- | Repeat this recursively, accumulating Deltas in a single Transaction that is the final result of the process.
runActions :: Transaction -> MonadPerspectivesTransaction Transaction
runActions t = do
  (as :: Array ActionInstance) <- lift $ liftEffect $ actionInstancesDependingOn $ assumptionsInTransaction t
  if (null as)
    then pure t
    else do
      lift $ void $ AA.modify cloneEmptyTransaction
      for_ as \(ActionInstance ctxt atype) ->
        (lift $ liftEffect $ retrieveAction atype) >>= \ma ->
          case ma of
            Nothing -> pure unit
            (Just a) -> a ctxt
      nt <- lift AA.get
      pure <<< (<>) t =<< runActions nt
