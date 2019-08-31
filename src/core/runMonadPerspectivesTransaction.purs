module Perspectives.RunMonadPerspectivesTransaction where

import Control.Monad.Reader (lift, runReaderT)
import Effect.Aff.AVar (new)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives)
import Perspectives.Deltas (runTransactie)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Sync.Transactie (createTransactie)
import Prelude (Unit, ($), (<<<), (>>=), void, discard)
-----------------------------------------------------------
-- RUN IN A TRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall s o.
  s
  -> (s -> MonadPerspectivesTransaction o)
  -> (MonadPerspectives (Array Unit))
runMonadPerspectivesTransaction a f = lift (createTransactie "") >>= lift <<< new >>= runReaderT (runArrayT run)
  where
    run :: MonadPerspectivesTransaction Unit
    run = do
      -- 1. Execute the update functions: this will accumulate deltas in the Transaction.
      void $ f a
      -- 2. Now run actions.
      -- Derive changed assumptions from the Transaction and use the dependency administration set up for the LHS
      -- of actions to find the actions that should be run.
      -- The LHS of the actions will be run in MonadPerspectivesQuery and we will update the dependency administration
      -- with the results. The RHS's may cause more deltas to be added to the Transaction.
      -- When all actions are done, retrieve the Transaction.

      -- 3. Send deltas to other participants
      runTransactie
      -- 4. Finally re-run the active queries. Derive changed assumptions from the Transaction and use the dependency
      -- administration to find the queries that should be re-run.
