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
import Data.Array (filterA, null, sort)
import Data.Array.NonEmpty (toArray)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (new)
import Foreign.Object (empty)
import Perspectives.Actions (compileBotAction)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.ActionCache (retrieveAction)
import Perspectives.CoreTypes (ActionInstance(..), MonadPerspectives, MonadPerspectivesTransaction, (###=), (##>))
import Perspectives.Deltas (distributeTransaction)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (lookupActiveSupportedEffect)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.Extern.Couchdb (addModelToLocalStore')
import Perspectives.Identifiers (hasLocalName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (getMyType)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Persistent (getDomeinFile, tryRemoveEntiteit)
import Perspectives.PerspectivesState (publicRepository)
import Perspectives.Query.UnsafeCompiler (getCalculatedRoleInstances)
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransactie, isEmptyTransaction)
import Perspectives.Types.ObjectGetters (actionsClosure_, isAutomatic, specialisesRoleType_)
import Prelude (Unit, bind, discard, join, not, pure, unit, void, ($), (<$>), (<<<), (<>), (=<<), (>>=))

-----------------------------------------------------------
-- RUN MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall o.
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction authoringRole a = runMonadPerspectivesTransaction' true authoringRole a

runMonadPerspectivesTransaction' :: forall o.
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction' share authoringRole a = getUserIdentifier >>= lift <<< createTransactie authoringRole >>= lift <<< new >>= runReaderT (runArrayT run)
  where
    run :: MonadPerspectivesTransaction o
    run = do
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a
      -- 2. Now run actions, collecting further Deltas in a new Transaction. Locally, side effects are cached and saved to Couchdb already.
      (ft@(Transaction{correlationIdentifiers, contextsToBeRemoved, rolesToBeRemoved}) :: Transaction) <- lift AA.get >>= runActions
      -- 3. Send deltas to other participants, save changed domeinfiles.
      if share then lift $ lift $ distributeTransaction ft else pure unit
      -- Definitively remove instances
      -- log ("Will remove these contexts: " <> show contextsToBeRemoved)
      lift2 $ for_ contextsToBeRemoved tryRemoveEntiteit
      -- log ("Will remove these roles: " <> show rolesToBeRemoved)
      lift2 $ for_ rolesToBeRemoved tryRemoveEntiteit
      -- log "==========RUNNING EFFECTS============"
      -- Sort from low to high, so we can never actualise a client side component after it has been removed.
      lift $ lift $ for_ (sort correlationIdentifiers) \corrId -> do
        me <- pure $ lookupActiveSupportedEffect corrId
        case me of
          Nothing -> pure unit
          (Just {runner}) -> do
            -- logShow corrId
            runner unit
      pure r

-- | Run and discard the transaction.
runSterileTransaction :: forall o. MonadPerspectivesTransaction o -> (MonadPerspectives (Array o))
runSterileTransaction a =
  getUserIdentifier
  >>= lift <<< createTransactie (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
  >>= lift <<< new
  >>= runReaderT (runArrayT a)

-- | Execute every ActionInstance that is triggered by Deltas in the Transaction.
-- | Also execute ActionInstances for created contexts.
-- | We need not trigger actions on a context instance that is deleted.
-- | Repeat this recursively, accumulating Deltas in a single Transaction that is the final result of the process.
runActions :: Transaction -> MonadPerspectivesTransaction Transaction
runActions t = do
  -- Collect all combinations of context instances and user types.
  -- Check if the type of 'me' is among them.
  -- If so, execute the automatic actions for 'me'.
  -- log "==========RUNNING ACTIONS============"
  (as :: Array ActionInstance) <- (lift $ AA.gets (_.affectedContexts <<< unwrap)) >>= traverse getAllAutomaticActions >>= pure <<< join
  lift $ void $ AA.modify cloneEmptyTransaction
  for_ as \(ActionInstance ctxt atype) ->
      case retrieveAction atype of
        (Just (Tuple _ updater)) -> do
          -- log ("Evaluating " <> unwrap atype)
          updater ctxt
        Nothing -> do
          (Tuple _ updater) <- lift2 $ compileBotAction atype
          -- log ("Evaluating " <> unwrap atype)
          updater ctxt
          pure unit
  nt <- lift AA.get
  if isEmptyTransaction nt
    then pure t
    else pure <<< (<>) t =<< runActions nt

getAllAutomaticActions :: AffectedContext -> MonadPerspectivesTransaction (Array ActionInstance)
getAllAutomaticActions (AffectedContext{contextInstances, userTypes}) = join <$> for (toArray contextInstances) \contextInstance -> do
  (mmyType :: Maybe RoleType) <- lift2 (contextInstance ##> getMyType)
  case mmyType of
    Nothing -> pure []
    Just (CR myType) -> if isGuestRole myType
      then do
        mmguest <- lift2 (contextInstance ##> getCalculatedRoleInstances myType)
        case mmguest of
          -- If the Guest role is not filled, don't execute bots on its behalf!
          Nothing -> pure []
          otherwise -> do
            (automaticActions :: Array ActionType) <- lift2 (CR myType ###= filter actionsClosure_ isAutomatic)
            pure $ (ActionInstance contextInstance) <$> automaticActions
      else do
        r <- lift2 $ filterA (\userType -> (CR myType) `specialisesRoleType_` userType) userTypes
        if not $ null r
          then do
            (automaticActions :: Array ActionType) <- lift2 (CR myType ###= filter actionsClosure_ isAutomatic)
            pure $ (ActionInstance contextInstance) <$> automaticActions
          else pure []
    Just (ENR myType) -> do
      r <- lift2 $ filterA (\userType -> (ENR myType) `specialisesRoleType_` userType) userTypes
      if not $ null r
        then do
          (automaticActions :: Array ActionType) <- lift2 (ENR myType ###= filter actionsClosure_ isAutomatic)
          pure $ (ActionInstance contextInstance) <$> automaticActions
        else pure []
  where
    isGuestRole :: CalculatedRoleType -> Boolean
    isGuestRole (CalculatedRoleType cr) = cr `hasLocalName` "Guest"

lift2 :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
lift2 = lift <<< lift

-----------------------------------------------------------
-- LOADMODELIFMISSING
-----------------------------------------------------------
-- | Retrieves from the repository the model, if necessary.
-- TODO. This function relies on a repository URL in PerspectivesState. That is a stub.
loadModelIfMissing :: String -> MonadPerspectivesTransaction Unit
loadModelIfMissing modelName = do
  mDomeinFile <- lift2 $ tryRetrieveDomeinFile modelName
  if isNothing mDomeinFile
    then do
      repositoryUrl <- lift2 publicRepository
      addModelToLocalStore' (repositoryUrl <> modelName)
      -- Now create a binding of the model description in sys:PerspectivesSystem$ModelsInUse.
      DomeinFile{modelDescription} <- lift2 $ getDomeinFile (DomeinFileId modelName)
      mySys <- lift2 $ getMySystem
      case modelDescription of
        Nothing -> pure unit
        Just (PerspectRol{_id}) -> void $ createAndAddRoleInstance
          (EnumeratedRoleType "model:System$PerspectivesSystem$ModelsInUse")
          mySys
          (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just $ unwrap _id})
    else pure unit
