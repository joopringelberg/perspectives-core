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

module Perspectives.RunMonadPerspectivesTransaction where

import Control.Monad.AvarMonadAsk (get, modify) as AA
import Control.Monad.Error.Class (catchError, try)
import Control.Monad.Reader (lift, runReaderT)
import Data.Array (head, null, sort)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Effect.Aff.AVar (new)
import Foreign.Object (empty)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextStateCompiler (enteringState, evaluateContextState, exitingState)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, StateEvaluation(..), liftToInstanceLevel, (##>), (##>>), (##=))
import Perspectives.Deltas (distributeTransaction)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (lookupActiveSupportedEffect)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (addModelToLocalStore')
import Perspectives.Identifiers (hasLocalName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (context, contextType, getMyType, roleType)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile, tryRemoveEntiteit)
import Perspectives.PerspectivesState (addBinding, publicRepository, pushFrame, restoreFrame)
import Perspectives.Query.UnsafeCompiler (getCalculatedRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleStateCompiler (enteringRoleState, evaluateRoleState, exitingRoleState)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransaction, isEmptyTransaction)
import Perspectives.Types.ObjectGetters (roleRootStates, contextRootStates)
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (<$>), (<<<), (<>), (=<<), (>=>), (>>=))

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
runMonadPerspectivesTransaction' share authoringRole a = getUserIdentifier >>= lift <<< createTransaction authoringRole >>= lift <<< new >>= runReaderT (runArrayT run)
  where
    run :: MonadPerspectivesTransaction o
    run = do
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a
      -- 2. Now run states, collecting further Deltas in a new Transaction. Locally, side effects are cached and saved to Couchdb already.
      (ft@(Transaction{correlationIdentifiers, contextsToBeRemoved, rolesToBeRemoved, modelsToBeRemoved}) :: Transaction) <- lift AA.get >>= runStates
      -- 3. Send deltas to other participants, save changed domeinfiles.
      if share then lift $ lift $ distributeTransaction ft else pure unit
      -- Definitively remove instances
      -- log ("Will remove these contexts: " <> show contextsToBeRemoved)
      -- TODO. Moeten hier niet de onExits worden uitgevoerd?
      lift2 $ for_ contextsToBeRemoved tryRemoveEntiteit
      -- log ("Will remove these roles: " <> show rolesToBeRemoved)
      lift2 $ for_ rolesToBeRemoved tryRemoveEntiteit
      -- log ("Will remove these models: " <> show modelsToBeRemoved)
      lift2 $ for_ modelsToBeRemoved tryRemoveEntiteit
      -- log "==========RUNNING EFFECTS============"
      -- Sort from low to high, so we can never actualise a client side component after it has been removed.
      lift $ lift $ for_ (sort correlationIdentifiers) \corrId -> do
        mEffect <- pure $ lookupActiveSupportedEffect corrId
        case mEffect of
          Nothing -> pure unit
          (Just {runner}) -> do
            -- logShow corrId
            runner unit
      pure r

-- | Run and discard the transaction.
runSterileTransaction :: forall o. MonadPerspectivesTransaction o -> (MonadPerspectives (Array o))
runSterileTransaction a =
  getUserIdentifier
  >>= lift <<< createTransaction (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
  >>= lift <<< new
  >>= runReaderT (runArrayT a)

runStates :: Transaction -> MonadPerspectivesTransaction Transaction
runStates t = do
  -- log "==========RUNNING STATES============"
  Transaction{invertedQueryResults, createdContexts, createdRoles, contextsToBeRemoved, rolesToBeRemoved} <- lift $ AA.get
  (stateEvaluations :: Array StateEvaluation) <- join <$> traverse computeStateEvaluations invertedQueryResults
  -- Only now install a fresh transaction.
  lift $ void $ AA.modify cloneEmptyTransaction
  -- Evaluate all collected stateEvaluations.
  for_ stateEvaluations \s -> case s of
    ContextStateEvaluation stateId contextId roleType -> do
      -- Provide a new frame for the current context variable binding.
      oldFrame <- lift2 pushFrame
      lift2 $ addBinding "currentcontext" [unwrap contextId]
      -- Error boundary.
      catchError (evaluateContextState contextId roleType stateId)
        \e -> logPerspectivesError $ Custom ("Cannot evaluate context state, because " <> show e)
      lift2 $ restoreFrame oldFrame
    RoleStateEvaluation stateId roleId roleType -> do
      cid <- lift2 (roleId ##>> context)
      oldFrame <- lift2 pushFrame
      lift2 $ addBinding "currentcontext" [unwrap cid]
      catchError (evaluateRoleState roleId roleType stateId)
        \e -> logPerspectivesError $ Custom ("Cannot evaluate role state, because " <> show e)
      lift2 $ restoreFrame oldFrame
  -- Enter the rootState of new contexts.
  for_ createdContexts
    \ctxt -> do
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
          if null states
            then pure unit
            else do
              -- Provide a new frame for the current context variable binding.
              oldFrame <- lift2 pushFrame
              lift2 $ addBinding "currentcontext" [unwrap ctxt]
              -- Error boundary.
              catchError (for_ states (enteringState ctxt myType))
                \e -> logPerspectivesError $ Custom ("Cannot enter state, because " <> show e)
              lift2 $ restoreFrame oldFrame
  -- Exit the rootState of contexts that are deleted.
  for_ contextsToBeRemoved
    \ctxt -> do
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
          if null states
            then pure unit
            else do
              -- Provide a new frame for the current context variable binding.
              oldFrame <- lift2 pushFrame
              lift2 $ addBinding "currentcontext" [unwrap ctxt]
              -- Error boundary.
              catchError (for_ states (exitingState ctxt myType))
                \e -> logPerspectivesError $ Custom ("Cannot exit state, because " <> show e)
              lift2 $ restoreFrame oldFrame
  -- Enter the rootState of roles that are created.
  for_ createdRoles
    \rid -> do
      ctxt <- lift2 (rid ##>> context)
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
          if null states
            then pure unit
            else do
              oldFrame <- lift2 pushFrame
              lift2 $ addBinding "currentcontext" [unwrap ctxt]
              -- Error boundary.
              catchError (for_ states (enteringRoleState rid myType))
                \e -> logPerspectivesError $ Custom ("Cannot enter role state, because " <> show e)
              lift2 $ restoreFrame oldFrame
  -- Exit the rootState of roles that are removed.
  for_ rolesToBeRemoved
    \rid -> do
      ctxt <- lift2 (rid ##>> context)
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
          if null states
            then pure unit
            else do
              oldFrame <- lift2 pushFrame
              lift2 $ addBinding "currentcontext" [unwrap ctxt]
              -- Error boundary.
              catchError (for_ states (exitingRoleState rid myType))
                \e -> logPerspectivesError $ Custom ("Cannot enter role state, because " <> show e)
              lift2 $ restoreFrame oldFrame
  nt <- lift AA.get
  if isEmptyTransaction nt
    then pure t
    else pure <<< (<>) t =<< runStates nt
  where
    -- Add to each context or role instance the user role type and the RootState type.
    computeStateEvaluations :: InvertedQueryResult -> MonadPerspectivesTransaction (Array StateEvaluation)
    computeStateEvaluations (ContextStateQuery contextInstances) = join <$> do
      case head contextInstances of
        Nothing -> pure []
        Just contextInstance -> do
          -- Neem aspecten hier ook mee!
          states <- lift2 (contextInstance ##= contextType >=> liftToInstanceLevel contextRootStates)
          for contextInstances \cid -> do
            -- Note that the user may play different roles in the various context instances.
            (mmyType :: Maybe RoleType) <- lift2 (cid ##> getMyType)
            case mmyType of
              Nothing -> pure []
              Just (CR myType) -> if isGuestRole myType
                then do
                  (mmguest :: Maybe RoleInstance) <- lift2 (cid ##> getCalculatedRoleInstances myType)
                  case mmguest of
                    -- If the Guest role is not filled, don't execute bots on its behalf!
                    Nothing -> pure []
                    otherwise -> pure $ (\state -> ContextStateEvaluation state cid (CR myType)) <$> states
                else pure $ (\state -> ContextStateEvaluation state cid (CR myType)) <$> states
              Just (ENR myType) -> pure $ (\state -> ContextStateEvaluation state cid (ENR myType)) <$> states
    computeStateEvaluations (RoleStateQuery roleInstances) = join <$> do
      case head roleInstances of
        Nothing -> pure []
        Just roleInstance -> do
          -- Neem aspecten hier ook mee!
          states <- lift2 (roleInstance ##= roleType >=> liftToInstanceLevel roleRootStates)
          for roleInstances \rid -> do
            (mmyType :: Maybe RoleType) <- lift2 (rid ##> context >=> getMyType)
            case mmyType of
              Nothing -> pure []
              Just (CR myType) -> if isGuestRole myType
                then do
                  (mmguest :: Maybe RoleInstance) <- lift2 (rid ##> context >=> getCalculatedRoleInstances myType)
                  case mmguest of
                    Nothing -> pure []
                    otherwise -> pure $ (\state -> RoleStateEvaluation state rid (CR myType)) <$> states
                else pure $ (\state -> RoleStateEvaluation state rid (CR myType)) <$> states
              Just (ENR myType) -> pure $ (\state -> RoleStateEvaluation state rid (ENR myType)) <$> states

    isGuestRole :: CalculatedRoleType -> Boolean
    isGuestRole (CalculatedRoleType cr) = cr `hasLocalName` "Guest"

-- | Execute every ActionInstance that is triggered by Deltas in the Transaction.
-- | Also execute ActionInstances for created contexts.
-- | We need not trigger actions on a context instance that is deleted.
-- | Repeat this recursively, accumulating Deltas in a single Transaction that is the final result of the process.
-- runActions :: Transaction -> MonadPerspectivesTransaction Transaction
-- runActions t = do
--   -- Collect all combinations of context instances and user types.
--   -- Check if the type of 'me' is among them.
--   -- If so, execute the automatic actions for 'me'.
--   -- log "==========RUNNING ACTIONS============"
--   (as :: Array ActionInstance) <- (lift $ AA.gets (_.invertedQueryResults <<< unwrap)) >>= traverse getAllAutomaticActions >>= pure <<< join
--   -- Collect all contexts that are created
--   (ccs :: Array ContextInstance) <- lift $ AA.gets (_.createdContexts <<< unwrap)
--   -- Only now install a fresh transaction.
--   lift $ void $ AA.modify cloneEmptyTransaction
--   -- Run the actions on all combinations of an actiontype and context instance that were in the original transaction.
--   for_ as \(ActionInstance ctxt atype) -> run ctxt atype
--   -- Run all the automatic actions defined for the Me in each new context.
--   for_ ccs
--     \ctxt -> do
--       (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
--       case mmyType of
--         Nothing -> pure unit
--         Just myType -> do
--           (automaticActions :: Array ActionType) <- lift2 (myType ###= filter actionsClosure_ isAutomatic)
--           for_ automaticActions (run ctxt)
--
--   nt <- lift AA.get
--   if isEmptyTransaction nt
--     then pure t
--     else pure <<< (<>) t =<< runActions nt
--
--   where
--
--     run :: ContextInstance -> ActionType -> MonadPerspectivesTransaction Unit
--     run ctxt atype = case retrieveAction atype of
--       (Just updater) -> do
--         -- log ("Evaluating " <> unwrap atype)
--         updater ctxt
--       Nothing -> (try $ lift2 $ compileBotAction atype) >>= case _ of
--         Left e -> logPerspectivesError $ Custom ("Cannot compile rule, because " <> show e)
--         Right updater -> updater ctxt

-- REFACTOR door eerst states te berekenen. De deltas (toegevoegde en verwijderde states) gebruik je om
-- in Context- en Roltypen de automatische acties bij entry en exit op te zoeken.
-- Bovendien bepaal je aan de hand van die deltas wat de notifications zijn en of de current user genotificeerd moet worden.
-- getAllAutomaticActions :: AffectedContext -> MonadPerspectivesTransaction (Array ActionInstance)
-- getAllAutomaticActions (AffectedContext{contextInstances, userTypes}) = join <$> for (toArray contextInstances) \contextInstance -> do
--   (mmyType :: Maybe RoleType) <- lift2 (contextInstance ##> getMyType)
--   case mmyType of
--     Nothing -> pure []
--     Just (CR myType) -> if isGuestRole myType
--       then do
--         (mmguest :: RoleInstance) <- lift2 (contextInstance ##> getCalculatedRoleInstances myType)
--         case mmguest of
--           -- If the Guest role is not filled, don't execute bots on its behalf!
--           Nothing -> pure []
--           otherwise -> do
--             -- Pas de state runner toe op de root state (van het type), de context instance, en de guest user.
--             (automaticActions :: Array ActionType) <- lift2 (CR myType ###= filter actionsClosure_ isAutomatic)
--             pure $ (ActionInstance contextInstance) <$> automaticActions
--       else do
--         r <- lift2 $ filterA (\userType -> (CR myType) `specialisesRoleType_` userType) userTypes
--         if not $ null r
--           then do
--             (automaticActions :: Array ActionType) <- lift2 (CR myType ###= filter actionsClosure_ isAutomatic)
--             pure $ (ActionInstance contextInstance) <$> automaticActions
--           else pure []
--     Just (ENR myType) -> do
--       r <- lift2 $ filterA (\userType -> (ENR myType) `specialisesRoleType_` userType) userTypes
--       if not $ null r
--         then do
--           (automaticActions :: Array ActionType) <- lift2 (ENR myType ###= filter actionsClosure_ isAutomatic)
--           pure $ (ActionInstance contextInstance) <$> automaticActions
--         else pure []
--   where
--     isGuestRole :: CalculatedRoleType -> Boolean
--     isGuestRole (CalculatedRoleType cr) = cr `hasLocalName` "Guest"

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
      (lift2 $ try $ getDomeinFile (DomeinFileId modelName)) >>=
        handleDomeinFileError "loadModelIfMissing"
        \(DomeinFile{modelDescription}) -> do
          mySys <- lift2 $ getMySystem
          case modelDescription of
            Nothing -> pure unit
            Just (PerspectRol{_id}) -> void $ createAndAddRoleInstance
              (EnumeratedRoleType "model:System$PerspectivesSystem$ModelsInUse")
              mySys
              (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just $ unwrap _id})
    else pure unit
