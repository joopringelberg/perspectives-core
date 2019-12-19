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
import Data.Array (elemIndex, foldM, singleton, union)
import Data.Foldable (for_)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Monoid (class Semigroup)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (new)
import Foreign.Object (values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Actions (compileBotAction)
import Perspectives.AffectedContextCalculation (AffectedContextCalculation(..))
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Assignment.ActionCache (retrieveAction)
import Perspectives.Assignment.DependencyTracking (rulesForContextInstance)
import Perspectives.ContextAndRole (context_id, context_me)
import Perspectives.CoreTypes (type (~~>), ActionInstance(..), Assumption, MonadPerspectives, MP, MonadPerspectivesTransaction, (##=))
import Perspectives.Deltas (runTransactie)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DependencyTracking.Dependency (findDependencies, lookupActiveSupportedEffect)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (roleType) as OG
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Query.Compiler (getHiddenFunction)
import Perspectives.Representation.Class.PersistentType (getAction, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRoleRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType(..))
import Perspectives.Sync.Class.Assumption (assumption)
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransactie, isEmptyTransaction)
import Perspectives.TypesForDeltas (ContextDelta(..), PropertyDelta(..), RoleDelta(..))
import Prelude (class Monoid, Unit, bind, const, discard, join, pure, unit, void, ($), (<$>), (<<<), (<>), (=<<), (>=>), (>>=), (>>>))
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
runActions t = do
  -- action instances deriving from Deltas.
  (AISet as) <- lift $ lift $ execWriterT $ actionsTriggeredByTransaction t
  lift $ void $ AA.modify cloneEmptyTransaction
  for_ as \(ActionInstance ctxt atype) ->
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
actionsTriggeredByTransaction :: Transaction -> CollectAIs
actionsTriggeredByTransaction (Transaction{contextDeltas, roleDeltas, propertyDeltas, createdContexts}) = do
  for_ createdContexts (addAllAutomaticActions <<< context_id)
  for_ contextDeltas aisInContextDelta
  for_ roleDeltas aisInRoleDelta
  for_ propertyDeltas aisInPropertyDelta

-- | From the ContextDelta, collect ActionInstances. Compile all queries that are used to collect
-- | affected context instances, first.
aisInContextDelta :: ContextDelta -> CollectAIs
aisInContextDelta (ContextDelta{id, roleType, roleInstance}) = do
  contextCalculations <- lift $ compileDescriptions _onContextDelta_context roleType
  roleCalculations <- lift $ compileDescriptions _onContextDelta_role roleType
  case roleInstance of
    Just ri -> do
      for_ contextCalculations \(AffectedContextCalculation{compilation, action}) -> do
        -- Find all affected contexts, starting from the role instance of the Delta.
        affectedContexts <- lift (ri ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
        -- For each affected context,
        for_ affectedContexts (addAutomaticActions action)
    Nothing -> pure unit
  for_ roleCalculations \(AffectedContextCalculation{compilation, action}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: ContextInstance ~~> ContextInstance)
    -- For each affected context,
    for_ affectedContexts (addAutomaticActions action)

-- | Changes the model in cache (but not in Couchdb). For a given lens that retrieves one of onRoleDelta_binder,
-- | onRoleDelta_binding, onContextDelta_role or onContextDelta_context, and an EnumeratedRoleType, compile the
-- | description in the AffectedContextCalculations.
compileDescriptions :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array AffectedContextCalculation)
compileDescriptions onX rt@(EnumeratedRoleType ert) =  do
  modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
  (df :: DomeinFile) <- retrieveDomeinFile modelName
  -- Get the AffectedContextCalculations in onContextDelta_context.
  (calculations :: Array AffectedContextCalculation) <- pure $ unsafePartial $ fromJust $ preview (onDelta rt) df
  -- Compile the descriptions.
  compiledCalculations <- traverse compile calculations
  -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
  modifyDomeinFileInCache (over (onDelta rt) (const compiledCalculations)) modelName
  pure compiledCalculations

  where
    onDelta :: EnumeratedRoleType -> Traversal' DomeinFile (Array AffectedContextCalculation)
    onDelta (EnumeratedRoleType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedRoles") <<< at x <<< traversed <<< _Newtype <<< onX

    compile :: AffectedContextCalculation -> MP AffectedContextCalculation
    compile ac@(AffectedContextCalculation{description, compilation, action}) = case compilation of
      Just c -> pure ac
      Nothing -> do
        c <- getHiddenFunction description
        pure $ AffectedContextCalculation{description, compilation: Just (unsafeCoerce c), action}

_onContextDelta_context :: CalculationsLens
_onContextDelta_context = prop (SProxy :: SProxy "onContextDelta_context")

_onContextDelta_role :: CalculationsLens
_onContextDelta_role = prop (SProxy :: SProxy "onContextDelta_role")

type CalculationsLens = Lens' EnumeratedRoleRecord (Array AffectedContextCalculation)

aisInRoleDelta :: RoleDelta -> CollectAIs
aisInRoleDelta (RoleDelta{id, binding}) = pure unit
  -- do
  -- (EnumeratedRole{onRoleDelta_binding}) <- lift (id ##>> roleType >=> getEnumeratedRole)
  -- for_ onRoleDelta_binding \(AffectedContextCalculation{description, compilation}) -> do

aisInPropertyDelta :: PropertyDelta -> CollectAIs
aisInPropertyDelta (PropertyDelta{id})= pure unit

getMe :: ContextInstance ~~> RoleInstance
getMe ctxt = ArrayT (lift $ getPerspectContext ctxt >>= pure <<< maybe [] singleton <<< context_me)

allActions :: EnumeratedRoleType ~~> ActionType
allActions rt = ArrayT (lift $ getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> values >>> join >>> pure)

isAutomatic :: ActionType ~~> Boolean
isAutomatic at = ArrayT (lift $ getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)

addAutomaticActions :: ActionType -> ContextInstance -> CollectAIs
addAutomaticActions action affectedContext = do
  -- if the action belongs to 'me', add an ActionInstance constructed from action and the affected context.

  -- find all automatic action types of the me role.
  (automaticActions :: Array ActionType) <- lift (affectedContext ##= filter (getMe >=> OG.roleType >=> allActions) isAutomatic)
  if (isJust $ elemIndex action automaticActions)
    then tell $ AISet [ActionInstance affectedContext action]
    else pure unit

addAllAutomaticActions :: ContextInstance -> CollectAIs
addAllAutomaticActions affectedContext = do
  -- find all automatic action types of the me role.
  (automaticActions :: Array ActionType) <- lift (affectedContext ##= filter (getMe >=> OG.roleType >=> allActions) isAutomatic)
  tell $ AISet ((ActionInstance affectedContext) <$> automaticActions)

type CollectAIs = WriterT AISet MonadPerspectives Unit

newtype AISet = AISet (Array ActionInstance)

instance semigroupAISet :: Semigroup AISet where
  append (AISet a1) (AISet a2) = AISet $ union a1 a2

instance monoidAISet :: Monoid AISet where
  mempty = AISet []
