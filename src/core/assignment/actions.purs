module Perspectives.Actions where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldMap, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.ActionCache (cacheAction, retrieveAction)
import Perspectives.Assignment.DependencyTracking (ActionInstance(..), cacheActionInstanceDependencies, removeContextInstanceDependencies)
import Perspectives.Assignment.Update (PropertyUpdater, RoleUpdater, addProperty, addRol, removeProperty, removeRol, setProperty, setRol)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, RoleGetter, Updater, WithAssumptions, ContextPropertyValueGetter, runMonadPerspectivesQuery, (##>>))
import Perspectives.Instances.ObjectGetters (contextType)
import Perspectives.Query.Compiler (context2propertyValue, context2role)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Action (condition, effect, object)
import Perspectives.Representation.Assignment (AssignmentStatement(..))
import Perspectives.Representation.Class.PersistentType (ActionType, getPerspectType)
import Perspectives.Representation.Class.Role (Role, getCalculation, getRole)
import Perspectives.Representation.Context (Context, actions)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)

-----------------------------------------------------------
-- CONSTRUCTACTIONFUNCTION
-----------------------------------------------------------
type RHS = WithAssumptions Value -> MonadPerspectivesTransaction Unit

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect for a Context, conditional on a set of given boolean values.
constructRHS :: AssignmentStatement -> RoleGetter -> ActionType -> MonadPerspectives (ContextInstance -> RHS)
constructRHS a objectGetter actionType = case a of
  (SetRol rt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: RoleGetter) <- context2role query
    pure $ f rt valueComputer setRol
  (AddToRol rt (query :: QueryFunctionDescription)) -> do
    valueComputer <- context2role query
    pure $ f rt valueComputer addRol
  (RemoveFromRol rt (query :: QueryFunctionDescription)) -> do
    valueComputer <- context2role query
    pure $ f rt valueComputer removeRol
  (SetProperty pt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: ContextPropertyValueGetter) <- context2propertyValue query
    pure $ g pt valueComputer setProperty
  (AddToProperty pt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: ContextPropertyValueGetter) <- context2propertyValue query
    pure $ g pt valueComputer addProperty
  (RemoveFromProperty pt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: ContextPropertyValueGetter) <- context2propertyValue query
    pure $ g pt valueComputer removeProperty
  (EffectFullFunction fun args) -> case fun of
    -- TODO: vervang dit zodra de parser weer werkt.
    "storeDomeinFile" -> pure \contextInstance bools -> pure unit
    otherwise -> throwError (error ("Unknown EffectFullFunction in constructRHS: " <> show a))

  where
    f :: EnumeratedRoleType -> RoleGetter -> RoleUpdater -> (ContextInstance -> RHS)
    f rt roleGetter roleUpdater contextId (Tuple bools a0 :: WithAssumptions Value) = if (alaF Conj foldMap (eq (Value "true")) bools)
        then do
          (Tuple value a1 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId roleGetter
          -- The Object of the Action (where the bot is the Subject).
          -- We compute it here just for the assumptions.
          (Tuple object a2 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId objectGetter
          -- Cache the association between the assumptions found for this ActionInstance.
          pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) (union a0 (union a1 a2))
          void $ pure $ roleUpdater contextId rt value
        else pure unit

    g :: EnumeratedPropertyType -> ContextPropertyValueGetter -> PropertyUpdater -> (ContextInstance -> RHS)
    g rt valueGetter propertyUpdater contextId (Tuple bools a0 :: WithAssumptions Value) = if (alaF Conj foldMap (eq (Value "true")) bools)
        then do
          (Tuple value a1 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery contextId valueGetter
          -- The Object of the Action (where the bot is the Subject).
          (Tuple object a2 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId objectGetter
          -- Cache the association between the assumptions found for this ActionInstance.
          pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) (union a0 (union a1 a2))
          void $ pure $ propertyUpdater object rt value
        else pure unit

-- | From the description of an Action, compile an Updater of ContextInstance.
-- | The results of these rather expensive computations are cached.
compileBotAction :: ActionType -> ContextInstance -> MonadPerspectives (Updater ContextInstance)
compileBotAction actionType contextId =
  case retrieveAction actionType of
    (Just a) -> pure a
    Nothing -> do
      (action :: Action) <- getPerspectType actionType
      -- objectOfAction is a type of Role.
      (objectOfAction :: Role) <- getRole (object action)
      -- objectGetter is a function that computes the actual instance(s) of the objectRole of the Action.
      -- The result of this function is dependent on any number of Assumptions.
      (objectGetter :: RoleGetter) <- getCalculation objectOfAction >>= context2role
      -- The Right Hand Side of the Action has side effects (updates Roles and Properties)
      (makeRHS :: (ContextInstance -> RHS)) <- constructRHS (unsafePartial $ fromJust $ effect action) objectGetter actionType
      -- The Left Hand Side of the Action is a query that computes boolean values.
      -- These values depend on a number of Assumptions.
      (lhs :: (ContextInstance ~~> Value)) <- condition action >>= context2propertyValue
      -- Now construct the updater by combining the lhs with the rhs.
      (updater :: Updater ContextInstance) <- pure (((lift <<< lift <<< flip runMonadPerspectivesQuery lhs) >=> (makeRHS contextId)))
      -- Cache the result.
      _ <- pure $ cacheAction actionType updater
      pure updater

-- | For a Context, set up its Actions. Register these Actions in the ActionRegister. Register their dependency
-- | on Assumptions in the actionAssumptionRegister in PerspectivesState.
setupBotActions :: ContextInstance -> MonadPerspectives Unit
setupBotActions cid = do
  -- TODO: filter, keeping just those actions that are to be executed by a Bot.
  (ct :: ContextType) <- cid ##>> contextType
  (actions :: Array ActionType) <- (getPerspectType ct :: MonadPerspectives Context) >>= pure <<< actions
  -- Run the updater once. It will collect the Assumptions it depends on and register itself in the
  -- actionAssumptionRegister in PerspectivesState.
  for_ actions \(a :: ActionType) -> (compileBotAction a cid) >>= \(updater :: Updater ContextInstance) -> pure $ updater cid

-- | Remove all actions associated with this context.
tearDownBotActions :: ContextInstance -> MonadPerspectives Unit
tearDownBotActions = pure <<< removeContextInstanceDependencies
