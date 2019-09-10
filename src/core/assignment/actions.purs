module Perspectives.Actions where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldMap, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Perspectives.Assignment.ActionCache (cacheAction, retrieveAction)
import Perspectives.Assignment.DependencyTracking (ActionInstance(..), cacheActionInstanceDependencies, removeContextInstanceDependencies)
import Perspectives.Assignment.Update (PropertyUpdater, RoleUpdater, addProperty, addRol, removeProperty, removeRol, setProperty, setRol)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, RoleGetter, Updater, WithAssumptions, ContextPropertyValueGetter, runMonadPerspectivesQuery, (##>>))
import Perspectives.Instances.ObjectGetters (contextType)
import Perspectives.Query.Compiler (context2propertyValue, context2role)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.Action (Action, condition, effect, object)
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
      (objectGetter :: RoleGetter) <- context2role $ getCalculation objectOfAction
      -- The Right Hand Side of the Action has side effects (updates Roles and Properties)
      (makeRHS :: (ContextInstance -> RHS)) <- constructRHS (effect action) objectGetter actionType
      -- The Left Hand Side of the Action is a query that computes boolean values.
      -- These values depend on a number of Assumptions.
      (lhs :: (ContextInstance ~~> Value)) <- context2propertyValue $ condition action
      -- Now construct the updater by combining the lhs with the rhs.
      (updater :: Updater ContextInstance) <- pure (((lift <<< lift <<< flip runMonadPerspectivesQuery lhs) >=> (makeRHS contextId)))
      -- Cache the result.
      _ <- pure $ cacheAction actionType updater
      pure updater

-- | For a Context, set up its Actions. Register these Actions in the ActionRegister. Register their dependency
-- | on Assumptions in the actionAssumptionRegister in PerspectivesState.
setupBotActions :: ContextInstance -> MonadPerspectives Unit
setupBotActions cid = do
  -- TODO: filter, keeping just those actions with a subject that has RoleKind BotRole.
  (ct :: ContextType) <- cid ##>> contextType
  (actions :: Array ActionType) <- (getPerspectType ct :: MonadPerspectives Context) >>= pure <<< actions
  -- Run the updater once. It will collect the Assumptions it depends on and register itself in the
  -- actionAssumptionRegister in PerspectivesState.
  for_ actions \(a :: ActionType) -> (compileBotAction a cid) >>= \(updater :: Updater ContextInstance) -> pure $ updater cid

-- | Remove all actions associated with this context.
tearDownBotActions :: ContextInstance -> MonadPerspectives Unit
tearDownBotActions = pure <<< removeContextInstanceDependencies

{-
  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructRHS: " <> s <> " for: " <> t <> " " <> unwrap actionType)

    actionFunction :: StringTypedTripleGetter -> StringTypedTripleGetter -> AnyContext -> PerspectivesEffect String
    actionFunction valueComputer' objectGetter' contextId bools = case head bools of
      Nothing -> pure unit
      (Just "true") -> do
        (value :: Maybe Value) <- runTypedTripleGetterToMaybeObject contextId valueComputer'
        (object :: Maybe RoleInstance) <- runTypedTripleGetterToMaybeObject contextId objectGetter'
        void $ pure $ setRol' <$> Just (unwrap rt) <*> object <*> value

  do
  actionType <- onNothing (errorMessage "no type found" "")
    (actionInstanceID ##> contextType)
  case actionType of
    "model:Perspectives$assignToRol" -> do
      -- The assignment operation to be executed. Possible values are: "add", "remove", "set"
      operation <- onNothing (errorMessage "no operation name provided" actionType) (actionInstanceID ##> (searchExternalProperty (PropertyDef $psp "assignToRol$buitenRolBeschrijving$operation")))
      -- The rol we will assign to. The qualified name is used!
      (rol :: String) <- onNothing
        (errorMessage "no rol provided to assign to" actionType)
        (actionInstanceID ##> getBindingOfRol (psp "assignToRol$rol"))
      -- The description of the value (probably a query, on the context holding the Action).
      value <- onNothing
        (errorMessage "no value provided to assign" actionType)
        (actionInstanceID ##> getBindingOfRol (psp "assignToRol$value"))
      -- The function that will compute the value from the context.
      valueComputer <- compileQuery value

      action <- pure (\f contextId bools -> case head bools of
        Just "true" -> do
          val <- (runTypedTripleGetterToMaybeObject contextId valueComputer)
          case val of
            Nothing -> pure unit
            (Just v) -> do
              object <- (runTypedTripleGetterToMaybeObject contextId objectGetter)
              case object of
                Nothing -> pure unit
                (Just o) -> void $ f rol v o

        _ -> pure unit)

      case unwrap operation of
        "add" -> pure $ action addRol'
        "remove" -> pure $ action removeRol'
        "set" -> pure $ action setRol'
        _ -> throwError (error $ "constructRHS: unknown operation for assignToRol: '" <> unwrap operation <> "'")

    "model:Perspectives$assignToProperty" -> do
      -- The assignment operation to be executed. Possible values are: "add", "remove", "set"
      operation <- onNothing (errorMessage "no operation name provided" actionType) (actionInstanceID ##> (searchExternalProperty (PropertyDef $ psp "assignToProperty$buitenRolBeschrijving$operation") ))
      -- The property we will assign to.
      (property :: String) <- onNothing
        (errorMessage "no property provided to assign to" actionType)
        (actionInstanceID ##> getBindingOfRol (psp "assignToProperty$property"))
      -- The description of the value (probably a query, on the context holding the Action).
      value <- onNothing
        (errorMessage "no value provided to assign" actionType)
        (actionInstanceID ##> getBindingOfRol (psp "assignToProperty$value"))
      -- The function that will compute the value from the context.
      valueComputer <- compileQuery value

      action <- pure (\f contextId bools -> case head bools of
        Just "true" -> do
          -- TODO. Indien de relatie relationeel is, ken dan alle waarden toe!
          val <- (runTypedTripleGetterToMaybeObject contextId valueComputer)
          case val of
            Nothing -> pure unit
            (Just v) -> do
              object <- (runTypedTripleGetterToMaybeObject contextId objectGetter)
              case object of
                Nothing -> pure unit
                (Just o) -> void $ f property v o
        _ -> pure unit)

      case (unwrap operation) of
        "add" -> pure $ action addProperty'
        "remove" -> pure $ action removeProperty'
        "set" -> pure $ action setProperty'
        _ -> throwError (error $ "constructRHS: unknown operation for assignToProperty: '" <> (unwrap operation) <> "'")

    "model:Perspectives$effectFullFunction" -> do
      functionName <- onNothing (errorMessage "no function name provided" actionType) (actionInstanceID ##> (searchExternalProperty $ (PropertyDef $ psp "effectFullFunction$buitenRolBeschrijving$functionName")))
      -- The parameters
      (parameters :: Array String) <- (actionInstanceID %% getBindingOfRol (psp "effectFullFunction$parameter"))
      case unwrap functionName of
        -- TODO. AS SOON AS we have the Arc parser online, this case can be re-activated.
        -- The Action is for the bot that plays a role in a model:CrlText$Text context.
        -- The contextId identifies this context, hence we need no parameters when calling this function.
        -- "storeDomeinFileInCouchdb" -> pure $ \contextId bools -> case head bools of
        --   Just "true" -> void $ storeDomeinFile contextId
        --   _ -> pure unit

        _ -> throwError (error $ "constructRHS: unknown functionName for effectFullFunction: '" <> (unwrap functionName) <> "'")

    _ -> throwError (error $ "constructRHS: unknown type description: '" <> actionType <> "'")
  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructRHS: " <> s <> " for: " <> t <> " " <> actionInstanceID)
-}
