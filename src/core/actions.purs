module Perspectives.Actions where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Perspectives.ApiTypes (Value)
import Perspectives.ContextAndRole (addContext_rolInContext, addRol_gevuldeRollen, addRol_property, changeRol_binding, removeContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, removeRol_property, rol_pspType, setContext_rolInContext, setRol_property)
import Perspectives.ContextRolAccessors (getRolMember)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), ObjectsGetter, TripleRef(..), TypedTripleGetter(..), (##>>), StringTypedTripleGetter, type (~~>))
import Perspectives.Deltas (addDelta)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, MemberName, PropertyName, RolID, RolName)
import Perspectives.Identifiers (psp)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol)
import Perspectives.Instances (class PersistentInstance)
import Perspectives.Instances (getPerspectEntiteit, saveVersionedEntiteit)
import Perspectives.Instances.Aliases (AnyContext)
import Perspectives.Instances.ObjectGetters (binding, context, contextType)
import Perspectives.Query.Compiler (compileQuery)
import Perspectives.Representation.Action (Action, condition, effect, object)
import Perspectives.Representation.Assignment (AssignmentStatement(..))
import Perspectives.Representation.Class.Persistent (EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheCachedEntiteit)
import Perspectives.Representation.Class.PersistentType (ActionType, getPerspectType)
import Perspectives.Representation.Class.Role (Role, getCalculation, getRole)
import Perspectives.Representation.Context (Context, actions)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType)
import Perspectives.TypesForDeltas (BindingDelta(..), RoleDelta(..), DeltaType(..), InverseBindingDelta(..), PropertyDelta(..))

-----------------------------------------------------------
-- CONSTRUCTACTIONFUNCTION
-----------------------------------------------------------
-- type Action = (PBool ~~> Value)

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect for a Context, conditional on a given boolean value.
-- | As this function is used exclusively in setting up bot actions, we do not set up bot actions in this function
-- | itself!
constructActionFunction :: AssignmentStatement -> StringTypedTripleGetter -> MonadPerspectives (ContextID -> PerspectivesEffect String)
constructActionFunction a objectGetter = case a of
  (SetRol rt query) -> do
    valueComputer <- compileQuery query
    pure \contextId bools -> case head bools of
      (Just "true") -> do
        (value :: Maybe Value) <- runTypedTripleGetterToMaybeObject contextId valueComputer
        (object :: Maybe RoleInstance) <- runTypedTripleGetterToMaybeObject contextId objectGetter
        void $ pure $ setRol <$> Just (unwrap rt) <*> object <*> value
      _ -> pure unit

  otherwise -> throwError (error ("Unknown AssignmentStatement in constructActionFunction: " <> show a))

  -- where
  --   actionFunction :: StringTypedTripleGetter -> StringTypedTripleGetter -> AnyContext -> PerspectivesEffect String
  --   actionFunction valueComputer' objectGetter' contextId bools = case head bools of
  --     Nothing -> pure unit
  --     (Just "true") -> do
  --       (value :: Maybe Value) <- runTypedTripleGetterToMaybeObject contextId valueComputer'
  --       (object :: Maybe RoleInstance) <- runTypedTripleGetterToMaybeObject contextId objectGetter'
  --       void $ pure $ setRol' <$> Just (unwrap rt) <*> object <*> value

{-
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
        _ -> throwError (error $ "constructActionFunction: unknown operation for assignToRol: '" <> unwrap operation <> "'")

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
              -- TODO. Voer de actie uit voor elk (indirect) object!
              object <- (runTypedTripleGetterToMaybeObject contextId objectGetter)
              case object of
                Nothing -> pure unit
                (Just o) -> void $ f property v o
        _ -> pure unit)

      case (unwrap operation) of
        "add" -> pure $ action addProperty'
        "remove" -> pure $ action removeProperty'
        "set" -> pure $ action setProperty'
        _ -> throwError (error $ "constructActionFunction: unknown operation for assignToProperty: '" <> (unwrap operation) <> "'")

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

        _ -> throwError (error $ "constructActionFunction: unknown functionName for effectFullFunction: '" <> (unwrap functionName) <> "'")

    _ -> throwError (error $ "constructActionFunction: unknown type description: '" <> actionType <> "'")
  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructActionFunction: " <> s <> " for: " <> t <> " " <> actionInstanceID)
-}
type ActionID = String

-- | From the description of an Action, compile a StringTypedTripleGetter that should be applied to an instance of
-- | the Context holding the Action, to conditionally execute it.
compileBotAction :: ActionType -> ContextID -> MonadPerspectives StringTypedTripleGetter
compileBotAction actionType contextId = do
  (action :: Action) <- getPerspectType actionType
  (obj :: Role) <- getRole (object action)
  (objectGetter :: StringTypedTripleGetter) <- compileQuery $ getCalculation obj
  (conditionalEffect :: (ContextID -> PerspectivesEffect String)) <- constructActionFunction (effect action) objectGetter
  (conditionQuery :: StringTypedTripleGetter) <- compileQuery $ condition action
  -- We can use the id of the Action to name the function. In the dependency network, the triple will
  -- be identified by the combination of the StringTypedTripleGetter and this Action name. That gives an unique name.
  pure $ (conditionQuery ~> (NamedFunction (unwrap actionType) (conditionalEffect contextId)))

  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructActionFunction: " <> s <> " for: " <> t <> " " <> unwrap actionType)

setupBotActions :: AnyContext -> MonadPerspectives Unit
setupBotActions cid = do
  -- TODO: filter, keeping just those actions with a subject that has RoleKind BotRole.
  (ct :: ContextType) <- contextType cid
  (actions :: Array ActionType) <- (getPerspectType ct :: MonadPerspectives Context) >>= pure <<< actions
  for_ actions \(a :: ActionType) -> (compileBotAction a cid) >>= \tg -> cid ## tg

-- | Remove all actions associated with this context. To do so, we recompute the actions, to get at their names.
-- | This might be computationally expensive. Some form of caching will speed it up greatly.
-- | A sneaky way would be to find triples on cid whose predicate match with the pattern "~>".
tearDownBotActions :: ContextID -> MonadPerspectives Unit
tearDownBotActions cid = do
  -- TODO: filter, keeping just those actions with a subject that has RoleKind BotRole.
  (ct :: ContextType) <- contextType cid
  (actions :: Array ActionType) <- (getPerspectType ct :: MonadPerspectives Context) >>= pure <<< actions
  for_ actions \(a :: ActionType) -> (compileBotAction a cid) >>= \(TypedTripleGetter name _) -> liftEffect $ unRegisterTriple (TripleRef {subject: cid, predicate: name})
