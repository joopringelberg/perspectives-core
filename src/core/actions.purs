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
import Perspectives.QueryEffect (PerspectivesEffect, (~>))
import Perspectives.Representation.Action (Action, condition, effect, object)
import Perspectives.Representation.Assignment (AssignmentStatement(..))
import Perspectives.Representation.Class.Persistent (EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheCachedEntiteit)
import Perspectives.Representation.Class.PersistentType (ActionType, getPerspectType)
import Perspectives.Representation.Class.Role (Role, getCalculation, getRole)
import Perspectives.Representation.Context (Context, actions)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType)
import Perspectives.TypesForDeltas (BindingDelta(..), ContextDelta(..), Delta(..), DeltaType(..), InverseBindingDelta(..), PropertyDelta(..))

{-
Om een door de gebruiker aangebrachte wijziging door te voeren, moet je:
  - een Delta maken;
  - die versturen aan alle betrokkenen.
    - wat is het type van de context?
    - wie zijn de betrokkenen?
    - welke betrokkenen hebben een Actie met als lijdend voorwerp de entiteit?
    - heeft die Actie een view met de betreffende property?
  - de wijziging doorvoeren op de interne representatie;
  - de consequenties doorvoeren in de triple administratie;
  - de gewijzigde context opslaan;

Om een door een andere gebruiker aangebrachte wijziging door te voeren, moet je:
  - controleren of de author wel gerechtigd is tot de wijziging;
    - in welke rol is de author betrokken bij de context (van de rol)?
    - heeft die rol een actie die de betreffende delta oplevert?
      - past het werkwoord bij de DeltaType?
      - is het lijdend voorwerp de betreffende rol of context?
      - heeft de view op het lijdend voorwerp de relevante property (indien het gaat om een property delta)?
  - de wijziging doorvoeren op de interne representatie;
  - de consequenties doorvoeren in de triple administratie;
  - de gewijzigde context opslaan;
-}
-- | Create update functions on PerspectContext or PerspectRol.
-- | The result is an ObjectsGetter that always returns the (ID of the) PersistentInstance.
-- | Sets up the Bot actions for a Context.

-----------------------------------------------------------
-- UPDATE A BINDING
-----------------------------------------------------------
setBinding :: RoleInstance -> (RoleInstance ~~> ContextInstance)
setBinding rid bnd = do
  (pe :: PerspectRol) <- getPerspectEntiteit rid
  oldBinding <- binding pe
  changedRole <- pure $ changeRol_binding pe bnd
  addDelta $ BindingDelta
              { id : rid
              , binding: bnd
              , deltaType: Change
              }

  -- Handle inverse binding.
  case head oldBinding of
    Nothing -> pure unit
    (Just ob) -> do
      -- Remove this roleinstance as a binding role from the old binding.
      cob <- pure $ removeRol_gevuldeRollen ob (rol_pspType pe) rid
      cacheCachedEntiteit ob cob
      saveVersionedEntiteit ob cob
      addDelta $ InverseBindingDelta
                  { id : ob
                  , binding: rid
                  , role: (rol_pspType pe)
                  , deltaType: Remove
                  }
      -- Add this roleinstance as a binding role for the new binding.
      cbnd <- pure $ addRol_gevuldeRollen bnd rol_pspType pe rid
      cacheCachedEntiteit bnd cbnd
      saveVersionedEntiteit bnd cbnd
      addDelta $ InverseBindingDelta
                  { id : bnd
                  , binding: rid
                  , role: (rol_pspType pe)
                  , deltaType: Add
                  }

  cacheCachedEntiteit rid changedRole
  saveVersionedEntiteit rid changedRole
  pure [context pe]

-- | Removes the binding R of the rol, if any.
-- | Removes the rol as value of 'gevuldeRollen' for psp:Rol$binding from the binding R.
removeBinding :: (RoleInstance ~~> ContextInstance)
removeBinding rid = do
  (pe :: PerspectRol) <- getPerspectEntiteit rid
  oldBinding <- binding pe
  changedRole <- pure $ removeRol_binding pe
  addDelta $ BindingDelta
              { id : rid
              , binding: oldBinding
              , deltaType: Remove
              }

  -- Handle inverse binding.
  case head oldBinding of
    Nothing -> pure unit
    (Just ob) -> do
      -- Remove this roleinstance as a binding role from the old binding.
      cob <- pure $ removeRol_gevuldeRollen ob (rol_pspType pe) rid
      cacheCachedEntiteit ob cob
      saveVersionedEntiteit ob cob
      addDelta $ InverseBindingDelta
                  { id : ob
                  , binding: rid
                  , role: (rol_pspType pe)
                  , deltaType: Remove
                  }

  cacheCachedEntiteit rid changedRole
  saveVersionedEntiteit rid changedRole
  pure [context pe]

-----------------------------------------------------------
-- UPDATE A ROLE
-----------------------------------------------------------
addRol :: ContextInstance -> EnumeratedRoleType -> (RoleInstance ~~> ContextInstance)
addRol cid rolName rolInstance = do
  (pe :: PerspectContext) <- getPerspectEntiteit cid
  changedContext <- pure $ addContext_rolInContext pe rolName rolInstance
  addDelta $ ContextDelta
              { id : cid
              , role: rolName
              , deltaType: Add
              , instance: rolInstance
              }
  cacheCachedEntiteit cid changedContext
  saveVersionedEntiteit cid changedContext
  pure [cid]

removeRol :: ContextInstance -> EnumeratedRoleType -> (RoleInstance ~~> ContextInstance)
removeRol cid rolName rolInstance = do
  (pe :: PerspectContext) <- getPerspectEntiteit cid
  changedContext <- pure $ removeContext_rolInContext pe rolName rolInstance
  addDelta $ ContextDelta
              { id : cid
              , role: rolName
              , deltaType: Remove
              , instance: rolInstance
              }
  cacheCachedEntiteit cid changedContext
  saveVersionedEntiteit cid changedContext
  pure [cid]

setRol :: ContextInstance -> EnumeratedRoleType -> (RoleInstance ~~> ContextInstance)
setRol cid rolName rolInstance = do
  (pe :: PerspectContext) <- getPerspectEntiteit cid
  changedContext <- pure $ setContext_rolInContext pe rolName (rolInstance :: RoleInstance)
  addDelta $ ContextDelta
              { id : cid
              , role: rolName
              , deltaType: Change
              , instance: rolInstance
              }
  cacheCachedEntiteit cid changedContext
  saveVersionedEntiteit cid changedContext
  pure [cid]

-----------------------------------------------------------
-- UPDATE A PROPERTY
-----------------------------------------------------------
addProperty :: RoleInstance -> EnumeratedPropertyType -> (Value ~~> ContextInstance)
addProperty rid propertyName val = do
  (pe :: PerspectRol) <- getPerspectEntiteit rid
  changedContext <- pure $ addRol_property pe propertyName val
  addDelta $ PropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Add
              , value: val
              }
  cacheCachedEntiteit rid changedContext
  saveVersionedEntiteit rid changedContext
  pure [context pe]

removeProperty :: RoleInstance -> EnumeratedPropertyType -> (Value ~~> ContextInstance)
removeProperty rid propertyName val = do
  (pe :: PerspectRol) <- getPerspectEntiteit rid
  changedContext <- pure $ removeRol_property pe propertyName val
  addDelta $ PropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Remove
              , value: val
              }
  cacheCachedEntiteit rid changedContext
  saveVersionedEntiteit rid changedContext
  pure [context rid]

setProperty :: RoleInstance -> EnumeratedPropertyType -> (Value ~~> ContextInstance)
setProperty rid propertyName val = do
  (pe :: PerspectRol) <- getPerspectEntiteit rid
  changedContext <- pure $ setRol_property pe propertyName val
  addDelta $ PropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Change
              , value: val
              }
  cacheCachedEntiteit rid changedContext
  saveVersionedEntiteit rid changedContext
  pure [context rid]

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
