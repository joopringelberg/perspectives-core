module Perspectives.ActionCompiler where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Perspectives.BasicActionFunctions (storeDomeinFile)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, NamedFunction(..), ObjectsGetter, TypedTripleGetter, (%%>), (%%))
import Perspectives.DataTypeObjectGetters (contextType, rolBindingDef)
import Perspectives.Deltas (addProperty, addRol, removeProperty, removeRol, setProperty, setRol)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.ObjectGetterConstructors (getExternalProperty, getRol)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.QueryCompiler (propertyQuery, rolQuery)
import Perspectives.QueryEffect ((~>))
import Perspectives.RunMonadPerspectivesQuery ((##>))
import Perspectives.Utilities (onNothing)

type Action e = (Array String -> MonadPerspectivesQuery (AjaxAvarCache e) Unit)

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect for a Context, conditional on a given boolean value.
constructActionFunction :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (ContextID -> Action e)
constructActionFunction actionInstanceID = do
  actionType <- onNothing (errorMessage "no type found" "")
    (actionInstanceID %%> contextType)
  case actionType of
    "model:ActionAst$assignToRol" -> do
      -- The assignment operation to be executed. Possible values are: "add", "remove", "set"
      operation <- onNothing (errorMessage "no operation name provided" actionType) (actionInstanceID %%> (getExternalProperty "model:ActionAst$assignToRol$buitenRolBeschrijving$operation") )
      -- The rol we will assign to. The qualified name is used!
      (rol :: String) <- onNothing
        (errorMessage "no rol provided to assign to" actionType)
        (actionInstanceID %%> getBindingOfRol "model:ActionAst$assignToRol$rol")
      -- The description of the value (probably a query, on the context holding the Action).
      value <- onNothing
        (errorMessage "no value provided to assign" actionType)
        (actionInstanceID %%> getBindingOfRol "model:ActionAst$assignToRol$value")
      -- The function that will compute the value from the context.
      valueComputer <- rolQuery value

      action <- pure (\f contextId boolArr -> case head boolArr of
        Nothing -> pure unit
        (Just bool) -> case bool of
          "true" -> do
            value <- lift (contextId ##> valueComputer)
            case value of
              Nothing -> pure unit
              (Just v) -> void $ lift $ f rol v contextId
          _ -> pure unit)

      case operation of
        "add" -> pure $ action addRol
        "remove" -> pure $ action removeRol
        "set" -> pure $ action setRol
        _ -> throwError (error $ "constructActionFunction: unknown operation for assigntoRol: '" <> operation <> "'")

    "model:ActionAst$assignToProperty" -> do
      -- The assignment operation to be executed. Possible values are: "add", "remove", "set"
      operation <- onNothing (errorMessage "no operation name provided" actionType) (actionInstanceID %%> (getExternalProperty "model:ActionAst$assignToRol$buitenRolBeschrijving$operation") )
      -- The property we will assign to.
      (property :: String) <- onNothing
        (errorMessage "no property provided to assign to" actionType)
        (actionInstanceID %%> getBindingOfRol "model:ActionAst$assignToRol$property")
      -- The description of the value (probably a query, on the context holding the Action).
      value <- onNothing
        (errorMessage "no value provided to assign" actionType)
        (actionInstanceID %%> getBindingOfRol "model:ActionAst$assignToRol$value")
      -- The function that will compute the value from the context.
      valueComputer <- propertyQuery value

      action <- pure (\f rolId boolArr -> case head boolArr of
        Nothing -> pure unit
        (Just bool) -> case bool of
          "true" -> do
            value <- lift (rolId ##> valueComputer)
            case value of
              Nothing -> pure unit
              (Just v) -> void $ lift $ f property v rolId
          _ -> pure unit)

      case operation of
        "add" -> pure $ action addProperty
        "remove" -> pure $ action removeProperty
        "set" -> pure $ action setProperty
        _ -> throwError (error $ "constructActionFunction: unknown operation for assigntoProperty: '" <> operation <> "'")

    "model:ActionAst$effectFullFunction" -> do
      -- The effectful function to be applied. NOTE: the syntax coloring goes mad when we write out the propertyname below in full...
      functionName <- onNothing (errorMessage "no function name provided" actionType) (actionInstanceID %%> (getExternalProperty $ "model:ActionAst$effectFullFunction$buitenRolBeschrijving$" <>
        "functionName"))
      -- The parameters
      (parameters :: Array String) <- (actionInstanceID %% getBindingOfRol "model:ActionAst$effectFullFunction$parameter")
      case functionName of
        -- The Action is for the bot that plays a role in a model:CrlText$Text context.
        -- The contextId identifies this context, hence we need no parameters when calling this function.
        "storeDomeinFileInCouchdb" -> pure $ \contextId boolArr -> case head boolArr of
          Nothing -> pure unit
          (Just bool) -> case bool of
            "true" -> lift $ storeDomeinFile contextId
            _ -> pure unit

        _ -> throwError (error $ "constructActionFunction: unknown functionName for effectFullFunction: '" <> functionName <> "'")

    _ -> throwError (error $ "constructActionFunction: unknown type description: '" <> actionType <> "'")
  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructQueryFunction: " <> s <> " for: " <> t <> " " <> actionInstanceID)

getBindingOfRol :: forall e. ID -> ObjectsGetter e
getBindingOfRol rolName = getRol rolName /-/ rolBindingDef

type ActionID = String

-- | From the description of an Action, compile a TypedTripleGetter that should be applied to an instance of
-- | the Context holding the Action, to conditionally execute it.
compileAction :: forall e. ActionID -> ContextID -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
compileAction actionType contextId = do
  condition <- onNothing
    (errorMessage "no condition provided in Action" actionType)
    (actionType %%> getBindingOfRol "model:Perspectives$Actie$condition")
  action <- onNothing
    (errorMessage "no effect provided in Action" actionType)
    (actionType %%> getBindingOfRol "model:Perspectives$Actie$effect")
  (actionObjectsGetter :: (ContextID -> Action e)) <- constructActionFunction action
  (conditionQuery :: TypedTripleGetter e) <- propertyQuery condition
  pure $ conditionQuery ~> NamedFunction "" (actionObjectsGetter contextId)

  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructQueryFunction: " <> s <> " for: " <> t <> " " <> actionType)
