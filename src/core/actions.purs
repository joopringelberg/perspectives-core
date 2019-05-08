module Perspectives.Actions where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (Value)
import Perspectives.BasicActionFunctions (storeDomeinFile)
import Perspectives.ContextAndRole (addContext_rolInContext, addRol_gevuldeRollen, addRol_property, changeContext_displayName, changeContext_type, changeRol_binding, changeRol_context, changeRol_type, removeContext_rolInContext, removeRol_gevuldeRollen, removeRol_property, setContext_rolInContext, setRol_property)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, NamedFunction(..), ObjectsGetter, (##>), (##>>), (%%))
import Perspectives.DataTypeObjectGetters (context, contextType, rolBindingDef)
import Perspectives.DataTypeTripleGetters (contextType) as DTG
import Perspectives.Deltas (addDelta, addDomeinFileToTransactie)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, MemberName, PropertyName, RolID, RolName)
import Perspectives.Identifiers (deconstructModelName, isUserEntiteitID, psp)
import Perspectives.ModelBasedTripleGetters (botActiesInContext)
import Perspectives.ObjectGetterConstructors (getContextRol, searchExternalProperty)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectEntiteit (class PerspectEntiteit, cacheCachedEntiteit, cacheInDomeinFile)
import Perspectives.PerspectivesTypes (ActieDef, ContextDef(..), PBool, PropertyDef(..), RolDef(..), RolInContext(..), genericBinding)
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryEffect (PerspectivesEffect, (~>))
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (saveVersionedEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runTypedTripleGetterToMaybeObject, (##), (##=))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter)
import Perspectives.TripleGetterComposition (followedBy, (>->))
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Perspectives.Utilities (onNothing)

-----------------------------------------------------------
-- UPDATEPERSPECTENTITEIT
-----------------------------------------------------------
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
-- | The result is an ObjectsGetter that always returns the (ID of the) PerspectEntiteit.
updatePerspectEntiteit :: forall a. PerspectEntiteit a =>
  (Value -> a -> a) ->
  (ID -> ID -> Delta) ->
  Value -> ObjectsGetter
updatePerspectEntiteit changeEntity createDelta value cid = do
  updatePerspectEntiteit' changeEntity cid value
  addDelta $ createDelta cid value
  setupBotActions cid
  pure [cid]

updatePerspectEntiteit' :: forall a. PerspectEntiteit a =>
  (Value -> a -> a) ->
  ID -> Value -> MonadPerspectives Unit
updatePerspectEntiteit' changeEntity cid value = do
  (entity) <- (getPerspectEntiteit cid)
  if (isUserEntiteitID cid)
    then do
      -- Change the entity in cache:
      void $ cacheCachedEntiteit cid (changeEntity value entity)
      void $ saveVersionedEntiteit cid entity
    else do
      let dfId = (unsafePartial (fromJust (deconstructModelName cid)))
      addDomeinFileToTransactie dfId
      cacheInDomeinFile dfId (changeEntity value entity)

  -- rev <- lift $ onNothing' ("updatePerspectEntiteit: context has no revision, deltas are impossible: " <> cid) (getRevision' context)
  -- -- Store the changed entity in couchdb.
  -- newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
  -- -- Set the new revision in the entity.
  -- lift $ cacheCachedEntiteit cid (setRevision newRev context)

setContextType :: ID -> ObjectsGetter
setContextType = updatePerspectEntiteit
  changeContext_type
  (\theType cid -> Delta
    { id : cid
    , memberName: psp "type"
    , deltaType: Change
    , value:  (Just theType)
    , isContext: true
    })

setRolType :: ID -> ObjectsGetter
setRolType = updatePerspectEntiteit
  changeRol_type
  (\theType cid -> Delta
    { id : cid
    , memberName: psp "type"
    , deltaType: Change
    , value:  (Just theType)
    , isContext: false
    })

setContextDisplayName :: ID -> ObjectsGetter
setContextDisplayName = updatePerspectEntiteit
  changeContext_displayName
  (\displayName cid -> Delta
    { id : cid
    , memberName: psp "label"
    , deltaType: Change
    , value: (Just displayName)
    , isContext: true
    })

setContext :: ID -> ObjectsGetter
setContext = updatePerspectEntiteit
  changeRol_context
  (\rol cid -> Delta
    { id : cid
    , memberName: psp "context"
    , deltaType: Change
    , value: (Just rol)
    , isContext: false
    })

setBinding :: ID -> ID -> MonadPerspectives Unit
setBinding rid boundRol = do
  oldBinding <- genericBinding rid
  updatePerspectEntiteit' changeRol_binding rid boundRol
  case head oldBinding of
    Nothing -> pure unit
    (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob (psp "binding") rid
  updatePerspectEntiteitMember' addRol_gevuldeRollen boundRol (psp "binding") rid
  cid <- (RolInContext rid) ##>> context
  setupBotActions cid
  addDelta $ Delta
    { id : rid
    , memberName: psp "binding"
    , deltaType: Change
    , value: (Just boundRol)
    , isContext: false
    }

-----------------------------------------------------------
-- UPDATEPERSPECTENTITEITMEMBER
-----------------------------------------------------------
-- | Create an ObjectsGetter from a function that modifies the member (such as a role or property) of a PerspectEntiteit (respectively a context or role).
-- | The result of this ObjectsGetter is always the (ID of the) PerspectEntiteit that is passed in.
updatePerspectEntiteitMember :: forall a. PerspectEntiteit a =>
  (a -> MemberName -> Value -> a) ->
  (ID -> MemberName -> Value -> Delta) ->
  MemberName -> Value -> ObjectsGetter
updatePerspectEntiteitMember changeEntityMember createDelta memberName value cid = do
  updatePerspectEntiteitMember' changeEntityMember cid memberName value
  -- TODO. Om te proberen: maak alleen een delta als er echt iets verandert.
  addDelta $ createDelta memberName value cid
  pure [cid]

updatePerspectEntiteitMember' :: forall a. PerspectEntiteit a =>
  (a -> MemberName -> Value -> a) ->
  ID -> MemberName -> Value -> MonadPerspectives Unit
updatePerspectEntiteitMember' changeEntityMember pid memberName value = do
  (pe :: a) <- getPerspectEntiteit pid
  -- Change the entity in cache:
  (changedEntity :: a) <- cacheCachedEntiteit pid (changeEntityMember pe memberName value)
  -- And save it to Couchdb.
  void $ saveVersionedEntiteit pid changedEntity

-- | Add a rol to a context (and inversely register the context with the rol)
-- | TODO In a functional rol, remove the old value if present.
addRol' :: RolName -> RolID -> ObjectsGetter
addRol' =
  updatePerspectEntiteitMember
    addContext_rolInContext
    (\rolName rolId cid ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Add
        , value: (Just rolId)
        , isContext: true
        })

addRol :: RolName -> RolID -> ObjectsGetter
addRol = setUpBotActionsAfter addRol'

setUpBotActionsAfter :: (ID -> ID -> ObjectsGetter) -> ID -> ID -> (ObjectsGetter)
-- setUpBotActionsAfter g mn mid cid = g mn mid cid <* setupBotActions cid
setUpBotActionsAfter g mn mid cid = do
  r <- g mn mid cid
  setupBotActions cid
  pure r


removeRol' :: RolName -> RolID -> ObjectsGetter
removeRol' =
  updatePerspectEntiteitMember
    removeContext_rolInContext
    (\rolName rolId cid ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Remove
        , value: (Just rolId)
        , isContext: true
        })

removeRol :: RolName -> RolID -> ObjectsGetter
removeRol = setUpBotActionsAfter removeRol'

setRol' :: RolName -> RolID -> ObjectsGetter
setRol' =
  updatePerspectEntiteitMember
    setContext_rolInContext
    (\rolName rolId cid ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Change
        , value: (Just rolId)
        , isContext: true
        })

setRol :: RolName -> RolID -> ObjectsGetter
setRol = setUpBotActionsAfter setRol'

addProperty' :: PropertyName -> Value -> ObjectsGetter
addProperty' =
  updatePerspectEntiteitMember
    addRol_property
    (\propertyName value rid ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Add
        , value: (Just value)
        , isContext: false
        })

addProperty :: RolName -> RolID -> ObjectsGetter
addProperty = setUpBotActionsAfter addProperty'

removeProperty' :: PropertyName -> Value -> ObjectsGetter
removeProperty' =
  updatePerspectEntiteitMember
    removeRol_property
    (\propertyName value rid ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Remove
        , value: (Just value)
        , isContext: false
        })

removeProperty :: RolName -> RolID -> ObjectsGetter
removeProperty = setUpBotActionsAfter removeProperty'

setProperty' :: PropertyName -> Value -> ObjectsGetter
setProperty' =
  updatePerspectEntiteitMember
    setRol_property
    (\propertyName value rid ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Change
        , value: (Just value)
        , isContext: false
        })

setProperty :: PropertyName -> Value -> ObjectsGetter
setProperty = setUpBotActionsAfter setProperty'

-----------------------------------------------------------
-- CONSTRUCTACTIONFUNCTION
-----------------------------------------------------------
type Action = (PBool ~~> Value)

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect for a Context, conditional on a given boolean value.
constructActionFunction :: ContextID -> StringTypedTripleGetter -> MonadPerspectives (ContextID -> PerspectivesEffect String)
constructActionFunction actionInstanceID objectGetter = do
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
      valueComputer <- constructQueryFunction value

      action <- pure (\f contextId bools -> case head bools of
        Just "true" -> do
          val <- (runTypedTripleGetterToMaybeObject contextId valueComputer)
          case val of
            Nothing -> pure unit
            (Just v) -> do
              object <- (runTypedTripleGetterToMaybeObject contextId objectGetter)
              case object of
                Nothing -> pure unit
                (Just o) -> f rol v o *> setupBotActions contextId

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
      valueComputer <- constructQueryFunction value

      action <- pure (\f contextId bools -> case head bools of
        Just "true" -> do
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
        _ -> throwError (error $ "constructActionFunction: unknown operation for assignToProperty: '" <> (unwrap operation) <> "'")

    "model:Perspectives$effectFullFunction" -> do
      functionName <- onNothing (errorMessage "no function name provided" actionType) (actionInstanceID ##> (searchExternalProperty $ (PropertyDef $ psp "effectFullFunction$buitenRolBeschrijving$functionName")))
      -- The parameters
      (parameters :: Array String) <- (actionInstanceID %% getBindingOfRol (psp "effectFullFunction$parameter"))
      case unwrap functionName of
        -- The Action is for the bot that plays a role in a model:CrlText$Text context.
        -- The contextId identifies this context, hence we need no parameters when calling this function.
        "storeDomeinFileInCouchdb" -> pure $ \contextId bools -> case head bools of
          Just "true" -> void $ storeDomeinFile contextId
          _ -> pure unit

        _ -> throwError (error $ "constructActionFunction: unknown functionName for effectFullFunction: '" <> (unwrap functionName) <> "'")

    _ -> throwError (error $ "constructActionFunction: unknown type description: '" <> actionType <> "'")
  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructActionFunction: " <> s <> " for: " <> t <> " " <> actionInstanceID)

getBindingOfRol :: ID -> ObjectsGetter
getBindingOfRol rolName = getContextRol (RolDef rolName) /-/ rolBindingDef

type ActionID = String

-- | From the description of an Action, compile a StringTypedTripleGetter that should be applied to an instance of
-- | the Context holding the Action, to conditionally execute it.
compileBotAction :: ActieDef -> ContextID -> MonadPerspectives StringTypedTripleGetter
compileBotAction actionType contextId = do
  condition <- onNothing
    (errorMessage "no condition provided in Action" (unwrap actionType))
    (unwrap actionType ##> getBindingOfRol (psp "Actie$condition"))
  action <- onNothing
    (errorMessage "no effect provided in Action" (unwrap actionType))
    (unwrap actionType ##> getBindingOfRol (psp "Actie$effect"))
  object <- onNothing
    (errorMessage "no effect provided in Action" (unwrap actionType))
    (unwrap actionType ##> getBindingOfRol (psp "Actie$object"))
  (objectGetter :: StringTypedTripleGetter) <- constructQueryFunction object
  (conditionalEffect :: (ContextID -> PerspectivesEffect String)) <- constructActionFunction action objectGetter
  (conditionQuery :: StringTypedTripleGetter) <- constructQueryFunction condition
  -- We can use the id of the Action to name the function. In the dependency network, the triple will
  -- be identified by the combination of the StringTypedTripleGetter and this Action name. That gives an unique name.
  pure $ (conditionQuery ~> (NamedFunction (unwrap actionType) (conditionalEffect contextId)))

  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructActionFunction: " <> s <> " for: " <> t <> " " <> unwrap actionType)

setupBotActions :: ContextID -> MonadPerspectives Unit
setupBotActions cid = do
  (actions :: Array ActieDef) <- cid ##= DTG.contextType `followedBy` ContextDef >-> botActiesInContext
  for_ actions \(a :: ActieDef) -> (compileBotAction a cid) >>= \tg -> cid ## tg
