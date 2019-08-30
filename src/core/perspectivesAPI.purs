module Perspectives.Api where

import Control.Aff.Sockets (ConnectionProcess, EmitFunction, Emitter, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Producer, Process, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (Step(..), produce')
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign, ForeignError, MultipleErrors, unsafeToForeign)
import Foreign.Class (decode)
import Partial.Unsafe (unsafePartial)
import Perspectives.Actions (addRol, removeBinding, setBinding, setProperty, setupBotActions)
import Perspectives.ApiTypes (ApiEffect, ContextSerialization(..), CorrelationIdentifier, Request(..), RequestRecord, Response(..), ResponseRecord, mkApiEffect, showRequestRecord)
import Perspectives.ApiTypes (RequestType(..)) as Api
import Perspectives.BasicConstructors (constructAnotherRol, constructContext)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), TripleRef(..), (##>>), (##>), StringTypedTripleGetter)
import Perspectives.DependencyTracking.Dependency (registerSupportedEffect)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName, RolID, RolName, Subject)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (LocalName, buitenRol)
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Instances (saveEntiteit)
import Perspectives.Instances.ObjectGetters (contextType) as DTO
import Perspectives.Query.Compiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.Context (lookForUnqualifiedRoleType)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.SaveUserData (removeUserContext, removeUserRol, saveUserContext)
import Prelude (Unit, bind, pure, show, unit, void, ($), (<<<), (<>), discard, (*>), negate, (==))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
foreign import createRequestEmitterImpl :: EffectFn3 (Foreign -> Step Foreign Unit) (Unit -> Step Foreign Unit) (EmitFunction Foreign Unit) Unit

createRequestEmitter :: Emitter Foreign Unit
createRequestEmitter = runEffectFn3 createRequestEmitterImpl Emit Finish

-- A Producer for Requests.
requestProducer :: Producer Foreign (MonadPerspectives) Unit
requestProducer = produce' createRequestEmitter

-- | Create a process that consumes requests from a producer fed by the user interface.
setupApi :: MonadPerspectives Unit
setupApi = runProcess $ (requestProducer $~ (forever (transform decodeRequest))) $$ consumeRequest
  where
    decodeRequest :: Foreign -> Request
    decodeRequest f = case unwrap $ runExceptT (decode f) of
      (Right r) -> r
      (Left e) -> Request
        { request: Api.WrongRequest
        , subject: ("Perspectives could not decode this request: '" <> show e <> "'")
        , predicate: ""
        , object: ""
        , corrId: -1
        , reactStateSetter: Just $ unsafeToForeign (\x -> pure unit :: Aff Unit)
        , contextDescription: unsafeToForeign ""
        , rolDescription: Nothing}

-- | Create a process that consumes requests from a producer that connects to a source over TCP.
setupTcpApi :: MonadPerspectives Unit
setupTcpApi = runProcess server
  where

    server :: Process MonadPerspectives Unit
    server = (connectionProducer defaultTCPOptions) $$ (connectionConsumer connectionHandler)

    connectionHandler :: ConnectionProcess MonadPerspectives
    connectionHandler connection =
      (dataProducer connection $~ (forever (transform addCallback))) $$ consumeRequest
      where
        -- Here we add a callback to a Request (we don't receive a callback over TCP!).
        addCallback :: (Either MultipleErrors Request) -> Request
        addCallback (Right (Request r@{request, subject, object, predicate, corrId, contextDescription, rolDescription})) =
          Request
            { request: request
            , subject
            , predicate
            , object
            , corrId
            , reactStateSetter: Just setter
            , contextDescription
            , rolDescription }
        addCallback (Left e) =
          Request
            { request: Api.WrongRequest
            , subject: ("Perspectives could not decode this request: '" <> show e <> "'")
            , predicate: ""
            , object: ""
            , corrId: -1
            , reactStateSetter: Just setter
            , contextDescription: unsafeToForeign ""
            , rolDescription: Nothing }

        setter :: Foreign
        setter = unsafeToForeign (launchAff_ <<< (writeData connection :: ResponseRecord -> Aff Boolean))

        -- marshallRequest (Left e) = WrongRequest ("Perspectives does not recognise this request: '" <> show e <> "'") (launchAff_ <<< writeData connection) "universalErrorHandler"

consumeRequest :: Consumer Request MonadPerspectives Unit
consumeRequest = forever do
  request <- await
  lift $ dispatchOnRequest (unwrap request)

dispatchOnRequest :: RequestRecord -> MonadPerspectives Unit
dispatchOnRequest r@{request, subject, predicate, object, reactStateSetter, corrId, contextDescription, rolDescription} =
  case request of
    -- Given the qualified name of the RolType.
    Api.GetRolBinding -> registerSupportedEffect corrId setter binding (RoleInstance subject)
    -- Api.GetRolBinding -> getRolBinding subject predicate setter corrId
    -- Given the rolinstance;
    Api.GetBinding -> subscribeToObjects subject binding setter corrId
    Api.GetBindingType -> subscribeToObjects subject (binding >-> roleType) setter corrId
    Api.GetRol -> getRol subject predicate setter corrId
    Api.GetUnqualifiedRol -> getRolFromLocalName subject predicate setter corrId
    Api.GetRolContext -> subscribeToObjects subject context setter corrId
    Api.GetContextType -> subscribeToObjects subject contextType setter corrId
    Api.GetRolType -> subscribeToObjects subject roleType setter corrId
    Api.GetUnqualifiedRolType -> subscribeToObjects subject (lookForUnqualifiedRoleType predicate) setter corrId
    Api.GetProperty -> getProperty subject predicate setter corrId
    Api.GetViewProperties -> -- subject is the roltype.
      if (predicate == "allProperties")
        then subscribeToObjects subject (effectiveRolType >-> propertiesDef) setter corrId
        else subscribeToObjects subject (effectiveRolType >-> searchView predicate >-> propertyReferenties >-> genericRolBindingDef) setter corrId
    Api.CreateContext -> case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextSerialization) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextSerialization cd) :: Either (NonEmptyList ForeignError) ContextSerialization) -> do
        ctxt <- constructContext (ContextSerialization cd {id = "model:User$c" <> (show $ guid unit)})
        case ctxt of
          (Left messages) -> sendResponse (Error corrId (show messages)) setter
          (Right id) -> do
            saveUserContext id
            setupBotActions id
            sendResponse (Result corrId [buitenRol id]) setter
    Api.DeleteContext -> do
      removeUserContext subject
      sendResponse (Result corrId []) setter
    Api.RemoveRol -> do
        removeUserRol object
        -- void $ removeRol predicate object subject
        sendResponse (Result corrId []) setter
    Api.CreateRol -> do
      rol <- constructAnotherRol predicate subject (unsafePartial $ fromJust rolDescription)
      case rol of
        (Left messages) -> sendResponse (Error corrId (show messages)) setter
        (Right id) -> do
          -- save the rol.
          void (saveEntiteit id :: MonadPerspectives PerspectRol)
          sendResponse (Result corrId [id]) setter
    -- Check whether a role exists for ContextDef with the localRolName and then create a new instance of it according to the rolDescription.
    -- subject :: Context, predicate :: localRolName, object :: ContextDef. rolDescription must be present!
    Api.CreateRolWithLocalName -> do
      mqrolname <- (ContextDef object) ##> (lookForUnqualifiedRoleType predicate)
      case mqrolname of
        Nothing -> sendResponse (Error corrId ("Cannot find Rol with local name '" <> predicate <> "' on context type '" <> object <> "'!")) setter
        (Just qrolname) -> do
          rol <- constructAnotherRol (unwrap qrolname) subject (unsafePartial $ fromJust rolDescription)
          case rol of
            (Left messages) -> sendResponse (Error corrId (show messages)) setter
            (Right id) -> do
              -- save the rol.
              void (saveEntiteit id :: MonadPerspectives PerspectRol)
              sendResponse (Result corrId [id]) setter
    Api.AddRol -> void $ addRol predicate object subject
    Api.SetProperty -> catchError
      ((setProperty predicate object subject) *> sendResponse (Result corrId ["ok"]) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    Api.SetBinding -> catchError
      ((setBinding subject object) *> (sendResponse (Result corrId ["ok"]) setter))
      (\e -> sendResponse (Error corrId (show e)) setter)
    Api.RemoveBinding -> catchError
      ((removeBinding subject) *> (sendResponse (Result corrId ["ok"]) setter))
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Create a new instance of the roletype RolDef in Context and fill the role with RolID.
    -- subject :: Context, predicate :: RolDef, object :: RolID
    Api.BindInNewRol -> catchError
      (do
        rol <- constructAnotherRol predicate subject (unsafePartial $ fromJust rolDescription)
        case rol of
          (Left messages) -> sendResponse (Error corrId (show messages)) setter
          (Right newRol) -> do
            -- save the rol.
            void (saveEntiteit newRol :: MonadPerspectives PerspectRol)
            setBinding newRol object
            sendResponse (Result corrId ["ok"]) setter
            )
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Check whether a role exists for ContextDef with the localRolName and whether it allows RolID as binding.
    -- subject :: ContextDef, predicate :: localRolName, object :: RolID
    Api.CheckBinding -> do
      mtypeOfRolToBindTo <- subject RP.##> (searchUnqualifiedRolDefinition predicate)
      case mtypeOfRolToBindTo of
        Nothing -> sendResponse (Error corrId ("No roltype found for '" <> predicate <> "'.")) setter
        (Just typeOfRolToBindTo) -> do
          ok <- checkBinding (RolDef typeOfRolToBindTo) object
          sendResponse (Result corrId [(show ok)]) setter
    Api.Unsubscribe -> unsubscribeFromObjects subject corrId
    Api.WrongRequest -> sendResponse (Error corrId subject) setter
    otherwise -> sendResponse (Error corrId ("Perspectives could not handle this request: '" <> (showRequestRecord r) <> "'")) (mkApiEffect reactStateSetter)
  where
    setter = (mkApiEffect reactStateSetter)

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter = Array String -> Effect Unit


type QueryUnsubscriber e = Effect Unit

unsubscribeFromObjects :: Subject -> CorrelationIdentifier -> MonadPerspectives Unit
unsubscribeFromObjects subject corrId = lift $ liftEffect $ unRegisterTriple $ TripleRef {subject, predicate: show corrId}

-- | Retrieve the binding of the rol from the context, subscribe to it.
getRolBinding :: ContextID -> RolName -> ApiEffect -> CorrelationIdentifier -> MonadPerspectives Unit
getRolBinding cid rn setter corrId = do
  rf <- getRoleFunction rn
  subscribeToObjects cid (rf >-> binding) setter corrId
  -- subscribeToObjects cid (rf >-> binding) (setter <<< Result corrId)

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for RolInContext/ContextRol, not BinnenRol or BuitenRol.
getRol :: ContextID -> RolName -> ApiEffect -> CorrelationIdentifier -> MonadPerspectives Unit
getRol cid rn setter corrId = do
  qf <- getRoleFunction rn
  subscribeToObjects cid qf setter corrId

getRolFromLocalName :: ContextID -> LocalName -> ApiEffect -> CorrelationIdentifier -> MonadPerspectives Unit
getRolFromLocalName cid ln setter corrId = do
  contextType <- cid ##>> DTO.contextType
  qf <- lookForUnqualifiedRoleType ln contextType
  subscribeToObjects cid qf setter corrId

-- | Retrieve the property from the rol, subscribe to it.
getProperty :: RolID -> PropertyName -> ApiEffect -> CorrelationIdentifier -> MonadPerspectives Unit
getProperty rid pn setter corrId = do
  qf <- getPropertyFunction pn
  subscribeToObjects rid qf setter corrId

-- getPropertyFunction :: RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
-- getPropertyFunction rid pn = do
--   rolType <- rid ##>> DTG.rolType
--   m <- runMonadPerspectivesQueryCompiler rolType (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
--   case m of
--     (Left message) -> throwError $ error (show message)
--     (Right id) -> constructQueryFunction id

-- | Apply an ApiEffect to a Response, in effect sending it through the API to the caller.
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse r ae = liftEffect $ (unsafeCoerce ae) (Api.convertResponse r)
