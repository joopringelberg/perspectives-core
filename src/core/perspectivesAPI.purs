module Perspectives.Api where

import Control.Aff.Sockets (ConnectionProcess, EmitFunction, Emitter, Left, Right, SOCKETIO, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Producer, Process, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (produce')
import Control.Monad.Aff (catchError, error, launchAff_, throwError)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Uncurried (EffFn3, runEffFn3)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors, unsafeFromForeign)
import Data.Foreign.Class (encode)
import Perspectives.Actions (addRol, setBinding, setProperty)
import Perspectives.ApiTypes (ContextSerialization(..), CorrelationIdentifier, Request(..), RolSerialization, Value, response)
import Perspectives.BasicConstructors (constructAnotherRol, constructContext)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), TripleRef(..), TypedTripleGetter, runMonadPerspectivesQueryCompiler)
import Perspectives.DataTypeTripleGetters (binding, context, contextType, rolType) as DTG
import Perspectives.Deltas (runTransactie)
import Perspectives.Effects (AjaxAvarCache, ApiEffects, REACT)
import Perspectives.EntiteitAndRDFAliases (ContextID, Predicate, PropertyName, RolID, RolName, Subject, ViewName)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol)
import Perspectives.ModelBasedTripleGetters (propertyReferentiesM)
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryEffect (QueryEffect, (~>))
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.RunMonadPerspectivesQuery ((##), (##>>))
import Perspectives.SaveUserData (saveUserData)
import Perspectives.TripleAdministration (unRegisterTriple)
import Perspectives.TripleGetterComposition ((>->))
import Prelude (Unit, bind, map, pure, show, unit, void, ($), (<<<), (<>), discard, (*>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest e =
  GetRolBinding ContextID RolName (ReactStateSetter e) CorrelationIdentifier
  | GetBinding RolID (ReactStateSetter e) CorrelationIdentifier
  | GetBindingType RolID (ReactStateSetter e) CorrelationIdentifier
  | GetRolContext RolID (ReactStateSetter e) CorrelationIdentifier
  | GetContextType ContextID (ReactStateSetter e) CorrelationIdentifier
  | GetRolType RolID  (ReactStateSetter e) CorrelationIdentifier
  | GetRol ContextID RolName (ReactStateSetter e) CorrelationIdentifier
  | GetProperty RolName PropertyName (ReactStateSetter e) CorrelationIdentifier
  | GetViewProperties ViewName (ReactStateSetter e) CorrelationIdentifier
  | ShutDown
  | WrongRequest -- Represents a request from the client Perspectives does not recognize.
  | Unsubscribe Subject Predicate CorrelationIdentifier
  | CreateContext ContextSerialization (ReactStateSetter e)
  | CreateRol ContextID RolName RolSerialization (ReactStateSetter e)
  | AddRol ContextID RolName RolID
  | SetBinding RolID RolID (ReactStateSetter e)
  | SetProperty RolID PropertyName Value (ReactStateSetter e)

type RequestRecord e =
  { request :: String
  , subject :: String
  , predicate :: String
  , object :: String
  , reactStateSetter :: ReactStateSetter e
  , setterId :: CorrelationIdentifier
  , contextDescription :: ContextSerialization
  , rolDescription :: RolSerialization}

showRequestRecord :: forall e. RequestRecord e -> String
showRequestRecord {request, subject, predicate} = "{" <> request <> ", " <> subject <> ", " <> predicate <> "}"

foreign import createRequestEmitterImpl :: forall e eff. EffFn3 (avar :: AVAR | e) (Left (RequestRecord eff)) (Right (RequestRecord eff)) (EmitFunction (RequestRecord eff) Unit e) Unit

createRequestEmitter :: forall e eff. Emitter (RequestRecord eff) Unit e
createRequestEmitter = runEffFn3 createRequestEmitterImpl Left Right

-- A Producer for Requests.
requestProducer :: forall e eff. Producer (RequestRecord eff) (MonadPerspectives (avar :: AVAR | e)) Unit
requestProducer = produce' createRequestEmitter

-- | Create a process that consumes requests from a producer fed by the user interface.
setupApi :: forall e. MonadPerspectives (ApiEffects (now :: NOW | e)) Unit
setupApi = runProcess $ (requestProducer $~ (forever (transform marshallRequestRecord))) $$ consumeRequest

-- | Create a process that consumes requests from a producer that connects to a source over TCP.
setupTcpApi :: forall e. MonadPerspectives (ApiEffects (socketio :: SOCKETIO, now :: NOW | e)) Unit
setupTcpApi = runProcess server
  where

    server :: Process (MonadPerspectives (ApiEffects (socketio :: SOCKETIO, now :: NOW | e))) Unit
    server = (connectionProducer defaultTCPOptions) $$ (connectionConsumer connectionHandler)

    connectionHandler :: ConnectionProcess (MonadPerspectives (ApiEffects (socketio :: SOCKETIO, now :: NOW | e)))
    connectionHandler connection =
      (dataProducer connection $~ (forever (transform marshallRequest))) $$ consumeRequest
      where
        marshallRequest :: (Either MultipleErrors Request) -> ApiRequest (socketio :: SOCKETIO, now :: NOW | e)
        marshallRequest (Right (Request r@{rtype, subject, object, predicate, setterId, contextDescription, rolDescription})) =
          marshallRequestRecord
            { request: (unsafeFromForeign (encode rtype))
            , subject
            , predicate
            , object
            , setterId
            , reactStateSetter: launchAff_ <<< writeData connection <<< response setterId
            , contextDescription
            , rolDescription }
        marshallRequest (Left e) = WrongRequest

consumeRequest :: forall e. Consumer (ApiRequest (now :: NOW | e)) (MonadPerspectives (ApiEffects (now :: NOW | e))) Unit
consumeRequest = forever do
  request <- await
  lift $ dispatchOnRequest request
  lift $ runTransactie

-- | The client of Perspectives sends a record of arbitrary form that we
-- | try to fit into ApiRequest.
marshallRequestRecord :: forall e. RequestRecord e -> ApiRequest e
marshallRequestRecord r@{request} = do
  case request of
    "GetRolBinding" -> GetRolBinding r.subject r.predicate r.reactStateSetter r.setterId
    "GetBinding" -> GetBinding r.subject r.reactStateSetter r.setterId
    "GetBindingType" -> GetBindingType r.subject r.reactStateSetter r.setterId
    "GetRol" -> GetRol r.subject r.predicate r.reactStateSetter r.setterId
    "GetRolContext" -> GetRolContext r.subject r.reactStateSetter r.setterId
    "GetContextType" -> GetContextType r.subject r.reactStateSetter r.setterId
    "GetRolType" -> GetRolType r.subject r.reactStateSetter r.setterId
    "GetProperty" -> GetProperty r.subject r.predicate r.reactStateSetter r.setterId
    "GetViewProperties" -> GetViewProperties r.subject r.reactStateSetter r.setterId
    "Unsubscribe" -> Unsubscribe r.subject r.predicate r.setterId
    "ShutDown" -> ShutDown
    "CreateContext" -> CreateContext r.contextDescription r.reactStateSetter
    "CreateRol" -> CreateRol r.subject r.predicate r.rolDescription r.reactStateSetter
    "AddRol" -> AddRol r.subject r.predicate r.object
    "SetProperty" -> SetProperty r.subject r.predicate r.object r.reactStateSetter
    "SetBinding" -> SetBinding r.subject r.object r.reactStateSetter
    otherwise -> WrongRequest

dispatchOnRequest :: forall e. ApiRequest e -> MonadPerspectives (ApiEffects e) Unit
dispatchOnRequest req =
  case req of
    (GetRolBinding cid rn setter setterId) -> do
      getRolBinding cid rn setter setterId
    (GetBinding rid setter setterId) -> subscribeToObjects rid DTG.binding setter setterId
    (GetBindingType rid setter setterId) -> subscribeToObjects rid (DTG.binding >-> DTG.rolType) setter setterId
    (GetRol cid rn setter setterId) -> getRol cid rn setter setterId
    (GetRolContext rid setter setterId) -> subscribeToObjects rid DTG.context setter setterId
    (GetContextType rid setter setterId) -> subscribeToObjects rid DTG.contextType setter setterId
    (GetRolType rid setter setterId) -> subscribeToObjects rid DTG.rolType setter setterId
    (GetProperty rid pn setter setterId) -> getProperty rid pn setter setterId
    (GetViewProperties vn setter setterId) -> subscribeToObjects vn (propertyReferentiesM >-> DTG.binding >-> DTG.context) setter setterId
    (CreateContext (ContextSerialization cd) setter) -> do
      r <- constructContext (ContextSerialization cd {id = "model:User$c" <> (show $ guid unit)})
      case r of
        (Left messages) -> liftEff $ setter (map show messages)
        (Right id) -> do
          saveUserData [buitenRol id]
          liftEff $ setter ["ok", buitenRol id] -- saveUserData
    (CreateRol cid rn rolSerialisation setter) -> do
      r <- constructAnotherRol rn cid rolSerialisation
      case r of
        (Left messages) -> liftEff $ setter (map show messages)
        (Right id) -> liftEff $ setter ["ok", id]
    (AddRol cid rn rid) -> void $ addRol rn rid cid
    (SetProperty rid pn v setter) -> catchError ((setProperty pn v rid) *> (liftEff $ setter ["ok"])) (\e -> liftEff $ setter [show e])
    (SetBinding rid bid setter) -> catchError ((setBinding rid bid) *> (liftEff $ setter ["ok"])) (\e -> liftEff $ setter [show e])
    (Unsubscribe subject predicate setterId) -> unsubscribeFromObjects subject predicate setterId
    -- Notice that a WrongRequest fails silently. No response is ever given.
    -- A Shutdown has no effect.
    otherwise -> pure unit

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter e = Array String -> Eff (ApiEffects e) Unit

type QueryUnsubscriber e = Eff (gm :: GLOBALMAP | e) Unit

-- | Runs a the query and adds the ReactStateSetter to the result.
subscribeToObjects :: forall e. Subject -> TypedTripleGetter (react :: REACT | e) -> ReactStateSetter e -> CorrelationIdentifier -> MonadPerspectives (ApiEffects e) Unit
subscribeToObjects subject query setter setterId = do
  (effectInReact :: QueryEffect (react :: REACT | e)) <- pure $ NamedFunction setterId setter
  void $ (subject ## query ~> effectInReact)

unsubscribeFromObjects :: forall e. Subject -> Predicate -> CorrelationIdentifier -> MonadPerspectives (ApiEffects e) Unit
unsubscribeFromObjects subject predicate setterId = lift $ liftEff $ unRegisterTriple $ TripleRef {subject, predicate: effectPredicate}
  where
    effectPredicate :: String
    effectPredicate = "(" <>  predicate <> " ~> " <> setterId <> ")"

-- | Retrieve the binding of the rol from the context, subscribe to it.
getRolBinding :: forall e. ContextID -> RolName -> ReactStateSetter e -> CorrelationIdentifier -> MonadPerspectives (ApiEffects e) Unit
getRolBinding cid rn setter  setterId= do
  rf <- getRolFunction cid rn
  subscribeToObjects cid (rf >-> DTG.binding) setter setterId

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRol :: forall e. ContextID -> RolName -> ReactStateSetter e -> CorrelationIdentifier -> MonadPerspectives (ApiEffects e) Unit
getRol cid rn setter setterId = do
  qf <- getRolFunction cid rn
  subscribeToObjects cid qf setter setterId

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRolFunction :: forall e. ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e)  (TypedTripleGetter e)
getRolFunction cid rn = do
  ctxtType <- cid ##>> DTG.contextType
  m <- runMonadPerspectivesQueryCompiler ctxtType (compileElementaryQueryStep (QualifiedRol rn) (rn <> "_getter"))
  case m of
    (Left message) -> throwError $ error (show message)
    (Right id) -> constructQueryFunction id

-- | Retrieve the property from the rol, subscribe to it.
getProperty :: forall e. RolID -> PropertyName -> ReactStateSetter e -> CorrelationIdentifier -> MonadPerspectives (ApiEffects e) Unit
getProperty rid pn setter setterId = do
  qf <- getPropertyFunction rid pn
  subscribeToObjects rid qf setter setterId

getPropertyFunction :: forall e. RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
getPropertyFunction rid pn = do
  rolType <- rid ##>> DTG.rolType
  m <- runMonadPerspectivesQueryCompiler rolType (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
  case m of
    (Left message) -> throwError $ error (show message)
    (Right id) -> constructQueryFunction id
