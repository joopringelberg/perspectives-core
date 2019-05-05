module Perspectives.Api where

import Control.Aff.Sockets (ConnectionProcess, EmitFunction, Emitter, Left, Right, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Producer, Process, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (produce')
import Effect.Aff (catchError, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Foreign (MultipleErrors, unsafeFromForeign)
import Foreign.Class (encode)
import Perspectives.Actions (addRol, setBinding, setProperty)
import Perspectives.ApiTypes (ContextSerialization(..), CorrelationIdentifier, Request(..), RolSerialization, Value, response)
import Perspectives.BasicConstructors (constructAnotherRol, constructContext)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), TripleRef(..))
import Perspectives.DataTypeTripleGetters (contextType, genericBinding, genericRolType, genericContext) as DTG
import Perspectives.Deltas (runTransactie)

import Perspectives.EntiteitAndRDFAliases (ContextID, Predicate, PropertyName, RolID, RolName, Subject, ViewName)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol)
import Perspectives.PerspectivesTypes (BuitenRol(..)) as PT
import Perspectives.QueryCompiler (getPropertyFunction, getRolFunction)
import Perspectives.QueryEffect (QueryEffect, (~>))
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.SaveUserData (saveUserData)
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter, propertyReferenties)
import Perspectives.TripleAdministration (unRegisterTriple)
import Perspectives.TripleGetterComposition ((>->))
import Prelude (Unit, bind, map, pure, show, unit, void, ($), (<<<), (<>), discard, (*>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest =
  GetRolBinding ContextID RolName (ReactStateSetter) CorrelationIdentifier
  | GetBinding RolID (ReactStateSetter) CorrelationIdentifier
  | GetBindingType RolID (ReactStateSetter) CorrelationIdentifier
  | GetRolContext RolID (ReactStateSetter) CorrelationIdentifier
  | GetContextType ContextID (ReactStateSetter) CorrelationIdentifier
  | GetRolType RolID  (ReactStateSetter) CorrelationIdentifier
  | GetRol ContextID RolName (ReactStateSetter) CorrelationIdentifier
  | GetProperty RolName PropertyName (ReactStateSetter) CorrelationIdentifier
  | GetViewProperties ViewName (ReactStateSetter) CorrelationIdentifier
  | ShutDown
  | WrongRequest -- Represents a request from the client Perspectives does not recognize.
  | Unsubscribe Subject Predicate CorrelationIdentifier
  | CreateContext ContextSerialization (ReactStateSetter)
  | CreateRol ContextID RolName RolSerialization (ReactStateSetter)
  | AddRol ContextID RolName RolID
  | SetBinding RolID RolID (ReactStateSetter)
  | SetProperty RolID PropertyName Value (ReactStateSetter)

type RequestRecord =
  { request :: String
  , subject :: String
  , predicate :: String
  , object :: String
  , reactStateSetter :: ReactStateSetter
  , setterId :: CorrelationIdentifier
  , contextDescription :: ContextSerialization
  , rolDescription :: RolSerialization}

showRequestRecord :: RequestRecord -> String
showRequestRecord {request, subject, predicate} = "{" <> request <> ", " <> subject <> ", " <> predicate <> "}"

foreign import createRequestEmitterImpl :: EffectFn3 (Left RequestRecord) (Right RequestRecord) (EmitFunction RequestRecord Unit) Unit

createRequestEmitter :: Emitter RequestRecord Unit
createRequestEmitter = runEffectFn3 createRequestEmitterImpl Left Right

-- A Producer for Requests.
requestProducer :: Producer RequestRecord (MonadPerspectives) Unit
requestProducer = produce' createRequestEmitter

-- | Create a process that consumes requests from a producer fed by the user interface.
setupApi :: MonadPerspectives Unit
setupApi = runProcess $ (requestProducer $~ (forever (transform marshallRequestRecord))) $$ consumeRequest

-- | Create a process that consumes requests from a producer that connects to a source over TCP.
setupTcpApi :: MonadPerspectives Unit
setupTcpApi = runProcess server
  where

    server :: Process MonadPerspectives Unit
    server = (connectionProducer defaultTCPOptions) $$ (connectionConsumer connectionHandler)

    connectionHandler :: ConnectionProcess MonadPerspectives
    connectionHandler connection =
      (dataProducer connection $~ (forever (transform marshallRequest))) $$ consumeRequest
      where
        marshallRequest :: (Either MultipleErrors Request) -> ApiRequest
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

consumeRequest :: Consumer ApiRequest MonadPerspectives Unit
consumeRequest = forever do
  request <- await
  lift $ dispatchOnRequest request
  lift $ runTransactie

-- | The client of Perspectives sends a record of arbitrary form that we
-- | try to fit into ApiRequest.
marshallRequestRecord :: RequestRecord -> ApiRequest
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

dispatchOnRequest :: ApiRequest -> MonadPerspectives Unit
dispatchOnRequest req =
  case req of
    (GetRolBinding cid rn setter setterId) -> do
      getRolBinding cid rn setter setterId
    (GetBinding rid setter setterId) -> subscribeToObjects rid DTG.genericBinding setter setterId
    (GetBindingType rid setter setterId) -> subscribeToObjects rid (DTG.genericBinding >-> DTG.genericRolType) setter setterId
    (GetRol cid rn setter setterId) -> getRol cid rn setter setterId
    (GetRolContext rid setter setterId) -> subscribeToObjects rid DTG.genericContext setter setterId
    (GetContextType rid setter setterId) -> subscribeToObjects rid DTG.contextType setter setterId
    (GetRolType rid setter setterId) -> subscribeToObjects rid DTG.genericRolType setter setterId
    (GetProperty rid pn setter setterId) -> getProperty rid pn setter setterId
    (GetViewProperties vn setter setterId) -> subscribeToObjects vn (propertyReferenties >-> DTG.genericBinding >-> DTG.genericContext) setter setterId
    (CreateContext (ContextSerialization cd) setter) -> do
      r <- constructContext (ContextSerialization cd {id = "model:User$c" <> (show $ guid unit)})
      case r of
        (Left messages) -> liftEffect $ setter (map show messages)
        (Right id) -> do
          -- TODO. Binnen- en buitenrol!
          saveUserData [PT.BuitenRol $ buitenRol id]
          liftEffect $ setter ["ok", buitenRol id] -- saveUserData
    (CreateRol cid rn rolSerialisation setter) -> do
      r <- constructAnotherRol rn cid rolSerialisation
      case r of
        (Left messages) -> liftEffect $ setter (map show messages)
        (Right id) -> liftEffect $ setter ["ok", id]
    (AddRol cid rn rid) -> void $ addRol rn rid cid
    (SetProperty rid pn v setter) -> catchError ((setProperty pn v rid) *> (liftEffect $ setter ["ok"])) (\e -> liftEffect $ setter [show e])
    (SetBinding rid bid setter) -> catchError ((setBinding rid bid) *> (liftEffect $ setter ["ok"])) (\e -> liftEffect $ setter [show e])
    (Unsubscribe subject predicate setterId) -> unsubscribeFromObjects subject predicate setterId
    -- Notice that a WrongRequest fails silently. No response is ever given.
    -- A Shutdown has no effect.
    otherwise -> pure unit

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter = Array String -> Effect Unit

type QueryUnsubscriber e = Effect Unit

-- | Runs a the query and adds the ReactStateSetter to the result.
subscribeToObjects :: Subject -> StringTypedTripleGetter -> ReactStateSetter -> CorrelationIdentifier -> MonadPerspectives Unit
subscribeToObjects subject query setter setterId = do
  (effectInReact :: QueryEffect) <- pure $ NamedFunction setterId setter
  void $ (subject ## query ~> effectInReact)

unsubscribeFromObjects :: Subject -> Predicate -> CorrelationIdentifier -> MonadPerspectives Unit
unsubscribeFromObjects subject predicate setterId = lift $ liftEffect $ unRegisterTriple $ TripleRef {subject, predicate: effectPredicate}
  where
    effectPredicate :: String
    effectPredicate = "(" <>  predicate <> " ~> " <> setterId <> ")"

-- | Retrieve the binding of the rol from the context, subscribe to it.
getRolBinding :: ContextID -> RolName -> ReactStateSetter -> CorrelationIdentifier -> MonadPerspectives Unit
getRolBinding cid rn setter  setterId= do
  rf <- getRolFunction rn
  subscribeToObjects cid (rf >-> DTG.genericBinding) setter setterId

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRol :: ContextID -> RolName -> ReactStateSetter -> CorrelationIdentifier -> MonadPerspectives Unit
getRol cid rn setter setterId = do
  qf <- getRolFunction rn
  subscribeToObjects cid qf setter setterId

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
-- getRolFunction :: ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e)  (StringTypedTripleGetter e)
-- getRolFunction cid rn = do
--   ctxtType <- cid ##>> DTG.contextType
--   m <- runMonadPerspectivesQueryCompiler ctxtType (compileElementaryQueryStep (QualifiedRol rn) (rn <> "_getter"))
--   case m of
--     (Left message) -> throwError $ error (show message)
--     (Right id) -> constructQueryFunction id

-- | Retrieve the property from the rol, subscribe to it.
getProperty :: RolID -> PropertyName -> ReactStateSetter -> CorrelationIdentifier -> MonadPerspectives Unit
getProperty rid pn setter setterId = do
  qf <- getPropertyFunction pn
  subscribeToObjects rid qf setter setterId

-- getPropertyFunction :: RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
-- getPropertyFunction rid pn = do
--   rolType <- rid ##>> DTG.rolType
--   m <- runMonadPerspectivesQueryCompiler rolType (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
--   case m of
--     (Left message) -> throwError $ error (show message)
--     (Right id) -> constructQueryFunction id
