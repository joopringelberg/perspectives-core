module Perspectives.Api where

import Control.Aff.Sockets (EmitFunction, Left, Right, Emitter)
import Control.Coroutine (Consumer, Producer, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (produce')
import Control.Monad.Aff (error, throwError)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn3, runEffFn3)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), TripleRef(..), TypedTripleGetter(..), runMonadPerspectivesQueryCompiler)
import Perspectives.DataTypeTripleGetters (bindingM, contextM, contextTypeM, rolTypeM)
import Perspectives.Effects (AjaxAvarCache, ApiEffects, REACT)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName, RolID, RolName, ViewName, Subject, Predicate)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.ModelBasedTripleGetters (propertyReferentiesM)
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryEffect (QueryEffect, (~>))
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.RunMonadPerspectivesQuery ((##), (##>>))
import Perspectives.TripleAdministration (unRegisterTriple)
import Perspectives.TripleGetterComposition ((>->))
import Prelude (Unit, bind, const, pure, show, unit, void, ($), (<<<), (<>), (>=>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest e =
  GetRolBinding ContextID RolName (ReactStateSetter e) ReactStateSetterIdentifier
  | GetBinding RolID (ReactStateSetter e) ReactStateSetterIdentifier
  | GetBindingType RolID (ReactStateSetter e) ReactStateSetterIdentifier
  | GetRolContext RolID (ReactStateSetter e) ReactStateSetterIdentifier
  | GetContextType ContextID (ReactStateSetter e) ReactStateSetterIdentifier
  | GetRol ContextID RolName (ReactStateSetter e) ReactStateSetterIdentifier
  | GetProperty RolName PropertyName (ReactStateSetter e) ReactStateSetterIdentifier
  | GetViewProperties ViewName (ReactStateSetter e) ReactStateSetterIdentifier
  | ShutDown
  | WrongRequest -- Represents a request from the client Perspectives does not recognize.
  | Unsubscribe Subject Predicate ReactStateSetterIdentifier

data ApiResponse e = Unsubscriber (QueryUnsubscriber e)
  | Error String

type RequestRecord e =
  { request :: String
  , subject :: String
  , predicate :: String
  , reactStateSetter :: ReactStateSetter e
  , setterId :: ReactStateSetterIdentifier}

showRequestRecord :: forall e. RequestRecord e -> String
showRequestRecord {request, subject, predicate} = "{" <> request <> ", " <> subject <> ", " <> predicate <> "}"

foreign import createRequestEmitterImpl :: forall e eff. EffFn3 (avar :: AVAR | e) (Left (RequestRecord eff)) (Right (RequestRecord eff)) (EmitFunction (RequestRecord eff) Unit e) Unit

createRequestEmitter :: forall e eff. Emitter (RequestRecord eff) Unit e
createRequestEmitter = runEffFn3 createRequestEmitterImpl Left Right

-- A Producer for Requests.
requestProducer :: forall e eff. Producer (RequestRecord eff) (MonadPerspectives (avar :: AVAR | e)) Unit
requestProducer = produce' createRequestEmitter

-- | Create a process that consumes requests from a producer fed by the user interface.
setupApi :: forall e. MonadPerspectives (ApiEffects e) Unit
setupApi = runProcess $ (requestProducer $~ (forever (transform marshallRequestRecord))) $$ consumeRequest

consumeRequest :: forall e. Consumer (ApiRequest e) (MonadPerspectives (ApiEffects e)) Unit
consumeRequest = forever do
  request <- await
  lift $ dispatchOnRequest request

-- | The client of Perspectives sends a record of arbitrary form that we
-- | try to fit into ApiRequest.
-- TODO. Instead of a ReactStateSetter, we will receive a correlation identifier.
-- Create a function from it that sends the new values, along with the correlation identifier,
-- into the channel.
marshallRequestRecord :: forall e. RequestRecord e -> ApiRequest e
marshallRequestRecord r@{request} = do
  case request of
    "GetRolBinding" -> GetRolBinding r.subject r.predicate r.reactStateSetter r.setterId
    "GetBinding" -> GetBinding r.subject r.reactStateSetter r.setterId
    "GetBindingType" -> GetBindingType r.subject r.reactStateSetter r.setterId
    "GetRol" -> GetRol r.subject r.predicate r.reactStateSetter r.setterId
    "GetRolContext" -> GetRolContext r.subject r.reactStateSetter r.setterId
    "GetContextType" -> GetContextType r.subject r.reactStateSetter r.setterId
    "GetProperty" -> GetProperty r.subject r.predicate r.reactStateSetter r.setterId
    "GetViewProperties" -> GetViewProperties r.subject r.reactStateSetter r.setterId
    "Unsubscribe" -> Unsubscribe r.subject r.predicate r.setterId
    "ShutDown" -> ShutDown
    otherwise -> WrongRequest

dispatchOnRequest :: forall e. ApiRequest e -> MonadPerspectives (ApiEffects e) Unit
dispatchOnRequest req =
  if (isShutDown req) -- Catch ShutDown here, so...
    then pure unit
    else do
      case req of
        (GetRolBinding cid rn setter setterId) -> do
          getRolBinding cid rn setter setterId
        (GetBinding rid setter setterId) -> subscribeToObjects rid bindingM setter setterId
        (GetBindingType rid setter setterId) -> subscribeToObjects rid (bindingM >-> rolTypeM) setter setterId
        (GetRol cid rn setter setterId) -> getRol cid rn setter setterId
        (GetRolContext rid setter setterId) -> subscribeToObjects rid contextM setter setterId
        (GetContextType rid setter setterId) -> subscribeToObjects rid contextTypeM setter setterId
        (GetProperty rid pn setter setterId) -> getProperty rid pn setter setterId
        (GetViewProperties vn setter setterId) -> subscribeToObjects vn (propertyReferentiesM >-> bindingM >-> contextM) setter setterId
        otherwise -> pure unit
  where
    isShutDown :: ApiRequest e -> Boolean
    isShutDown req = case req of
      ShutDown -> true
      otherwise -> false

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter e = Array String -> Eff (AjaxAvarCache (react :: REACT | e)) (NullOrUndefined Unit)

type QueryUnsubscriber e = Eff (gm :: GLOBALMAP | e) Unit

type ReactStateSetterIdentifier = String

-- | Runs a the query and adds the ReactStateSetter to the result.
-- | Returns a function of no arguments that the external program can use to unsubscribe the ReactStateSetter.
subscribeToObjects :: forall e. Subject -> TypedTripleGetter (react :: REACT | e) -> ReactStateSetter e -> ReactStateSetterIdentifier -> MonadPerspectives (ApiEffects e) Unit
subscribeToObjects subject query@(TypedTripleGetter qn _) setter setterId = do
  (effectInReact :: QueryEffect (react :: REACT | e)) <- pure $ NamedFunction setterId (setter >=> pure <<< const unit)
  void $ (subject ## query ~> effectInReact)

unsubscribeFromObjects :: forall e. Subject -> Predicate -> ReactStateSetterIdentifier -> MonadPerspectives (ApiEffects e) Unit
unsubscribeFromObjects subject predicate setterId = lift $ liftEff $ unRegisterTriple $ TripleRef {subject, predicate: effectPredicate}
  where
    effectPredicate :: String
    effectPredicate = "(" <>  predicate <> " ~> " <> setterId <> ")"

-- | Retrieve the binding of the rol from the context, subscribe to it.
getRolBinding :: forall e. ContextID -> RolName -> ReactStateSetter e -> ReactStateSetterIdentifier -> MonadPerspectives (ApiEffects e) Unit
getRolBinding cid rn setter  setterId= do
  rf <- getRolFunction cid rn
  subscribeToObjects cid (rf >-> bindingM) setter setterId

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRol :: forall e. ContextID -> RolName -> ReactStateSetter e -> ReactStateSetterIdentifier -> MonadPerspectives (ApiEffects e) Unit
getRol cid rn setter setterId = do
  qf <- getRolFunction cid rn
  subscribeToObjects cid qf setter setterId

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRolFunction :: forall e. ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e)  (TypedTripleGetter e)
getRolFunction cid rn = do
  ctxtType <- cid ##>> contextTypeM
  m <- runMonadPerspectivesQueryCompiler ctxtType (compileElementaryQueryStep (QualifiedRol rn) (rn <> "_getter"))
  case m of
    (Left message) -> throwError $ error (show message)
    (Right id) -> constructQueryFunction id

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getProperty :: forall e. RolID -> PropertyName -> ReactStateSetter e -> ReactStateSetterIdentifier -> MonadPerspectives (ApiEffects e) Unit
getProperty rid pn setter setterId = do
  qf <- getPropertyFunction rid pn
  subscribeToObjects rid qf setter setterId

getPropertyFunction :: forall e. RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
getPropertyFunction rid pn = do
  rolType <- rid ##>> rolTypeM
  m <- runMonadPerspectivesQueryCompiler rolType (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
  case m of
    (Left message) -> throwError $ error (show message)
    (Right id) -> constructQueryFunction id
