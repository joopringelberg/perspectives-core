module Perspectives.Api where

import Control.Monad.Aff (launchAff_, throwError, error)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff) as Promise
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), Triple(..), TripleRef(..), TypedTripleGetter(..), runMonadPerspectivesQueryCompiler)
import Perspectives.DataTypeTripleGetters (bindingM, contextM, contextTypeM, rolTypeM)
import Perspectives.Effects (AjaxAvarCache, ApiEffects, REACT)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName, RolID, RolName, ViewName)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.ModelBasedTripleGetters (propertyReferentiesM)
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryEffect ((~>))
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.RunMonadPerspectivesQuery ((##), (##>>))
import Perspectives.TripleAdministration (unRegisterTriple)
import Perspectives.TripleGetterComposition ((>->))
import Prelude (Unit, bind, const, discard, flip, pure, show, unit, void, ($), (<<<), (<>), (>=>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest e =
  GetRolBinding ContextID RolName (ReactStateSetter e)
  | GetBinding RolID (ReactStateSetter e)
  | GetBindingType RolID (ReactStateSetter e)
  | GetRolContext RolID (ReactStateSetter e)
  | GetContextType ContextID (ReactStateSetter e)
  | GetRol ContextID RolName (ReactStateSetter e)
  | GetProperty RolName PropertyName (ReactStateSetter e)
  | GetViewProperties ViewName (ReactStateSetter e)
  | ShutDown
  | WrongRequest -- Represents a request from the client Perspectives does not recognize.

data ApiResponse e = Unsubscriber (QueryUnsubscriber e)
  | Error String

type RequestRecord e =
  { request :: String
  , subject :: String
  , predicate :: String
  , reactStateSetter :: ReactStateSetter e}

showRequestRecord :: forall e. RequestRecord e -> String
showRequestRecord {request, subject, predicate} = "{" <> request <> ", " <> subject <> ", " <> predicate <> "}"

type ApiChannel e =
  { request :: AVar (RequestRecord e)
  , response :: AVar (ApiResponse e)
  , getter :: AVar (ApiRequest e) -> Eff (avar :: AVAR) (Promise.Promise (ApiRequest e))
  , setter :: (ApiRequest e) -> AVar (ApiRequest e) -> Eff (avar :: AVAR) Unit }

-- | Create a source of ApiRequests and a sink for ApiResponses.
setUpApi :: forall e. MonadPerspectives (ApiEffects e) Unit
setUpApi = do
  (request :: AVar (RequestRecord e)) <- lift makeEmptyVar
  (response :: AVar (ApiResponse e)) <- lift makeEmptyVar
  void $ liftEff $ connect
    { request: request
    , response: response
    , getter: Promise.fromAff <<< takeVar
    , setter: \val var -> launchAff_ $ putVar val var
    }
  dispatch request response

foreign import connect :: forall e1 e2. ApiChannel e1 -> Eff (react:: REACT | e2) (NullOrUndefined Unit)

-- | The client of Perspectives sends a record of arbitrary form that we
-- | try to fit into ApiRequest.
marshallRequestRecord :: forall e. RequestRecord e -> ApiRequest e
marshallRequestRecord r@{request} = case request of
  "GetRolBinding" -> GetRolBinding r.subject r.predicate r.reactStateSetter
  "GetBinding" -> GetBinding r.subject r.reactStateSetter
  "GetBindingType" -> GetBindingType r.subject r.reactStateSetter
  "GetRol" -> GetRol r.subject r.predicate r.reactStateSetter
  "GetRolContext" -> GetRolContext r.subject r.reactStateSetter
  "GetContextType" -> GetContextType r.subject r.reactStateSetter
  "GetProperty" -> GetProperty r.subject r.predicate r.reactStateSetter
  "GetViewProperties" -> GetViewProperties r.subject r.reactStateSetter
  "ShutDown" -> ShutDown
  otherwise -> WrongRequest

-----------------------------------------------------------
-- REQUEST AND RESPONSE
-----------------------------------------------------------
-- | The dispatch function listens on the ApiRequest source and responds on the ApiResponse sink.
-- | If a correct request has been sent, it will be responded to.
-- | Otherwise, an error response will be given.
dispatch :: forall e. AVar (RequestRecord e) -> AVar (ApiResponse e) -> MonadPerspectives (ApiEffects e) Unit
dispatch request response = do
  reqr <- lift $ takeVar request
  let req = marshallRequestRecord reqr
  if (isShutDown req) -- Catch ShutDown here, so...
    then pure unit
    else do
      case req of
        (GetRolBinding cid rn setter) -> do
          unsubscriber <- getRolBinding cid rn setter
          send (Unsubscriber unsubscriber)
        (GetBinding rid setter) -> do
          unsubscriber <- getQuery rid bindingM setter
          send (Unsubscriber unsubscriber)
        (GetBindingType rid setter) -> do
          unsubscriber <- getQuery rid (bindingM >-> rolTypeM) setter
          send (Unsubscriber unsubscriber)
        (GetRol cid rn setter) -> do
          unsubscriber <- getRol cid rn setter
          send (Unsubscriber unsubscriber)
        (GetRolContext rid setter) -> do
          unsubscriber <- getQuery rid contextM setter
          send (Unsubscriber unsubscriber)
        (GetContextType rid setter) -> do
          unsubscriber <- getQuery rid contextTypeM setter
          send (Unsubscriber unsubscriber)
        (GetProperty rid pn setter) -> do
          unsubscriber <- getProperty rid pn setter
          send (Unsubscriber unsubscriber)
        (GetViewProperties vn setter) -> do
          unsubscriber <- getQuery vn (propertyReferentiesM >-> bindingM >-> contextM) setter
          send (Unsubscriber unsubscriber)
        WrongRequest -> send $ Error ("This request is invalid: " <> showRequestRecord reqr)
        ShutDown -> send $ Error "shutdown" -- ...this case will never occur!
      -- Recursive call.
      dispatch request response
  where
    send :: ApiResponse e -> MonadPerspectives (ApiEffects e) Unit
    send = lift <<< flip putVar response

    isShutDown :: ApiRequest e -> Boolean
    isShutDown req = case req of
      ShutDown -> true
      otherwise -> false

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter e = Array String -> Eff (AjaxAvarCache (react :: REACT | e)) (NullOrUndefined Unit)

type QueryUnsubscriber e = Eff (gm :: GLOBALMAP | e) Unit

-- | Runs a the query and adds the ReactStateSetter to the result.
-- | Returns a function of no arguments that the external program can use to unsubscribe the ReactStateSetter.
getQuery :: forall e. ContextID -> TypedTripleGetter (react :: REACT | e) -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getQuery cid query@(TypedTripleGetter qn _) setter = do
  (Triple{subject, predicate}) <- cid ## query ~> NamedFunction (cid <> qn) (setter >=> pure <<< const unit)
  pure $ unRegisterTriple $ TripleRef {subject, predicate}

-- | Retrieve the binding of the rol from the context, subscribe to it.
getRolBinding :: forall e. ContextID -> RolName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getRolBinding cid rn setter = do
  rf <- getRolFunction cid rn
  getQuery cid (rf >-> bindingM) setter

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRol :: forall e. ContextID -> RolName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getRol cid rn setter = do
  qf <- getRolFunction cid rn
  getQuery cid qf setter

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRolFunction :: forall e. ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e)  (TypedTripleGetter e)
getRolFunction cid rn = do
  ctxtType <- cid ##>> contextTypeM
  m <- runMonadPerspectivesQueryCompiler ctxtType (compileElementaryQueryStep (QualifiedRol rn) (rn <> "_getter"))
  case m of
    (Left message) -> throwError $ error (show message)
    (Right id) -> constructQueryFunction id

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getProperty :: forall e. RolID -> PropertyName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getProperty rid pn setter = do
  qf <- getPropertyFunction rid pn
  getQuery rid qf setter

getPropertyFunction :: forall e. RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
getPropertyFunction rid pn = do
  rolType <- rid ##>> rolTypeM
  m <- runMonadPerspectivesQueryCompiler rolType (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
  case m of
    (Left message) -> throwError $ error (show message)
    (Right id) -> constructQueryFunction id
