module Perspectives.Api where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff) as Promise
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Perspectives.CoreTypes (TypedTripleGetter(..), NamedFunction(..), Triple(..), TripleRef(..), MonadPerspectives)
import Perspectives.DataTypeTripleGetters (bindingM, rolTypeM)
import Perspectives.Effects (AjaxAvarCache, ApiEffects, REACT)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolName, PropertyName, RolID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.QueryEffect ((~>))
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.TripleAdministration (unRegisterTriple)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (constructRolGetter, constructRolPropertyGetter)
import Prelude (class Show, Unit, bind, const, discard, flip, pure, unit, void, ($), (<<<), (<>), (>=>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest e =
  GetRolBinding ContextID RolName (ReactStateSetter e)
  | GetBinding RolID String (ReactStateSetter e)
  | GetBindingType RolID String (ReactStateSetter e)
  | GetRol ContextID RolName (ReactStateSetter e)
  | GetProperty RolName PropertyName (ReactStateSetter e)
  | ShutDown
  | WrongRequest -- Represents a request from the client Perspectives does not recognize.

instance showApiRequest :: Show (ApiRequest e) where
  show (GetRolBinding cid rn _) = "{GetRolBinding " <> cid <> " " <> rn <> "}"
  show (GetBinding rid _ _) = "{GetBinding" <> rid <> "}"
  show (GetBindingType rid _ _) = "{GetBindingType" <> rid <> "}"
  show (GetRol cid rn _) = "{GetRol " <> cid <> " " <> rn <> "}"
  show (GetProperty rn pn _) = "{GetProperty " <> rn <> " " <> pn <> "}"
  show ShutDown = "ShutDown"
  show WrongRequest = "WrongRequest"

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
  "GetBinding" -> GetBinding r.subject "" r.reactStateSetter
  "GetBindingType" -> GetBindingType r.subject "" r.reactStateSetter
  "GetRol" -> GetRol r.subject r.predicate r.reactStateSetter
  "GetProperty" -> GetProperty r.subject r.predicate r.reactStateSetter
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
        (GetBinding rid _ setter) -> do
          unsubscriber <- getBinding rid setter
          send (Unsubscriber unsubscriber)
        (GetBindingType rid _ setter) -> do
          unsubscriber <- getBindingType rid setter
          send (Unsubscriber unsubscriber)
        (GetRol cid rn setter) -> do
          unsubscriber <- getRol cid rn setter
          send (Unsubscriber unsubscriber)
        (GetProperty rid pn setter) -> do
          unsubscriber <- getProperty rid pn setter
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
  getQuery cid (constructRolGetter rn >-> bindingM) setter

-- | Retrieve the binding of the rol that is passed in, subscribe to it.
getBinding :: forall e. ContextID -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getBinding cid setter = do
  getQuery cid bindingM setter

-- | Retrieve the type of the binding of the rol that is passed in, subscribe to it.
getBindingType :: forall e. ContextID -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getBindingType cid setter = do
  getQuery cid (bindingM >-> rolTypeM) setter

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getRol :: forall e. ContextID -> RolName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getRol cid rn setter = do
  getQuery cid (constructRolGetter rn) setter

-- | Retrieve the rol from the context, subscribe to it. NOTE: only for ContextInRol, not BinnenRol or BuitenRol.
getProperty :: forall e. RolID -> PropertyName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getProperty rid pn setter = do
  getQuery rid (constructRolPropertyGetter pn) setter
