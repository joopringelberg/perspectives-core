module Perspectives.Api where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff) as Promise
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Perspectives.CoreTypes (TypedTripleGetter(..), NamedFunction(..), Triple(..), TripleRef(..), MonadPerspectives)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.Effects (AjaxAvarCache, ApiEffects, REACT)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolName)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Property (getContextTypeF)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.QueryEffect ((~>))
import Perspectives.SystemQueries (binding)
import Perspectives.TripleAdministration (unRegisterTriple)
import Perspectives.TripleGetter (constructRolGetter)
import Prelude (Unit, bind, const, discard, flip, pure, unit, void, ($), (<<<), (<>), (>=>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest e =
  GetRolBinding ContextID RolName (ReactStateSetter e)
  | ShutDown
  | WrongRequest

data ApiResponse e = Unsubscriber (QueryUnsubscriber e)
  | Error

type RequestRecord e =
  { request :: String
  , contextID :: String
  , rolName :: String
  , reactStateSetter :: ReactStateSetter e}

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
  "GetRolBinding" -> GetRolBinding r.contextID r.rolName r.reactStateSetter
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
  if (isShutDown req)
    then pure unit
    else do
      case req of
        (GetRolBinding cid rn setter) -> do
          unsubscriber <- getRolBinding cid rn setter
          send (Unsubscriber unsubscriber)
        WrongRequest -> send Error
        ShutDown -> send Error -- this case will never occur!
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
type ReactStateSetter e = Array String -> Eff (AjaxAvarCache (ref :: REF, react :: REACT | e)) (NullOrUndefined Unit)

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
  contextType <- getContextTypeF cid
  getQuery cid (constructRolGetter rn >-> binding) setter

-- | Retrieve the rol from the context, subscribe to it.
getRol :: forall e. ContextID -> RolName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getRol cid rn setter = do
  contextType <- getContextTypeF cid
  getQuery cid (constructRolGetter rn) setter
