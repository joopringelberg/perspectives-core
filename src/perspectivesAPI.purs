module Perspectives.Api where

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff) as Promise
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Perspectives.Effects (AjaxAvarCache, ApiEffects, REACT)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolName)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.QueryEffect ((~>))
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleRef(..), unRegisterTriple)
import Perspectives.TripleGetter (constructRolGetter, (##))
import Prelude (Unit, bind, const, discard, flip, pure, unit, void, ($), (<<<), (<>), (>=>))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
data ApiRequest e =
  GetRol ContextID RolName (ReactStateSetter e)
  | ShutDown

data ApiResponse e = Unsubscriber (QueryUnsubscriber e)
  | Error

type RequestRecord = forall r. {|r}

type ApiChannel e =
  { request :: AVar RequestRecord
  , response :: AVar (ApiResponse e)
  , getter :: AVar (ApiRequest e) -> Eff (avar :: AVAR) (Promise.Promise (ApiRequest e))
  , setter :: (ApiRequest e) -> AVar (ApiRequest e) -> Eff (avar :: AVAR) Unit }

-- | Create a source of ApiRequests and a sink for ApiResponses.
setUpApi :: forall e. MonadPerspectives (ApiEffects e) Unit
setUpApi = do
  (request :: AVar RequestRecord) <- lift makeEmptyVar
  (response :: AVar (ApiResponse e)) <- lift makeEmptyVar
  dispatch request response
  void $ liftEff $ connect
    { request: request
    , response: response
    , getter: Promise.fromAff <<< takeVar
    , setter: \val var -> launchAff_ $ putVar val var
    }

foreign import connect :: forall e1 e2. ApiChannel e1 -> Eff (react:: REACT | e2) (NullOrUndefined Unit)

-- | The client of Perspectives sends a record of arbitrary form that we
-- | try to fit into ApiRequest.
marshallRequestRecord :: forall r e. {|r} -> ApiRequest e
marshallRequestRecord r = ShutDown

-----------------------------------------------------------
-- REQUEST AND RESPONSE
-----------------------------------------------------------
-- | The dispatch function listens on the ApiRequest source and responds on the ApiResponse sink.
-- | If a correct request has been sent, it will be responded to.
-- | Otherwise, an error response will be given.
dispatch :: forall e. AVar RequestRecord -> AVar (ApiResponse e) -> MonadPerspectives (ApiEffects e) Unit
dispatch request response = do
  reqr <- lift $ takeVar request
  let req = marshallRequestRecord reqr
  if (isShutDown req)
    then pure unit
    else do
      case req of
        (GetRol cid rn setter) -> do
          unsubscriber <- getRol cid rn setter
          send (Unsubscriber unsubscriber)
        otherwise -> send Error
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

-- | Runs a query that retrieves the rol from the context. Adds the ReactStateSetter to the result.
-- | Returns a function of no arguments that the external program can use to unsubscribe.
getRol :: forall e. ContextID -> RolName -> ReactStateSetter e -> MonadPerspectives (ApiEffects e) (QueryUnsubscriber e)
getRol cid rn setter = do
  (Triple{subject, predicate}) <- cid ## constructRolGetter rn ~> NamedFunction (cid <> rn) (setter >=> pure <<< const unit)
  pure $ unRegisterTriple $ TripleRef {subject, predicate}
