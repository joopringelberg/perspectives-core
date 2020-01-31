-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Api where

import Control.Aff.Sockets (ConnectionProcess, EmitFunction, Emitter, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Producer, Process, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (Step(..), produce')
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, null)
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
import Perspectives.ApiTypes (ApiEffect, RequestType(..)) as Api
import Perspectives.ApiTypes (ContextSerialization(..), Request(..), RequestRecord, Response(..), RolSerialization(..), mkApiEffect, showRequestRecord)
import Perspectives.Assignment.Update (addRol, removeBinding, removeRolFromContext, saveEntiteit, setBinding, setProperty)
import Perspectives.BasicConstructors (constructAnotherRol, constructContext)
import Perspectives.Checking.PerspectivesTypeChecker (checkBinding)
import Perspectives.ContextAndRole (addRol_gevuldeRollen)
import Perspectives.CoreTypes (MonadPerspectives, PropertyValueGetter, RoleGetter, (##>), (##=), MP)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (registerSupportedEffect, unregisterSupportedEffect)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, context, contextType, roleType)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Query.Compiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Role (rangeOfRoleCalculation')
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType, RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.View (View, propertyReferences)
import Perspectives.RunMonadPerspectivesTransaction (getMe, runMonadPerspectivesTransaction)
import Perspectives.SaveUserData (removeRoleInstance, removeContextInstance, saveContextInstance, saveAndConnectRoleInstance)
import Perspectives.Types.ObjectGetters (lookForUnqualifiedRoleType, lookForUnqualifiedViewType, propertiesOfRole)
import Prelude (Unit, bind, pure, show, unit, void, ($), (<<<), (<>), discard, negate, (>=>), (==), (<$>), (>>=))

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
        setter = unsafeToForeign (launchAff_ <<< (writeData connection :: Response -> Aff Boolean))

        -- marshallRequest (Left e) = WrongRequest ("Perspectives does not recognise this request: '" <> show e <> "'") (launchAff_ <<< writeData connection) "universalErrorHandler"

consumeRequest :: Consumer Request MonadPerspectives Unit
consumeRequest = forever do
  request <- await
  lift $ dispatchOnRequest (unwrap request)

dispatchOnRequest :: RequestRecord -> MonadPerspectives Unit
dispatchOnRequest r@{request, subject, predicate, object, reactStateSetter, corrId, contextDescription, rolDescription} =
  case request of
    -- Given the context instance identifier and the qualified name of the RolType.
    Api.GetRolBinding -> do
      (f :: RoleGetter) <- (getRoleFunction predicate)
      registerSupportedEffect corrId setter (f >=> binding) (ContextInstance subject)
    -- Given the rolinstance;
    Api.GetBinding -> registerSupportedEffect corrId setter binding (RoleInstance subject)
    Api.GetBindingType -> registerSupportedEffect corrId setter (binding >=> roleType) (RoleInstance subject)
    Api.GetRol -> do
      (f :: RoleGetter) <- (getRoleFunction predicate)
      registerSupportedEffect corrId setter f (ContextInstance subject)
    Api.GetUnqualifiedRol -> do
      mctype <- (ContextInstance subject) ##> contextType
      case mctype of
        Nothing -> sendResponse (Error corrId ("No contexttype found for '" <> subject <> "'")) setter
        (Just (ctype :: ContextType)) -> do
          rtypes <- runArrayT $ lookForUnqualifiedRoleType predicate ctype
          case head rtypes of
            Nothing -> sendResponse (Error corrId ("No roletype found for '" <> predicate <> "' on '" <> subject <> "'")) setter
            (Just (rtype :: RoleType)) -> do
              (f :: RoleGetter) <- (getRoleFunction (roletype2string rtype))
              registerSupportedEffect corrId setter f (ContextInstance subject)
    Api.GetRolContext -> registerSupportedEffect corrId setter context (RoleInstance subject)
    Api.GetContextType -> registerSupportedEffect corrId setter contextType (ContextInstance subject)
    Api.GetRolType -> registerSupportedEffect corrId setter roleType (RoleInstance subject)
    -- {request: "GetUnqualifiedRolType", subject: contextType, predicate: localRolName}
    Api.GetUnqualifiedRolType -> do
      rtypes <- runArrayT $ lookForUnqualifiedRoleType predicate (ContextType subject)
      case head rtypes of
        Nothing -> sendResponse (Error corrId ("No roletype found for '" <> predicate <> "' on '" <> subject <> "'")) setter
        (Just rtype) -> sendResponse (Result corrId [roletype2string rtype]) setter
    -- Api.GetUnqualifiedRolType -> do
    --   mctype <- (ContextInstance subject) ##> contextType
    --   case mctype of
    --     Nothing -> sendResponse (Error corrId ("No contexttype found for '" <> subject <> "'")) setter
    --     (Just (ctype :: ContextType)) -> do
    --       rtypes <- runArrayT $ lookForUnqualifiedRoleType predicate ctype
    --       case head rtypes of
    --         Nothing -> sendResponse (Error corrId ("No roletype found for '" <> predicate <> "' on '" <> subject <> "'")) setter
    --         (Just rtype) -> sendResponse (Result corrId [roletype2string rtype]) setter
    Api.GetProperty -> do
      (f :: PropertyValueGetter) <- (getPropertyFunction predicate)
      registerSupportedEffect corrId setter f (RoleInstance subject)
      -- For a Role and a View, return the properties in that View.
      -- If View equals "allProperties", return all properties of the Role.
    -- {request: "GetViewProperties", subject: RoleType, predicate: unqualifiedViewName}
    -- Note that RoleType actually is the qualified name of either an EnumeratedRoleType or a CalculatedRoleType,
    -- not the Newtype.
    Api.GetViewProperties -> if (predicate == "allProperties")
      then do
        props <- runArrayT $ propertiesOfRole subject
        sendResponse (Result corrId (propertytype2string <$> props)) setter
      else do
        adt <- rangeOfRoleCalculation' subject
        views <- runArrayT $ lookForUnqualifiedViewType predicate adt
        case head views of
          Nothing -> sendResponse (Error corrId ("View '" <> predicate <> "' is not available for role '" <> subject <> "'.")) setter
          -- NOTE: we arbitrarily take the first matching View.
          (Just (v :: ViewType)) -> do
            (props :: Array PropertyType) <- ((getPerspectType v) :: MP View) >>= pure <<< propertyReferences
            sendResponse (Result corrId (propertytype2string <$> props)) setter
    Api.GetMeForContext -> do
      me <- (RoleInstance subject) ##= context >=> getMe >=> roleType
      if null me
        then sendResponse (Error corrId ("No role for user in context instance '" <> subject <> "'!")) setter
        else sendResponse (Result corrId (unwrap <$> me)) setter
    Api.CreateContext -> case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextSerialization) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextSerialization cd) :: Either (NonEmptyList ForeignError) ContextSerialization) -> do
        ctxt <- constructContext (ContextSerialization cd {id = "model:User$c" <> (show $ guid unit)})
        case ctxt of
          (Left messages) -> sendResponse (Error corrId (show messages)) setter
          (Right id) -> do
            void $ runMonadPerspectivesTransaction $ saveContextInstance id
            sendResponse (Result corrId [buitenRol $ unwrap id]) setter
    Api.DeleteContext -> do
      void $ runMonadPerspectivesTransaction $ removeContextInstance (ContextInstance subject)
      sendResponse (Result corrId []) setter
    -- {request: "RemoveRol", subject: contextID, predicate: rolName, object: rolID}
    Api.RemoveRol -> do
        void $ runMonadPerspectivesTransaction do
          removeRolFromContext (ContextInstance subject) (EnumeratedRoleType predicate) [(RoleInstance object)]
          removeRoleInstance (RoleInstance object)
        sendResponse (Result corrId []) setter
    -- TODO: DeleteRol
    Api.CreateRol -> do
      (rolInsts :: Array RoleInstance) <- runMonadPerspectivesTransaction do
        desc@(RolSerialization{binding}) <- pure (unsafePartial $ fromJust rolDescription)
        role@(PerspectRol{_id}) <- lift $ lift $ constructAnotherRol (EnumeratedRoleType predicate) subject desc
        case binding of
          Just bndg -> do
            b <- lift $ lift $ getPerspectRol (RoleInstance bndg)
            saveEntiteit (RoleInstance bndg) (addRol_gevuldeRollen b (EnumeratedRoleType predicate) _id)
          Nothing -> pure unit

        saveAndConnectRoleInstance (identifier role)
        pure (identifier role)
      sendResponse (Result corrId (unwrap <$> rolInsts)) setter
    -- Check whether a role exists for ContextDef with the localRolName and then create a new instance of it according to the rolDescription.
    -- subject :: Context, predicate :: localRolName, object :: ContextDef. rolDescription must be present!
    Api.CreateRolWithLocalName -> do
      qrolnames <- runArrayT $ lookForUnqualifiedRoleType predicate (ContextType object)
      case head qrolnames of
        Nothing -> sendResponse (Error corrId ("Cannot find Rol with local name '" <> predicate <> "' on context type '" <> object <> "'!")) setter
        (Just (qrolname :: RoleType)) -> case qrolname of
          -- (CR ctype) -> pure unit
          (CR ctype) -> sendResponse (Error corrId ("Cannot construct an instance of CalculatedRole '" <> unwrap ctype <> "'!")) setter
          (ENR eroltype) -> do
            rol <- runMonadPerspectivesTransaction do
              desc@(RolSerialization{binding}) <- pure (unsafePartial $ fromJust rolDescription)
              role@(PerspectRol{_id}) <- lift $ lift $ constructAnotherRol eroltype subject desc
              -- TODO. Set the inverse administration.
              case binding of
                Just bndg -> do
                  b <- lift $ lift $ getPerspectRol (RoleInstance bndg)
                  saveEntiteit (RoleInstance bndg) (addRol_gevuldeRollen b (EnumeratedRoleType predicate) _id)
                Nothing -> pure unit
              saveAndConnectRoleInstance (identifier role)
              pure $ (identifier :: PerspectRol -> RoleInstance) role
            sendResponse (Result corrId (unwrap <$> rol)) setter
    Api.AddRol -> catchError
      do
        void $ runMonadPerspectivesTransaction $ addRol (ContextInstance subject) (EnumeratedRoleType predicate) [(RoleInstance object)]
        sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    Api.SetProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction (setProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate) [(Value object)])
        sendResponse (Result corrId ["ok"]) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- {request: "SetBinding", subject: rolID, object: bindingID},
    Api.SetBinding -> catchError
      do
        void $ runMonadPerspectivesTransaction $ setBinding (RoleInstance subject) (RoleInstance object)
        sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- {request: "RemoveBinding", subject: rolID}
    Api.RemoveBinding -> catchError
      do
        void $ runMonadPerspectivesTransaction $ removeBinding (RoleInstance subject)
        sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Create a new instance of the roletype RolDef in Context and fill the role with RolID.
    -- subject :: Context, predicate :: RolDef, object :: RolID
    Api.BindInNewRol -> catchError
      do
        void $ runMonadPerspectivesTransaction do
          desc@(RolSerialization{binding}) <- pure (unsafePartial $ fromJust rolDescription)
          role@(PerspectRol{_id}) <- lift $ lift $ constructAnotherRol (EnumeratedRoleType predicate) subject desc
          -- TODO. Set the inverse administration.
          case binding of
            Just bndg -> do
              b <- lift $ lift $ getPerspectRol (RoleInstance bndg)
              saveEntiteit (RoleInstance bndg) (addRol_gevuldeRollen b (EnumeratedRoleType predicate) _id)
            Nothing -> pure unit
          saveAndConnectRoleInstance (identifier role)
          setBinding (identifier role) (RoleInstance object)
        sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Check whether a role exists for ContextDef with the localRolName and whether it allows RolID as binding.
    -- subject :: ContextDef, predicate :: localRolName, object :: RolID
    Api.CheckBinding -> do
      typeOfRolesToBindTo <- runArrayT $ lookForUnqualifiedRoleType predicate (ContextType subject)
      case head typeOfRolesToBindTo of
        Nothing -> sendResponse (Error corrId ("No roltype found for '" <> predicate <> "'.")) setter
        (Just typeOfRolToBindTo) -> do
          ok <- checkBinding typeOfRolToBindTo (RoleInstance object)
          sendResponse (Result corrId [(show ok)]) setter
    Api.Unsubscribe -> unregisterSupportedEffect corrId
    Api.WrongRequest -> sendResponse (Error corrId subject) setter
    otherwise -> sendResponse (Error corrId ("Perspectives could not handle this request: '" <> (showRequestRecord r) <> "'")) (mkApiEffect reactStateSetter)
  where
    setter = (mkApiEffect reactStateSetter)

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter = Array String -> Effect Unit

type QueryUnsubscriber e = Effect Unit

-- | Apply an ApiEffect to a Response, in effect sending it through the API to the caller.
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse r ae = liftEffect $ ae r
