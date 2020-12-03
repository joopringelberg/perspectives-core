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

import Control.Aff.Sockets (ConnectionProcess, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Producer, Process, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (Step(..), produce', Emitter)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Control.Plus ((<|>))
import Data.Array (elemIndex, head)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign, ForeignError, MultipleErrors, unsafeToForeign)
import Foreign.Class (decode)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ApiEffect, RequestType(..)) as Api
import Perspectives.ApiTypes (ContextSerialization(..), ContextsSerialisation(..), PropertySerialization(..), Request(..), RequestRecord, Response(..), RolSerialization(..), mkApiEffect, showRequestRecord)
import Perspectives.Assignment.Update (deleteProperty, setProperty)
import Perspectives.Checking.PerspectivesTypeChecker (checkBinding)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MP, MonadPerspectives, MonadPerspectivesTransaction, PropertyValueGetter, RoleGetter, liftToInstanceLevel, (##=), (##>), type (~~>))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (registerSupportedEffect, unregisterSupportedEffect)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol, isExternalRole, isQualifiedName, unsafeDeconstructModelName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (binding, context, contextType, getEnumeratedRoleInstances, getMyType, getRoleBinders, getUnqualifiedRoleBinders, roleType, roleType_, siblings)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Query.QueryTypes (queryFunction, secondOperand)
import Perspectives.Query.UnsafeCompiler (getRoleFunction, getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT, reduce)
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (calculation, getRoleType, kindOfRole, rangeOfRoleCalculation, rangeOfRoleCalculation', roleADT)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType, RoleKind(..), RoleType(..), ViewType, propertytype2string, roletype2string, toRoleType_)
import Perspectives.Representation.View (View, propertyReferences)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction, runMonadPerspectivesTransaction', loadModelIfMissing)
import Perspectives.SaveUserData (handleNewPeer, removeBinding, setBinding, removeAllRoleInstances, removeRoleInstance, removeContextIfUnbound)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (localRoleSpecialisation, lookForRoleType, lookForUnqualifiedRoleType, lookForUnqualifiedViewType, propertiesOfRole)
import Perspectives.User (getSystemIdentifier)
import Prelude (Unit, bind, discard, map, negate, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
foreign import createRequestEmitterImpl :: EffectFn3
  (Foreign -> Step Foreign Unit)
  (Unit -> Step Foreign Unit)
  (Emitter Effect Foreign Unit)
  Unit

-- createRequestEmitter :: Emitter Foreign Unit
createRequestEmitter :: Emitter Effect Foreign Unit -> Effect Unit
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
        , rolDescription: Nothing
        , authoringRole: Nothing}

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
        addCallback (Right (Request r@{request, subject, object, predicate, corrId, contextDescription, rolDescription, authoringRole})) =
          Request
            { request: request
            , subject
            , predicate
            , object
            , corrId
            , reactStateSetter: Just setter
            , contextDescription
            , rolDescription
            , authoringRole }
        addCallback (Left e) =
          Request
            { request: Api.WrongRequest
            , subject: ("Perspectives could not decode this request: '" <> show e <> "'")
            , predicate: ""
            , object: ""
            , corrId: -1
            , reactStateSetter: Just setter
            , contextDescription: unsafeToForeign ""
            , rolDescription: Nothing
            , authoringRole: Nothing}

        setter :: Foreign
        setter = unsafeToForeign (launchAff_ <<< (writeData connection :: Response -> Aff Boolean))

        -- marshallRequest (Left e) = WrongRequest ("Perspectives does not recognise this request: '" <> show e <> "'") (launchAff_ <<< writeData connection) "universalErrorHandler"

consumeRequest :: Consumer Request MonadPerspectives Unit
consumeRequest = forever do
  request <- await
  lift $ dispatchOnRequest (unwrap request)

dispatchOnRequest :: RequestRecord -> MonadPerspectives Unit
dispatchOnRequest r@{request, subject, predicate, object, reactStateSetter, corrId, contextDescription, rolDescription, authoringRole: as} = do
  -- The authoringRole is the System User by default.
  authoringRole <- case as of
    Nothing -> pure $ ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User"
    Just x -> getRoleType x
  case request of
    -- Given the context instance identifier and the qualified name of the RolType.
    Api.GetRolBinding -> do
      (f :: RoleGetter) <- (getRoleFunction predicate)
      registerSupportedEffect corrId setter (f >=> binding) (ContextInstance subject)
    -- Given the rolinstance;
    Api.GetBinding -> registerSupportedEffect corrId setter binding (RoleInstance subject)
    Api.GetBindingType -> registerSupportedEffect corrId setter (binding >=> roleType) (RoleInstance subject)
    -- {request: "GetRoleBinders", subject: <RoleInstance>, predicate: <EnumeratedRoleType>}
    Api.GetRoleBinders -> do
      PerspectRol{pspType} <- getPerspectRol (RoleInstance subject)
      void $ runMonadPerspectivesTransaction' false authoringRole (loadModelIfMissing $ unsafeDeconstructModelName (unwrap pspType))
      registerSupportedEffect corrId setter (getRoleBinders (EnumeratedRoleType predicate)) (RoleInstance subject)

    -- {request: "GetUnqualifiedRoleBinders", subject: <RoleInstance>, predicate: <local role name>}
    Api.GetUnqualifiedRoleBinders -> do
      PerspectRol{pspType} <- getPerspectRol (RoleInstance subject)
      void $ runMonadPerspectivesTransaction' false authoringRole (loadModelIfMissing $ unsafeDeconstructModelName (unwrap pspType))
      registerSupportedEffect corrId setter (getUnqualifiedRoleBinders predicate) (RoleInstance subject)

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
    Api.GetRoleKind -> do
      kind <- ((getEnumeratedRole $ EnumeratedRoleType subject) >>= pure <<< kindOfRole) <|> ((getCalculatedRole $ CalculatedRoleType subject) >>= pure <<< kindOfRole)
      sendResponse (Result corrId [show kind]) setter
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

    -- Looks up a property on the role instance and recursively on its binding.
    -- {request: "GetProperty", subject: rolID, predicate: propertyName, object: roleType},
    Api.GetProperty -> do
      (adt :: ADT EnumeratedRoleType) <- getRoleType object >>= rangeOfRoleCalculation
      result <- try (getDynamicPropertyGetter predicate adt)
      case result of
        Left e -> sendResponse (Error corrId ("No propertytype '" <> predicate <> "' found on roletype '" <> object <> "': " <> show e)) setter
        Right (f :: PropertyValueGetter) -> registerSupportedEffect corrId setter f (RoleInstance subject)

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
    -- E.g. send in an External Role instance, get back the type of role the current user plays in the context. Returns the role type with local name "Guest" if the user has no role.
    -- If the role is a contextrole, we treat it as if we got its binding.
    Api.GetMeForContext -> do
      roleKind <- roleType_ (RoleInstance subject) >>= getEnumeratedRole >>= pure <<< kindOfRole
      case roleKind of
        ContextRole -> registerSupportedEffect corrId setter (map toRoleType_ <<< (binding >=> context >=> getMyType)) (RoleInstance subject)
        otherwise -> registerSupportedEffect corrId setter (map toRoleType_ <<< (context >=> getMyType)) (RoleInstance subject)
    -- { request: "GetLocalRoleSpecialisation", subject: contextInstance, predicate: localAspectName}
    Api.GetLocalRoleSpecialisation -> registerSupportedEffect corrId setter (contextType >=> (liftToInstanceLevel $ localRoleSpecialisation predicate)) (ContextInstance subject)
    Api.GetUserIdentifier -> do
      sysId <- getSystemIdentifier
      sendResponse (Result corrId [sysId]) setter
    -- {request: "CreateContext", subject: contextId, predicate: roleType, object: ContextType, contextDescription: contextDescription, authoringRole: myroletype}
    -- roleType may be a local name.
    Api.CreateContext -> withLocalName predicate (ContextType object)
      \(qrolname :: RoleType) -> case qrolname of
        -- If a CalculatedRole AND a Database Query Role, do not create a role instance.
        (CR embeddingctype) -> do
          isDBQ <- isDatabaseQueryRole embeddingctype
          if isDBQ
            then withNewContext authoringRole
              \(ContextInstance id) -> lift2 $ sendResponse (Result corrId [buitenRol id]) setter
            else sendResponse (Error corrId (predicate <> " is Calculated but not a Database Query Role!")) setter
        (ENR eroltype) -> withNewContext authoringRole
          \(ContextInstance id) ->  do
            -- now bind it in a new instance of the roletype in the given context.
            -- TODO vang de nieuwe contextrol op en geef die (ook) terug?
            contextRole <- createAndAddRoleInstance eroltype subject (RolSerialization
              { id: Nothing
              , properties: PropertySerialization empty
              , binding: Just $ buitenRol id })
            lift2 $ sendResponse (Result corrId [buitenRol id, unwrap contextRole]) setter
    -- {request: "CreateContext_", subject: roleInstance, contextDescription: contextDescription, authoringRole: myroletype}
    Api.CreateContext_ -> withNewContext authoringRole
      \(ContextInstance id) -> do
        -- now bind it in the role instance.
        void $ setBinding (RoleInstance subject) (RoleInstance $ buitenRol id) Nothing
        handleNewPeer (RoleInstance subject)
        lift2 $ sendResponse (Result corrId [buitenRol id]) setter
    Api.ImportTransaction -> case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) TransactionForPeer) -> sendResponse (Error corrId (show e)) setter
      (Right tfp@(TransactionForPeer _)) -> do
        executeTransaction tfp
        sendResponse (Result corrId []) setter
    Api.ImportContexts -> case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextsSerialisation) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextsSerialisation ctxts) :: Either (NonEmptyList ForeignError) ContextsSerialisation) -> void $
        runMonadPerspectivesTransaction' false authoringRole do
          result <- runExceptT $ traverse
            (\ctxt@(ContextSerialization{ctype}) -> do
              lift $ loadModelIfMissing $ unsafeDeconstructModelName ctype
              constructContext ctxt)
            ctxts
          case result of
            Left e -> lift2 $ sendResponse (Error corrId (show e)) setter
            Right ids -> lift2 $ sendResponse (Result corrId (unwrap <$> ids)) setter
    -- {request: "RemoveRol", subject: rolID, predicate: rolName, object: contextType, authoringRole: myroletype}
    Api.RemoveRol -> do
      if (isExternalRole subject)
        -- now we must have a predicate and an object.
        then withLocalName predicate (ContextType object)
          \(qrolname :: RoleType) -> case qrolname of
            (CR ctype) -> do
              isDBQ <- isDatabaseQueryRole ctype
              if isDBQ
                then void $ runMonadPerspectivesTransaction authoringRole $ removeContextIfUnbound (RoleInstance subject) true
                else sendResponse (Error corrId ("Cannot remove an external role from non-database query role " <> (unwrap ctype))) setter
            (ENR rtype) -> sendResponse (Error corrId ("Cannot remove an external role from enumerated role " <> (unwrap rtype) <> " - use unbind instead!")) setter
        else do
          void $ runMonadPerspectivesTransaction authoringRole $ removeRoleInstance (RoleInstance subject)
          sendResponse (Result corrId []) setter
    Api.DeleteRole -> do
      -- TODO. Hanteer het geval dat subject een DBQ role is. Of misschien veiliger om er DeleteAllInstances van te maken?
      void $ runMonadPerspectivesTransaction authoringRole $ removeAllRoleInstances (EnumeratedRoleType subject) (ContextInstance object)
      sendResponse (Result corrId []) setter
    -- subject :: ContextInstance, predicate :: EnumeratedRoleType
    Api.CreateRol -> do
      if isQualifiedName predicate
        then do
          (rolInsts :: Array RoleInstance) <- runMonadPerspectivesTransaction authoringRole $ createAndAddRoleInstance (EnumeratedRoleType predicate) subject (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
          sendResponse (Result corrId (unwrap <$> rolInsts)) setter
        else sendResponse (Error corrId ("Could not create a role instance for: " <> predicate <> " in " <> subject)) setter
    -- {request: "Bind", subject: contextinstance, predicate: roleType, object: contextType, rolDescription: rolDescription, authoringRole: myroletype },
    -- Provide the binding in the rolDescription!
    -- roleType may be a local name.
    Api.Bind -> withLocalName predicate (ContextType object)
      \(qrolname :: RoleType) -> case qrolname of
        (CR ctype) -> sendResponse (Error corrId ("Cannot construct an instance of CalculatedRole '" <> unwrap ctype <> "'!")) setter
        (ENR eroltype) -> do
          RolSerialization{binding: mbnd} <- pure $ unsafePartial $ fromJust rolDescription
          case mbnd of
            Just bnd -> do
              contextInstanceId <- ContextInstance <$> (expandDefaultNamespaces subject)
              bindings <- contextInstanceId ##= (getEnumeratedRoleInstances eroltype >=> binding)
              if isJust $ elemIndex (RoleInstance bnd) bindings
                then sendResponse (Error corrId ("Cannot not bind the same role instance twice in the same role type")) setter
                else do
                  rol <- runMonadPerspectivesTransaction authoringRole $ createAndAddRoleInstance eroltype subject (unsafePartial $ fromJust rolDescription)
                  sendResponse (Result corrId (unwrap <$> rol)) setter
            Nothing -> do
              rol <- runMonadPerspectivesTransaction authoringRole $ createAndAddRoleInstance eroltype subject (unsafePartial $ fromJust rolDescription)
              sendResponse (Result corrId (unwrap <$> rol)) setter
    -- {request: "Bind_", subject: binder, object: binding, authoringRole: myroletype},
    Api.Bind_ -> catchError
      do
        -- Find the other role instances of the same type as subject in this context and check whether one of them
        -- binds the object.
        bindings <- (RoleInstance subject) ##= siblings >=> binding
        if isJust $ elemIndex (RoleInstance object) bindings
          then sendResponse (Error corrId ("Cannot not bind the same role instance twice in the same role type")) setter
          else do
            void $ runMonadPerspectivesTransaction authoringRole
              do
                void $ setBinding (RoleInstance subject) (RoleInstance object) Nothing
                handleNewPeer (RoleInstance subject)
            sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- {request: "RemoveBinding", subject: rolID}
    Api.RemoveBinding -> catchError
      do
        -- Remove the binding from subject. We don't know whether that binding will be removed itself, hence bindingRemoved=false.
        void $ runMonadPerspectivesTransaction authoringRole $ removeBinding false (RoleInstance subject)
        sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Check whether a role exists for contextType with the localRolName and whether it allows RolID as binding.
    -- {request: "CheckBinding", subject: contextType, predicate: localRolName, object: rolInstance}
    Api.CheckBinding -> withLocalName predicate (ContextType subject)
      \(typeOfRolToBindTo :: RoleType) -> do
        PerspectRol{pspType} <- getPerspectRol (RoleInstance object)
        void $ runMonadPerspectivesTransaction' false authoringRole (loadModelIfMissing $ unsafeDeconstructModelName (unwrap pspType))
        ok <- checkBinding typeOfRolToBindTo (RoleInstance object)
        sendResponse (Result corrId [(show ok)]) setter
    Api.SetProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (setProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate) [(Value object)])
        sendResponse (Result corrId ["ok"]) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    Api.DeleteProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (deleteProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate))
        sendResponse (Result corrId ["Ok"]) setter)
        (\e -> sendResponse (Error corrId (show e)) setter)
    Api.Unsubscribe -> unregisterSupportedEffect corrId
    Api.WrongRequest -> sendResponse (Error corrId subject) setter
    otherwise -> sendResponse (Error corrId ("Perspectives could not handle this request: '" <> (showRequestRecord r) <> "'")) (mkApiEffect reactStateSetter)
  where
    setter = (mkApiEffect reactStateSetter)

    withLocalName :: String -> ContextType -> (RoleType -> MonadPerspectives Unit) -> MonadPerspectives Unit
    withLocalName localRoleName contextType effect = do
      qrolNames <- runArrayT $ lookForUnqualifiedRoleType localRoleName contextType
      case head qrolNames of
        (Just (qrolname :: RoleType)) -> effect qrolname
        Nothing -> do
          qrolnames' <- runArrayT $ lookForRoleType localRoleName contextType
          case head qrolnames' of
            Nothing -> sendResponse (Error corrId ("Cannot find Rol with name '" <> localRoleName <> "' on context type '" <> unwrap contextType <> "'!")) setter
            (Just (qrolname :: RoleType)) -> effect qrolname

    withNewContext :: RoleType -> (ContextInstance -> MonadPerspectivesTransaction Unit) -> MonadPerspectives Unit
    withNewContext authoringRole effect = case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextSerialization) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextSerialization cd@{ctype}) :: Either (NonEmptyList ForeignError) ContextSerialization) -> do
        void $ runMonadPerspectivesTransaction authoringRole do
          g <- liftEffect guid
          ctxt <- runExceptT $ constructContext (ContextSerialization cd {id = "model:User$c" <> (show g)})
          case ctxt of
            (Left messages) -> lift2 $ sendResponse (Error corrId (show messages)) setter
            (Right ctxtId) -> effect ctxtId

-- | Tests whether we have a sequence of which the last part applies an ExternalCoreRoleGetter function.
isDatabaseQueryRole :: CalculatedRoleType -> MonadPerspectives Boolean
isDatabaseQueryRole cr = do
  calculatedRole <- getPerspectType cr
  qfd <- calculation calculatedRole
  case queryFunction qfd of
    (BinaryCombinator SequenceF) -> case queryFunction <$> secondOperand qfd of
      Just (ExternalCoreRoleGetter _) -> roleADT calculatedRole >>= isExternal
      otherwise -> pure false
    otherwise -> pure false
  where
    isExternal = reduce (pure <<< isExternalRole <<< unwrap)

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter = Array String -> Effect Unit

type QueryUnsubscriber e = Effect Unit

-- | Apply an ApiEffect to a Response, in effect sending it through the API to the caller.
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse r ae = liftEffect $ ae r
