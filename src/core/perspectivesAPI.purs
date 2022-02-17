-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Api where

-- import Control.Aff.Sockets (ConnectionProcess, connectionConsumer, connectionProducer, dataProducer, defaultTCPOptions, writeData)
import Control.Coroutine (Consumer, Producer, await, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (Step(..), produce', Emitter)
import Control.Monad.AvarMonadAsk (gets)
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
import Effect.Aff (Aff, catchError, try)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign, ForeignError, unsafeToForeign)
import Foreign.Class (decode)
import Foreign.Generic (encodeJSON)
import Foreign.Object (empty)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ApiEffect, RequestType(..)) as Api
import Perspectives.ApiTypes (ContextSerialization(..), ContextsSerialisation(..), PropertySerialization(..), Request(..), RequestRecord, Response(..), RolSerialization(..), mkApiEffect, showRequestRecord)
import Perspectives.Assignment.Update (deleteProperty, setPreferredUserRoleType, setProperty)
import Perspectives.Checking.PerspectivesTypeChecker (checkBinding)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CompileAssignment (compileAssignment)
import Perspectives.CompileRoleAssignment (compileAssignmentFromRole, scheduleContextRemoval, scheduleRoleRemoval)
import Perspectives.CoreTypes (MP, MonadPerspectives, MonadPerspectivesTransaction, PropertyValueGetter, RoleGetter, liftToInstanceLevel, (##=), (##>), (##>>), (###>))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (registerSupportedEffect, unregisterSupportedEffect)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Fuzzysort (matchIndexedContextNames)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, isExternalRole, isQualifiedName, unsafeDeconstructModelName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (binding, context, contextType, getContextActions, getFilledRoles, getRoleName, roleType, roleType_, siblings)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.PerspectivesState (addBinding, pushFrame, restoreFrame)
import Perspectives.Query.UnsafeCompiler (getAllMyRoleTypes, getDynamicPropertyGetter, getDynamicPropertyGetterFromLocalName, getMeInRoleAndContext, getMyType, getRoleFunction, getRoleInstances)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Action (Action(..)) as ACTION
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getContext, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (getRoleType, kindOfRole, rangeOfRoleCalculation, roleKindOfRoleType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType, RoleKind(..), RoleType(..), ViewType, propertytype2string, roletype2string, toRoleType_)
import Perspectives.Representation.View (View, propertyReferences)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction, runMonadPerspectivesTransaction', loadModelIfMissing)
import Perspectives.SaveUserData (handleNewPeer, removeAllRoleInstances, removeBinding, removeContextIfUnbound, setBinding, setFirstBinding)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.TypePersistence.PerspectiveSerialisation (perspectiveForContextAndUser, perspectivesForContextAndUser)
import Perspectives.Types.ObjectGetters (findPerspective, getAction, getContextAction, isDatabaseQueryRole, localRoleSpecialisation, lookForRoleType, lookForUnqualifiedRoleType, lookForUnqualifiedViewType, propertiesOfRole, string2RoleType)
import Prelude (Unit, bind, discard, identity, map, negate, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), eq)
import Simple.JSON (read)

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
        , predicate: (unsafeStringify f)
        , object: ""
        , corrId: -1
        , reactStateSetter: Just $ unsafeToForeign (\x -> pure unit :: Aff Unit)
        , contextDescription: unsafeToForeign ""
        , rolDescription: Nothing
        , authoringRole: Nothing}

consumeRequest :: Consumer Request MonadPerspectives Unit
consumeRequest = forever do
  (Request request@{corrId, reactStateSetter}) <- await
  lift (catchError
    (dispatchOnRequest request)
    (\e -> do
      logPerspectivesError (ApiErrorBoundary ((showRequestRecord request) <> ": " <> (show e)))
      sendResponse (Error corrId ("Request could not be handled, because: " <> (show e))) (mkApiEffect reactStateSetter))
    )

dispatchOnRequest :: RequestRecord -> MonadPerspectives Unit
dispatchOnRequest r@{request, subject, predicate, object, reactStateSetter, corrId, contextDescription, rolDescription, authoringRole: as} = do
  -- The authoringRole is the System User by default.
  authoringRole <- case as of
    Nothing -> pure $ ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User"
    Just x -> getRoleType x
  case request of
    -- Given the context instance identifier and the qualified name of the RolType.
    -- Api.GetRolBinding -> do
    --   (f :: RoleGetter) <- (getRoleFunction predicate)
    --   registerSupportedEffect corrId setter (f >=> binding) (ContextInstance subject)
    -- Given the rolinstance;
    Api.GetBinding -> registerSupportedEffect corrId setter binding (RoleInstance subject)
    -- Api.GetBindingType -> registerSupportedEffect corrId setter (binding >=> roleType) (RoleInstance subject)

    -- {request: "GetRoleBinders", subject: <RoleInstance>, predicate: <EnumeratedRoleType>, object: Maybe <ContextType>}
    -- The empty string represents Nothing when used as object.
    -- subject is the Filler.
    -- predicate is the type of Filled.
    -- object is the context type of Filled.
    Api.GetRoleBinders -> (try $ getPerspectRol (RoleInstance subject)) >>=
      case _ of
        Left err -> do
          logPerspectivesError $ RolErrorBoundary "Api.GetRoleBinders" (show err)
          sendResponse (Error corrId (show $ RolErrorBoundary "Api.GetRoleBinders" (show err))) setter
        Right (PerspectRol{pspType:fillerType, context:fillerContext}) -> case object of
            "" -> (try $ getPerspectType (EnumeratedRoleType predicate)) >>=
              case _ of
                Left err -> do
                  logPerspectivesError $ TypeErrorBoundary "Api.GetRoleBinders" (show err)
                  sendResponse (Error corrId (show $ TypeErrorBoundary "Api.GetRoleBinders" (show err))) setter
                Right (EnumeratedRole{context:filledContextType}) ->do
                  void $ runMonadPerspectivesTransaction' false authoringRole (loadModelIfMissing $ unsafeDeconstructModelName (unwrap fillerType))
                  registerSupportedEffect corrId setter (getFilledRoles filledContextType (EnumeratedRoleType predicate)) (RoleInstance subject)
            filledContextType -> (try $ getContext (ContextType filledContextType)) >>=
              case _ of
                Left err -> do
                  logPerspectivesError $ ContextErrorBoundary "Api.GetRoleBinders" (show err)
                  sendResponse (Error corrId (show $ ContextErrorBoundary "Api.GetRoleBinders" (show err))) setter
                Right cType -> do
                  void $ runMonadPerspectivesTransaction' false authoringRole (loadModelIfMissing $ unsafeDeconstructModelName (unwrap fillerType))
                  registerSupportedEffect corrId setter (getFilledRoles (ContextType filledContextType) (EnumeratedRoleType predicate)) (RoleInstance subject)
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
    -- {request: "GetProperty", subject: rolID, predicate: propertyName, object: roleType}
    Api.GetProperty -> do
      (adt :: ADT EnumeratedRoleType) <- getRoleType object >>= rangeOfRoleCalculation
      result <- try (getDynamicPropertyGetter predicate adt)
      case result of
        Left e -> sendResponse (Error corrId ("No propertytype '" <> predicate <> "' found on roletype '" <> object <> "': " <> show e)) setter
        Right (f :: PropertyValueGetter) -> registerSupportedEffect corrId setter f (RoleInstance subject)

    -- Looks up a property on the role instance and recursively on its binding,
    -- given its local name.
    -- {request: "GetPropertyFromLocalName", subject: rolID, predicate: propertyName, object: roleType}
    Api.GetPropertyFromLocalName -> do
      (adt :: ADT EnumeratedRoleType) <- getRoleType object >>= rangeOfRoleCalculation
      result <- try (getDynamicPropertyGetterFromLocalName predicate adt)
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
        views <- getRoleType subject >>= runArrayT <<< lookForUnqualifiedViewType predicate
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
    -- `subject` is an external role instance. Returns all RoleTypes that sys:Me
    -- ultimately fills an instance of in the corresponding context instance.
    Api.GetAllMyRoleTypes -> do
      allUserRoleTypes <- (RoleInstance subject) ##= (context >=> getAllMyRoleTypes)
      sendResponse (Result corrId (roletype2string <$> allUserRoleTypes)) setter
    -- { request: "GetLocalRoleSpecialisation", subject: contextInstance, predicate: localAspectName}
    Api.GetLocalRoleSpecialisation -> registerSupportedEffect corrId setter (contextType >=> (liftToInstanceLevel $ localRoleSpecialisation predicate)) (ContextInstance subject)
    -- {request: "matchContextName", subject: name}
    Api.MatchContextName -> do
      matches <- matchIndexedContextNames subject
      sendResponse (Result corrId [encodeJSON matches]) setter
    Api.GetCouchdbUrl -> do
      url <- gets \s -> maybe "" identity s.userInfo.couchdbUrl
      sendResponse (Result corrId [url]) setter
    -- {object: Role Instance}
    Api.GetRoleName -> registerSupportedEffect
      corrId
      setter
      getRoleName
      (RoleInstance object)
    Api.GetUserIdentifier -> do
      sysId <- getSystemIdentifier
      sendResponse (Result corrId [sysId]) setter

    -- { request: "GetPerspectives", subject: roleType, object: contextInstance }
    Api.GetPerspectives -> do
      userRoleType <- getRoleType subject
      subjectGetter <- getRoleFunction subject
      muserRoleInstance <- (ContextInstance object) ##> subjectGetter
      case muserRoleInstance of
        Nothing -> sendResponse (Error corrId ("There is no user role instance for role type '" <> subject <> "' in context instance '" <> object <> "'!")) setter
        Just userRoleInstance -> registerSupportedEffect
          corrId
          setter
          (perspectivesForContextAndUser userRoleInstance userRoleType)
          (ContextInstance object)

    -- { request: "GetPerspective", subject: UserRoleType OPTIONAL, predicate: RoleInstance, object: ContextInstance OPTIONAL }
    Api.GetPerspective -> do
      contextInstance <- if object == ""
        then (RoleInstance predicate) ##>> context
        else pure $ ContextInstance object
      contextType <- contextInstance ##>> contextType
      (objectRoleType :: EnumeratedRoleType) <- (RoleInstance predicate) ##>> roleType
      (muserRoleType :: Maybe RoleType) <- if subject == ""
        then contextInstance ##> getMyType
        else contextType ###> lookForRoleType subject
      case muserRoleType of
        Nothing -> do
          message <- if subject == ""
            then pure ("Cannot find your role in the context " <> object)
            else pure ("Cannot find the role type '" <> subject <> "' in context type '" <> show contextType <> "'.")
          sendResponse (Error corrId message) setter
        Just userRoleType -> do
          muserRoleInstance <- contextInstance ##> getRoleInstances userRoleType
          case muserRoleInstance of
            Nothing -> sendResponse (Error corrId ("There is no user role instance for role type '" <> subject <> "' in context instance '" <> object <> "'!")) setter
            Just userRoleInstance -> registerSupportedEffect
              corrId
              setter
              (perspectiveForContextAndUser userRoleInstance userRoleType (ENR objectRoleType))
              contextInstance

    -- { request: GetContextActions
    -- , subject: RoleType // the user role type
    -- , object: ContextInstance
    -- }
    Api.GetContextActions -> do
      userRoleType <- getRoleType subject
      subjectGetter <- getRoleFunction subject
      muserRoleInstance <- (ContextInstance object) ##> subjectGetter
      case muserRoleInstance of
        Nothing -> sendResponse (Error corrId ("There is no user role instance for role type '" <> subject <> "' in context instance '" <> object <> "'!")) setter
        -- The computation of actions depends on subject- and context state.
        Just userRoleInstance -> registerSupportedEffect
          corrId
          setter
          (getContextActions userRoleType userRoleInstance)
          (ContextInstance object)

    -- { request: "GetRolesWithProperties", object: ContextInstance, predicate: roleType}
    -- Api.GetRolesWithProperties ->
    --   registerSupportedEffect
    --     corrId
    --     setter
    --     (getRolesWithProperties predicate)
    --     (ContextInstance object)

    -- {request: "CreateContext", subject: contextId, predicate: roleType, object: ContextType, contextDescription: contextDescription, authoringRole: myroletype}
    -- roleType may be a local name.
    -- The context type given in object must be described in a locally installed model.
    Api.CreateContext -> withLocalName predicate (ContextType object)
      \(qrolname :: RoleType) -> case qrolname of
        -- If a CalculatedRole AND a Database Query Role, do not create a role instance.
        (CR embeddingctype) -> do
          isDBQ <- isDatabaseQueryRole qrolname
          if isDBQ
            then withNewContext authoringRole (Just qrolname)
              \(ContextInstance id) -> lift2 $ sendResponse (Result corrId [buitenRol id]) setter
            else sendResponse (Error corrId (predicate <> " is Calculated but not a Database Query Role!")) setter
        (ENR eroltype) -> withNewContext authoringRole (Just qrolname)
          \(ContextInstance id) ->  do
            -- now bind it in a new instance of the roletype in the given context.
            -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
            contextRole <- unsafePartial $ fromJust <$> createAndAddRoleInstance eroltype subject (RolSerialization
              { id: Nothing
              , properties: PropertySerialization empty
              , binding: Just $ buitenRol id })
            lift2 $ sendResponse (Result corrId [buitenRol id, unwrap contextRole]) setter
    -- {request: "CreateContext_", subject: roleInstance, contextDescription: contextDescription, authoringRole: myroletype}
    Api.CreateContext_ -> do
      rtype <- roleType_ (RoleInstance subject)
      withNewContext authoringRole (Just $ ENR rtype)
        \(ContextInstance id) -> do
          -- now bind it in the role instance.
          void $ setFirstBinding (RoleInstance subject) (RoleInstance $ buitenRol id) Nothing
          lift2 $ sendResponse (Result corrId [buitenRol id]) setter
    Api.ImportTransaction -> case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) TransactionForPeer) -> sendResponse (Error corrId (show e)) setter
      (Right tfp@(TransactionForPeer _)) -> do
        executeTransaction tfp
        sendResponse (Result corrId []) setter
    -- TODO/NOTE that we cannot provide a context role type that will bind these contexts.
    -- This can only be correct if the contexts have the aspect RootContext.
    Api.ImportContexts -> case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextsSerialisation) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextsSerialisation ctxts) :: Either (NonEmptyList ForeignError) ContextsSerialisation) -> void $
        runMonadPerspectivesTransaction' false authoringRole do
          result <- runExceptT $ traverse
            (\ctxt@(ContextSerialization{ctype}) -> do
              lift $ loadModelIfMissing $ unsafeDeconstructModelName ctype
              constructContext Nothing ctxt)
            ctxts
          case result of
            Left e -> lift2 $ sendResponse (Error corrId (show e)) setter
            Right ids -> lift2 $ sendResponse (Result corrId (unwrap <$> ids)) setter
    -- {request: "RemoveRol", subject: rolID, predicate: rolName, object: contextType, authoringRole: myroletype}
    -- The context type given in object must be described in a locally installed model.
    Api.RemoveRol -> do
      if (isExternalRole subject)
        then do
          (qrolname :: RoleType) <- string2RoleType subject
          case qrolname of
            cr@(CR ctype) -> do
              isDBQ <- isDatabaseQueryRole cr
              if isDBQ
                then void $ runMonadPerspectivesTransaction authoringRole $ removeContextIfUnbound (RoleInstance subject) (Just cr)
                else sendResponse (Error corrId ("Cannot remove an external role from non-database query role " <> (unwrap ctype))) setter
            (ENR rtype) -> sendResponse (Error corrId ("Cannot remove an external role from enumerated role " <> (unwrap rtype) <> " - use unbind instead!")) setter
        else do
          void $ runMonadPerspectivesTransaction authoringRole $ scheduleRoleRemoval (RoleInstance subject)
          sendResponse (Result corrId []) setter
    -- {request: "RemoveContext", subject: rolID, predicate: rolName, authoringRole: myroletype}
    -- The context type given in object must be described in a locally installed model.
    -- The RoleType (given in the predicate) must be of kind ContextRole. It may be an unqualified name.
    Api.RemoveContext -> do
      (autorizedRole :: RoleType) <- string2RoleType predicate
      kind <- roleKindOfRoleType autorizedRole
      if kind == ContextRole
        then do
          void $ runMonadPerspectivesTransaction authoringRole do
            scheduleRoleRemoval (RoleInstance subject)
            contextToBeRemoved <- lift2 ((RoleInstance subject) ##>> binding >=> context)
            scheduleContextRemoval (Just autorizedRole) contextToBeRemoved
          sendResponse (Result corrId []) setter
        else sendResponse (Error corrId ("Cannot remove a context from a non-context role kind: " <> show autorizedRole )) setter
    Api.DeleteRole -> do
      -- TODO. Hanteer het geval dat subject een DBQ role is. Of misschien veiliger om er DeleteAllInstances van te maken?
      void $ runMonadPerspectivesTransaction authoringRole $ removeAllRoleInstances (EnumeratedRoleType subject) (ContextInstance object)
      sendResponse (Result corrId []) setter
    -- subject :: ContextInstance, predicate :: EnumeratedRoleType
    Api.CreateRol -> do
      if isQualifiedName predicate
        then do
          -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
          (rolInsts :: Array RoleInstance) <- runMonadPerspectivesTransaction authoringRole $ unsafePartial $ fromJust <$> createAndAddRoleInstance (EnumeratedRoleType predicate) subject (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
          sendResponse (Result corrId (unwrap <$> rolInsts)) setter
        else sendResponse (Error corrId ("Could not create a role instance for: " <> predicate <> " in " <> subject)) setter
    -- {request: "Bind", subject: contextinstance, predicate: roleType, object: contextType, rolDescription: rolDescription, authoringRole: myroletype },
    -- Provide the binding in the rolDescription!
    -- roleType may be a local name.
    -- The context type given in object must be described in a locally installed model.
    Api.Bind -> withLocalName predicate (ContextType object)
      \(qrolname :: RoleType) -> case qrolname of
        (CR ctype) -> sendResponse (Error corrId ("Cannot construct an instance of CalculatedRole '" <> unwrap ctype <> "'!")) setter
        (ENR eroltype) -> do
          RolSerialization{binding: mbnd} <- pure $ unsafePartial $ fromJust rolDescription
          case mbnd of
            Just bnd -> do
              contextInstanceId <- ContextInstance <$> (expandDefaultNamespaces subject)
              bindings <- contextInstanceId ##= (getRoleInstances qrolname >=> binding)
              if isJust $ elemIndex (RoleInstance bnd) bindings
                then sendResponse (Error corrId ("Cannot not bind the same role instance twice in the same role type")) setter
                else do
                  -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
                  rolarr <- runMonadPerspectivesTransaction authoringRole $ createAndAddRoleInstance eroltype subject (unsafePartial $ fromJust rolDescription)
                  case head rolarr of
                    Nothing -> sendResponse (Error corrId ("Could not create role instance of " <> show eroltype)) setter
                    Just Nothing -> sendResponse (Error corrId ("Could not create role instance of " <> show eroltype)) setter
                    Just (Just rol) -> sendResponse (Result corrId [(unwrap rol)]) setter
            Nothing -> do
              -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
              rol <- runMonadPerspectivesTransaction authoringRole $ unsafePartial fromJust <$> createAndAddRoleInstance eroltype subject (unsafePartial $ fromJust rolDescription)
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
        void $ runMonadPerspectivesTransaction authoringRole $ removeBinding (RoleInstance subject)
        sendResponse (Result corrId ["ok"]) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Check whether a role exists for contextType with the localRolName and whether it allows RolID as binding.
    -- The context type given in object must be described in a locally installed model.
    -- {request: "CheckBinding", subject: contextType, predicate: localRolName, object: rolInstance}
    Api.CheckBinding -> withLocalName predicate (ContextType subject)
      \(typeOfRolToBindTo :: RoleType) -> (try $ getPerspectRol (RoleInstance object)) >>=
        case _ of
          Left err -> do
            logPerspectivesError $ RolErrorBoundary "Api.CheckBinding" (show err)
            sendResponse (Error corrId (show $ RolErrorBoundary "Api.CheckBinding" (show err))) setter
          Right (PerspectRol{pspType}) -> do
            void $ runMonadPerspectivesTransaction' false authoringRole (loadModelIfMissing $ unsafeDeconstructModelName (unwrap pspType))
            ok <- checkBinding typeOfRolToBindTo (RoleInstance object)
            sendResponse (Result corrId [(show ok)]) setter
    Api.SetProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (setProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate) [(Value object)])
        sendResponse (Result corrId ["ok"]) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- {request: "DeleteProperty", subject: rolID, predicate: propertyName, authoringRole: myroletype}
    Api.DeleteProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (deleteProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate))
        sendResponse (Result corrId ["Ok"]) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- { request: Action
    -- , subject: <user role instance>
    -- , predicate: <object of perspective role instance>
    -- , object: <context instance>
    -- , contextDescription:
    -- 	{ perspectiveId:
    -- 	, actionName:
    -- 	}
    -- ...}
    Api.Action -> catchError
        (case read contextDescription of
          Left err -> sendResponse (Error corrId ("Incorrectly formed description of Action on applying an action in context instance '" <> object <> "' and object role instance '" <> predicate <> "'. Errors: " <> show err )) setter
          Right ({perspectiveId, actionName} :: {perspectiveId :: String, actionName :: String}) -> do
            -- Find the action from the authoringRole, the perspective id, the Action name.
            maction <- (map $ getAction actionName) <$> (findPerspective authoringRole (\(Perspective{id})-> pure $ id `eq` perspectiveId))
            mauthoringRoleInstance <- (ContextInstance object) ##> getMeInRoleAndContext authoringRole
            case mauthoringRoleInstance, maction of
              Just author, Just (Just (ACTION.Action action)) -> void $ runMonadPerspectivesTransaction authoringRole
                do
                  oldFrame <- lift2 $ pushFrame
                  lift2 $ addBinding "currentcontext" [object]
                  lift2 $ addBinding "origin" [predicate]
                  lift2 $ addBinding "currentactor" [unwrap author]
                  updater <- lift2 $ compileAssignmentFromRole action
                  updater (RoleInstance predicate)
                  lift2 $ restoreFrame oldFrame
              _, _ -> sendResponse (Error corrId $ "cannot identify Action with role type '" <> show authoringRole <> "', perspectiveId '" <> perspectiveId <> "' and action name '" <> actionName <> "'.") setter
            )
      (\e -> sendResponse (Error corrId (show e)) setter)

    -- { request: Action
    -- , subject: RoleType // the user role type
    -- , predicate: String // action identifier
    -- , object: RoleInstance // the context identifier.
    -- }
    Api.ContextAction -> catchError
      (do
        -- Find the action from the subject type and the Action name.
        userRoleType <- getRoleType subject
        maction <- getContextAction predicate userRoleType
        muserRoleInstance <- (ContextInstance object) ##> getMeInRoleAndContext userRoleType
        case muserRoleInstance, maction of
          Just user, Just (ACTION.Action action) -> void $ runMonadPerspectivesTransaction authoringRole
            do
              oldFrame <- lift2 $ pushFrame
              lift2 $ addBinding "currentcontext" [object]
              lift2 $ addBinding "origin" [object]
              lift2 $ addBinding "currentactor" [unwrap user] -- userRoleType??
              updater <- lift2 $ compileAssignment action
              updater (ContextInstance object)
              lift2 $ restoreFrame oldFrame
          _, _ -> sendResponse (Error corrId $ "cannot identify Action with role type '" <> show userRoleType <>
            "' and action name '" <> predicate <> "'.") setter
        )
      (\e -> sendResponse (Error corrId (show e)) setter)

    -- `subject` is an external role, object is a role type identifier (either
    -- enumerated or calculated).
    Api.SetPreferredUserRoleType -> catchError
      (do
        mroleType <- (RoleInstance subject) ##> (context >=> contextType >=> (liftToInstanceLevel $ lookForRoleType object))
        case mroleType of
          Nothing -> sendResponse (Error corrId ("Cannot find a role type with name '" <> object <> "' in the context of the external role '" <> subject <> "'.")) setter
          Just roleType -> do
            ctxt <- (RoleInstance subject) ##>> context
            void $ runMonadPerspectivesTransaction authoringRole (setPreferredUserRoleType ctxt [roleType]))
      \e -> sendResponse (Error corrId (show e)) setter

    Api.Unsubscribe -> unregisterSupportedEffect corrId
    Api.WrongRequest -> sendResponse (Error corrId subject) setter
    otherwise -> sendResponse (Error corrId ("Perspectives could not handle this request: '" <> (showRequestRecord r) <> "'")) (mkApiEffect reactStateSetter)
  where
    setter = (mkApiEffect reactStateSetter)

    -- The identifier in the ContextType must be fully qualified.
    -- The model describing the ContextType must be locally installed.
    withLocalName :: String -> ContextType -> (RoleType -> MonadPerspectives Unit) -> MonadPerspectives Unit
    withLocalName localRoleName contextType effect = do
      qrolNames <- if isQualifiedName localRoleName
        then if isExternalRole localRoleName
          then if deconstructBuitenRol localRoleName == (unwrap contextType)
            then pure [(ENR $ (EnumeratedRoleType localRoleName))]
            else pure []
          else runArrayT $ lookForRoleType localRoleName contextType
        else runArrayT $ lookForUnqualifiedRoleType localRoleName contextType
      case head qrolNames of
        (Just (qrolname :: RoleType)) -> effect qrolname
        Nothing -> do
          qrolnames' <- runArrayT $ lookForRoleType localRoleName contextType
          case head qrolnames' of
            Nothing -> sendResponse (Error corrId ("Cannot find Rol with name '" <> localRoleName <> "' on context type '" <> unwrap contextType <> "'!")) setter
            (Just (qrolname :: RoleType)) -> effect qrolname

    withNewContext :: RoleType -> (Maybe RoleType) -> (ContextInstance -> MonadPerspectivesTransaction Unit) -> MonadPerspectives Unit
    withNewContext authoringRole mroleType effect = case unwrap $ runExceptT $ decode contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextSerialization) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextSerialization cd@{ctype}) :: Either (NonEmptyList ForeignError) ContextSerialization) -> do
        void $ runMonadPerspectivesTransaction authoringRole do
          loadModelIfMissing $ unsafeDeconstructModelName ctype
          g <- liftEffect guid
          ctxt <- runExceptT $ constructContext mroleType (ContextSerialization cd {id = "model:User$c" <> (show g)})
          case ctxt of
            (Left messages) -> lift2 $ sendResponse (Error corrId (show messages)) setter
            (Right ctxtId) -> effect ctxtId

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter = Array String -> Effect Unit

type QueryUnsubscriber e = Effect Unit

-- | Apply an ApiEffect to a Response, in effect sending it through the API to the caller.
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse r ae = liftEffect $ ae r
