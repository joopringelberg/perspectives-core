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
import Control.Coroutine.Aff (produce')
import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Control.Plus ((<|>))
import Data.Array (elemIndex, foldM, head)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (catchError, try)
import Effect.Class (liftEffect)
import Foreign (Foreign, ForeignError, unsafeToForeign)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ApiEffect, RequestType(..)) as Api
import Perspectives.ApiTypes (ContextSerialization(..), ContextsSerialisation(..), PropertySerialization(..), RecordWithCorrelationidentifier(..), Request(..), RequestRecord, Response(..), RolSerialization(..), mkApiEffect, showRequestRecord)
import Perspectives.Assignment.Update (RoleProp(..), addProperty, deleteProperty, getPropertyBearingRoleInstance, saveFile, setPreferredUserRoleType, setProperty)
import Perspectives.Checking.PerspectivesTypeChecker (checkBinding)
import Perspectives.CompileAssignment (compileAssignment)
import Perspectives.CompileRoleAssignment (compileAssignmentFromRole)
import Perspectives.CoreTypes (MP, MonadPerspectives, MonadPerspectivesTransaction, PropertyValueGetter, RoleGetter, liftToInstanceLevel, (##=), (##>), (##>>), (###=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DependencyTracking.Dependency (registerSupportedEffect, unregisterSupportedEffect)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Fuzzysort (matchIndexedContextNames)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, isExternalRole, isTypeUri, typeUri2ModelUri_, typeUri2couchdbFilename)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.Me (getAllMyRoleTypes, getMeInRoleAndContext, getMyType)
import Perspectives.Instances.ObjectGetters (binding, context, contextType, contextType_, getContextActions, getFilledRoles, getMe, getProperty, getRoleName, roleType, roleType_, siblings)
import Perspectives.Instances.Values (parsePerspectivesFile)
import Perspectives.ModelDependencies (actualSharedFileServer, fileShareCredentials, identifiableFirstName, identifiableLastName, mySharedFileServices, sharedFileServices, sysUser)
import Perspectives.Names (expandDefaultNamespaces, getMySystem, getUserIdentifier, lookupIndexedContext)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (getAttachment, toFile)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (getPerspectRol, saveMarkedResources)
import Perspectives.PerspectivesState (addBinding, getPerspectivesUser, pushFrame, restoreFrame)
import Perspectives.Proxy (createRequestEmitter, retrieveRequestEmitter)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter, getDynamicPropertyGetterFromLocalName, getPropertyFromTelescope, getPropertyValues, getPublicUrl, getRoleFunction, getRoleInstances)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Action (Action(..)) as ACTION
import Perspectives.Representation.Class.PersistentType (DomeinFileId(..), getCalculatedRole, getContext, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (getRoleType, kindOfRole, rangeOfRoleCalculation, roleKindOfRoleType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), ViewType, propertytype2string, roletype2string, toRoleType_, StateIdentifier(..))
import Perspectives.Representation.View (View, propertyReferences)
import Perspectives.ResourceIdentifiers (createPublicIdentifier, guid, resourceIdentifier2DocLocator)
import Perspectives.RoleStateCompiler (evaluateRoleState)
import Perspectives.RunMonadPerspectivesTransaction (detectPublicStateChanges, runMonadPerspectivesTransaction, runMonadPerspectivesTransaction')
import Perspectives.SaveUserData (removeAllRoleInstances, removeBinding, removeContextIfUnbound, scheduleContextRemoval, scheduleRoleRemoval, setBinding, setFirstBinding, synchronise)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.TypePersistence.ContextSerialisation (screenForContextAndUser, serialisedTableFormForContextAndUser)
import Perspectives.TypePersistence.PerspectiveSerialisation (perspectiveForContextAndUser, perspectivesForContextAndUser)
import Perspectives.Types.ObjectGetters (findPerspective, getAction, getContextAction, isDatabaseQueryRole, localRoleSpecialisation, lookForRoleType, lookForUnqualifiedRoleType, lookForUnqualifiedViewType, propertiesOfRole, rolesWithPerspectiveOnRoleAndProperty, string2EnumeratedRoleType, string2RoleType)
import Prelude (Unit, bind, discard, eq, identity, map, negate, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Simple.JSON (read, unsafeStringify, writeJSON)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
-- A Producer for Requests.
requestProducer :: Producer Foreign (MonadPerspectives) Unit
requestProducer = produce' createRequestEmitter

resumeApi :: MonadPerspectives Unit
resumeApi = runProcess $ ((produce' retrieveRequestEmitter) $~ (forever (transform decodeRequest))) $$ consumeRequest

-- | Create a process that consumes requests from a producer fed by the user interface.
setupApi :: MonadPerspectives Unit
setupApi = runProcess $ (requestProducer $~ (forever (transform decodeRequest))) $$ consumeRequest

decodeRequest :: Foreign -> Request
decodeRequest f = case read f of
  (Right r) -> r
  (Left e) -> case read f of
    -- If we have a correlation identifier and a callback, we can send a message back to the client.
    Right (RecordWithCorrelationidentifier {corrId, reactStateSetter}) -> 
      Request
        { request: Api.WrongRequest
        , subject: ("Perspectives could not decode this request: '" <> show e <> "'")
        , predicate: (unsafeStringify f)
        , object: ""
        , corrId
        , reactStateSetter
        , contextDescription: unsafeToForeign ""
        , rolDescription: Nothing
        , authoringRole: Nothing
        , onlyOnce: false}
    -- NOTE that this request will never lead to a response to the client.
    Left _ -> Request
        { request: Api.WrongRequest
        , subject: ("Perspectives could not decode this request: '" <> show e <> "'")
        , predicate: (unsafeStringify f)
        , object: ""
        , corrId: -1
        , reactStateSetter: Just $ unsafeToForeign (\_ -> pure unit :: Effect Unit)
        , contextDescription: unsafeToForeign ""
        , rolDescription: Nothing
        , authoringRole: Nothing
        , onlyOnce: false} 


consumeRequest :: Consumer Request MonadPerspectives Unit
consumeRequest = forever do
  (Request request@{corrId, reactStateSetter}) <- await
  lift (catchError
    (do 
      dispatchOnRequest request
      detectPublicStateChanges)
    (\e -> do
      logPerspectivesError (ApiErrorBoundary ((showRequestRecord request) <> ": " <> (show e)))
      sendResponse (Error corrId ("Request could not be handled, because: " <> (show e))) (mkApiEffect reactStateSetter))
    )

dispatchOnRequest :: RequestRecord -> MonadPerspectives Unit
dispatchOnRequest r@{request, subject, predicate, object, reactStateSetter, corrId, contextDescription, rolDescription, authoringRole: as, onlyOnce} = do
  -- The authoringRole is the System User by default.
  authoringRole <- case as of
    Nothing -> pure $ ENR $ EnumeratedRoleType sysUser
    Just x -> getRoleType x
  case request of
    -- Given the context instance identifier and the qualified name of the RolType.
    -- Api.GetRolBinding -> do
    --   (f :: RoleGetter) <- (getRoleFunction predicate)
    --   registerSupportedEffect corrId setter (f >=> binding) (ContextInstance subject)
    -- Given the rolinstance;
    Api.GetBinding -> registerSupportedEffect corrId setter binding (RoleInstance subject) onlyOnce
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
        Right (PerspectRol{pspType:fillerType}) -> case object of
            "" -> (try $ getPerspectType (EnumeratedRoleType predicate)) >>=
              case _ of
                Left err -> do
                  logPerspectivesError $ TypeErrorBoundary "Api.GetRoleBinders" (show err)
                  sendResponse (Error corrId (show $ TypeErrorBoundary "Api.GetRoleBinders" (show err))) setter
                Right (EnumeratedRole{context:filledContextType}) ->do
                  void $ runMonadPerspectivesTransaction' false authoringRole (lift $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ (unwrap fillerType)))
                  registerSupportedEffect corrId setter (getFilledRoles filledContextType (EnumeratedRoleType predicate)) (RoleInstance subject) onlyOnce
            filledContextType -> (try $ getContext (ContextType filledContextType)) >>=
              case _ of
                Left err -> do
                  logPerspectivesError $ ContextErrorBoundary "Api.GetRoleBinders" (show err)
                  sendResponse (Error corrId (show $ ContextErrorBoundary "Api.GetRoleBinders" (show err))) setter
                Right _ -> do
                  void $ runMonadPerspectivesTransaction' false authoringRole (lift $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ (unwrap fillerType)))
                  registerSupportedEffect corrId setter (getFilledRoles (ContextType filledContextType) (EnumeratedRoleType predicate)) (RoleInstance subject) onlyOnce
    Api.GetRol -> do
      (f :: RoleGetter) <- (getRoleFunction predicate)
      registerSupportedEffect corrId setter f (ContextInstance subject) onlyOnce

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
              registerSupportedEffect corrId setter f (ContextInstance subject) onlyOnce
    Api.GetRolContext -> do
      res <- (RoleInstance subject) ##= context
      sendResponse (Result corrId (unwrap <$> res)) setter
    Api.GetContextType -> do
      res <- (ContextInstance subject) ##= contextType
      sendResponse (Result corrId (unwrap <$> res)) setter
    Api.GetRolType -> do
      res <- (RoleInstance subject) ##= roleType
      sendResponse (Result corrId (unwrap <$> res)) setter
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
        Right (f :: PropertyValueGetter) -> registerSupportedEffect corrId setter f (RoleInstance subject) onlyOnce

    -- Looks up a property on the role instance and recursively on its binding,
    -- given its local name.
    -- {request: "GetPropertyFromLocalName", subject: rolID, predicate: propertyName, object: roleType}
    Api.GetPropertyFromLocalName -> do
      (adt :: ADT EnumeratedRoleType) <- getRoleType object >>= rangeOfRoleCalculation
      result <- try (getDynamicPropertyGetterFromLocalName predicate adt)
      case result of
        Left e -> sendResponse (Error corrId ("No propertytype '" <> predicate <> "' found on roletype '" <> object <> "': " <> show e)) setter
        Right (f :: PropertyValueGetter) -> registerSupportedEffect corrId setter f (RoleInstance subject) onlyOnce

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
        ContextRole -> registerSupportedEffect corrId setter (map toRoleType_ <<< (binding >=> context >=> getMyType)) (RoleInstance subject) onlyOnce
        _ -> registerSupportedEffect corrId setter (map toRoleType_ <<< (context >=> getMyType)) (RoleInstance subject) onlyOnce
    -- E.g. send in an External Role instance, get back the role instance in the context ultimately filled by the PerspectivesUser
    -- that represents the current user.
    -- If the role is a contextrole, we treat it as if we got its binding.
    Api.GetMeInContext -> do
      roleKind <- roleType_ (RoleInstance subject) >>= getEnumeratedRole >>= pure <<< kindOfRole
      case roleKind of
        ContextRole -> registerSupportedEffect corrId setter ((binding >=> context >=> getMe)) (RoleInstance subject) onlyOnce
        _ -> registerSupportedEffect corrId setter ((context >=> getMe)) (RoleInstance subject) onlyOnce
    Api.GetFileShareCredentials ->  do 
      me <- getUserIdentifier
      registerSupportedEffect 
        corrId 
        setter 
        (\_ -> ArrayT $ lift do
          -- Ensure model://perspectives.domains#SharedFileServices is available.
          void $ retrieveDomeinFile (DomeinFileId sharedFileServices)
          -- Get the indexed context sfs:MySharedFileServices
          -- Then get the role ActualSharedFileServer
          -- Finally return the property FileShareCredentials
          mservices <- lookupIndexedContext mySharedFileServices
          case mservices of 
            Nothing -> pure []
            Just services -> services ##= (getRoleInstances (CR $ CalculatedRoleType actualSharedFileServer) >=> getPropertyValues (CP $ CalculatedPropertyType fileShareCredentials)))
        -- Notice that the argument plays no role in the computation, but it has to be provided.
        (RoleInstance me)
        onlyOnce
    -- `subject` is a role instance. Returns all RoleTypes that sys:Me
    -- ultimately fills an instance of in the corresponding context instance.
    Api.GetAllMyRoleTypes -> do
      allUserRoleTypes <- (RoleInstance subject) ##= (context >=> getAllMyRoleTypes)
      sendResponse (Result corrId (roletype2string <$> allUserRoleTypes)) setter
    -- { request: "GetLocalRoleSpecialisation", subject: contextInstance, predicate: localAspectName}
    Api.GetLocalRoleSpecialisation -> registerSupportedEffect corrId setter (contextType >=> (liftToInstanceLevel $ localRoleSpecialisation predicate)) (ContextInstance subject) onlyOnce
    -- {request: "matchContextName", subject: name}
    Api.MatchContextName -> do 
      mysystem <- getMySystem
      registerSupportedEffect corrId setter (matchIndexedContextNames subject) (ContextInstance mysystem) onlyOnce
    Api.GetChatParticipants -> registerSupportedEffect 
      corrId 
      setter 
      (\(chatRoleInstance :: RoleInstance) -> ArrayT do 
        chatRoleType <- lift $ roleType_ chatRoleInstance
        chatContextInstance <- runArrayT $ context chatRoleInstance
        contextType <- ((_.context <<< unwrap) <$> (lift $ getEnumeratedRole chatRoleType))
        userRoleTypes <- lift $ contextType ###= (unsafePartial rolesWithPerspectiveOnRoleAndProperty (ENR chatRoleType) (ENP $ EnumeratedPropertyType predicate))
        userRoles <- foldM
          (\cumulator userRoleType -> do
            is <- runArrayT (getRoleInstances userRoleType (unsafePartial fromJust $ head chatContextInstance))
            pure ( cumulator <> is))
          []
          userRoleTypes
        -- {roleInstance, firstname, lastname, avatar [OPTIONAL]}
        for userRoles \roleInstance -> do
          firstname <- head <$> (runArrayT $ getPropertyFromTelescope (EnumeratedPropertyType identifiableFirstName) roleInstance)
          lastname <- head <$> (runArrayT $ getPropertyFromTelescope (EnumeratedPropertyType identifiableLastName) roleInstance)
          pure $ ChatParticipant (writeJSON ({firstname, lastname, roleInstance, avatar: Nothing} :: ChatParticipantFields))
      )
      (RoleInstance subject)
      onlyOnce
    Api.GetCouchdbUrl -> do
      url <- gets \s -> maybe "" identity s.couchdbUrl
      sendResponse (Result corrId [url]) setter
    -- {object: Role Instance}
    Api.GetRoleName -> registerSupportedEffect
      corrId 
      setter
      getRoleName
      (RoleInstance object)
       onlyOnce
    Api.GetSystemIdentifier -> do 
      sysId <- getSystemIdentifier
      sendResponse (Result corrId [sysId]) setter
    Api.GetPerspectivesUser -> do  
      PerspectivesUser id <- getPerspectivesUser
      sendResponse (Result corrId [id]) setter
    Api.GetPublicUrl -> do
      mrepoUrl <- getPublicUrl (ContextInstance subject)
      case mrepoUrl of 
        Nothing -> sendResponse (Result corrId []) setter
        Just repoUrl -> do
          schemeLess <- guid subject
          sendResponse (Result corrId [(createPublicIdentifier repoUrl schemeLess)]) setter
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
          onlyOnce

    -- { request: "GetPerspective", subject: PerspectiveObjectRoleType OPTIONAL, predicate: RoleInstanceOfContext }
    Api.GetPerspective -> do
      contextInstance <- (RoleInstance predicate) ##>> context
      (objectRoleType :: RoleType) <- if subject == ""
        -- No explicit type given for the perspective object; assume that the predicate has the instance of the role that we want a perspective on.
        then ENR <$> ((RoleInstance predicate) ##>> roleType)
        -- Type has been given in subject. 
        else string2RoleType subject
      (muserRoleType :: Maybe RoleType) <- contextInstance ##> getMyType
      case muserRoleType of
        Nothing -> do
          sendResponse (Error corrId ("Cannot find your role in the context " <> show contextInstance)) setter
        Just userRoleType -> do
          muserRoleInstance <- contextInstance ##> getRoleInstances userRoleType
          case muserRoleInstance of
            Nothing -> sendResponse (Error corrId ("There is no user role instance for role type '" <> subject <> "' in context instance '" <> object <> "'!")) setter
            Just userRoleInstance -> registerSupportedEffect
              corrId
              setter
              (perspectiveForContextAndUser userRoleInstance userRoleType objectRoleType)
              contextInstance
              onlyOnce

    -- { request: "GetScreen", subject: UserRoleType, predicate: ContextType, object: ContextInstance }
    Api.GetScreen -> do
      userRoleType <- getRoleType subject
      subjectGetter <- getRoleFunction subject
      muserRoleInstance <- (ContextInstance object) ##> subjectGetter
      case muserRoleInstance of
        Nothing -> sendResponse (Error corrId ("There is no user role instance for role type '" <> subject <> "' in context instance '" <> object <> "'!")) setter
        Just userRoleInstance -> registerSupportedEffect
          corrId
          setter
          (screenForContextAndUser userRoleInstance userRoleType (ContextType predicate))
          (ContextInstance object)
          onlyOnce
    -- { request: "GetTableForm", subject: UserRoleType, predicate: ContextInstance, object: RoleType }
    Api.GetTableForm -> do 
      userRoleType <- getRoleType subject
      objectRoleType <- getRoleType object
      subjectGetter <- getRoleFunction subject
      contextType <- contextType_ (ContextInstance predicate)
      muserRoleInstance <- (ContextInstance predicate) ##> subjectGetter
      case muserRoleInstance of
        Nothing -> sendResponse (Error corrId ("There is no user role instance for role type '" <> subject <> "' in context instance '" <> predicate <> "'!")) setter
        Just userRoleInstance -> registerSupportedEffect
          corrId
          setter
          (serialisedTableFormForContextAndUser userRoleInstance userRoleType contextType objectRoleType)
          (ContextInstance predicate)
          onlyOnce
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
        -- The computation of actions depends on subject- and context state, but the client will always send a new request.
        Just userRoleInstance -> do
          res <- (ContextInstance object) ##= getContextActions userRoleType userRoleInstance
          sendResponse (Result corrId (writeJSON <$> res)) setter

    -- { request: "CreateContext"
    -- , subject: contextinstance                       the context instance to add a role instance to.
    -- , predicate: roleType                            the qualified identifier of the role type to create.
    -- , contextDescription: contextDescription
    -- , authoringRole: myroletype },
    --
    -- roleType may be a local name.
    -- The context type given in object must be described in a locally installed model.
    Api.CreateContext -> do
      qrolname <- getRoleType predicate
      case qrolname of
        -- If a CalculatedRole AND a Database Query Role, do not create a role instance.
        (CR _) -> do
          isDBQ <- isDatabaseQueryRole qrolname
          if isDBQ
            then withNewContext authoringRole (Just qrolname)
              \(ContextInstance id) -> lift $ sendResponse (Result corrId [buitenRol id]) setter
            else sendResponse (Error corrId (predicate <> " is Calculated but not a Database Query Role!")) setter
        (ENR eroltype) -> withNewContext authoringRole (Just qrolname)
          \(ContextInstance id) ->  do
            -- now bind it in a new instance of the roletype in the given context.
            -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
            contextRole <- unsafePartial $ fromJust <$> createAndAddRoleInstance eroltype subject (RolSerialization
              { id: Nothing
              , properties: PropertySerialization empty
              , binding: Just $ buitenRol id })
            lift $ sendResponse (Result corrId [buitenRol id, unwrap contextRole]) setter
    -- {request: "CreateContext_", subject: roleInstance, contextDescription: contextDescription, authoringRole: myroletype}
    Api.CreateContext_ -> do
      rtype <- roleType_ (RoleInstance subject)
      withNewContext authoringRole (Just $ ENR rtype)
        \(ContextInstance id) -> do
          -- now bind it in the role instance.
          void $ setFirstBinding (RoleInstance subject) (RoleInstance $ buitenRol id) Nothing
          lift $ sendResponse (Result corrId [buitenRol id]) setter
    Api.ImportTransaction -> case read contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) TransactionForPeer) -> sendResponse (Error corrId (show e)) setter
      (Right tfp@(TransactionForPeer _)) -> do
        (runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType sysUser)
          (executeTransaction tfp))
        sendResponse (Result corrId []) setter
    -- TODO/NOTE that we cannot provide a context role type that will bind these contexts.
    -- This can only be correct if the contexts have the aspect RootContext.
    -- TODO/NOTE the context type must be fully qualified with a model URN.
    Api.ImportContexts -> case read contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextsSerialisation) -> sendResponse (Error corrId (show e)) setter
      (Right (ContextsSerialisation ctxts) :: Either (NonEmptyList ForeignError) ContextsSerialisation) -> void $
        runMonadPerspectivesTransaction' false authoringRole do
          result <- runExceptT $ traverse
            (\ctxt@(ContextSerialization{ctype}) -> do
              void $ lift $ lift $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ ctype)
              constructContext Nothing ctxt)
            ctxts
          case result of
            Left e -> lift $ sendResponse (Error corrId (show e)) setter
            Right ids -> lift $ sendResponse (Result corrId (unwrap <$> ids)) setter
    -- {request: "RemoveRol", subject: rolID, predicate: rolName, object: contextType, authoringRole: myroletype}
    -- The context type given in object must be described in a locally installed model.
    -- the predicate is the type of the role to be removed. It is the 'authorizedRole'
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
          void $ runMonadPerspectivesTransaction authoringRole $ scheduleRoleRemoval synchronise (RoleInstance subject)
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
            mcontextToBeRemoved <- lift ((RoleInstance subject) ##> binding >=> context)
            case mcontextToBeRemoved of
              Nothing -> void $ scheduleRoleRemoval synchronise (RoleInstance subject)
              Just contextToBeRemoved -> do
                users <- scheduleRoleRemoval synchronise (RoleInstance subject)
                scheduleContextRemoval (Just autorizedRole) users contextToBeRemoved
                
          sendResponse (Result corrId []) setter
        else sendResponse (Error corrId ("Cannot remove a context from a non-context role kind: " <> show autorizedRole )) setter
    Api.DeleteRole -> do
      -- TODO. Hanteer het geval dat subject een DBQ role is. Of misschien veiliger om er DeleteAllInstances van te maken?
      void $ runMonadPerspectivesTransaction authoringRole $ removeAllRoleInstances (EnumeratedRoleType subject) (ContextInstance object)
      sendResponse (Result corrId []) setter
    -- subject :: ContextInstance, predicate :: EnumeratedRoleType
    Api.CreateRol -> do
      if isTypeUri predicate
        then do
          -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
          (rolInst :: RoleInstance) <- runMonadPerspectivesTransaction authoringRole $ unsafePartial $ fromJust <$> createAndAddRoleInstance (EnumeratedRoleType predicate) subject (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
          sendResponse (Result corrId [(unwrap rolInst)]) setter
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
                  mrole <- runMonadPerspectivesTransaction authoringRole $ createAndAddRoleInstance eroltype subject (unsafePartial $ fromJust rolDescription)
                  case mrole of
                    Nothing -> sendResponse (Error corrId ("Could not create role instance of " <> show eroltype)) setter
                    Just rol -> sendResponse (Result corrId [(unwrap rol)]) setter
            Nothing -> do
              -- Notice that createAndAddRoleInstance adds the model describing the eroltype if necessary.
              rol <- runMonadPerspectivesTransaction authoringRole $ unsafePartial fromJust <$> createAndAddRoleInstance eroltype subject (unsafePartial $ fromJust rolDescription)
              sendResponse (Result corrId [(unwrap rol)]) setter
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
            sendResponse (Result corrId []) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- {request: "RemoveBinding", subject: rolID}
    Api.RemoveBinding -> catchError
      do
        -- Remove the binding from subject. We don't know whether that binding will be removed itself, hence bindingRemoved=false.
        void $ runMonadPerspectivesTransaction authoringRole $ removeBinding (RoleInstance subject)
        sendResponse (Result corrId []) setter
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- Check whether the role type in predicate allows RolID as binding.
    -- The context type given in object must be described in a locally installed model.
    -- {request: "CheckBinding", predicate: localRolName, object: rolInstance}
    Api.CheckBinding -> do 
      typeOfRoleToBindTo <- string2EnumeratedRoleType predicate
      (try $ getPerspectRol (RoleInstance object)) >>=
        case _ of
          Left err -> do
            logPerspectivesError $ RolErrorBoundary "Api.CheckBinding" (show err)
            sendResponse (Error corrId (show $ RolErrorBoundary "Api.CheckBinding" (show err))) setter
          Right (PerspectRol{pspType}) -> do
            void $ runMonadPerspectivesTransaction' false authoringRole (lift $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ (unwrap pspType)))
            ok <- checkBinding typeOfRoleToBindTo (RoleInstance object)
            sendResponse (Result corrId [(show ok)]) setter
    Api.SetProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (setProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate) Nothing [(Value object)])
        sendResponse (Result corrId []) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    Api.AddProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (addProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate) [(Tuple (Value object) Nothing)])
        sendResponse (Result corrId []) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- {request: "DeleteProperty", subject: rolID, predicate: propertyName, authoringRole: myroletype}
    Api.DeleteProperty -> catchError
      (do
        void $ runMonadPerspectivesTransaction authoringRole (deleteProperty [(RoleInstance subject)] (EnumeratedPropertyType predicate) Nothing) 
        sendResponse (Result corrId []) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- { request: "SaveFile"
    -- , subject: PerspectivesFile serialised (may be without database)
    -- , contextDescription: buf
    -- , authoringRole: myroletype
    -- , onlyOnce: true}
    Api.SaveFile -> catchError      
      (case parsePerspectivesFile subject of 
        Left errs -> sendResponse (Error corrId ("Could not parse PerspectivesFile: " <> subject <> ".")) setter
        Right {roleFileName, propertyType, mimeType} -> do
          newVal <- runMonadPerspectivesTransaction authoringRole 
            (do
              result <- saveFile (RoleInstance roleFileName) propertyType contextDescription mimeType
              setProperty [RoleInstance roleFileName] propertyType Nothing [Value result]
              pure result)
          sendResponse (Result corrId [newVal]) setter)
      (\e -> sendResponse (Error corrId (show e)) setter)
    Api.GetFile -> catchError
      (do 
        mrid <- getPropertyBearingRoleInstance (EnumeratedPropertyType predicate) (RoleInstance subject)
        RoleProp rid replacementProperty <- case mrid of 
          -- Notice that we now assume the property is indeed represented on instances of this type.
          -- This may go wrong when we actually have no property value yet but it should NOT be represented on the given instance.
          Nothing -> pure $ RoleProp (RoleInstance subject) (EnumeratedPropertyType predicate)
          Just x -> pure x
        {database, documentName} <- resourceIdentifier2DocLocator (unwrap rid)
        ma <- getAttachment database documentName (typeUri2couchdbFilename predicate)
        case ma of
          Nothing -> sendResponse (Error corrId ("No file found for property " <> predicate <> ".")) setter
          -- NOTE that we force the Foreign value through the api. This MUST be handled correctly in the proxy!
          Just a -> do
            -- We need the property value because it may contain a user-specified name for the file to download.
            mv <- rid ##> getProperty replacementProperty
            case mv of
              Nothing -> sendResponse (Error corrId ("No file information found for property " <> predicate <> ".")) setter
              Just (Value v) -> case parsePerspectivesFile v of
                Left e -> sendResponse (Error corrId (show (PerspectivesFileFormatError v (show e)))) setter
                Right rec -> do
                  file <- liftEffect $ toFile rec.fileName rec.mimeType a
                  -- Notice that we force the foreign value to comply to String. 
                  -- The proxy will handle it!
                  sendResponse (Result corrId [unsafeCoerce file]) setter
      )
      (\e -> sendResponse (Error corrId (show e)) setter)
    -- { request: Action
    -- , subject: <user role instance>
    -- , predicate: <object of perspective role instance>
    -- , object: <context instance>
    -- , contextDescription:
    --   { perspectiveId:
    --   , actionName:
    --   }
    -- ...}
    Api.Action -> catchError
        (case read contextDescription of
          Left err -> sendResponse (Error corrId ("Incorrectly formed description of Action on applying an action in context instance '" <> object <> "' and object role instance '" <> predicate <> "'. Errors: " <> show err )) setter
          Right ({perspectiveId, actionName} :: {perspectiveId :: String, actionName :: String}) -> do
            -- Find the action from the authoringRole, the perspective id, the Action name.
            maction <- (map $ getAction actionName) <$> (findPerspective authoringRole (\(Perspective{id})-> pure $ id `eq` perspectiveId))
            mauthoringRoleInstance <- (ContextInstance object) ##> getMeInRoleAndContext authoringRole
            case mauthoringRoleInstance, maction of
              Just author, Just (Just (ACTION.Action action)) -> do
                void $ runMonadPerspectivesTransaction authoringRole
                  do
                    oldFrame <- lift $ pushFrame
                    lift $ addBinding "currentcontext" [object]
                    lift $ addBinding "currentactor" [unwrap author]
                    updater <- lift $ compileAssignmentFromRole action
                    updater (RoleInstance predicate)
                    lift $ restoreFrame oldFrame
                sendResponse (Result corrId []) setter
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
          Just user, Just (ACTION.Action action) -> do 
            void $ runMonadPerspectivesTransaction userRoleType
              do
                oldFrame <- lift $ pushFrame
                lift $ addBinding "currentcontext" [object]
                lift $ addBinding "currentactor" [unwrap user]
                updater <- lift $ compileAssignment action
                updater (ContextInstance object)
                lift $ restoreFrame oldFrame
            sendResponse (Result corrId []) setter
          _, _ -> sendResponse (Error corrId $ "cannot identify Action with role type '" <> show userRoleType <>
            "' and action name '" <> predicate <> "'.") setter
        )
      (\e -> sendResponse (Error corrId (show e)) setter)

    -- `subject` is a role, object is a role type identifier (either
    -- enumerated or calculated).
    Api.SetPreferredUserRoleType -> catchError
      (do
        mroleType <- (RoleInstance subject) ##> (context >=> contextType >=> (liftToInstanceLevel $ lookForRoleType object))
        case mroleType of
          Nothing -> sendResponse (Error corrId ("Cannot find a role type with name '" <> object <> "' in the context of the external role '" <> subject <> "'.")) setter
          Just roleType -> do
            ctxt <- (RoleInstance subject) ##>> context
            void $ runMonadPerspectivesTransaction authoringRole (setPreferredUserRoleType ctxt [roleType])
            sendResponse (Result corrId []) setter)
      \e -> sendResponse (Error corrId (show e)) setter
    
    Api.Save -> catchError 
      (do
        saveMarkedResources
        sendResponse (Result corrId []) setter)
      \e -> sendResponse (Error corrId (show e)) setter

    Api.EvaluateRoleInstance -> catchError
      (do 
        roleType <- roleType_ (RoleInstance subject)
        void $ runMonadPerspectivesTransaction authoringRole (evaluateRoleState (RoleInstance subject) (StateIdentifier $ unwrap roleType)))
      \e -> sendResponse (Error corrId (show e)) setter

    Api.Unsubscribe -> unregisterSupportedEffect corrId
    Api.WrongRequest -> sendResponse (Error corrId subject) setter
    _ -> sendResponse (Error corrId ("Perspectives could not handle this request: '" <> (showRequestRecord r) <> "'")) (mkApiEffect reactStateSetter)
  where
    setter = (mkApiEffect reactStateSetter)

    -- The identifier in the ContextType must be fully qualified.
    -- The model describing the ContextType must be locally installed.
    withLocalName :: String -> ContextType -> (RoleType -> MonadPerspectives Unit) -> MonadPerspectives Unit
    withLocalName localRoleName contextType effect = do
      qrolNames <- if isTypeUri localRoleName
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
    withNewContext authoringRole mroleType effect = case read contextDescription of
      (Left e :: Either (NonEmptyList ForeignError) ContextSerialization) -> sendResponse (Error corrId (show e)) setter
      (Right cd@(ContextSerialization {ctype}) :: Either (NonEmptyList ForeignError) ContextSerialization) -> do
        void $ runMonadPerspectivesTransaction authoringRole do
          void $ lift $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ ctype)
          ctxt <- runExceptT $ constructContext mroleType cd
          case ctxt of
            (Left messages) -> lift $ sendResponse (Error corrId (show messages)) setter
            (Right ctxtId) -> effect ctxtId

-----------------------------------------------------------
-- API FUNCTIONS
-----------------------------------------------------------
type ReactStateSetter = Array String -> Effect Unit

type QueryUnsubscriber :: forall k. k -> Type
type QueryUnsubscriber e = Effect Unit

-- | Apply an ApiEffect to a Response, in effect sending it through the API to the caller.
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse r ae = liftEffect $ ae r

newtype ChatParticipant = ChatParticipant String
derive instance Newtype ChatParticipant _
type ChatParticipantFields = {roleInstance :: RoleInstance, firstname :: Maybe Value, lastname :: Maybe Value, avatar :: Maybe String} -- avatar will be a PSharedFile.
