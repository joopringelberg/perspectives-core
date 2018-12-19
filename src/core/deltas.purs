module Perspectives.Deltas where

import Control.Monad.Aff (error, throwError)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State.Trans (StateT, execStateT, get, lift, put)
import Data.Array (cons, delete, deleteAt, elemIndex, find, findIndex, foldr, head)
import Data.Foreign.Generic (encodeJSON)
import Data.Function (flip)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.StrMap (StrMap, empty, insert, lookup)
import Data.Traversable (for_, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Network.HTTP.Affjax (AffjaxResponse, put) as AJ
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (addContext_rolInContext, addRol_gevuldeRollen, addRol_property, changeContext_displayName, changeContext_type, changeRol_binding, changeRol_context, changeRol_type, removeContext_rolInContext, removeRol_gevuldeRollen, removeRol_property, setRol_property, setContext_rolInContext)
import Perspectives.CoreTypes (MonadPerspectives, Transactie(..), Triple(..), TypedTripleGetter, createTransactie, transactieID, (%%>>))
import Perspectives.DataTypeObjectGetters (binding, context)
import Perspectives.DataTypeTripleGetters (identityM, rolTypeM)
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.Effects (AjaxAvarCache, TransactieEffects)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, MemberName, PropertyName, RolID, RolName, Value)
import Perspectives.Identifiers (deconstructModelName, isUserEntiteitID)
import Perspectives.ModelBasedTripleGetters (actiesInContextDefM, ownRollenDefM, rolInContextDefM, inverse_subjectRollenDefM, propertyIsFunctioneelM, rolIsFunctioneelM, bindingDefM, objectRollenDefM, objectViewDefM, propertyReferentiesM, rolUserM, subjectRollenDefM)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, cacheCachedEntiteit, cacheInDomeinFile)
import Perspectives.PerspectivesState (setTransactie, transactie)
import Perspectives.QueryCombinators (contains, filter, intersect, notEmpty, rolesOf, toBoolean)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (saveVersionedEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##), (##>))
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..))
import Perspectives.TheoryChange (addTripleToQueue, modifyTriple, updateFromSeeds)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..))
import Perspectives.User (getUser)
import Perspectives.Utilities (maybeM)
import Perspectives.Utilities (onNothing, onNothing') as Util
import Prelude (type (~>), Unit, bind, discard, id, pure, show, unit, void, ($), (&&), (<<<), (<>), (==), (>>=), (||))

-- TODO: doe ook wat met de andere modificaties in de transactie?
runTransactie :: forall e. MonadPerspectives (TransactieEffects e) Unit
runTransactie = do
  user <- getUser
  t@(Transactie{deltas, changedDomeinFiles}) <- transactie
  -- register a triple for each delta, add it to the queue, run the queue.
  -- maybeTriples <- traverse (lift <<< modifyTriple) deltas
  -- modifiedTriples <- pure (foldr (\mt a -> maybe a (flip cons a) mt) [] maybeTriples)
  -- Propagate the triple changes.
  -- _ <- updateFromSeeds modifiedTriples
  for_ changedDomeinFiles saveCachedDomeinFile
  -- Send the Transaction to all involved.
  distributeTransactie t
  (lift $ createTransactie user) >>= setTransactie

distributeTransactie :: forall e. Transactie -> MonadPerspectives (AjaxAvarCache e) Unit
distributeTransactie t = do
  (customizedTransacties :: StrMap Transactie) <- transactieForEachUser t
  _ <- forWithIndex customizedTransacties sendTransactieToUser
  pure unit

addContextToTransactie :: forall e. PerspectContext ->
  MonadPerspectives (avar :: AVAR | e) Unit
addContextToTransactie c = do
  (Transactie tf@{createdContexts}) <- transactie
  setTransactie $ Transactie tf {createdContexts = cons c createdContexts}
  -- put $ Transactie tf {createdContexts = cons c createdContexts}

addRolToTransactie :: forall e. PerspectRol -> MonadPerspectives (avar :: AVAR | e) Unit
addRolToTransactie c = do
  (Transactie tf@{createdRoles}) <- transactie
  setTransactie $ Transactie tf {createdRoles = cons c createdRoles}

deleteContextFromTransactie :: forall e. PerspectContext -> MonadPerspectives (avar :: AVAR | e) Unit
deleteContextFromTransactie c@(PerspectContext{_id}) = do
  (Transactie tf@{createdContexts, deletedContexts}) <- transactie
  case findIndex (\(PerspectContext{_id: i}) -> _id == i) createdContexts of
    Nothing -> setTransactie (Transactie tf{deletedContexts = cons _id deletedContexts})
    (Just i) -> setTransactie (Transactie tf{createdContexts = unsafePartial $ fromJust $ deleteAt i createdContexts})

deleteRolFromTransactie :: forall e. PerspectRol -> MonadPerspectives (avar :: AVAR | e) Unit
deleteRolFromTransactie c@(PerspectRol{_id}) = do
  (Transactie tf@{createdRoles, deletedRoles}) <- transactie
  case findIndex (\(PerspectRol{_id: i}) -> _id == i) createdRoles of
    Nothing -> setTransactie (Transactie tf{deletedRoles = cons _id deletedRoles})
    (Just i) -> setTransactie (Transactie tf{createdRoles = unsafePartial $ fromJust $ deleteAt i createdRoles})

addDomeinFileToTransactie :: forall e. ID -> MonadPerspectives (avar :: AVAR | e) Unit
addDomeinFileToTransactie dfId = do
  (Transactie tf@{changedDomeinFiles}) <- transactie
  case elemIndex dfId changedDomeinFiles of
    Nothing -> setTransactie $ Transactie tf {changedDomeinFiles = cons dfId changedDomeinFiles}
    otherwise -> pure unit

{-
1. Bepaal of de delta precies zo voorkomt in de lijst in de transactie. Zo ja, negeer dan de nieuwe delta.
2. Bepaal of de rol functioneel is.
ZO JA:
3. Zoek een delta waarvan de id, rolName en DeltaType gelijk zijn aan die van de nieuwe.
	Indien gevonden: vervang die door de nieuwe.
	Indien niet gevonden: zoek een delta waarvan id en rolName overeenkomen.
		Indien gevonden, als geldt:
			het ene DeltaType is Add en het andere Remove, verwijder dan de oude.
			het oude DeltaType is Change en het nieuwe Remove, vervang de oude dan door de nieuwe
			het oude DeltaType is Add en het nieuwe is Change, vervang dan in de oude de rolID door die van de nieuwe.
		Indien niet gevonden: voeg de nieuwe toe.
ZO NEE:
4. zoek een delta waarvan id, rolName en rolID gelijk zijn aan die van de nieuwe en het ene DeltaType Add is en het andere Remove.
	Indien gevonden: verwijder de oude.
	Anders: voeg de nieuwe toe.
-}
addDelta :: forall e. Delta -> MonadPerspectives (AjaxAvarCache e) Unit
addDelta newCD@(Delta{id: id', memberName, deltaType, value, isContext}) = do
  t@(Transactie tf@{deltas}) <- transactie
  case elemIndex newCD deltas of
    (Just _) -> pure unit
    Nothing -> do
      maybeM (pure unit) addTripleToQueue (lift $ modifyTriple newCD)
      (isfunc :: Boolean) <- runMonadPerspectivesQuery memberName (toBoolean (if isContext then rolIsFunctioneelM else propertyIsFunctioneelM ))
      if isfunc
        then do
          x <- pure $ findIndex equalExceptRolID deltas
          case x of
            (Just oldCD) -> setTransactie (replace oldCD newCD t)
            Nothing -> do
              mCdelta <- pure $ find equalIdRolName deltas
              case mCdelta of
                Nothing -> setTransactie (add newCD t)
                (Just oldCD@(Delta oldF@{deltaType: d})) -> do
                  indexOld <- pure (unsafePartial (fromJust (elemIndex oldCD deltas)))
                  case d of
                    Add | (deltaType == Remove) -> setTransactie (remove oldCD t)
                    Remove | (deltaType == Add) -> setTransactie (remove oldCD t)
                    Change | (deltaType == Remove) -> setTransactie (replace indexOld newCD t)
                    Add | (deltaType == Change) -> setTransactie (replace indexOld (Delta oldF {value = value}) t)
                    otherwise -> setTransactie (add newCD t)
        else do
          x <- pure $ findIndex equalExceptDeltaType deltas
          case x of
            Nothing -> setTransactie (add newCD t)
            (Just i) -> setTransactie (replace i newCD t)
  pure unit
  where
    equalExceptDeltaType :: Delta  -> Boolean
    equalExceptDeltaType
      (Delta{id: i, memberName: r, deltaType: d, value: ri}) =
        id' == i &&
        memberName == r &&
        value == ri &&
        ((deltaType == Add && d == Remove) || (deltaType == Remove && d == Add))
    equalExceptRolID :: Delta -> Boolean
    equalExceptRolID
      (Delta{id: i, memberName: r, deltaType: d}) =
        id' == i &&
        memberName == r &&
        deltaType == d
    equalIdRolName :: Delta -> Boolean
    equalIdRolName (Delta{id: i, memberName: r}) =
      id' == i &&
      memberName == r
    add :: Delta -> Transactie -> Transactie
    add delta (Transactie tf@{deltas}) = Transactie tf {deltas = cons delta deltas}
    replace :: Int -> Delta -> Transactie -> Transactie
    replace i delta (Transactie tf@{deltas}) = Transactie tf {deltas = cons newCD (maybe deltas id (deleteAt i deltas))}
    remove :: Delta -> Transactie -> Transactie
    remove i (Transactie tf@{deltas}) = Transactie tf {deltas = (delete i deltas)}


sendTransactieToUser :: forall e. ID -> Transactie -> MonadPerspectives (AjaxAvarCache e) Unit
sendTransactieToUser userId t = do
  tripleUserIP <- userId ##> identityM -- TODO. Het lijkt erop dat hier een getter toegepast moet worden die het IP adres van de user oplevert!
  (userIP :: String) <- onNothing' ("sendTransactieToUser: user has no IP: " <> userId) tripleUserIP
  -- TODO controleer of hier authentication nodig is!
  (res :: AJ.AffjaxResponse String)  <- liftAff $ AJ.put (userIP <> "/" <> userId <> "_post/" <> transactieID t) (encodeJSON t)
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> pure unit
    false -> throwError $ error ("sendTransactieToUser " <> transactieID t <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  pure unit

-- TODO. De verbinding tussen Actie en Rol is omgekeerd en is niet
-- langer geregistreerd als een rol van de Actie, maar als rol van de Rol (objectRol en subjectRol).
-- | The (IDs of the) users that play a role in, and have a relevant perspective on, the Context that is modified;
-- | or the users that play a role in the context of the Role that is modified and have a relevant perspective on that Role.
usersInvolvedInDelta :: forall e. Delta -> MonadPerspectives (AjaxAvarCache e) (Array ID)
usersInvolvedInDelta dlt@(Delta{isContext}) = if isContext then usersInvolvedInContext dlt else usersInvolvedInRol dlt
  where

  -- From the instance of a Rol, retrieve the instances of users that play another Rol
  -- in the same context, such that they have an Actie with an objectView that has the
  -- changed property (as identified by memberName).
  usersInvolvedInRol :: Delta -> MonadPerspectives (AjaxAvarCache e) (Array ID)
  usersInvolvedInRol (Delta{id, memberName}) =
    do
      (contextId :: ContextID) <- id %%>> context
      (Triple {object}) <-
        id ## rolTypeM >-> subjectsOfRelevantActies >-> (rolesOf contextId) >-> rolUserM
      pure $ object
    where
      -- roles in context that play the subjectRol in the relevant acties
      -- psp:Rol -> psp:Rol
      subjectsOfRelevantActies = filter (notEmpty (intersect subjectRollenDefM relevantActies)) (rolInContextDefM >-> ownRollenDefM)

      -- acties that have an objectView with the memberName
      -- psp:Rol -> psp:Actie
      relevantActies = filter (hasRelevantView memberName) (rolInContextDefM >-> actiesInContextDefM)

  -- From the instance of the context, retrieve the instances of the users that play
  -- a Rol in this context that have a subjectRol bound to an Actie that is bound as the
  -- objectRol of the Rol of which memberName is an instance.
  usersInvolvedInContext :: Delta -> MonadPerspectives (AjaxAvarCache e) (Array ID)
  usersInvolvedInContext (Delta{id, memberName}) =
    do
      (Triple {object}) <- id ## rolTypeM >-> actorsForObject >-> (rolesOf id) >-> rolUserM
      pure object
    where
      -- All Rollen that are the subject of Acties that have the Rol as object.
      -- `psp:Rol -> psp:Rol`
      actorsForObject = objectRollenDefM >-> inverse_subjectRollenDefM

  -- Tests an Actie for having memberName in the view that is its objectView.
  -- psp:Actie -> psp:Boolean
  hasRelevantView :: ID -> TypedTripleGetter e
  hasRelevantView id = contains id (objectViewDefM >-> propertyReferentiesM >-> bindingDefM)

{-
Bouw een transactie eerst op, splits hem dan in versies voor elke gebruiker.
Doorloop de verzameling deltas en bepaal per delta welke gebruikers betrokken zijn.
Bouw al doende een StrMap van userId en gespecialiseerde transacties op, waarbij je een transactie toevoegt voor een gebruiker die nog niet in de StrMap voorkomt.
-}
transactieForEachUser :: forall e. Transactie -> MonadPerspectives (AjaxAvarCache e)(StrMap Transactie)
transactieForEachUser t@(Transactie{deltas}) = do
  execStateT
    (for_ deltas (\d -> (lift $ usersInvolvedInDelta d) >>= (\users -> transactieForEachUser' d users)))
    empty
  where
    transactieForEachUser' :: Delta -> (Array ID) -> StateT (StrMap Transactie) (MonadPerspectives (AjaxAvarCache e)) Unit
    transactieForEachUser' d users = do
      trs <- get
      for_
        users
        (\user -> case lookup user trs of
          Nothing -> put $ insert user (transactieCloneWithJustDelta t d) trs
          (Just (Transactie tr@{deltas: ds})) -> put $ insert user (Transactie tr {deltas = cons d ds}) trs)
    transactieCloneWithJustDelta :: Transactie -> Delta -> Transactie
    transactieCloneWithJustDelta (Transactie tr) d = Transactie tr {deltas = [d]}
-----------------------------------------------------------
-- UPDATEPERSPECTENTITEIT
-----------------------------------------------------------
{-
Om een door de gebruiker aangebrachte wijziging door te voeren, moet je:
  - een Delta maken;
  - die versturen aan alle betrokkenen.
    - wat is het type van de context?
    - wie zijn de betrokkenen?
    - welke betrokkenen hebben een Actie met als lijdend voorwerp de entiteit?
    - heeft die Actie een view met de betreffende property?
  - de wijziging doorvoeren op de interne representatie;
  - de consequenties doorvoeren in de triple administratie;
  - de gewijzigde context opslaan;

Om een door een andere gebruiker aangebrachte wijziging door te voeren, moet je:
  - controleren of de author wel gerechtigd is tot de wijziging;
    - in welke rol is de author betrokken bij de context (van de rol)?
    - heeft die rol een actie die de betreffende delta oplevert?
      - past het werkwoord bij de DeltaType?
      - is het lijdend voorwerp de betreffende rol of context?
      - heeft de view op het lijdend voorwerp de relevante property (indien het gaat om een property delta)?
  - de wijziging doorvoeren op de interne representatie;
  - de consequenties doorvoeren in de triple administratie;
  - de gewijzigde context opslaan;
-}
-- | Create update functions on PerspectContext or PerspectRol.
updatePerspectEntiteit :: forall e a. PerspectEntiteit a =>
  (Value -> a -> a) ->
  (ID -> ID -> Delta) ->
  ID -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
updatePerspectEntiteit changeEntity createDelta cid value = do
  updatePerspectEntiteit' changeEntity cid value
  addDelta $ createDelta cid value

updatePerspectEntiteit' :: forall e a. PerspectEntiteit a =>
  (Value -> a -> a) ->
  ID -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
updatePerspectEntiteit' changeEntity cid value = do
  (entity) <- (getPerspectEntiteit cid)
  if (isUserEntiteitID cid)
    then do
      -- Change the entity in cache:
      void $ cacheCachedEntiteit cid (changeEntity value entity)
      void $ saveVersionedEntiteit cid entity
    else do
      let dfId = (unsafePartial (fromJust (deconstructModelName cid)))
      addDomeinFileToTransactie dfId
      cacheInDomeinFile dfId (changeEntity value entity)

  -- rev <- lift $ onNothing' ("updatePerspectEntiteit: context has no revision, deltas are impossible: " <> cid) (getRevision' context)
  -- -- Store the changed entity in couchdb.
  -- newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
  -- -- Set the new revision in the entity.
  -- lift $ cacheCachedEntiteit cid (setRevision newRev context)

setContextType :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Unit
setContextType = updatePerspectEntiteit
  changeContext_type
  (\cid theType -> Delta
    { id : cid
    , memberName: "model:Perspectives$type"
    , deltaType: Change
    , value:  (Just theType)
    , isContext: true
    })

setRolType :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Unit
setRolType = updatePerspectEntiteit
  changeRol_type
  (\cid theType -> Delta
    { id : cid
    , memberName: "model:Perspectives$type"
    , deltaType: Change
    , value:  (Just theType)
    , isContext: false
    })

setContextDisplayName :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Unit
setContextDisplayName = updatePerspectEntiteit
  changeContext_displayName
  (\cid displayName -> Delta
    { id : cid
    , memberName: "model:Perspectives$label"
    , deltaType: Change
    , value: (Just displayName)
    , isContext: true
    })

setContext :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Unit
setContext = updatePerspectEntiteit
  changeRol_context
  (\cid rol -> Delta
    { id : cid
    , memberName: "model:Perspectives$context"
    , deltaType: Change
    , value: (Just rol)
    , isContext: false
    })

setBinding :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Unit
setBinding rid boundRol = do
  oldBinding <- binding rid
  updatePerspectEntiteit' changeRol_binding rid boundRol
  case head oldBinding of
    Nothing -> pure unit
    (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob "model:Perspectives$binding" rid
  updatePerspectEntiteitMember' addRol_gevuldeRollen boundRol "model:Perspectives$binding" rid
  addDelta $ Delta
    { id : rid
    , memberName: "model:Perspectives$binding"
    , deltaType: Change
    , value: (Just boundRol)
    , isContext: false
    }

-----------------------------------------------------------
-- UPDATEPERSPECTENTITEITMEMBER
-----------------------------------------------------------
updatePerspectEntiteitMember :: forall e a. PerspectEntiteit a =>
  (a -> MemberName -> Value -> a) ->
  (ID -> MemberName -> Value -> Delta) ->
  ID -> MemberName -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
updatePerspectEntiteitMember changeEntityMember createDelta cid memberName value = do
  updatePerspectEntiteitMember' changeEntityMember cid memberName value
  addDelta $ createDelta cid memberName value

updatePerspectEntiteitMember' :: forall e a. PerspectEntiteit a =>
  (a -> MemberName -> Value -> a) ->
  ID -> MemberName -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
updatePerspectEntiteitMember' changeEntityMember cid memberName value = do
  (context :: a) <- getPerspectEntiteit cid
  -- Change the entity in cache:
  void $ cacheCachedEntiteit cid (changeEntityMember context memberName value)
  -- Save the entity to Couchdb.
  (changedContext :: a) <- getPerspectEntiteit cid
  void $ saveVersionedEntiteit cid changedContext

  -- rev <- lift $ onNothing' ("updateRoleProperty: context has no revision, deltas are impossible: " <> cid) (un(getRevision' context))
  -- -- Store the changed entity in couchdb.
  -- newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
  -- -- Set the new revision in the entity.
  -- lift $ cacheCachedEntiteit cid (setRevision newRev context)

-- | Add a rol to a context (and inversely register the context with the rol)
-- | TODO In a functional rol, remove the old value if present.
addRol :: forall e. ContextID -> RolName -> RolID -> MonadPerspectives (AjaxAvarCache e) Unit
addRol =
  updatePerspectEntiteitMember
    addContext_rolInContext
    (\cid rolName rolId ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Add
        , value: (Just rolId)
        , isContext: true
        })

removeRol :: forall e. ContextID -> RolName -> RolID -> MonadPerspectives (AjaxAvarCache e) Unit
removeRol =
  updatePerspectEntiteitMember
    removeContext_rolInContext
    (\cid rolName rolId ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Remove
        , value: (Just rolId)
        , isContext: true
        })

setRol :: forall e. ContextID -> RolName -> RolID -> MonadPerspectives (AjaxAvarCache e) Unit
setRol =
  updatePerspectEntiteitMember
    setContext_rolInContext
    (\cid rolName rolId ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Change
        , value: (Just rolId)
        , isContext: true
        })

addProperty :: forall e. RolID -> PropertyName -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
addProperty =
  updatePerspectEntiteitMember
    addRol_property
    (\rid propertyName value ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Add
        , value: (Just value)
        , isContext: false
        })

removeProperty :: forall e. RolID -> PropertyName -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
removeProperty =
  updatePerspectEntiteitMember
    removeRol_property
    (\rid propertyName value ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Remove
        , value: (Just value)
        , isContext: false
        })

setProperty :: forall e. RolID -> PropertyName -> Value -> MonadPerspectives (AjaxAvarCache e) Unit
setProperty =
  updatePerspectEntiteitMember
    setRol_property
    (\rid propertyName value ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Change
        , value: (Just value)
        , isContext: false
        })

onNothing :: forall m a. MonadThrow Error m => String -> m (Maybe a) -> m a
onNothing = Util.onNothing <<< error

onNothing' :: forall m. MonadThrow Error m => String -> Maybe ~> m
onNothing' = Util.onNothing' <<< error
