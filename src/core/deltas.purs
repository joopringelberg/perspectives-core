module Perspectives.Deltas where

import Control.Monad.Aff (error, throwError)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (catchException, Error)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State.Trans (StateT, execStateT, get, lift, put)
import Data.Array (cons, delete, deleteAt, elemIndex, find, findIndex, foldr, head)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (encodeJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Function (flip)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, toISOString)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.StrMap (StrMap, empty, insert, lookup)
import Data.Traversable (for_, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Network.HTTP.Affjax (AffjaxResponse, put) as AJ
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (addContext_rolInContext, addRol_gevuldeRollen, addRol_property, changeContext_displayName, changeContext_type, changeRol_binding, changeRol_context, changeRol_type, removeContext_rolInContext, removeRol_gevuldeRollen, removeRol_property, setRol_property, setContext_rolInContext)
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.CoreTypes (TypedTripleGetter, MonadPerspectives, runMonadPerspectivesQuery, (##), tripleObjects)
import Perspectives.Effects (AjaxAvarCache, TransactieEffects)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, MemberName, PropertyName, RolID, RolName, Value)
import Perspectives.Identifiers (deconstructNamespace, isUserEntiteitID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, cacheCachedEntiteit, cacheInDomeinFile)
import Perspectives.Property (getRolBinding)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (contains, rolesOf, toBoolean, filter)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (saveVersionedEntiteit)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..))
import Perspectives.SystemQueries (binding, buitenRol, contextType, identity, isFunctional, lijdendVoorwerpBepaling, propertyReferentie, rolContext, rolUser)
import Perspectives.TheoryChange (modifyTriple, updateFromSeeds)
import Perspectives.TripleGetter (constructInverseRolGetter, constructRolGetter)
import Perspectives.TypesForDeltas (Delta(..), DeltaType(..), encodeDefault)
import Perspectives.User (getUser)
import Perspectives.Utilities (onNothing, onNothing') as Util
import Prelude (class Show, type (~>), Unit, bind, discard, id, pure, show, unit, void, ($), (&&), (<<<), (<>), (==), (>>=), (||))

-----------------------------------------------------------
-- DATETIME
-- We need a newtype for DateTime in order to be able to serialize and show it.
-----------------------------------------------------------
newtype SerializableDateTime = SerializableDateTime DateTime

instance encodeSerializableDateTime :: Encode SerializableDateTime where
  encode d = toForeign $ show d

instance showSerializableDateTime :: Show SerializableDateTime where
  show (SerializableDateTime d) = runPure (catchException (\err -> pure "Could not serialize DateTime") (toISOString (fromDateTime d)))

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transactie = Transactie
  { author :: String
  , timeStamp :: SerializableDateTime
  , deltas :: Array Delta
  , createdContexts :: Array PerspectContext
  , createdRoles :: Array PerspectRol
  , deletedContexts :: Array ID
  , deletedRoles :: Array ID
  , changedDomeinFiles :: Array ID
  }

derive instance genericRepTransactie :: Generic Transactie _

instance showTransactie :: Show Transactie where
  show = genericShow

instance encodeTransactie :: Encode Transactie where
  encode = encodeDefault

createTransactie :: forall e. String -> Eff (now :: NOW | e) Transactie
createTransactie author =
  do
    n <- now
    pure $ Transactie{ author: author, timeStamp: SerializableDateTime (toDateTime n), deltas: [], createdContexts: [], createdRoles: [], deletedContexts: [], deletedRoles: [], changedDomeinFiles: []}

transactieID :: Transactie -> String
transactieID (Transactie{author, timeStamp}) = author <> "_" <> show timeStamp

type MonadTransactie e = StateT Transactie (MonadPerspectives (AjaxAvarCache e))

-- TODO: kan het zo zijn dat er al een transactie loopt? En wat dan? Denk aan acties met effect.
runInTransactie :: forall e.
  -- MonadTransactie (now :: NOW | e) Unit
  StateT Transactie (MonadPerspectives (TransactieEffects e)) Unit ->
  MonadPerspectives (TransactieEffects e) Unit
runInTransactie m = do
  user <- getUser
  s <- liftEff (createTransactie user)
  t@(Transactie{deltas, changedDomeinFiles}) <- execStateT m s
  -- register a triple for each delta, add it to the queue, run the queue.
  maybeTriples <- traverse (lift <<< modifyTriple) deltas
  modifiedTriples <- pure (foldr (\mt a -> maybe a (flip cons a) mt) [] maybeTriples)
  -- Propagate the triple changes.
  _ <- updateFromSeeds modifiedTriples
  for_ changedDomeinFiles saveCachedDomeinFile
  -- Send the Transaction to all involved.
  distributeTransactie t

distributeTransactie :: forall e. Transactie -> MonadPerspectives (AjaxAvarCache e) Unit
distributeTransactie t = do
  (customizedTransacties :: StrMap Transactie) <- transactieForEachUser t
  _ <- forWithIndex customizedTransacties sendTransactieToUser
  pure unit

addContextToTransactie :: forall e. PerspectContext ->
  -- MonadTransactie e Unit
  MonadTransactie e Unit
addContextToTransactie c = do
  (Transactie tf@{createdContexts}) <- get
  put $ Transactie tf {createdContexts = cons c createdContexts}

addRolToTransactie :: forall e. PerspectRol -> MonadTransactie e Unit
addRolToTransactie c = do
  (Transactie tf@{createdRoles}) <- get
  put $ Transactie tf {createdRoles = cons c createdRoles}

deleteContextFromTransactie :: forall e. PerspectContext -> MonadTransactie e Unit
deleteContextFromTransactie c@(PerspectContext{_id}) = do
  (Transactie tf@{createdContexts, deletedContexts}) <- get
  case findIndex (\(PerspectContext{_id: i}) -> _id == i) createdContexts of
    Nothing -> put (Transactie tf{deletedContexts = cons _id deletedContexts})
    (Just i) -> put (Transactie tf{createdContexts = unsafePartial $ fromJust $ deleteAt i createdContexts})

deleteRolFromTransactie :: forall e. PerspectRol -> MonadTransactie e Unit
deleteRolFromTransactie c@(PerspectRol{_id}) = do
  (Transactie tf@{createdRoles, deletedRoles}) <- get
  case findIndex (\(PerspectRol{_id: i}) -> _id == i) createdRoles of
    Nothing -> put (Transactie tf{deletedRoles = cons _id deletedRoles})
    (Just i) -> put (Transactie tf{createdRoles = unsafePartial $ fromJust $ deleteAt i createdRoles})

addDomeinFileToTransactie :: forall e. ID -> MonadTransactie e Unit
addDomeinFileToTransactie dfId = do
  (Transactie tf@{changedDomeinFiles}) <- get
  case elemIndex dfId changedDomeinFiles of
    Nothing -> put $ Transactie tf {changedDomeinFiles = cons dfId changedDomeinFiles}
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
addDelta :: forall e. Delta -> MonadTransactie e Unit
addDelta newCD@(Delta{id: id', memberName, deltaType, value}) = do
  t@(Transactie tf@{deltas}) <- get
  case elemIndex newCD deltas of
    (Just _) -> pure unit
    Nothing -> do
      (isfunc :: Boolean) <- lift $ runMonadPerspectivesQuery memberName (toBoolean isFunctional)
      if isfunc
        then do
          x <- pure $ findIndex equalExceptRolID deltas
          case x of
            (Just oldCD) -> put (replace oldCD newCD t)
            Nothing -> do
              mCdelta <- pure $ find equalIdRolName deltas
              case mCdelta of
                Nothing -> put (add newCD t)
                (Just oldCD@(Delta oldF@{deltaType: d})) -> do
                  indexOld <- pure (unsafePartial (fromJust (elemIndex oldCD deltas)))
                  case d of
                    Add | (deltaType == Remove) -> put (remove oldCD t)
                    Remove | (deltaType == Add) -> put (remove oldCD t)
                    Change | (deltaType == Remove) -> put (replace indexOld newCD t)
                    Add | (deltaType == Change) -> put (replace indexOld (Delta oldF {value = value}) t)
                    otherwise -> put (add newCD t)
        else do
          x <- pure $ findIndex equalExceptDeltaType deltas
          case x of
            Nothing -> put (add newCD t)
            (Just i) -> put (replace i newCD t)
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
  tripleUserIP <- userId ## identity
  (userIP :: String) <- onNothing' ("sendTransactieToUser: user has no IP: " <> userId) (head (tripleObjects tripleUserIP))
  -- TODO controleer of hier authentication nodig is!
  (res :: AJ.AffjaxResponse String)  <- liftAff $ AJ.put (userIP <> "/" <> userId <> "_post/" <> transactieID t) (encodeJSON t)
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> pure unit
    false -> throwError $ error ("sendTransactieToUser " <> transactieID t <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  pure unit

-- | The (IDs of the) users that play a role in, and have a relevant perspective on, the Context that is modified;
-- | or the users that play a role in the context of the Role that is modified and have a relevant perspective on that Role.
usersInvolvedInDelta :: forall e. Delta -> MonadPerspectives (AjaxAvarCache e) (Array ID)
usersInvolvedInDelta dlt@(Delta{isContext}) = if isContext then usersInvolvedInContext dlt else usersInvolvedInRol dlt
  where

  usersInvolvedInRol :: Delta -> MonadPerspectives (AjaxAvarCache e) (Array ID)
  usersInvolvedInRol (Delta{id, memberName}) =
    do
      queryResult <-
        -- Get the RolInContext (types) that have a perspective on (the context represented by) 'id'.
        id ## (filter (hasRelevantView memberName) actiesWithThatLijdendVoorwerp) >-> onderwerpFillers >->
        -- Get the instances of these roles in 'id'
        (rolesOf id) >->
        -- Get the users that are at the bottom of the telescope of these role instances.
        rolUser
      pure $ tripleObjects queryResult
    where
      actiesWithThatLijdendVoorwerp = isLijdendVoorwerpIn >-> rolContext

  usersInvolvedInContext :: Delta -> MonadPerspectives (AjaxAvarCache e) (Array ID)
  usersInvolvedInContext (Delta{id, memberName}) =
    do
      queryResult <-
        -- Get the RolInContext (types) that have a perspective on (the context represented by) 'id'.
        id ## (filter (hasRelevantView memberName) actiesWithThatLijdendVoorwerp) >-> onderwerpFillers >->
        -- Get the instances of these roles in 'id'
        (rolesOf id) >->
        -- Get the users that are at the bottom of the telescope of these role instances.
        rolUser
      pure $ tripleObjects queryResult
    where
      actiesWithThatLijdendVoorwerp = contextType >-> buitenRol >-> isLijdendVoorwerpIn >-> rolContext

  -- Roles that fill the syntactic role "LijdendVoorwerp".
  isLijdendVoorwerpIn :: TypedTripleGetter e
  isLijdendVoorwerpIn = (constructInverseRolGetter "model:Perspectives$:lijdendVoorwerp")
  -- Fillers of the syntactic role "Onderwerp".
  onderwerpFillers = (constructRolGetter "model:Arc$onderwerp") >-> binding
  -- Tests an Actie for having memberName in the view that is its lijdendVoorwerpBepaling.
  hasRelevantView :: ID -> TypedTripleGetter e
  hasRelevantView id = contains id (lijdendVoorwerpBepaling >-> propertyReferentie)

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
  ID -> Value -> MonadTransactie e Unit
updatePerspectEntiteit changeEntity createDelta cid value = do
  updatePerspectEntiteit' changeEntity cid value
  addDelta $ createDelta cid value

updatePerspectEntiteit' :: forall e a. PerspectEntiteit a =>
  (Value -> a -> a) ->
  ID -> Value -> MonadTransactie e Unit
updatePerspectEntiteit' changeEntity cid value = do
  (entity) <- lift $ onNothing ("updatePerspectEntiteit: cannot find this context: " <> cid) (getPerspectEntiteit cid)
  if (isUserEntiteitID cid)
    then do
      -- Change the entity in cache:
      void $ lift $ cacheCachedEntiteit cid (changeEntity value entity)
      void $ lift $ saveVersionedEntiteit cid entity
    else do
      let dfId = (unsafePartial (fromJust (deconstructNamespace cid)))
      addDomeinFileToTransactie dfId
      lift $ cacheInDomeinFile dfId (changeEntity value entity)

  -- rev <- lift $ onNothing' ("updatePerspectEntiteit: context has no revision, deltas are impossible: " <> cid) (unNullOrUndefined (getRevision' context))
  -- -- Store the changed entity in couchdb.
  -- newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
  -- -- Set the new revision in the entity.
  -- lift $ cacheCachedEntiteit cid (setRevision newRev context)

setContextType :: forall e. ID -> ID -> MonadTransactie e Unit
setContextType = updatePerspectEntiteit
  changeContext_type
  (\cid theType -> Delta
    { id : cid
    , memberName: "model:Perspectives$type"
    , deltaType: Change
    , value: NullOrUndefined (Just theType)
    , isContext: true
    })

setRolType :: forall e. ID -> ID -> MonadTransactie e Unit
setRolType = updatePerspectEntiteit
  changeRol_type
  (\cid theType -> Delta
    { id : cid
    , memberName: "model:Perspectives$type"
    , deltaType: Change
    , value: NullOrUndefined (Just theType)
    , isContext: false
    })

setContextDisplayName :: forall e. ID -> ID -> MonadTransactie e Unit
setContextDisplayName = updatePerspectEntiteit
  changeContext_displayName
  (\cid displayName -> Delta
    { id : cid
    , memberName: "model:Perspectives$label"
    , deltaType: Change
    , value: NullOrUndefined (Just displayName)
    , isContext: true
    })

setContext :: forall e. ID -> ID -> MonadTransactie e Unit
setContext = updatePerspectEntiteit
  changeRol_context
  (\cid rol -> Delta
    { id : cid
    , memberName: "model:Perspectives$context"
    , deltaType: Change
    , value: NullOrUndefined (Just rol)
    , isContext: false
    })

setBinding :: forall e. ID -> ID -> MonadTransactie e Unit
setBinding rid binding = do
  oldBinding <- lift $ getRolBinding rid
  updatePerspectEntiteit' changeRol_binding rid binding
  case head oldBinding of
    Nothing -> pure unit
    (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob "model:Perspectives$binding" rid
  updatePerspectEntiteitMember' addRol_gevuldeRollen binding "model:Perspectives$binding" rid
  addDelta $ Delta
    { id : rid
    , memberName: "model:Perspectives$binding"
    , deltaType: Change
    , value: NullOrUndefined (Just binding)
    , isContext: false
    }

-----------------------------------------------------------
-- UPDATEPERSPECTENTITEITMEMBER
-----------------------------------------------------------
updatePerspectEntiteitMember :: forall e a. PerspectEntiteit a =>
  (a -> MemberName -> Value -> a) ->
  (ID -> MemberName -> Value -> Delta) ->
  ID -> MemberName -> Value -> MonadTransactie e Unit
updatePerspectEntiteitMember changeEntityMember createDelta cid memberName value = do
  updatePerspectEntiteitMember' changeEntityMember cid memberName value
  addDelta $ createDelta cid memberName value

updatePerspectEntiteitMember' :: forall e a. PerspectEntiteit a =>
  (a -> MemberName -> Value -> a) ->
  ID -> MemberName -> Value -> MonadTransactie e Unit
updatePerspectEntiteitMember' changeEntityMember cid memberName value = do
  (context) <- lift $ onNothing ("updateRoleProperty: cannot find this context: " <> cid) (getPerspectEntiteit cid)
  -- Change the entity in cache:
  void $ lift $ cacheCachedEntiteit cid (changeEntityMember context memberName value)
  void $ lift $ saveVersionedEntiteit cid context

  -- rev <- lift $ onNothing' ("updateRoleProperty: context has no revision, deltas are impossible: " <> cid) (unNullOrUndefined (getRevision' context))
  -- -- Store the changed entity in couchdb.
  -- newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
  -- -- Set the new revision in the entity.
  -- lift $ cacheCachedEntiteit cid (setRevision newRev context)

-- | Add a rol to a context (and inversely register the context with the rol)
-- | In a functional rol, remove an old
addRol :: forall e. ContextID -> RolName -> RolID -> MonadTransactie e Unit
addRol =
  updatePerspectEntiteitMember
    addContext_rolInContext
    (\cid rolName rolId ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Add
        , value: NullOrUndefined (Just rolId)
        , isContext: true
        })

removeRol :: forall e. ContextID -> RolName -> RolID -> MonadTransactie e Unit
removeRol =
  updatePerspectEntiteitMember
    removeContext_rolInContext
    (\cid rolName rolId ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Remove
        , value: NullOrUndefined (Just rolId)
        , isContext: true
        })

setRol :: forall e. ContextID -> RolName -> RolID -> MonadTransactie e Unit
setRol =
  updatePerspectEntiteitMember
    setContext_rolInContext
    (\cid rolName rolId ->
      Delta
        { id : cid
        , memberName: rolName
        , deltaType: Change
        , value: NullOrUndefined (Just rolId)
        , isContext: true
        })

addProperty :: forall e. RolID -> PropertyName -> Value -> MonadTransactie e Unit
addProperty =
  updatePerspectEntiteitMember
    addRol_property
    (\rid propertyName value ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Add
        , value: NullOrUndefined (Just value)
        , isContext: false
        })

removeProperty :: forall e. RolID -> PropertyName -> Value -> MonadTransactie e Unit
removeProperty =
  updatePerspectEntiteitMember
    removeRol_property
    (\rid propertyName value ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Remove
        , value: NullOrUndefined (Just value)
        , isContext: false
        })

setProperty :: forall e. RolID -> PropertyName -> Value -> MonadTransactie e Unit
setProperty =
  updatePerspectEntiteitMember
    setRol_property
    (\rid propertyName value ->
      Delta
        { id : rid
        , memberName: propertyName
        , deltaType: Change
        , value: NullOrUndefined (Just value)
        , isContext: false
        })

onNothing :: forall m a. MonadThrow Error m => String -> m (Maybe a) -> m a
onNothing = Util.onNothing <<< error

onNothing' :: forall m. MonadThrow Error m => String -> Maybe ~> m
onNothing' = Util.onNothing' <<< error
