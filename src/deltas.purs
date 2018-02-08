module Perspectives.Deltas where

import Control.Monad.Aff (Aff, error, liftEff', throwError)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Exception (Error, catchException)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State.Trans (StateT, execStateT, lift)
import Data.Array (head)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericEncode)
import Data.Foreign.Generic.Class (class GenericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, toISOString)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Network.HTTP.Affjax (AffjaxResponse, put)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.ContextAndRole (changeContext_rev, changeContext_type, context_rev)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectEntiteit (encode)
import Perspectives.Resource (changePerspectEntiteit, getPerspectEntiteit)
import Perspectives.ResourceRetrieval (modifyResourceInCouchdb)
import Perspectives.Syntax (ID, PerspectContext, PerspectRol)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (tripleObjects)
import Perspectives.TripleGetter ((##))
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<>), (==), (||))

newtype SerializableDateTime = SerializableDateTime DateTime

instance encodeSerializableDateTime :: Encode SerializableDateTime where
  encode d = toForeign $ show d

instance showSerializableDateTime :: Show SerializableDateTime where
  show (SerializableDateTime d) = runPure (catchException (\err -> pure "Could not serialize DateTime") (toISOString (fromDateTime d)))

newtype Transactie = Transactie
  { author :: String
  , timeStamp :: SerializableDateTime
  , rolDeltas :: Array RolDelta
  , contextDeltas :: Array ContextDelta
  , createdContexts :: Array PerspectContext
  , createdRoles :: Array PerspectRol
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
    pure $ Transactie{ author: author, timeStamp: SerializableDateTime (toDateTime n), rolDeltas: [], contextDeltas: [], createdContexts: [], createdRoles: []}

newtype RolDelta = RolDelta
  { id :: ID
  , propertyName :: String
  , deltaType :: DeltaType
  , value :: NullOrUndefined String
  }

derive instance genericRolDelta :: Generic RolDelta _

instance showRolDelta :: Show RolDelta where
  show = genericShow

instance encodeRolDelta :: Encode RolDelta where
  encode = encodeDefault

newtype ContextDelta = ContextDelta
  { id :: ID
  , rolName :: String
  , deltaType :: DeltaType
  , rolID :: NullOrUndefined String
  }

derive instance genericContextDelta :: Generic ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

instance encodeContextDelta :: Encode ContextDelta where
  encode = encodeDefault

data DeltaType = Add | Remove | Change

derive instance genericDeltaType :: Generic DeltaType _

instance showDeltaType :: Show DeltaType where
  show = genericShow

instance encodeDeltaType :: Encode DeltaType where
  encode = encodeDefault

encodeDefault :: forall t a. Generic a t => GenericEncode t => a -> Foreign
encodeDefault = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- TODO: encoding van Transactie enz.

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

runInTransactie :: forall e. StateT Transactie (Aff (AjaxAvarCache (now :: NOW | e))) Unit -> Aff (AjaxAvarCache (now :: NOW | e)) Unit
runInTransactie m = do
  s <- liftEff' (createTransactie "Joop")
  t <- execStateT m s
  distributeTransactie t

setContextType :: forall e. ID -> ID -> StateT Transactie (Aff (AjaxAvarCache e)) Unit
setContextType cid theType = do
  (context :: PerspectContext) <- lift $ onNothing ("setContextType: cannot find this context: " <> cid) (getPerspectEntiteit cid)
  -- Change the entity in cache:
  changedEntity <- lift $ changePerspectEntiteit cid (changeContext_type theType context)
  rev <- lift $ onNothing' ("setContextType: context has no revision, deltas are impossible: " <> cid) (context_rev context)
  -- Store the changed entity in couchdb.
  newRev <- lift $ modifyResourceInCouchdb cid rev (encode context)
  -- Set the new revision in the entity.
  lift $ changePerspectEntiteit cid (changeContext_rev newRev context)
  -- Create a delta and add it to the Transactie.
  addContextDelta $ ContextDelta
    { id : cid
    , rolName: "psp:type"
    , deltaType: Change
    , rolID: NullOrUndefined (Just theType)
    }

-- TODO: Voeg de delta toe of vervang een bestaande ermee.
addContextDelta :: forall e. ContextDelta -> StateT Transactie (Aff e) Unit
addContextDelta cd = pure unit

onNothing :: forall a m. MonadThrow Error m => String -> m (Maybe a) -> m a
onNothing message ma = do
  a <- ma
  case a of
    Nothing -> throwError $ error message
    (Just v) -> pure v

onNothing' :: forall a m. MonadThrow Error m => String -> Maybe a -> m a
onNothing' message ma = do
  case ma of
    Nothing -> throwError $ error message
    (Just v) -> pure v

distributeTransactie :: forall e. Transactie -> Aff (AjaxAvarCache e) Unit
distributeTransactie t = do
  others <- usersInvolvedInTransactie t
  _ <- for others (\other -> sendTransactieToUser other t)
  pure unit

-- TODO. Niet alle delta's in de transactie hoeven voor de gebruiker bedoeld te zijn!
sendTransactieToUser :: forall e. ID -> Transactie -> Aff (AjaxAvarCache e) Unit
sendTransactieToUser userId t = do
  tripleUserIP <- userId ## identity
  (userIP :: String) <- onNothing' ("sendTransactieToUser: user has no IP: " <> userId) (head (tripleObjects tripleUserIP))
  (res :: AffjaxResponse String)  <- put (userIP <> "/" <> userId <> "_post/" <> "transactie id hier") (encodeJSON t)
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> pure unit
    false -> throwError $ error ("sendTransactieToUser " <> "transactie id hier" <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  pure unit

usersInvolvedInTransactie :: forall e. Transactie -> Aff e (Array ID)
usersInvolvedInTransactie t = pure []
