module Perspectives.Sync.Transactie where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
import Control.Monad.AvarMonadAsk (modify)
import Data.DateTime.Instant (toDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.TypesForDeltas (RoleDelta, BindingDelta, PropertyDelta)
import Prelude (class Show, bind, ($), (<>), show, pure, (<<<))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transactie = Transactie
  { author :: String
  , timeStamp :: SerializableDateTime
  , roleDeltas :: Array RoleDelta
  , bindingDeltas :: Array BindingDelta
  , propertyDeltas :: Array PropertyDelta
  , createdContexts :: Array PerspectContext
  , createdRoles :: Array PerspectRol
  , deletedContexts :: Array ContextInstance
  , deletedRoles :: Array RoleInstance
  , changedDomeinFiles :: Array String
  }

derive instance genericRepTransactie :: Generic Transactie _

derive instance newtypeTransactie :: Newtype Transactie _

instance showTransactie :: Show Transactie where
  show = genericShow

derive newtype instance writeForeignTransactie :: WriteForeign Transactie

createTransactie :: String -> Aff Transactie
createTransactie author =
  do
    n <- liftEffect $ now
    pure $ Transactie
      { author: author
      , timeStamp: SerializableDateTime (toDateTime n)
      , roleDeltas: []
      , bindingDeltas: []
      , propertyDeltas: []
      , createdContexts: []
      , createdRoles: []
      , deletedContexts: []
      , deletedRoles: []
      , changedDomeinFiles: []}

transactieID :: Transactie -> String
transactieID (Transactie{author, timeStamp}) = author <> "_" <> show timeStamp

_createdContexts :: Lens' Transactie (Array PerspectContext)
_createdContexts = _Newtype <<< (prop (SProxy :: SProxy "createdContexts"))

_createdRoles :: Lens' Transactie (Array PerspectRol)
_createdRoles = _Newtype <<< (prop (SProxy :: SProxy "createdRoles"))

_deletedContexts :: Lens' Transactie (Array ContextInstance)
_deletedContexts = _Newtype <<< (prop (SProxy :: SProxy "deletedContexts"))

_deletedRoles :: Lens' Transactie (Array RoleInstance)
_deletedRoles = _Newtype <<< (prop (SProxy :: SProxy "deletedRoles"))
