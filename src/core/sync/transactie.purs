module Perspectives.Sync.Transaction where

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
import Data.Array (union)
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
import Prelude (class Semigroup, class Show, bind, ($), (<>), show, pure, (<<<))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transaction = Transaction
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

derive instance genericRepTransactie :: Generic Transaction _

derive instance newtypeTransactie :: Newtype Transaction _

instance showTransactie :: Show Transaction where
  show = genericShow

derive newtype instance writeForeignTransactie :: WriteForeign Transaction

instance semiGroupTransactie :: Semigroup Transaction where
  append t1@(Transaction {author, timeStamp, roleDeltas, bindingDeltas, propertyDeltas, createdContexts, createdRoles, deletedContexts, deletedRoles, changedDomeinFiles})
    t2@(Transaction {author: a, timeStamp: t, roleDeltas: r, bindingDeltas: b, propertyDeltas: p, createdContexts: cc, createdRoles: cr, deletedContexts: dc, deletedRoles: dr, changedDomeinFiles: cd}) = Transaction
      { author: author
      , timeStamp: timeStamp
      , roleDeltas: union roleDeltas r
      , bindingDeltas: union bindingDeltas b
      , propertyDeltas: union propertyDeltas p
      , createdContexts: union createdContexts cc
      , createdRoles: union createdRoles cr
      , deletedContexts: union deletedContexts dc
      , deletedRoles: union deletedRoles dr
      , changedDomeinFiles: union changedDomeinFiles cd
    }

createTransactie :: String -> Aff Transaction
createTransactie author =
  do
    n <- liftEffect $ now
    pure $ Transaction
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

cloneEmptyTransaction :: Transaction -> Transaction
cloneEmptyTransaction (Transaction{ author, timeStamp}) = Transaction
  { author: author
  , timeStamp: timeStamp
  , roleDeltas: []
  , bindingDeltas: []
  , propertyDeltas: []
  , createdContexts: []
  , createdRoles: []
  , deletedContexts: []
  , deletedRoles: []
  , changedDomeinFiles: []}

transactieID :: Transaction -> String
transactieID (Transaction{author, timeStamp}) = author <> "_" <> show timeStamp

_createdContexts :: Lens' Transaction (Array PerspectContext)
_createdContexts = _Newtype <<< (prop (SProxy :: SProxy "createdContexts"))

_createdRoles :: Lens' Transaction (Array PerspectRol)
_createdRoles = _Newtype <<< (prop (SProxy :: SProxy "createdRoles"))

_deletedContexts :: Lens' Transaction (Array ContextInstance)
_deletedContexts = _Newtype <<< (prop (SProxy :: SProxy "deletedContexts"))

_deletedRoles :: Lens' Transaction (Array RoleInstance)
_deletedRoles = _Newtype <<< (prop (SProxy :: SProxy "deletedRoles"))
