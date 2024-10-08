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

module Perspectives.TypesForDeltas where

import Control.Monad.AvarMonadAsk (gets)
import Data.Eq (class Eq)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, traverse)
import Decacheable (decache)
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, StorageScheme)
import Perspectives.InstanceRepresentation.PublicUrl (PublicUrl(..))
import Perspectives.Instances.ObjectGetters (context', publicUrl)
import Perspectives.Persistent (getPerspectContext, saveEntiteit)
import Perspectives.Query.UnsafeCompiler (getPublicUrl)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, ResourceType(..), RoleType)
import Perspectives.ResourceIdentifiers (addSchemeToResourceIdentifier, createPublicIdentifier, stripNonPublicIdentifiers)
import Perspectives.ResourceIdentifiers.Parser (ResourceIdentifier)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), toArray)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Ord, class Show, Unit, bind, discard, map, pure, show, void, ($), (&&), (<$>), (<<<), (<>), (==), (>>=))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- GENERIC
-----------------------------------------------------------
-- | The subject is the user role with the perspective that should include the right verbs to allow the delta
-- | and with an object that corresponds to the resource being modified by the delta.
-- | This is often taken from the transacton in which modifications are made. It is the member 'authoringRole' of Transaction, provided when executing a transaction with runMonadPerspectivesTransaction
-- | (not to be confused with member 'author', who must be an instance of sys:PerspectivesSystem$User). THIS REMARK MAY BE OBSOLETE!
type DeltaRecord f = {subject :: RoleType | f}

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTA
-----------------------------------------------------------
newtype UniverseContextDelta = UniverseContextDelta (DeltaRecord
  ( id :: ContextInstance
  , contextType :: ContextType
  , deltaType :: UniverseContextDeltaType
  ))

derive instance genericUniverseContextDelta :: Generic UniverseContextDelta _
derive instance newtypeUniverseContextDelta :: Newtype UniverseContextDelta _

instance showUniverseContextDelta :: Show UniverseContextDelta where
  show = genericShow

instance eqUniverseContextDelta :: Eq UniverseContextDelta where
  eq (UniverseContextDelta {id: i1, contextType: c1, deltaType: d1}) (UniverseContextDelta {id: i2, contextType: c2, deltaType: d2}) = i1 == i2 && c1 == c2 && d1 == d2

derive newtype instance WriteForeign UniverseContextDelta
derive newtype instance ReadForeign UniverseContextDelta

derive instance Ord UniverseContextDelta

instance StrippedDelta UniverseContextDelta where
  stripResourceSchemes (UniverseContextDelta r) = UniverseContextDelta r 
    { id = over ContextInstance stripNonPublicIdentifiers r.id
    }
  addResourceSchemes storageSchemes (UniverseContextDelta r) =  UniverseContextDelta r 
    { id = over ContextInstance (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) r.id
    }
  addPublicResourceScheme url (UniverseContextDelta r) = do
    curl <- addOwnStorageScheme (CType r.contextType) (unwrap r.id) >>= getUrlForPublishing url <<< ContextInstance
    rec <- pure r
      { id = over ContextInstance (createPublicIdentifier curl) r.id
      }
    void $ decache rec.id
    pure $ UniverseContextDelta rec

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTATYPE
-----------------------------------------------------------
data UniverseContextDeltaType = ConstructEmptyContext
derive instance genericUniverseContextDeltaType :: Generic UniverseContextDeltaType _
instance showUniverseContextDeltaType :: Show UniverseContextDeltaType where
  show = genericShow

instance eqUniverseContextDeltaType :: Eq UniverseContextDeltaType where
  eq = genericEq

instance WriteForeign UniverseContextDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign UniverseContextDeltaType where readImpl = enumReadForeign

instance prettyPrintUniverseContextDelta :: PrettyPrint UniverseContextDelta where
  prettyPrint' t (UniverseContextDelta r) = "UniverseContextDelta " <> prettyPrint' (t <> "  ") r

instance prettyPrintUniverseContextDeltaType :: PrettyPrint UniverseContextDeltaType where
  prettyPrint' t = show

derive instance Ord UniverseContextDeltaType

-----------------------------------------------------------
-- UNIVERSEROLEDELTA
-----------------------------------------------------------
newtype UniverseRoleDelta = UniverseRoleDelta (DeltaRecord
  ( id :: ContextInstance
  , contextType :: ContextType
  , roleType :: EnumeratedRoleType
  -- To be provided when deltaType is ConstructExternalRole or RemoveUnboundExternalRoleInstance or RemoveExternalRoleInstance.
  -- It is the context role type that binds the external role; this is the role type that the user is authorized to construct and fill.
  -- It can also be a calculated rol that results in external roles (usually taken from the database).
  , authorizedRole :: Maybe RoleType
  , roleInstances :: SerializableNonEmptyArray RoleInstance
  , deltaType :: UniverseRoleDeltaType
  -- Add, Remove
  ))

derive instance genericUniverseRoleDelta :: Generic UniverseRoleDelta _

derive instance newTypeUniverseRoleDelta :: Newtype UniverseRoleDelta _

instance showUniverseRoleDelta :: Show UniverseRoleDelta where
  show = genericShow

instance eqUniverseRoleDelta :: Eq UniverseRoleDelta where
  eq (UniverseRoleDelta {id:i1, roleType:r1, roleInstances:ri1, deltaType:d1}) (UniverseRoleDelta {id:i2, roleType:r2, roleInstances:ri2, deltaType:d2}) = i1 == i2 && r1 == r2 && ri1 == ri2 && d1 == d2

derive newtype instance WriteForeign UniverseRoleDelta
derive newtype instance ReadForeign UniverseRoleDelta

instance prettyPrintUniverseRoleDelta :: PrettyPrint UniverseRoleDelta where
  prettyPrint' t (UniverseRoleDelta r) = "UniverseRoleDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord UniverseRoleDelta

instance StrippedDelta UniverseRoleDelta where
  stripResourceSchemes (UniverseRoleDelta r) = UniverseRoleDelta r 
    { id = over ContextInstance stripNonPublicIdentifiers r.id
    , roleInstances = over SerializableNonEmptyArray (map (over RoleInstance stripNonPublicIdentifiers)) r.roleInstances
    }
  addResourceSchemes storageSchemes (UniverseRoleDelta r) =  UniverseRoleDelta r 
    { id = over ContextInstance (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) r.id
    , roleInstances = over SerializableNonEmptyArray (map (over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType r.roleType)))) r.roleInstances
    }
  addPublicResourceScheme url (UniverseRoleDelta r) = do
    curl <- addOwnStorageScheme (CType r.contextType) (unwrap r.id) >>= getUrlForPublishing url <<< ContextInstance
    rec <- pure r
      { id = over ContextInstance (createPublicIdentifier curl) r.id
      , roleInstances = over SerializableNonEmptyArray (map (over RoleInstance (createPublicIdentifier curl))) r.roleInstances
      }
    void $ decache rec.id
    for_ (toArray rec.roleInstances) decache
    pure $ UniverseRoleDelta rec

-----------------------------------------------------------
-- UNIVERSEROLEDELTATYPE
-----------------------------------------------------------
data UniverseRoleDeltaType = ConstructEmptyRole | ConstructExternalRole | RemoveRoleInstance | RemoveUnboundExternalRoleInstance | RemoveExternalRoleInstance
derive instance genericUniverseRoleDeltaType :: Generic UniverseRoleDeltaType _
instance showUniverseRoleDeltaType :: Show UniverseRoleDeltaType where
  show = genericShow

instance eqUniverseRoleDeltaType :: Eq UniverseRoleDeltaType where
  eq = genericEq

instance WriteForeign UniverseRoleDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign UniverseRoleDeltaType where readImpl = enumReadForeign

instance prettyPrintUniverseRoleDeltaType :: PrettyPrint UniverseRoleDeltaType where
  prettyPrint' t = show

derive instance Ord UniverseRoleDeltaType
-----------------------------------------------------------
-- CONTEXTDELTA
-----------------------------------------------------------
newtype ContextDelta = ContextDelta (DeltaRecord
  ( contextInstance :: ContextInstance
  , contextType :: ContextType
  , roleType :: EnumeratedRoleType
  , roleInstance :: RoleInstance
  , destinationContext :: Maybe ContextInstance
  , destinationContextType :: Maybe ContextType
  , deltaType :: ContextDeltaType
  ))

derive instance genericContextDelta :: Generic ContextDelta _
derive instance newtypeContextDelta :: Newtype ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

instance eqContextDelta :: Eq ContextDelta where
  eq (ContextDelta {contextInstance:i1, roleType:r1, roleInstance:ri1, {-destinationContext: dc1,-} deltaType:d1}) (ContextDelta {contextInstance:i2, roleType:r2, roleInstance:ri2, {-destinationContext: dc2,-} deltaType:d2}) = i1 == i2 && r1 == r2 && ri1 == ri2 && {-dc1 == dc2 &&-} d1 == d2

derive newtype instance WriteForeign ContextDelta
derive newtype instance ReadForeign ContextDelta

instance prettyPrintContextDelta :: PrettyPrint ContextDelta where
  prettyPrint' t (ContextDelta r) = "ContextDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord ContextDelta

instance StrippedDelta ContextDelta where
  stripResourceSchemes (ContextDelta r) = ContextDelta r 
    { contextInstance = over ContextInstance stripNonPublicIdentifiers r.contextInstance
    , roleInstance = over RoleInstance stripNonPublicIdentifiers r.roleInstance
    , destinationContext = maybe Nothing (Just <<< (over ContextInstance stripNonPublicIdentifiers)) r.destinationContext
    }
  addResourceSchemes storageSchemes (ContextDelta r) =  ContextDelta r 
    { contextInstance = over ContextInstance (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) r.contextInstance
    , roleInstance = over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType r.roleType)) r.roleInstance
    , destinationContext = unsafePartial case r.destinationContext, r.destinationContextType of
        Just dc, Just dct -> Just $ over ContextInstance (addSchemeToResourceIdentifier storageSchemes (CType dct)) dc
        _, _ -> Nothing
    }
  addPublicResourceScheme url (ContextDelta r) = do 
    curl <- addOwnStorageScheme (CType r.contextType) (unwrap r.contextInstance) >>= getUrlForPublishing url <<< ContextInstance
    durl <- case r.destinationContext, r.destinationContextType of 
      Just dc, Just dctype -> Just <$> (addOwnStorageScheme (CType dctype) (unwrap dc) >>= getUrlForPublishing url <<< ContextInstance)
      _, _ -> pure Nothing
    rec <- pure $ r
      { contextInstance = over ContextInstance (createPublicIdentifier curl) r.contextInstance
      , roleInstance = over RoleInstance (createPublicIdentifier curl) r.roleInstance
      , destinationContext = case r.destinationContext, durl of
        Just (ContextInstance dc), Just url' -> Just (ContextInstance (createPublicIdentifier url' dc))
        _, _ -> Nothing
      }
    void $ decache rec.contextInstance
    void $ decache rec.roleInstance
    void $ traverse decache rec.destinationContext
    pure $ ContextDelta rec

-----------------------------------------------------------
-- CONTEXTDELTATYPE
-----------------------------------------------------------
data ContextDeltaType =
  AddRoleInstancesToContext |
  AddExternalRole |
  MoveRoleInstancesToAnotherContext

derive instance genericContextDeltaType :: Generic ContextDeltaType _
instance showContextDeltaType :: Show ContextDeltaType where
  show = genericShow

instance eqContextDeltaType :: Eq ContextDeltaType where
  eq = genericEq

instance WriteForeign ContextDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign ContextDeltaType where readImpl = enumReadForeign

instance prettyPrintContextDeltaType :: PrettyPrint ContextDeltaType where
  prettyPrint' t = show

derive instance Ord ContextDeltaType

-----------------------------------------------------------
-- ROLEBINDINGDELTA
-----------------------------------------------------------
newtype RoleBindingDelta = RoleBindingDelta (DeltaRecord
  ( filled :: RoleInstance
  , filledType :: EnumeratedRoleType
  , filler :: Maybe RoleInstance
  , fillerType :: Maybe EnumeratedRoleType
  , oldFiller :: Maybe RoleInstance
  , oldFillerType :: Maybe EnumeratedRoleType
  , deltaType :: RoleBindingDeltaType
  -- Remove, Change
  ))

derive instance genericRoleDelta :: Generic RoleBindingDelta _
derive instance newtypeRoleBindingDelta :: Newtype RoleBindingDelta _

instance showRoleDelta :: Show RoleBindingDelta where
  show = genericShow

instance eqRoleDelta :: Eq RoleBindingDelta where
  eq (RoleBindingDelta {filled:i1, filler:b1, oldFiller:ob1, deltaType:d1}) (RoleBindingDelta {filled:i2, filler:b2, oldFiller:ob2, deltaType:d2}) = i1 == i2 && b1 == b2 && ob1 == ob2 && d1 == d2

derive newtype instance WriteForeign RoleBindingDelta
derive newtype instance ReadForeign RoleBindingDelta

instance prettyPrintRoleBindingDelta :: PrettyPrint RoleBindingDelta where
  prettyPrint' t (RoleBindingDelta r) = "RoleBindingDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord RoleBindingDelta

instance StrippedDelta RoleBindingDelta where
  stripResourceSchemes (RoleBindingDelta r) = RoleBindingDelta r 
    { filled = over RoleInstance stripNonPublicIdentifiers r.filled
    , filler = maybe Nothing (Just <<< (over RoleInstance stripNonPublicIdentifiers)) r.filler
    , oldFiller = maybe Nothing (Just <<< (over RoleInstance stripNonPublicIdentifiers)) r.oldFiller
    }
  addResourceSchemes storageSchemes (RoleBindingDelta r) =  RoleBindingDelta r 
    { filled = over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType r.filledType)) r.filled
    , filler = case r.filler, r.fillerType of
        Just f, Just ft -> Just $ over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType ft)) f
        _, _ -> Nothing
    , oldFiller = unsafePartial case r.oldFiller, r.oldFillerType of
        Just f, Just ft -> Just $ over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType ft)) f
        _, _ -> Nothing
    }
  addPublicResourceScheme url (RoleBindingDelta r) = do
    fillerUrl <- case r.filler, r.fillerType of 
      Just f, Just ftype -> addOwnStorageScheme (RType ftype) (unwrap f) >>= context' <<< RoleInstance >>= getUrlForPublishing url
      _, _ -> pure url 
    filledUrl <- addOwnStorageScheme (RType r.filledType) (unwrap r.filled) >>= context' <<< RoleInstance >>= getUrlForPublishing url
    rec <- pure $ r
      { filled = over RoleInstance (createPublicIdentifier filledUrl) r.filled 
      , filler = maybe Nothing (Just <<< (over RoleInstance (createPublicIdentifier fillerUrl))) r.filler
      , oldFiller = maybe Nothing (Just <<< (over RoleInstance (createPublicIdentifier fillerUrl))) r.oldFiller
      }
    void $ traverse decache rec.filler
    void $ decache rec.filled
    void $ traverse decache rec.oldFiller
    pure $ RoleBindingDelta rec
  -----------------------------------------------------------
-- ROLEBINDINGDELTATYPE
-----------------------------------------------------------
data RoleBindingDeltaType = SetFirstBinding | RemoveBinding | ReplaceBinding

derive instance genericRoleBindingDeltaType :: Generic RoleBindingDeltaType _

instance showRoleBindingDeltaType :: Show RoleBindingDeltaType where
  show = genericShow

instance eqRoleBindingDeltaType :: Eq RoleBindingDeltaType where
  eq = genericEq

instance prettyPrintRoleBindingDeltaType :: PrettyPrint RoleBindingDeltaType where
  prettyPrint' t = show

instance WriteForeign RoleBindingDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign RoleBindingDeltaType where readImpl = enumReadForeign

derive instance Ord RoleBindingDeltaType

-----------------------------------------------------------
-- ROLEPROPERTYDELTA
-----------------------------------------------------------
newtype RolePropertyDelta = RolePropertyDelta (DeltaRecord
  ( id :: RoleInstance
  , roleType :: EnumeratedRoleType
  , property :: EnumeratedPropertyType
  , values :: Array Value
  , deltaType :: RolePropertyDeltaType
  ))

derive instance genericPropertyDelta :: Generic RolePropertyDelta _
derive instance newtypeRolePropertyDelta :: Newtype RolePropertyDelta _

instance showPropertyDelta :: Show RolePropertyDelta where
  show = genericShow

instance eqPropertyDelta :: Eq RolePropertyDelta where
  eq (RolePropertyDelta {id:i1, property:p1, values:v1, deltaType:d1}) (RolePropertyDelta {id:i2, property:p2, values:v2, deltaType:d2}) = i1 == i2 && p1 == p2 && v1 == v2 && d1 == d2

derive newtype instance WriteForeign RolePropertyDelta
derive newtype instance ReadForeign RolePropertyDelta

instance prettyPrintRolePropertyDelta :: PrettyPrint RolePropertyDelta where
  prettyPrint' t (RolePropertyDelta r) = "RolePropertyDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord RolePropertyDelta

instance StrippedDelta RolePropertyDelta where
  stripResourceSchemes (RolePropertyDelta r) = RolePropertyDelta r 
    { id = over RoleInstance stripNonPublicIdentifiers r.id
    }
  addResourceSchemes storageSchemes (RolePropertyDelta r) =  RolePropertyDelta r 
    { id = over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType r.roleType)) r.id
    }
  addPublicResourceScheme url (RolePropertyDelta r) = do
    ctxt <- addOwnStorageScheme (RType r.roleType) (unwrap r.id) >>= context' <<< RoleInstance
    url' <- getUrlForPublishing url ctxt
    rec <- pure r
        { id = over RoleInstance (createPublicIdentifier url') r.id }
    void $ decache rec.id
    pure $ RolePropertyDelta rec

-----------------------------------------------------------
-- ROLEPROPERTYDELTATYPE
-----------------------------------------------------------
data RolePropertyDeltaType = AddProperty | RemoveProperty | DeleteProperty | SetProperty | UploadFile

derive instance genericRolePropertyDeltaType :: Generic RolePropertyDeltaType _
derive instance eqRolePropertyDeltaType :: Eq RolePropertyDeltaType

instance showRolePropertyDeltaType :: Show RolePropertyDeltaType where
  show = genericShow

instance WriteForeign RolePropertyDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign RolePropertyDeltaType where readImpl = enumReadForeign

instance prettyPrintRolePropertyDeltaType :: PrettyPrint RolePropertyDeltaType where
  prettyPrint' t = show

derive instance Ord RolePropertyDeltaType

-----------------------------------------------------------
-- STRIPPING RESOURCE IDENTIFIERS IN A DELTA
-----------------------------------------------------------
class StrippedDelta d where
  stripResourceSchemes :: d -> d
  addResourceSchemes :: Map ResourceType StorageScheme -> d -> d
  addPublicResourceScheme :: String -> d -> MonadPerspectives d 

-- | Given a default value for the url, return the actual url that should be used to publish the context identifier.
-- | Stores a value for `publicUrl` in the context instance if it didn't have one (most likely the value NONE).
getUrlForPublishing :: String -> ContextInstance -> MonadPerspectives String
getUrlForPublishing defaultUrl cid =  publicUrl cid >>= case _ of 
  Just NONE -> pure defaultUrl
  Just (URL url) -> pure url
  -- the context instance doesn't have a public url associated with it.
  Nothing -> do
    -- Now find the url of a public role in THE TYPE of the context instance.
    murl <- getPublicUrl cid
    case murl of
      Nothing -> do
        -- Save NONE in context instance.
        setPublicUrl (Just NONE)
        pure defaultUrl
      Just url -> do 
        -- save URL url in context instance
        setPublicUrl (Just (URL url))
        pure url
  where 
  -- | Stores the publicUrl with the context instance. Notice that we regard this to be a form of caching.
  -- | As there is no DeltaType describing PublicUrls, they are not synchronized.
  setPublicUrl :: (Maybe PublicUrl) -> MonadPerspectives Unit
  setPublicUrl murl = do 
    ctxt <- getPerspectContext cid
    void $ cacheEntity cid ctxt
    void $ saveEntiteit cid

addOwnStorageScheme :: ResourceType -> String -> MonadPerspectives ResourceIdentifier
addOwnStorageScheme rtype s = do 
  storageSchemes <- gets _.typeToStorage
  pure $ addSchemeToResourceIdentifier storageSchemes rtype s

