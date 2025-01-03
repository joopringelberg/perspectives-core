-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.StrippedDelta where

import Control.Monad.AvarMonadAsk (gets)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, unwrap)
import Data.Traversable (for_, traverse)
import Decacheable (decache)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, StorageScheme(..))
import Perspectives.Identifiers (isUrl)
import Perspectives.InstanceRepresentation.PublicUrl (PublicUrl(..))
import Perspectives.Instances.ObjectGetters (context', publicUrl)
import Perspectives.Persistent (entityExists, getPerspectContext, saveEntiteit)
import Perspectives.Query.UnsafeCompiler (getPublicUrl)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ResourceType(..))
import Perspectives.ResourceIdentifiers (addPublicScheme, createDefaultIdentifier, createLocalIdentifier, createPublicIdentifier, createRemoteIdentifier, isInPublicScheme, stripNonPublicIdentifiers, takeGuid)
import Perspectives.ResourceIdentifiers.Parser (ResourceIdentifier)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), toArray)
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseRoleDelta(..))
import Prelude (Unit, bind, discard, map, pure, void, ($), (<$>), (<<<), (>>=))

-----------------------------------------------------------
-- STRIPPING RESOURCE IDENTIFIERS IN A DELTA
-----------------------------------------------------------
class StrippedDelta d where
  stripResourceSchemes :: d -> d
  addResourceSchemes :: Map ResourceType StorageScheme -> d -> MonadPerspectives d
  addPublicResourceScheme :: String -> d -> MonadPerspectives d 

instance StrippedDelta UniverseContextDelta where
  stripResourceSchemes (UniverseContextDelta r) = UniverseContextDelta r 
    { id = over ContextInstance stripNonPublicIdentifiers r.id
    }
  addResourceSchemes storageSchemes (UniverseContextDelta r) = do
    id <- ContextInstance <$> (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) (unwrap r.id)
    pure $ UniverseContextDelta r 
      { id = id
      }
  addPublicResourceScheme url (UniverseContextDelta r) = do
    curl <- addOwnStorageScheme (CType r.contextType) (unwrap r.id) >>= getUrlForPublishing url <<< ContextInstance
    rec <- pure r
      { id = over ContextInstance (createPublicIdentifier curl) r.id
      }
    void $ decache rec.id
    pure $ UniverseContextDelta rec

instance StrippedDelta UniverseRoleDelta where 
  stripResourceSchemes (UniverseRoleDelta r) = UniverseRoleDelta r 
    { id = over ContextInstance stripNonPublicIdentifiers r.id
    , roleInstances = over SerializableNonEmptyArray (map (over RoleInstance stripNonPublicIdentifiers)) r.roleInstances
    }
  addResourceSchemes storageSchemes (UniverseRoleDelta r) = do
    id <- ContextInstance <$> (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) (unwrap r.id) 
    roleInstances <- (traverse (addSchemeToResourceIdentifier storageSchemes (RType r.roleType) <<< unwrap )) r.roleInstances
    pure $ UniverseRoleDelta r 
      { id = id
      , roleInstances = RoleInstance <$> roleInstances
      }

    --  UniverseRoleDelta r 
    -- { id = over ContextInstance (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) r.id
    -- , roleInstances = over SerializableNonEmptyArray (map (over RoleInstance (addSchemeToResourceIdentifier storageSchemes (RType r.roleType)))) r.roleInstances
    -- }
  addPublicResourceScheme url (UniverseRoleDelta r) = do
    curl <- addOwnStorageScheme (CType r.contextType) (unwrap r.id) >>= getUrlForPublishing url <<< ContextInstance
    rec <- pure r
      { id = over ContextInstance (createPublicIdentifier curl) r.id
      , roleInstances = over SerializableNonEmptyArray (map (over RoleInstance (createPublicIdentifier curl))) r.roleInstances
      }
    void $ decache rec.id
    for_ (toArray rec.roleInstances) decache
    pure $ UniverseRoleDelta rec

instance StrippedDelta ContextDelta where
  stripResourceSchemes (ContextDelta r) = ContextDelta r 
    { contextInstance = over ContextInstance stripNonPublicIdentifiers r.contextInstance
    , roleInstance = over RoleInstance stripNonPublicIdentifiers r.roleInstance
    , destinationContext = maybe Nothing (Just <<< (over ContextInstance stripNonPublicIdentifiers)) r.destinationContext
    }
  addResourceSchemes storageSchemes (ContextDelta r) = do
    contextInstance <- ContextInstance <$> (addSchemeToResourceIdentifier storageSchemes (CType r.contextType)) (unwrap r.contextInstance)
    roleInstance <- RoleInstance <$> (addSchemeToResourceIdentifier storageSchemes (RType r.roleType)) (unwrap r.roleInstance)
    destinationContext <- case r.destinationContext, r.destinationContextType of
      Just dc, Just dct -> Just <$> (ContextInstance <$> (addSchemeToResourceIdentifier storageSchemes (CType dct)) (unwrap dc))
      _, _ -> pure Nothing
    pure $ ContextDelta r 
      { contextInstance = contextInstance
      , roleInstance = roleInstance
      , destinationContext = destinationContext
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

instance StrippedDelta RoleBindingDelta where
  stripResourceSchemes (RoleBindingDelta r) = RoleBindingDelta r 
    { filled = over RoleInstance stripNonPublicIdentifiers r.filled
    , filler = maybe Nothing (Just <<< (over RoleInstance stripNonPublicIdentifiers)) r.filler
    , oldFiller = maybe Nothing (Just <<< (over RoleInstance stripNonPublicIdentifiers)) r.oldFiller
    }
  addResourceSchemes storageSchemes (RoleBindingDelta r) = do 
    filled <- RoleInstance <$> addSchemeToResourceIdentifier storageSchemes (RType r.filledType) (unwrap r.filled)
    filler <- case r.filler, r.fillerType of
      Just f, Just ft -> Just <<< RoleInstance <$> addSchemeToResourceIdentifier storageSchemes (RType ft) (unwrap f)
      _, _ -> pure Nothing
    oldFiller <- unsafePartial case r.oldFiller, r.oldFillerType of
      Just f, Just ft -> Just <<< RoleInstance <$> addSchemeToResourceIdentifier storageSchemes (RType ft) (unwrap f)
      _, _ -> pure Nothing
    pure $ RoleBindingDelta r 
      { filled = filled
      , filler = filler
      , oldFiller = oldFiller
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

instance StrippedDelta RolePropertyDelta where
  stripResourceSchemes (RolePropertyDelta r) = RolePropertyDelta r 
    { id = over RoleInstance stripNonPublicIdentifiers r.id
    }
  addResourceSchemes storageSchemes (RolePropertyDelta r) = do 
    id <- RoleInstance <$> addSchemeToResourceIdentifier storageSchemes (RType r.roleType) (unwrap r.id)
    pure $ RolePropertyDelta r 
      { id = id
      }
  addPublicResourceScheme url (RolePropertyDelta r) = do
    ctxt <- addOwnStorageScheme (RType r.roleType) (unwrap r.id) >>= context' <<< RoleInstance
    url' <- getUrlForPublishing url ctxt
    rec <- pure r
        { id = over RoleInstance (createPublicIdentifier url') r.id }
    void $ decache rec.id
    pure $ RolePropertyDelta rec

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
  addSchemeToResourceIdentifier storageSchemes rtype s

-----------------------------------------------------------
-- ADD SCHEME TO IDENTIFIER
-----------------------------------------------------------
-- | Add a storage scheme to an identifier based on the users own preferences.
-- | If no preference is available, use the Public scheme if the given identifier has the form of 
-- | an URL; make it a Default scheme otherwise
-- | This function will never create a resource identifier with the model: scheme.
addSchemeToResourceIdentifier :: Map ResourceType StorageScheme -> ResourceType -> String -> MonadPerspectives ResourceIdentifier
addSchemeToResourceIdentifier map t s = if isInPublicScheme s
  then addSchemeToResourceIdentifier map t (takeGuid s) >>= exists >>= if _
    then addSchemeToResourceIdentifier map t (takeGuid s) 
    else pure s
  else if isUrl s
    then pure $ addPublicScheme s
    else case lookup t map of
      Nothing -> pure $ createDefaultIdentifier s
      Just (Default _) -> pure $ createDefaultIdentifier s
      Just (Local dbName) -> pure $ createLocalIdentifier dbName s
      Just (Remote url) -> pure $ createRemoteIdentifier url s
  where
  exists :: ResourceIdentifier -> MonadPerspectives Boolean
  exists r = case t of
    CType _ -> entityExists (ContextInstance r)
    RType _ -> entityExists (RoleInstance r)