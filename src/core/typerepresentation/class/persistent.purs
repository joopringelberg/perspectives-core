module Perspectives.Representation.Class.Persistent
  ( module Perspectives.Representation.Class.Persistent
  , module Perspectives.Representation.Class.Revision
  , module Perspectives.Representation.TypeIdentifiers) where

-- | Members of Persistent trade identifiers for a representation.

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Except (throwError)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.AVar (AVar, isEmpty, empty, put, read, status, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Object (insert, lookup) as FO
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.Identifiers (deconstructModelName, deconstructNamespace)
import Perspectives.InstanceRepresentation (Revision) as B
import Perspectives.PerspectivesState (insert, lookup, remove)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.Revision (class Revision, changeRevision, rev)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..))
import Perspectives.Representation.View (View)

-- | Members of Persistent trade identifiers for a representation.

type Identifier = String
type Namespace = String

class (Identifiable v i, Revision v, Newtype i String) <= Persistent v i where
  cache :: i -> MonadPerspectives (GLStrMap (AVar v))
  -- | Create an empty AVar that will be filled by the PerspectEntiteit.
  representInternally :: i -> MonadPerspectives (AVar v)
  retrieveInternally :: i -> MonadPerspectives (Maybe (AVar v))
  removeInternally :: i -> MonadPerspectives (Maybe (AVar v))
  retrieveFromDomein :: i -> Namespace -> MonadPerspectives v
  cacheInDomeinFile :: i -> v -> MonadPerspectives Unit

-- | Get any type representation for Perspectives, either from cache or from a model file in
-- | couchdb.
getPerspectType :: forall v i. Persistent v i => i -> MonadPerspectives v
getPerspectType id =
  do
    (av :: Maybe (AVar v)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> do
        mns <- pure (deconstructNamespace (unwrap id))
        case mns of
          Nothing -> throwError (error $ "getPerspectType cannot retrieve type with incorrectly formed id: '" <> unwrap id <> "'.")
          (Just ns) -> retrieveFromDomein id ns

changeRevisionDefault :: forall f. String -> {_rev :: B.Revision | f} -> {_rev :: B.Revision | f}
changeRevisionDefault rev cr = cr {_rev = Just rev}

ensureInternalRepresentation :: forall i a. Persistent a i => i -> MonadPerspectives (AVar a)
ensureInternalRepresentation c = do
    mav <- retrieveInternally c
    case mav of
      Nothing -> representInternally c
      (Just av) -> pure av

-- | Caches the entiteit. If it was cached before, ensures that the newly cached
-- | entiteit has the same revision value as the old one.
cacheEntiteitPreservingVersion :: forall a i. Persistent a i => i -> a -> MonadPerspectives Unit
cacheEntiteitPreservingVersion id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    (Just avar) -> do
      ent <- liftAff $ take avar
      e' <- pure $ changeRevision (rev ent) e
      liftAff $ put e' avar

-- | If the entiteit is represented in an AVar, overwrites the stored value.
-- | Otherwise adds an AVar and stores the entiteit in it.
cacheEntiteit :: forall a i. Persistent a i => i -> a -> MonadPerspectives Unit
cacheEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> cacheUncachedEntiteit id e
    otherwise -> void $ cacheCachedEntiteit id e

-- | Store an internally created PerspectEntiteit for the first time in the local store.
cacheUncachedEntiteit :: forall a i. Persistent a i => i -> a -> MonadPerspectives Unit
cacheUncachedEntiteit id e = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> do
      (av :: AVar a) <- representInternally id
      liftAff $ put e av
      pure unit
    otherwise -> throwError $ error $ "cacheUncachedEntiteit: the cache should not hold an AVar for " <> unwrap id

-- | Modify a PerspectEntiteit in the cache.
cacheCachedEntiteit :: forall a i. Persistent a i => i -> a -> MonadPerspectives a
cacheCachedEntiteit id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error $ "cacheCachedEntiteit: cannot change an entiteit that is not cached: " <> unwrap id
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then liftAff $ put e avar *> pure e
        else do
          _ <- liftAff $ take avar
          liftAff $ put e avar *> pure e

-- | Returns an entity. Throws an error if the resource is not represented in cache or not
-- | immediately available in cache.
readEntiteitFromCache :: forall a i. Persistent a i => i -> MonadPerspectives a
readEntiteitFromCache id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("readEntiteitFromCache needs a locally stored resource for " <>  unwrap id)
    (Just avar) -> do
      empty <- liftAff $ (status >=> pure <<< isEmpty) avar
      if empty
        then throwError $ error ("readEntiteitFromCache found an empty AVar for " <> unwrap id)
        else liftAff $ read avar

-----------------------------------------------------------
-- ADD TO A DOMEINFILE
-----------------------------------------------------------
addContextToDomeinFile :: Context -> DomeinFile -> DomeinFile
addContextToDomeinFile c (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = FO.insert (unwrap $ (identifier c :: ContextType)) c contexts}

addEnumeratedRoleToDomeinFile :: EnumeratedRole -> DomeinFile -> DomeinFile
addEnumeratedRoleToDomeinFile c (DomeinFile dff@{enumeratedRoles}) = DomeinFile dff {enumeratedRoles = FO.insert (unwrap $ (identifier c :: EnumeratedRoleType)) c enumeratedRoles}

-- addQueryFunctionToDomeinFile :: String -> QueryFunction -> DomeinFile -> DomeinFile
-- addQueryFunctionToDomeinFile id c (DomeinFile dff@{queries}) = DomeinFile dff {queries = FO.insert (unwrap $ (identifier c :: QueryFunctionType)) c queries}

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------
ifNamespace :: forall i. Newtype i String => i -> (DomeinFile -> DomeinFile) -> MP Unit
ifNamespace i modifier = maybe (pure unit) (modifyDomeinFileInCache modifier) (deconstructModelName (unwrap i))

retrieveFromDomein_ :: forall v i. Persistent v i =>
  i
  -> (DomeinFile -> Maybe v)
  -> Namespace
  -> (MonadPerspectives v)
retrieveFromDomein_ id lookup ns = do
  df <- retrieveDomeinFile ns
  case lookup df of
    Nothing -> throwError $ error ("retrieveFromDomein': cannot find definition of " <> (unwrap id) <> " for " <> ns)
    (Just v) -> pure v

instance persistentContext :: Persistent Context ContextType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.contexts
  representInternally c = do
    av <- liftAff empty
    insert (cache (ContextType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (ContextType "")) (unwrap i)
  removeInternally i = remove (cache (ContextType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{contexts}) -> DomeinFile dff {contexts = FO.insert (unwrap i) v contexts})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{contexts}) -> FO.lookup (unwrap i) contexts)

instance persistentEnumeratedRole :: Persistent EnumeratedRole EnumeratedRoleType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.enumeratedRoles
  representInternally c = do
    av <- liftAff empty
    insert (cache (EnumeratedRoleType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (EnumeratedRoleType "")) (unwrap i)
  removeInternally i = remove (cache (EnumeratedRoleType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{enumeratedRoles}) -> DomeinFile dff {enumeratedRoles = FO.insert (unwrap i) v enumeratedRoles})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{enumeratedRoles}) -> FO.lookup (unwrap i) enumeratedRoles)

instance persistentCalculatedRole :: Persistent CalculatedRole CalculatedRoleType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.calculatedRoles
  representInternally c = do
    av <- liftAff empty
    insert (cache (CalculatedRoleType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (CalculatedRoleType "")) (unwrap i)
  removeInternally i = remove (cache (CalculatedRoleType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{calculatedRoles}) -> DomeinFile dff {calculatedRoles = FO.insert (unwrap i) v calculatedRoles})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{calculatedRoles}) -> FO.lookup (unwrap i) calculatedRoles)

instance persistentEnumeratedProperty :: Persistent EnumeratedProperty EnumeratedPropertyType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.enumeratedProperties
  representInternally c = do
    av <- liftAff empty
    insert (cache (EnumeratedPropertyType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (EnumeratedPropertyType "")) (unwrap i)
  removeInternally i = remove (cache (EnumeratedPropertyType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{enumeratedProperties}) -> DomeinFile dff {enumeratedProperties = FO.insert (unwrap i) v enumeratedProperties})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{enumeratedProperties}) -> FO.lookup (unwrap i) enumeratedProperties)

instance persistentCalculatedProperty :: Persistent CalculatedProperty CalculatedPropertyType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.calculatedProperties
  representInternally c = do
    av <- liftAff empty
    insert (cache (CalculatedPropertyType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (CalculatedPropertyType "")) (unwrap i)
  removeInternally i = remove (cache (CalculatedPropertyType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{calculatedProperties}) -> DomeinFile dff {calculatedProperties = FO.insert (unwrap i) v calculatedProperties})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{calculatedProperties}) -> FO.lookup (unwrap i) calculatedProperties)

instance persistentView :: Persistent View ViewType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.views
  representInternally c = do
    av <- liftAff empty
    insert (cache (ViewType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (ViewType "")) (unwrap i)
  removeInternally i = remove (cache (ViewType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{views}) -> DomeinFile dff {views = FO.insert (unwrap i) v views})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{views}) -> FO.lookup (unwrap i) views)

instance persistentAction :: Persistent Action ActionType where
  -- identifier = _._id <<< unwrap
  cache _ = gets _.actions
  representInternally c = do
    av <- liftAff empty
    insert (cache (ActionType "")) (unwrap c) av
  retrieveInternally i = lookup (cache (ActionType "")) (unwrap i)
  removeInternally i = remove (cache (ActionType "")) (unwrap i)
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{actions}) -> DomeinFile dff {actions = FO.insert (unwrap i) v actions})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{actions}) -> FO.lookup (unwrap i) actions)
