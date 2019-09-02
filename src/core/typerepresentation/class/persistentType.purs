module Perspectives.Representation.Class.PersistentType
  ( module Perspectives.Representation.Class.PersistentType
  , module Perspectives.Representation.Class.Persistent
  , module Perspectives.Representation.Class.Revision
  , module Perspectives.Representation.TypeIdentifiers) where

import Perspectives.Representation.Class.Revision

import Control.Monad.Except (throwError)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Exception (error)
import Foreign.Object (insert, lookup) as FO
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (deconstructModelName, deconstructNamespace)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.Persistent (class Persistent, retrieveInternally)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedPropertyType, CalculatedRoleType, ContextType, EnumeratedPropertyType, EnumeratedRoleType, ViewType)
import Perspectives.Representation.View (View)
import Prelude (Unit, bind, ($), pure, (<>), unit)

type Namespace = String

class (Identifiable v i, Revision v, Newtype i String) <= PersistentType v i where
  retrieveFromDomein :: i -> Namespace -> MonadPerspectives v
  cacheInDomeinFile :: i -> v -> MonadPerspectives Unit

-- | Get any type representation for Perspectives, either from cache or from a model file in
-- | couchdb.
getPerspectType :: forall v i. PersistentType v i => i -> MonadPerspectives v
getPerspectType id = do
  mns <- pure (deconstructNamespace (unwrap id))
  case mns of
    Nothing -> throwError (error $ "getPerspectType cannot retrieve type with incorrectly formed id: '" <> unwrap id <> "'.")
    (Just ns) -> retrieveFromDomein id ns

-----------------------------------------------------------
-- ADD TO A DOMEINFILE
-----------------------------------------------------------
addContextToDomeinFile :: Context -> DomeinFile -> DomeinFile
addContextToDomeinFile c (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = FO.insert (unwrap $ (identifier c :: ContextType)) c contexts}

addEnumeratedRoleToDomeinFile :: EnumeratedRole -> DomeinFile -> DomeinFile
addEnumeratedRoleToDomeinFile c (DomeinFile dff@{enumeratedRoles}) = DomeinFile dff {enumeratedRoles = FO.insert (unwrap $ (identifier c :: EnumeratedRoleType)) c enumeratedRoles}

-- addQueryFunctionToDomeinFile :: String -> QueryFunction -> DomeinFile -> DomeinFile
-- addQueryFunctionToDomeinFile id c (DomeinFile dff@{queries}) = DomeinFile dff {queries = FO.insert (unwrap $ (identifier c :: QueryFunctionType)) c queries}

ifNamespace :: forall i. Newtype i String => i -> (DomeinFile -> DomeinFile) -> MP Unit
ifNamespace i modifier = maybe (pure unit) (modifyDomeinFileInCache modifier) (deconstructModelName (unwrap i))

retrieveFromDomein_ :: forall v i. PersistentType v i =>
  i
  -> (DomeinFile -> Maybe v)
  -> Namespace
  -> (MonadPerspectives v)
retrieveFromDomein_ id lookupFunction ns = do
  df <- retrieveDomeinFile ns
  case lookupFunction df of
    Nothing -> throwError $ error ("retrieveFromDomein': cannot find definition of " <> (unwrap id) <> " for " <> ns)
    (Just v) -> pure v

-----------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------
instance persistentContext :: PersistentType Context ContextType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{contexts}) -> DomeinFile dff {contexts = FO.insert (unwrap i) v contexts})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{contexts}) -> FO.lookup (unwrap i) contexts)

instance persistentEnumeratedRole :: PersistentType EnumeratedRole EnumeratedRoleType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{enumeratedRoles}) -> DomeinFile dff {enumeratedRoles = FO.insert (unwrap i) v enumeratedRoles})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{enumeratedRoles}) -> FO.lookup (unwrap i) enumeratedRoles)

instance persistentCalculatedRole :: PersistentType CalculatedRole CalculatedRoleType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{calculatedRoles}) -> DomeinFile dff {calculatedRoles = FO.insert (unwrap i) v calculatedRoles})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{calculatedRoles}) -> FO.lookup (unwrap i) calculatedRoles)

instance persistentEnumeratedProperty :: PersistentType EnumeratedProperty EnumeratedPropertyType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{enumeratedProperties}) -> DomeinFile dff {enumeratedProperties = FO.insert (unwrap i) v enumeratedProperties})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{enumeratedProperties}) -> FO.lookup (unwrap i) enumeratedProperties)

instance persistentCalculatedProperty :: PersistentType CalculatedProperty CalculatedPropertyType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{calculatedProperties}) -> DomeinFile dff {calculatedProperties = FO.insert (unwrap i) v calculatedProperties})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{calculatedProperties}) -> FO.lookup (unwrap i) calculatedProperties)

instance persistentView :: PersistentType View ViewType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{views}) -> DomeinFile dff {views = FO.insert (unwrap i) v views})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{views}) -> FO.lookup (unwrap i) views)

instance persistentAction :: PersistentType Action ActionType where
  cacheInDomeinFile i v = ifNamespace i
    (\(DomeinFile dff@{actions}) -> DomeinFile dff {actions = FO.insert (unwrap i) v actions})
  retrieveFromDomein i = retrieveFromDomein_ i
    (\(DomeinFile{actions}) -> FO.lookup (unwrap i) actions)
