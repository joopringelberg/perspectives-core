module Perspectives.SetupUser where

import Control.Monad.Eff.Now (NOW)
import Data.Unit (Unit)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Deltas (addRol)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID)
import Perspectives.Identifiers (Namespace, LocalName)
import Perspectives.PerspectEntiteit (cacheUncachedEntiteit)
import Perspectives.ResourceRetrieval (saveUnversionedEntiteit)
import Perspectives.Syntax (ContextRecord, PerspectContext(..), PerspectRol(..), RolRecord, binding)
import Prelude (discard, ($), (<>), bind)

setupUser :: forall e. MonadPerspectives (AjaxAvarCache (now :: NOW | e)) Unit
setupUser = do
  cacheUncachedEntiteit "model:User$Systeem"
    (PerspectContext $ contextRecord "model:User" "Systeem" "model:Systeem$Systeem")
  cacheUncachedEntiteit "model:User$Systeem_BuitenRol"
    (PerspectRol $ buitenRolRecord  "model:User" "Systeem")
  cacheUncachedEntiteit "model:User$Me"
    (PerspectRol $ rolRecord "model:Systeem$Systeem$gebruiker" "model:User" "Me")
  cacheUncachedEntiteit "model:User$TrustedCluster"
    (PerspectContext $ contextRecord "model:User" "TrustedCluster" "model:Systeem$TrustedCluster")

  (_ :: PerspectContext) <- saveUnversionedEntiteit "model:User$Systeem"
  (_ :: PerspectRol) <- saveUnversionedEntiteit "model:User$Systeem_BuitenRol"
  (_ :: PerspectRol) <- saveUnversionedEntiteit "model:User$Me"
  (_ :: PerspectContext) <- saveUnversionedEntiteit "model:User$TrustedCluster"

  addRol "model:User$Systeem" "model:User$Systeem$gebruiker" "model:User$Me"

contextRecord :: Namespace -> LocalName -> ContextID -> ContextRecord
contextRecord ns ln tp = defaultContextRecord
  { _id = ns <> "$" <> ln
  , displayName = ln
  , pspType = tp
  , binnenRol = PerspectRol $ defaultRolRecord
    { _id = ns <> "$" <> ln <> "_BinnenRol"
    , pspType = "model:Perspectives$Context$binnenRol"
    , binding = binding $ ns <> "$" <> ln <> "_BuitenRol"
    }
  , buitenRol = ns <> "$" <> ln <> "_BuitenRol"
  }

rolRecord :: ContextID -> Namespace -> LocalName -> RolRecord
rolRecord tp ns ln = defaultRolRecord
  { _id = ns <> "$" <> ln <> "_BuitenRol"
  , pspType = "model:Perspectives$Context$buitenRol"
  , context = ns <> "$" <> ln
  }

buitenRolRecord :: Namespace -> LocalName -> RolRecord
buitenRolRecord = rolRecord "model:Perspectives$Context$buitenRol"
