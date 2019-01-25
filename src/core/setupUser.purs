module Perspectives.SetupUser where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.Process (PROCESS)
import Perspectives.Actions (addRol)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.Identifiers (Namespace, LocalName)
import Perspectives.LoadCRL (loadCRLFile)
import Perspectives.PerspectEntiteit (cacheUncachedEntiteit)
import Perspectives.Resource (tryGetPerspectEntiteit)
import Perspectives.ResourceRetrieval (saveUnversionedEntiteit)
import Perspectives.Syntax (ContextRecord, PerspectContext(..), PerspectRol(..), RolRecord, binding)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>))

setupUser :: forall e. MonadPerspectives (AjaxAvarCache (now :: NOW, console :: CONSOLE, fs :: FS, exception :: EXCEPTION, process :: PROCESS | e)) Unit
setupUser = do
  (mu :: Maybe PerspectContext) <- tryGetPerspectEntiteit "model:User$MijnSysteem"
  case mu of
    -- Nothing -> void $ setupUser'
    Nothing -> loadCRLFile "systeemInstanties.crl"
    otherwise -> pure unit

setupUser' :: forall e. MonadPerspectives (AjaxAvarCache (now :: NOW | e)) (Array ID)
setupUser' = do
  cacheUncachedEntiteit "model:User$MijnSysteem"
    (PerspectContext $ contextRecord "model:User" "Systeem" "model:Perspectives$Systeem")
  cacheUncachedEntiteit "model:User$MijnSysteem_BuitenRol"
    (PerspectRol $ buitenRolRecord  "model:User" "Systeem")
  cacheUncachedEntiteit "model:User$Me"
    (PerspectRol $ rolRecord "model:Perspectives$Systeem$gebruiker" "model:User" "Me")
  cacheUncachedEntiteit "model:User$TrustedCluster"
    (PerspectContext $ contextRecord "model:User" "TrustedCluster" "model:Perspectives$TrustedCluster")

  (_ :: PerspectContext) <- saveUnversionedEntiteit "model:User$MijnSysteem"
  (_ :: PerspectRol) <- saveUnversionedEntiteit "model:User$MijnSysteem_BuitenRol"
  (_ :: PerspectRol) <- saveUnversionedEntiteit "model:User$Me"
  (_ :: PerspectContext) <- saveUnversionedEntiteit "model:User$TrustedCluster"

  addRol "model:User$MijnSysteem$gebruiker" "model:User$Me" "model:User$MijnSysteem"

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
