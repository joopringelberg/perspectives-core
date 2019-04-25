module Perspectives.SetupUser where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.Process (PROCESS)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.LoadCRL (loadCRLFile, withoutSemanticChecks)
import Perspectives.Resource (tryGetPerspectEntiteit)
import Perspectives.Syntax (PerspectContext)
import Prelude (Unit, bind, pure, unit, void, ($))

setupUser :: forall e. MonadPerspectives (AjaxAvarCache (now :: NOW, console :: CONSOLE, fs :: FS, exception :: EXCEPTION, process :: PROCESS | e)) Unit
setupUser = do
  (mu :: Maybe PerspectContext) <- tryGetPerspectEntiteit "model:User$MijnSysteem"
  case mu of
    Nothing -> void $ loadCRLFile withoutSemanticChecks "systeemInstanties.crl"
    otherwise -> pure unit
