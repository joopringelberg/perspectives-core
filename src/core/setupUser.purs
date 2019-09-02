module Perspectives.SetupUser where

import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.Instances (tryGetPerspectEntiteit)
import Perspectives.LoadCRL (loadCRLFile)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Prelude (Unit, bind, pure, unit, void, ($))

setupUser :: MonadPerspectives Unit
setupUser = do
  (mu :: Maybe PerspectContext) <- tryGetPerspectEntiteit (ContextInstance "model:User$MijnSysteem")
  case mu of
    Nothing -> void $ loadCRLFile "systeemInstanties.crl"
    otherwise -> pure unit
