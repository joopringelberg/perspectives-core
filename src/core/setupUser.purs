module Perspectives.SetupUser where

import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)

import Perspectives.LoadCRL (loadCRLFile, withoutSemanticChecks)
import Perspectives.Instances (tryGetPerspectEntiteit)
import Perspectives.InstanceRepresentation (PerspectContext)
import Prelude (Unit, bind, pure, unit, void, ($))

setupUser :: MonadPerspectives Unit
setupUser = do
  (mu :: Maybe PerspectContext) <- tryGetPerspectEntiteit "model:User$MijnSysteem"
  case mu of
    Nothing -> void $ loadCRLFile withoutSemanticChecks "systeemInstanties.crl"
    otherwise -> pure unit
