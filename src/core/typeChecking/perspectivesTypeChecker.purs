module Perspectives.Checking.PerspectivesTypeChecker where

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Representation.Class.Persistent (identifier)
import Perspectives.Representation.Context (Context, defaultPrototype)
import Prelude (bind, pure, (==))

checkContext :: Context -> MonadPerspectives Boolean
checkContext c = do
  -- The default prototype should have the type that we are checking.
  case defaultPrototype c of
    Nothing -> pure true
    (Just pt) -> do
      (p :: PerspectContext) <- getPerspectEntiteit pt
      pure ((unwrap p).pspType == identifier c)
