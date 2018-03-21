module Perspectives.Utilities where

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Maybe (Maybe, maybe)
import Prelude (type (~>), pure, (>>=))

onNothing :: forall m a e. MonadThrow e m => e -> m (Maybe a) -> m a
onNothing s ma = ma >>= (maybe (throwError s) pure)

onNothing' :: forall m e. MonadThrow e m => e -> Maybe ~> m
onNothing' s = maybe (throwError s) pure
