module Perspectives.Guid where

import Prelude (Unit)

foreign import data Guid :: Type

foreign import guid :: forall a. Unit -> Guid
