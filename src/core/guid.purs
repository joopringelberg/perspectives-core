module Perspectives.Guid where

import Prelude (class Show, Unit)

foreign import data Guid :: Type

foreign import guid :: forall a. Unit -> Guid

foreign import show_ :: Guid -> String

instance showGuid :: Show Guid where
  show = show_
