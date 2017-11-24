module Perspectives.Guid where

import Prelude (Unit)

type Guid = String

foreign import guid :: Unit -> Guid
