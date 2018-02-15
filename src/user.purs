module Perspectives.User where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, readVar)
import Prelude (Unit, (>>=))

type User = AVar String

user :: forall e. Aff (avar :: AVAR | e) User
user = makeEmptyVar

getUser :: forall e. Aff (avar :: AVAR | e) String
getUser = user >>= readVar

setUser :: forall e. String -> Aff (avar :: AVAR | e) Unit
setUser id = user >>= putVar id
