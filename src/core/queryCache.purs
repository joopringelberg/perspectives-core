module Perspectives.QueryCache where

import Effect.Class (liftEffect)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, QueryCache, type (**>))
import Perspectives.GlobalUnsafeStrMap (new, peek, poke)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Prelude (pure, unit, ($), bind)

queryCache :: QueryCache
queryCache = new unit

queryCacheInsert :: forall s o. String -> (s **> o) -> MonadPerspectives ((s **> o))
queryCacheInsert qname getter = do
  _ <- lift $ liftEffect $ poke queryCache qname (typeWithPerspectivesTypes getter)
  pure getter

queryCacheLookup :: forall s o. String -> MonadPerspectives (Maybe ((s **> o)))
queryCacheLookup qname = liftEffect $ typeWithPerspectivesTypes $ peek queryCache qname
