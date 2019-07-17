module Perspectives.QueryCache where

import Effect.Class (liftEffect)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, QueryCache, StringTypedTripleGetter)
import Perspectives.GlobalUnsafeStrMap (new, peek, poke)
import Prelude (pure, unit, ($), bind)

queryCache :: QueryCache
queryCache = new unit

queryCacheInsert :: String -> StringTypedTripleGetter -> MonadPerspectives StringTypedTripleGetter
queryCacheInsert qname getter = do
  _ <- lift $ liftEffect $ poke queryCache qname getter
  pure getter

queryCacheLookup :: String -> MonadPerspectives (Maybe StringTypedTripleGetter)
queryCacheLookup qname = liftEffect $ peek queryCache qname
