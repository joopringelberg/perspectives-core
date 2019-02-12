module Perspectives.QueryCache where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, QueryCache, TypedTripleGetter)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, new, peek, poke)
import Prelude (pure, unit, ($), bind)

queryCache :: forall e. (QueryCache e)
queryCache = new unit

queryCacheInsert :: forall e1 e2. String -> TypedTripleGetter e1 -> MonadPerspectives (gm :: GLOBALMAP | e2) (TypedTripleGetter e1)
queryCacheInsert qname getter = do
  _ <- lift $ liftEff $ poke queryCache qname getter
  pure getter

queryCacheLookup :: forall e1 e2. String -> MonadPerspectives (gm :: GLOBALMAP | e1) (Maybe (TypedTripleGetter e2))
queryCacheLookup qname = liftEff $ peek queryCache qname
