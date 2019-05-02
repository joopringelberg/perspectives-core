module Perspectives.QueryCache where

import Effect.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, QueryCache, type (**>))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, new, peek, poke)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Prelude (pure, unit, ($), bind)

queryCache :: forall e. (QueryCache e)
queryCache = new unit

queryCacheInsert :: forall s o e1 e2. String -> (s **> o) e1 -> MonadPerspectives (gm :: GLOBALMAP | e2) ((s **> o) e1)
queryCacheInsert qname getter = do
  _ <- lift $ liftEff $ poke queryCache qname (typeWithPerspectivesTypes getter)
  pure getter

queryCacheLookup :: forall s o e1 e2. String -> MonadPerspectives (gm :: GLOBALMAP | e1) (Maybe ((s **> o) e2))
queryCacheLookup qname = liftEff $ typeWithPerspectivesTypes $ peek queryCache qname
