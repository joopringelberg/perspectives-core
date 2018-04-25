module Perspectives.QueryCache where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, QueryCache, TypedTripleGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, new, peek, poke)
import Prelude (unit, ($), pure, (*>))

queryCache :: forall e. (QueryCache e)
queryCache = new unit

queryCacheInsert :: forall e. String -> TypedTripleGetter e -> MonadPerspectives (gm :: GLOBALMAP | e) (TypedTripleGetter e)
queryCacheInsert qname getter = (pure $ poke queryCache qname getter) *> pure getter

queryCacheLookup :: forall e. String -> MonadPerspectives (AjaxAvarCache e) (Maybe (TypedTripleGetter e))
queryCacheLookup qname = liftEff $ peek queryCache qname
