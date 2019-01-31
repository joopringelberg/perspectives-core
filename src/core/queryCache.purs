module Perspectives.QueryCache where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, QueryCache, TypedTripleGetter)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, new, peek, poke)
import Prelude (pure, unit, ($), bind)

queryCache :: forall s p o c r b e. (QueryCache s p o c r b e)
queryCache = new unit

queryCacheInsert :: forall s p o c r b e1 e2.
  String ->
  TypedTripleGetter s p o c r b e1 ->
  MonadPerspectives c r b (gm :: GLOBALMAP | e2) (TypedTripleGetter s p o c r b e1)
queryCacheInsert qname getter = do
  _ <- lift $ liftEff $ poke queryCache qname getter
  pure getter

queryCacheLookup :: forall s p o c r b e1 e2. 
  String ->
  MonadPerspectives c r b (gm :: GLOBALMAP | e1) (Maybe (TypedTripleGetter s p o c r b e2))
queryCacheLookup qname = liftEff $ peek queryCache qname
