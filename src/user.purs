module Perspectives.User where

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, readVar)
import Data.Maybe (Maybe(..))
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke)
import Prelude (Unit, bind, unit, void, ($), discard)

type User = (AVar String)
type UserCache = GLStrMap User

userCache :: UserCache
userCache = new unit

getUser :: forall e. Aff (AvarCache e) String
getUser = do
  mAv <- liftEff' $ peek userCache "user"
  case mAv of
    Nothing -> do
      v <- makeEmptyVar
      void $ liftEff' $ poke userCache "user" v
      readVar v
    (Just v) -> readVar v

setUser :: forall e. String -> Aff (AvarCache e) Unit
setUser id = do
  mv <- liftEff' $ peek userCache "user"
  case mv of
    Nothing -> do
      v <- makeVar id
      void $ liftEff' (poke userCache "user" v)
    (Just v) -> void $ liftEff' (poke userCache "user" v)
