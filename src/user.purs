module Perspectives.User where

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, readVar)
import Data.Maybe (Maybe(..))
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke)
import Prelude (Unit, bind, unit, void, ($), discard, pure, (<>))

type UserInfo =
  { userName :: String
  , couchdbPassword :: String
  , couchdbBaseURL :: String
  }

type UserCache = GLStrMap (AVar String)

userCache :: UserCache
userCache = new unit

getUser :: forall e. Aff (AvarCache e) String
getUser = get "user"

setUser :: forall e. String -> Aff (AvarCache e) Unit
setUser = set "user"

setCouchdbPassword :: forall e. String -> Aff (AvarCache e) Unit
setCouchdbPassword = set "couchdbPassword"

getCouchdbPassword :: forall e. Aff (AvarCache e) String
getCouchdbPassword = get "couchdbPassword"

setCouchdbBaseURL :: forall e. String -> Aff (AvarCache e) Unit
setCouchdbBaseURL = set "couchdbBaseURL"

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall e. Aff (AvarCache e) String
getCouchdbBaseURL = get "couchdbBaseURL"

-- | Url terminated with a forward slash.
entitiesDatabase :: forall e. Aff (AvarCache e ) String
entitiesDatabase = do
  usr <- getUser
  cdbUrl <- getCouchdbBaseURL
  pure $ cdbUrl <> "user_" <> usr <> "_entities/"

set :: forall e. String -> String -> Aff (AvarCache e) Unit
set key id = do
  mv <- liftEff' $ peek userCache key
  case mv of
    Nothing -> do
      v <- makeVar id
      void $ liftEff' (poke userCache key v)
    (Just v) -> void $ liftEff' (poke userCache key v)

get :: forall e. String -> Aff (AvarCache e) String
get key = do
  mAv <- liftEff' $ peek userCache key
  case mAv of
    Nothing -> do
      v <- makeEmptyVar
      void $ liftEff' $ poke userCache key v
      readVar v
    (Just v) -> readVar v
