module Test.Perspectives.Utils where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Effect.Aff (Aff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb.Databases (createDatabase, deleteDatabase)
import Perspectives.RunPerspectives (runPerspectives)
import Test.Unit.Assert as Assert


runP :: forall a.
  MonadPerspectives a ->
  Aff a
runP t = runPerspectives "cor" "geheim" t

p :: String -> String
p s = "model:Perspectives$" <> s

q :: String -> String
q s = "model:QueryAst$" <> s

u :: String -> String
u s = "model:User$" <> s

shouldEqual :: forall a. Eq a => a -> a -> Aff Boolean
shouldEqual a = \b -> pure (a == b)

type Message = String

assertEqual :: forall a. Eq a => Show a =>
  Message ->
  MonadPerspectives a ->
  a ->
  Aff Unit
assertEqual message test result = do
  r <- runP test
  case result == r of
    true -> Assert.assert message true
    false -> Assert.assert (message <> "\nExpected: " <>
      show result <> "\nReceived: " <>
      show r)
      false

clearUserDatabase :: MonadPerspectives Unit
clearUserDatabase = do
  uname <- (AA.gets _.userInfo.userName)
  deleteDatabase $ "user_" <> uname <> "_entities"
  createDatabase $ "user_" <> uname <> "_entities"
