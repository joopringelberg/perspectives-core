module Test.Perspectives.Utils where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Perspectives.ContextAndRole (changeContext_me, changeRol_isMe)
import Perspectives.CoreTypes (MonadPerspectives, (##>>))
import Perspectives.Couchdb.Databases (createDatabase, deleteDatabase)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadCrlFile)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheOverwritingRevision)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
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

setupUser :: MonadPerspectives Unit
setupUser = do
  void $ loadCrlFile "systemInstances.crl" "./src/model"
  -- now set isMe of "model:User$MijnSysteem$User_0001" to true.
  (user :: RoleInstance) <- ContextInstance "model:User$MijnSysteem" ##>> getRole (EnumeratedRoleType "model:System$PerspectivesSystem$User")
  (userRol :: PerspectRol) <- getPerspectRol user
  void $ cacheOverwritingRevision user (changeRol_isMe userRol true)
  -- And set 'me' of "model:User$MijnSysteem"
  (mijnSysteem :: PerspectContext) <- getPerspectContext (ContextInstance "model:User$MijnSysteem")
  void $ cacheOverwritingRevision (ContextInstance "model:User$MijnSysteem") (changeContext_me mijnSysteem (Just user))
