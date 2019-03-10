module Test.Perspectives.Utils where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExcept)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON)
import Perspectives.ApiTypes (ContextSerialization)
import Perspectives.BasicConstructors (constructContext)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (buitenRol)
import Perspectives.PerspectivesState (runPerspectives)
import Perspectives.PerspectivesTypes (BuitenRol(..), AnyContext)
import Perspectives.SaveUserData (removeUserData, saveUserData)
import Test.Unit.Assert as Assert

type TestEffects e = AjaxAvarCache (now :: NOW | e)
type RunEffects e = (avar :: AVAR, now :: NOW | e)

runP :: forall a e.
  MonadPerspectives (RunEffects e) a ->
  Aff (RunEffects e) a
runP t = runPerspectives "cor" "geheim" t

p :: String -> String
p s = "model:Perspectives$" <> s

q :: String -> String
q s = "model:QueryAst$" <> s

u :: String -> String
u s = "model:User$" <> s

shouldEqual :: forall a e. Eq a => a -> a -> Aff e Boolean
shouldEqual a = \b -> pure (a == b)

type Message = String

assertEqual :: forall a e. Eq a =>
  Message ->
  MonadPerspectives (RunEffects e) a ->
  a ->
  Aff (RunEffects e) Unit
assertEqual message test result = do
  (Assert.assert message) =<<
    (runP test >>=
      (shouldEqual result))

addTestContext :: forall e. String -> Aff (TestEffects e) Unit
addTestContext s = void $ runP $ addTestContext' s
  where
    addTestContext' :: forall eff. String -> MonadPerspectives (AjaxAvarCache eff) Unit
    addTestContext' s = do
      (r :: Either MultipleErrors ContextSerialization) <- pure $ runExcept $ decodeJSON s
      case r of
        (Left m) -> throwError $ error $ show [NotWellFormedContextSerialization $ show m]
        (Right cs) -> do
          r <- constructContext cs
          case r of
            (Left messages) -> throwError (error (show messages))
            (Right id) -> do
              _ <- saveUserData [BuitenRol $ buitenRol id]
              pure unit

removeTestContext :: forall e. AnyContext -> Aff (TestEffects e) Unit
removeTestContext cid = void $ runP $ removeTestContext' (buitenRol cid)
  where

  removeTestContext' :: forall eff. AnyContext -> MonadPerspectives (AjaxAvarCache eff) Unit
  removeTestContext' = void <<< removeUserData <<< singleton <<< BuitenRol
