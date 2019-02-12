module Test.Perspectives.Utils where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Now (NOW)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesState (runPerspectives)
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
