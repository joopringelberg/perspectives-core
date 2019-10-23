module Temp.TryAttempt where

import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (logShow, log)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String (length)
import Prelude (bind, pure, ($), (*), (<), (<<<), (<=<), (<>), show)

type AffMaybe e a = MaybeT (Aff e) a

f :: forall e. Int -> AffMaybe e Int
f n | n < 1 = throwError $ error "n is too small"
f n = pure n

z :: forall e. Int -> Aff e (Either Error (Maybe Int))
z = attempt <<< runMaybeT <<< f

test = launchAff do
  r <- z 0
  case r of
    (Left err) -> log ("Error, message = " <> show err <> " (" <> show (length $ show err) <> ")")
    (Right x) -> logShow x

g :: forall e. Int -> AffMaybe e Int
g n = pure (n * n)

fg = f <=< g
