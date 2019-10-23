module Test.SignalTest where

import Signal
import Prelude
import Signal.Channel
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Timer (TIMER, setTimeout)

test :: forall e. Eff (console :: CONSOLE | e) Unit
test =
  let
    (hello :: Signal String) = constant "Hello Joop!"
    (helloEffect :: Signal (Eff (console :: CONSOLE | e) Unit)) = map log hello
  in
    runSignal helloEffect

test1 :: forall e. Eff (console :: CONSOLE, channel :: CHANNEL, timer :: TIMER | e) Unit
test1 = do
  (helloChannel :: Channel String) <- channel "Hello Joop"
  let
    (hello :: Signal String) = subscribe helloChannel
    helloEffect = map log hello
  _ <- setTimeout 1000 do
      log "Change signal"
      send helloChannel "Once again"
  runSignal helloEffect
