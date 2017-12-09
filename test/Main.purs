module Test.Main where

import Test.PrettyPrinter
import Test.TestEffects as TE
import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Prelude (Unit)

main :: forall e. Eff (TestEffects (console :: CONSOLE | e)) (Fiber (TestEffects (console :: CONSOLE | e)) Unit)
main = runAff TE.handleError test

-- main = test

-- import Prelude
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.Eff.Timer (TIMER, setTimeout)
-- import Signal.Channel (CHANNEL)
-- import Signal.Loop (Emitter, runLoop)
--
-- main :: Eff (console :: CONSOLE, channel :: CHANNEL, timer :: TIMER) Unit
-- main = void do
--   let view :: Int -> Emitter (console :: CONSOLE, channel :: CHANNEL, timer :: TIMER) Int
--       view n emit = void do
--         log $ "Received: " <> show n
--         setTimeout 1000 do
--           emit (n + 1)
--
--   -- The loop reads the most recent value from the "future" signal
--   -- and uses the view function to display it and simulate the next event.
--   runLoop 0 \future -> map view future
