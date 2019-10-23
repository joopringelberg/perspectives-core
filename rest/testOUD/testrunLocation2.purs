module Test.Location2 where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Perspectives.Location (saveInLocation, setLocationValue, runLocation, runTHEORYDELTA, locationValue)
import Prelude (Unit, discard, map, show, ($), (+), (<$>), (<*>), (<<<), (<>))

test :: forall e. Eff (console :: CONSOLE |e) Unit
test = let
  n = saveInLocation 1
  p = map ((+) 10) n
  s = (+) <$> n <*> p
  -- locWithEffect mimics a GUI element that shows a system value that can change.
  locWithEffect = map (log <<< ((<>) "The value of s has changed to: ") <<< show) s
  in runTHEORYDELTA do
    logShow $ "Showing the initial value, just once: " <> show (locationValue s)
    -- By using runLocation, we ensure that a new computation on locWithEffect is run.
    runLocation locWithEffect
    -- This will lead to a new value in s and a new computation in locWithEffect.
    setLocationValue n 10
