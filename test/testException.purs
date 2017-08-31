module Test.Exception where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (log, CONSOLE)


myError :: Error
myError = error "Dit is mijn fout."

-- Deze functie levert een computation, gelabeld met het EXCEPTION effect, van een error (d.w.z. het is een thunk
-- die bij evaluatie een error throwt in Javascript).
myExceptionThrower :: forall e. String -> Eff( exception :: EXCEPTION | e) String
myExceptionThrower x = throwException $ error x

-- Deze functie consumeert een error en levert een computation, gelabeld met het CONSOLE effect, die een string oplevert.
myExceptionHandler :: forall eff. Error -> Eff ( console:: CONSOLE |eff ) String
myExceptionHandler err = do
  log (message err)
  pure "Dit is de vervangende waarde die door de handler wordt geleverd."

-- Deze functie produceert een computation gelabeld met het CONSOLE effect. catchException stript het EXCEPTION
-- effect van het resultaat van myExceptionThrower. myExceptionHandler voegt daarna het CONSOLE effect toe.
myExceptionCatchingFunction :: forall eff. Eff (console:: CONSOLE |eff) String
myExceptionCatchingFunction = catchException myExceptionHandler (myExceptionThrower "oorzaak")

-- runit pakt de waarde uit de computation die myExceptionCatchingFunction levert. Daardoor worden alle effecten gesorteerd!
runit :: Eff( console :: CONSOLE ) String
runit = do myExceptionCatchingFunction
