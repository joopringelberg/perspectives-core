module Test.TestEffects where

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Property (PropDefsEffects)

type CancelerEffects e = (PropDefsEffects (console :: CONSOLE, gm :: GLOBALMAP | e))
type TestEffects e = CancelerEffects (exception :: EXCEPTION | e)
