module Test.Ref where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)

{-
Dit werkt zonder unsafeRunRef niet zoals verwacht. t is een computation die een nieuwe cel oplevert.
Dat betekent dat tref in test1, test2 en test3 telkens aan een nieuwe cel is gebonden.
Het verschil met ST lijkt alleen te zijn dat er geen handler moet worden toegepast (geen runST) om de berekening uit te laten voeren.

met runPure (usafeRunRef (newRef 1)) verkrijg je wel een cel die je direct kunt muteren.
-}

t :: forall r. Eff (ref :: REF | r) (Ref Int)
t = newRef 1
ref :: Ref Int
-- Run the computation and remove the REF Effect to obtain a direct Ref Int cell.
ref = runPure (unsafeRunRef t)

test1 :: forall r. Eff (ref :: REF, console :: CONSOLE | r ) Unit
test1 = do
  tval <- readRef ref
  log (show tval)

test2 :: forall r. Eff (ref :: REF | r ) Unit
test2 = writeRef ref 2


test3 :: forall r. Eff (ref :: REF, console :: CONSOLE | r ) Unit
test3 = do
  tval <- readRef ref
  log (show tval)
