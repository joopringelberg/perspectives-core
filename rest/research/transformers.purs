module Research.Transformers where

import Prelude
import Control.Monad.State
import Control.Monad.State.Class
import Data.Tuple
import Data.Array ((..))
import Data.Foldable (traverse_)
import Data.Identity (Identity(..))

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT (s :: s -> m (Tuple a s))) = s

-- x = execState do sumArray [1, 2, 3] 0
y = runState (do (sumArray [1,2])) 0

z :: StateT Int Identity Unit
z = do
  sumArray [1,2]
  sumArray [3,4]

z' :: StateT Int Identity Unit
z' = do
  (_ :: Unit) <- sumArray [1,2]
  (_ :: Unit) <- sumArray [1,2]
  pure unit

h :: StateT Int Identity Unit
h = do
  (_ :: Unit) <- sumArray [1,2]
  (_ :: Unit) <- sumArray [1,2]
  sumArray [2,3]

p :: StateT Int Identity Unit
p = do sumArray [1,2]

p'' :: StateT Int Identity Unit
p'' = sumArray [1,2]

p' :: StateT Int Identity Unit
p' = pure unit >>= (\unit -> sumArray [1,2 ])

x :: Unit -> Int
x = \_ -> 3
