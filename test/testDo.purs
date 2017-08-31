module Test.Do where
  import Prelude
  import Data.Identity

  x :: Int
  x = n where
    (Identity n) = do
      {v} <- Identity { v: 1}
      Identity (v + 1)

  foo :: Int -> Int
  foo i = n where
    (Identity n) = bar i

  bar :: Int -> Identity Int
  bar n = do
    t1 <- Identity n
    Identity (t1 + 10)

  y :: Int
  y = foo 1
