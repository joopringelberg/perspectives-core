module Test.ParseInt where

foreign import parseInt :: String -> Int

i1 :: Int
i1 = parseInt "10"
