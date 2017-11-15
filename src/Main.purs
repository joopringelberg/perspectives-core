module Main where

-- import Test.TestEffects
-- import Test.QueryEffects
import Perspectives.CommentParser
import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Eff (Eff)
import Perspectives.IndentParser (runIndentParser)
import Prelude (Unit)

-- main :: forall e. Eff (CancelerEffects e) (Fiber (CancelerEffects e) Unit)
-- main = runAff handleError test

main = runIndentParser test11 file
