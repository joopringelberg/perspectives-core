module ForeignImport where

import Control.Monad.Aff (Aff)
import Data.Function.Uncurried (runFn2, Fn2)

type F2 = String -> String -> String

type ModuleName = String
type FunctionName = String

-- | The implementation of this function uses javascript 'require' to import a module and take a function from it.
foreign import requireFn2 :: ModuleName -> FunctionName -> (Fn2 String String String)

-- | A curried function of two arguments taken from an external module.
requireF2 :: ModuleName -> FunctionName -> F2
requireF2 m f = runFn2 (requireFn2 m f)

-- Assuming a module math that has a function plus that adds the integers represented by strings
-- and returns the result as a string, we could have a property function plus in this way:
plus :: F2
plus = requireF2 "math" "plus"

type F2Aff e = String -> String -> Aff e String
