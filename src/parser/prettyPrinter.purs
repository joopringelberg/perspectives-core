module Perspectives.PrettyPrinter where

import Control.Monad.State (State, evalState, modify, get)
import Data.Array (replicate)
import Data.String (fromCharArray)
import Perspectives.Syntax2 (EntityCollection(..))
import Prelude (pure, ($), bind, (+), (-), (<>))

type IndentLevel = Int

type PerspectText = State IndentLevel String

type PrettyPrinter a = a -> PerspectText

identifier :: PrettyPrinter String
identifier s = do
  i <- get
  pure $ (fromCharArray (replicate i '\t')) <> s

prettyPrint :: EntityCollection -> String
prettyPrint ec = evalState ((indent identifier) "Klaar!") 0

indent :: forall a. PrettyPrinter a -> PrettyPrinter a
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result
