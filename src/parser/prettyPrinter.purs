module Perspectives.PrettyPrinter where

import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (replicate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.String (fromCharArray)
import Data.Tuple (snd)
import Perspectives.Syntax2 (EntityCollection, PerspectContext(..))
import Prelude (Unit, bind, discard, pure, unit, ($), (+), (-), (<>))

type IndentLevel = Int

-- type PerspectText = State IndentLevel String

type PerspectText = StateT IndentLevel (WriterT String Identity) Unit

type PrettyPrinter a = a -> PerspectText

identifier :: PrettyPrinter String
identifier s = do
  i <- get
  tell $ (fromCharArray (replicate i '\t')) <> s
  pure unit

space :: PerspectText
space = do
  tell " "
  pure unit

newline :: PerspectText
newline = do
  tell "\n"
  pure unit

prettyPrint :: EntityCollection -> String
prettyPrint ec = snd (unwrap (runWriterT $ evalStateT ((indent identifier) "Klaar!") 0))

prettyPrintContext :: PerspectContext -> String
prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

indent :: forall a. PrettyPrinter a -> PrettyPrinter a
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

context :: PrettyPrinter PerspectContext
context (PerspectContext r) = do
  typeDeclaration
  where
    typeDeclaration = do
      identifier r.pspType
      space
      identifier r.id
      newline
