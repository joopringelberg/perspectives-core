module Perspectives.PrettyPrinter where

import Control.Monad (class Monad)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (replicate)
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Data.StrMap (StrMap, foldM)
import Data.String (fromCharArray)
import Data.Tuple (snd)
import Perspectives.Resource (PROPDEFS, getRole)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax2 (Comment, Comments(..), PerspectContext(..), PerspectRol(..))
import Prelude (Unit, bind, discard, pure, show, unit, ($), (*>), (+), (-), (<>))

type IndentLevel = Int

-- type PerspectText = State IndentLevel String


type PerspectText2 e = StateT IndentLevel (WriterT String (Aff (DomeinFileEffects (prd :: PROPDEFS | e))))
-- type PerspectText = StateT IndentLevel (WriterT String Identity) Unit
type PerspectText e = PerspectText2 e Unit

type PrettyPrinter a e = a -> PerspectText e

identifier :: forall e. PrettyPrinter String e
identifier s = do
  i <- get
  tell $ (fromCharArray (replicate i '\t')) <> s
  pure unit

space :: forall e. PerspectText e
space = do
  tell " "
  pure unit

newline :: forall e. PerspectText e
newline = do
  tell "\n"
  pure unit

comment :: forall e. PrettyPrinter Comment e
comment c = identifier c *> newline

-- prettyPrintContext :: PerspectContext -> String
-- prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

prettyPrint :: forall a. a -> PrettyPrinter a ()-> Aff (DomeinFileEffects(prd :: PROPDEFS)) String
prettyPrint t pp = prettyPrint' $ pp t

prettyPrint' :: PerspectText () -> Aff (DomeinFileEffects (prd :: PROPDEFS)) String
prettyPrint' t = do
  x <- runWriterT (evalStateT t 0)
  pure $ snd x

indent :: forall a e. PrettyPrinter a e -> PrettyPrinter a e
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

context :: forall e. PrettyPrinter PerspectContext e
context (PerspectContext r) = do
  typeDeclaration
  -- LET OP: de buitenrol is geen integraal onderdeel van de context!
  (PerspectRol buitenRol) <- liftAff $ getRole r.buitenRol
  strMapTraverse_ publicProperty buitenRol.properties
  where
    typeDeclaration = do
      traverse_ comment (maybe [] (\(Comments c) -> c.commentBefore) r.comments)
      identifier r.pspType
      space
      identifier r.id
      newline
    publicProperty :: String -> Array String -> PerspectText e
    publicProperty prop val = do
      identifier "public"
      space
      identifier prop
      identifier " = "
      identifier (show val)
      newline


strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> StrMap a -> m Unit
strMapTraverse_ f map = foldM (\z s a -> f s a) unit map
