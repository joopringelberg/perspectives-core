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
import Data.Identity (Identity)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, fold, foldM)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..), snd)
import Perspectives.ResourceRetrieval (fetchRole)
import Perspectives.ResourceTypes (AsyncDomeinFile, PropDefs(..), DomeinFileEffects)
import Perspectives.Syntax2 (Comment, Comments(..), PerspectContext(..), PerspectRol(..))
import Prelude (Unit, bind, discard, pure, show, unit, ($), (*>), (+), (-), (<>))

type IndentLevel = Int

-- type PerspectText = State IndentLevel String


type PerspectText2 = StateT IndentLevel (WriterT String (Aff (DomeinFileEffects ())))
-- type PerspectText = StateT IndentLevel (WriterT String Identity) Unit
type PerspectText = PerspectText2 Unit

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

comment :: PrettyPrinter Comment
comment c = identifier c *> newline

-- prettyPrintContext :: PerspectContext -> String
-- prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

prettyPrint :: forall a. a -> PrettyPrinter a -> Aff (DomeinFileEffects()) String
prettyPrint t pp = prettyPrint' $ pp t

prettyPrint' :: PerspectText -> Aff (DomeinFileEffects()) String
prettyPrint' t = do
  x <- runWriterT (evalStateT t 0)
  pure $ snd x

indent :: forall a. PrettyPrinter a -> PrettyPrinter a
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

context :: PrettyPrinter PerspectContext
context (PerspectContext r) = do
  typeDeclaration
  -- LET OP: de buitenrol is geen integraal onderdeel van de context!
  (PerspectRol buitenRol) <- liftAff $ fetchRole r.buitenRol
  strMapTraverse_ publicProperty buitenRol.properties
  where
    typeDeclaration = do
      traverse_ comment (maybe [] (\(Comments c) -> c.commentBefore) r.comments)
      identifier r.pspType
      space
      identifier r.id
      newline
    publicProperty :: String -> Array String -> PerspectText
    publicProperty prop val = do
      identifier "public"
      space
      identifier prop
      identifier " = "
      identifier (show val)
      newline


strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> StrMap a -> m Unit
strMapTraverse_ f map = foldM (\z s a -> f s a) unit map
