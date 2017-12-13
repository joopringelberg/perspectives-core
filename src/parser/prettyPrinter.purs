module Perspectives.PrettyPrinter where

import Control.Monad (class Monad)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (catMaybes, elemIndex, replicate)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, foldM, lookup)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Perspectives.Resource (PROPDEFS, getContext, getRole)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), Comment, Comments(..), ContextRoleComments, PerspectContext(..), PerspectRol(..), PropertyName, ID)
import Prelude (Unit, bind, discard, pure, unit, ($), (*>), (+), (-), (<$>), (<>))

type IndentLevel = Int

-- type PerspectText = State IndentLevel String


type PerspectText2 e = StateT IndentLevel (WriterT String (Aff (DomeinFileEffects (prd :: PROPDEFS | e))))
-- type PerspectText = StateT IndentLevel (WriterT String Identity) Unit
type PerspectText e = PerspectText2 e Unit

type PrettyPrinter a e = a -> PerspectText e

-- | Indents the line and prints the string s.
identifier :: forall e. PrettyPrinter String e
identifier s = do
  i <- get
  tell $ (fromCharArray (replicate i '\t')) <> s

-- | Just prints the string s.
identifier' :: forall e. PrettyPrinter String e
identifier' s = tell s

simpleValue :: forall e. PrettyPrinter (Array String) e
simpleValue [v] = identifier' v
simpleValue a = do
  _ <- traverse identifier' a
  pure unit

space :: forall e. PerspectText e
space = tell " "

newline :: forall e. PerspectText e
newline = tell "\n"

-- | Indents and prints the comment, followed by a newline.
comment :: forall e. PrettyPrinter Comment e
comment c = identifier ( "--" <> c) *> newline

-- | Just prints the comment, preceded by a space.
comment' :: forall e. PrettyPrinter Comment e
comment' c = space *> identifier' ( "--" <> c)

-- prettyPrintContext :: PerspectContext -> String
-- prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

prettyPrint :: forall a e. a -> PrettyPrinter a e -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) String
prettyPrint t pp = prettyPrint' $ pp t

prettyPrint' :: forall e. PerspectText e -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) String
prettyPrint' t = do
  x <- runWriterT (evalStateT t 0)
  pure $ snd x

indent :: forall a e. PrettyPrinter a e -> PrettyPrinter a e
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

withComments :: forall a e f. (a -> Comments f) -> PrettyPrinter a e -> PrettyPrinter a e
withComments getComments p el = do
  let (Comments{commentBefore, commentAfter}) = getComments el
  traverse_ comment commentBefore
  p el
  traverse_ comment commentAfter

getCommentBefore :: forall f. Comments f -> Array Comment
getCommentBefore (Comments {commentBefore}) = commentBefore

getCommentAfter :: forall f. Comments f -> Array Comment
getCommentAfter (Comments {commentAfter}) = commentAfter

context :: forall e. Array ID -> PrettyPrinter PerspectContext e
context definedResources (PerspectContext c) = do
  withComments (\r->r.comments) contextDeclaration c
  publicProperties
  privateProperties
  traverse_ (indent roleBinding) c.rolInContext
  where
    contextDeclaration x = identifier x.pspType *> space *> identifier' x.id

    publicProperties = do
      -- LET OP: de buitenrol is geen integraal onderdeel van de context!
      maybeBuitenRol <- liftAff $ getRole c.buitenRol
      case maybeBuitenRol of
        (Just (PerspectRol buitenRol)) -> do
          let
            publicProperty :: String -> Array String -> PerspectText e
            publicProperty prop = indent (\val -> do
              -- Dit vergt dat we een Comments structuur bewaren voor elke property.
              -- withComments (\r-> commentBeforeRolProperty r.comments prop)
              --   (\_-> identifier ("public " <> prop <> " = ") *> simpleValue val *> newline)
              --   buitenRol)

              traverse_ comment (commentBeforeRolProperty buitenRol.comments prop)
              identifier ("public " <> prop <> " = ")
              simpleValue val
              traverse_ comment' (commentAfterRolProperty buitenRol.comments prop)
              newline)
          strMapTraverse_ publicProperty buitenRol.properties
        Nothing -> pure unit

    privateProperties = do
      let
        (BinnenRol binnenRol) = c.binnenRol
        publicProperty :: String -> Array String -> PerspectText e
        publicProperty prop = indent (\val -> do
          traverse_ comment (commentBeforeRolProperty binnenRol.comments prop)
          identifier ("private " <> prop <> " = ")
          simpleValue val
          traverse_ comment' (commentAfterRolProperty binnenRol.comments prop)
          newline)
      strMapTraverse_ publicProperty binnenRol.properties

    roleBinding :: ID -> PerspectText e
    roleBinding rolId = do
      -- NB: This is the role of the context - not yet its binding!
      maybeRole <- liftAff $ getRole rolId
      case maybeRole of
        (Just (PerspectRol r)) -> do
          traverse_ comment (getCommentBefore c.comments)
          identifier $ r.pspType <> " => "
          case r.binding of
            Nothing -> pure unit
            (Just b) -> do
              -- (INLINE CONTEXT) If the binding is not defined in the same text, recursively display the context or
              -- role that is bound (But only if it has been defined at all! Otherwise merely display its name).
              case elemIndex b definedResources of
                Nothing -> do
                  maybeContext <- liftAff $ getContext b
                  case maybeContext of
                    Nothing -> reference b r
                    (Just contxt) -> do
                      newline
                      indent (context definedResources) contxt
                      strMapTraverse_ (roleProperty r) r.properties
                otherwise -> reference b r
        Nothing -> pure unit
      where
        reference :: forall f. String -> {properties :: StrMap (Array String), comments :: ContextRoleComments | f} -> PerspectText e
        reference b r = do
          identifier' b
          traverse_ comment (getCommentAfter c.comments)
          newline
          strMapTraverse_ (roleProperty r) r.properties

        roleProperty :: forall f. {comments :: ContextRoleComments | f} -> String -> Array String -> PerspectText e
        roleProperty r1 prop = indent (\val -> do
          traverse_ comment (commentBeforeRolProperty r1.comments prop)
          identifier (prop <> " = ")
          simpleValue val
          traverse_ comment' (commentAfterRolProperty r1.comments prop)
          newline)


commentBeforeRolProperty :: ContextRoleComments -> PropertyName -> Array Comment
commentBeforeRolProperty (Comments {propertyComments}) propname = case lookup propname propertyComments of
  Nothing -> []
  (Just (Comments{commentBefore})) -> commentBefore

commentAfterRolProperty :: ContextRoleComments -> PropertyName -> Array Comment
commentAfterRolProperty (Comments {propertyComments}) propname = case lookup propname propertyComments of
  Nothing -> []
  (Just (Comments{commentAfter})) -> commentAfter

strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> StrMap a -> m Unit
strMapTraverse_ f map = foldM (\z s a -> f s a) unit map

sourceText :: forall e. PrettyPrinter PerspectContext e
sourceText (PerspectContext theText) = do
  textDeclaration
  newline
  traverse_ definition theText.rolInContext
  where
    textDeclaration = do
      traverse_ comment (getCommentBefore theText.comments)
      identifier "Text "
      identifier' theText.id
      newline

    definition rolId = do
      -- NB: This is the role of the text - not yet the definition itself!
      -- TODO: only print the binding if there is no reference to it in this text.
      maybeRole <- liftAff $ getRole rolId
      case maybeRole of
        Nothing -> pure unit
        (Just (PerspectRol r)) ->
          case r.binding of
            Nothing -> newline
            (Just ident) -> do
              maybeDef <- liftAff $ getContext ident
              case maybeDef of
                Nothing -> pure unit
                (Just def) -> do
                  (resourceDefs :: Array (Maybe PerspectRol)) <- liftAff $ traverse getRole theText.rolInContext
                  definedResources <- pure ((\(PerspectRol {binding}) -> binding) <$> (catMaybes resourceDefs))
                  context (catMaybes definedResources) def
                  newline
