module Perspectives.PrettyPrinter where

import Control.Monad (class Monad)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (catMaybes, elemIndex, replicate, sortBy)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.StrMap (StrMap, foldM, values)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (roleIndexNr)
import Perspectives.Property (PropDefsEffects)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (ignoreCache)
import Perspectives.Resource (getContext, getRole)
import Perspectives.Syntax (BinnenRol(..), Comment, Comments(..), ID, PerspectContext(..), PerspectRol(..), PerspectRolProperties, PropertyName, PropertyValueWithComments, PerspectContextProperties, compareOccurrences, propertyValue)
import Perspectives.SystemQueries (binding, rolContext)
import Perspectives.TripleAdministration (Triple(..), tripleObjects)
import Perspectives.TripleGetter (constructRolGetter, (##))
import Prelude (Unit, bind, discard, id, join, pure, unit, ($), (*>), (+), (-), (<<<), (<>), (==))

type IndentLevel = Int

type PerspectText' e = StateT IndentLevel (WriterT String (Aff (PropDefsEffects e)))

type PerspectText e = PerspectText' e Unit

type PrettyPrinter a e = a -> PerspectText e

-- | Indents the line and prints the string s.
identifier :: forall e. PrettyPrinter String e
identifier s = do
  i <- get
  tell $ (fromCharArray (replicate i '\t')) <> s <> " "

-- | Just prints the string s.
identifier' :: forall e. PrettyPrinter String e
identifier' s = tell $ s <> " "

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

-- | Just prints the comment, preceded by a space and "--".
comment' :: forall e. PrettyPrinter Comment e
comment' c = identifier' ( "--" <> c)

-- prettyPrintContext :: PerspectContext -> String
-- prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

prettyPrint :: forall a e. a -> PrettyPrinter a e -> Aff (PropDefsEffects e) String
prettyPrint t pp = prettyPrint' $ pp t

prettyPrint' :: forall e. PerspectText e -> Aff (PropDefsEffects e) String
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
  space
  traverse_ comment' commentAfter
  newline

-- Prints each of the comments before on a separate line. Then executes the given PerspectText
-- and follows that with the comment after, terminated by a newline.
withComments' :: forall e f. Comments f -> PerspectText e -> PerspectText e
withComments' (Comments{commentBefore, commentAfter}) p = do
  traverse_ comment commentBefore
  p
  traverse_ comment' commentAfter
  newline

getCommentBefore :: forall f. Comments f -> Array Comment
getCommentBefore (Comments {commentBefore}) = commentBefore

getCommentAfter :: forall f. Comments f -> Array Comment
getCommentAfter (Comments {commentAfter}) = commentAfter

property :: forall e. PerspectText e -> PropertyName -> PropertyValueWithComments -> PerspectText e
property keyword prop = indent (\pvcomments -> do
  withComments' pvcomments
    (keyword *> identifier' (prop <> " =") *> simpleValue (propertyValue pvcomments)))

publicProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
publicProperty = property (identifier "public")

privateProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
privateProperty = property (identifier "private")

roleProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
roleProperty = property (pure unit)

context :: forall e. Array ID -> PrettyPrinter PerspectContext e
context definedResources (PerspectContext c) = do
  withComments (\r->r.comments) contextDeclaration c
  publicProperties
  privateProperties
  -- Sort the roles according to, first, their type, second, their occurrence.
  (bindings :: Array (Maybe (PerspectRol))) <- traverse (liftAff <<< getRole) (join (values c.rolInContext))
  traverse_ (indent roleBinding) (sortBy compareOccurrences (catMaybes bindings))
  where
    contextDeclaration :: PerspectContextProperties -> PerspectText e
    contextDeclaration x = identifier x.pspType *> identifier' x.displayName

    publicProperties = do
      -- LET OP: de buitenrol is geen integraal onderdeel van de context!
      maybeBuitenRol <- liftAff $ getRole c.buitenRol
      case maybeBuitenRol of
        (Just (PerspectRol buitenRol)) -> strMapTraverse_ publicProperty buitenRol.properties
        Nothing -> pure unit

    privateProperties = strMapTraverse_ privateProperty props where
      props = let (BinnenRol{properties}) = c.binnenRol
        in properties

    roleBinding :: PerspectRol -> PerspectText e
    roleBinding (PerspectRol r) = do
      -- NB: This is the role of the context - not yet its binding!
      let (binding :: ID) = (unsafePartial $ fromJust r.binding)
      let (occurrence :: String) = maybe "" id (roleIndexNr r.id)
      case elemIndex binding definedResources of
        Nothing -> do -- binding is not a BuitenRol of a context defined at top level in the Text.
          maybeRole <- liftAff $ getRole binding
          case maybeRole of
            Nothing -> reference r
            (Just (PerspectRol role)) -> do
              case role.pspType == "model:Perspectives$BuitenRol" of
                true -> do -- The role is a BuitenRol of some context.
                  maybeContext <- liftAff $ getContext role.context
                  case maybeContext of
                    Nothing -> reference r
                    (Just contxt) -> do
                      withComments' r.comments (identifier $ r.pspType <> " => ")
                      indent (context definedResources) contxt
                false -> reference role -- The role is a RoleInContext of some context.
        otherwise -> reference r
      strMapTraverse_ roleProperty r.properties

    reference :: PerspectRolProperties -> PerspectText e
    reference r = withComments' r.comments (identifier $ r.pspType <> " => " <> (maybe "" id r.binding))


strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> StrMap a -> m Unit
strMapTraverse_ f map = foldM (\z s a -> f s a) unit map

sourceText :: forall e. PrettyPrinter PerspectContext e
sourceText (PerspectContext theText) = do
  withComments' theText.comments (identifier( "Text " <> theText.displayName))
  newline

  (Triple{object: definedContexts}) <- liftAff (theText.id ## ignoreCache ((constructRolGetter "psp:text_Item") >-> binding)) -- Dit zijn dus buitenrollen
  (contextIds :: Triple e) <- liftAff (theText.id ## ignoreCache ((constructRolGetter "psp:text_Item") >-> binding >-> rolContext)) -- en dit zijn de contexten bij die buitenrollen.
  traverse_ (ppContext definedContexts) (tripleObjects contextIds)

  where
    ppContext :: Array ID -> ID -> PerspectText e
    ppContext definedContexts id = do
      mc <- liftAff $ getContext id
      case mc of
        (Just c) -> context definedContexts c
        Nothing -> pure unit
